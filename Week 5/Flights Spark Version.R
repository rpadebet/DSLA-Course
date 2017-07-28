
## Using Spark

library(sparklyr)
library(dplyr)
library(ggplot2)
library(ggthemes)



Sys.setenv(SPARK_HOME="/usr/lib/spark") # on aws cluster
Sys.setenv(SPARK_HOME="/home/rohit/spark") # on linux ubuntu vm

config <- spark_config()

sc <- spark_connect(master = "yarn-client", config = config) # on aws cluster
sc <- spark_connect(master = "local", config = config)

# find the tables in spark (hive tables also show up here but may need caching for speed)
# Code to pre-load some tables into Spark - not necessary when hive tables are present
source('~/Dropbox/R Projects/Spark/Database imports.R')
src_tbls(sc)


# Cache Hive tables into Spark and get reference
tbl_cache(sc, 'flights_hive')
flights_spark <- tbl(sc, 'flights_hive')

tbl_cache(sc, 'airlines_hive')
airlines_spark <- tbl(sc, 'airlines_hive')

tbl_cache(sc, 'airports_hive')
airports_spark <- tbl(sc, 'airports_hive')



# Get existing spark tables (if present already)
flights_spark <-tbl(sc,'flights_spark')
airlines_spark <- tbl(sc, 'airlines_spark')
airports_spark <- tbl(sc, 'airports_spark')
flights08_spark <-tbl(sc,'flights08_spark')

# Perform the best worst analysis
# Perform the best worst analysis again inside our Postgres database
flights_bestworst.spark <- flights_spark %>%
  select(year,month,uniquecarrier,depdelay) %>% 
  group_by(year,month) %>%
  filter(depdelay == min(depdelay) | depdelay == max(depdelay) )%>%
  arrange(month)

flights_bestworst.spark

# Speed va Memory: system.time() shows the time it takes to run the query and bring it into R memory
system.time(collect(flights_bestworst.spark))
#'        user  system elapsed 
#'       0.072   0.000   1.563   # on dummy data




###############################################################################
#                        Machine Learning
#'          We now do something more interesting
###############################################################################


## Machine Learning Steps

## Clean up the data 
#           Remove all the NA rows
## Create New Features
#'          1. We create a feature called 'gain' defined as 'dep_delay'-'arr_delay'
#'          2. We also create another feature called 'speed' defined as 'distance' by 'airtime'
## Join it with other lookup tables


model_data_spk <- flights_spark %>%
  filter(!is.na(arrdelay) & !is.na(depdelay) & !is.na(distance) & !is.na(airtime) & airtime>0)%>%
  filter(year >=1987 & year<=2007)%>%
  mutate(gain = depdelay - arrdelay)%>%
  mutate(speed = distance/airtime)%>%
  select(year, month, arrdelay, depdelay, distance, uniquecarrier, origin,dest, gain,speed)%>%
  left_join(airlines_spark, by = c('uniquecarrier' = 'carrier'))%>%  
  inner_join(y=airports_spark, by = c('origin'='faa'))%>%  
  inner_join(y=airports_spark, by = c('dest'='faa'))%>% 
  select(year, month, arrdelay, depdelay, distance, uniquecarrier, 
         origin,dest, gain,speed,name, name.x,name.y)%>%
  rename(dest_airport=name)%>%
  rename(airline_name=name.x)%>%  # Bug in dplyr causes us to do this thrice
  rename(origin_airport=name.y)%>%
  select(gain,distance,depdelay,speed,uniquecarrier,airline_name)

glimpse(model_data_spk)  


# Partition the data into training and validation sets
model_partition <- model_data_spk %>% 
  sdf_partition(train = 0.8, valid = 0.2, seed = 1234)

# Fit a linear model
ml1 <- ml_linear_regression(x = model_partition$train, 
                            reponse = "gain",
                            features = c("distance","depdelay","speed"))

# Summarize the linear model
summary(ml1)

# Calculate average gains by predicted decile
model_deciles <- lapply(model_partition, function(x) {
  sdf_predict(ml1, x) %>%
    mutate(decile = ntile(desc(prediction), 10)) %>%
    group_by(decile) %>%
    summarize(gain = mean(gain)) %>%
    select(decile, gain) %>%
    collect()
})

# Create a summary dataset for plotting
deciles <- rbind(
  data.frame(data = 'train', model_deciles$train),
  data.frame(data = 'valid', model_deciles$valid),
  make.row.names = FALSE
)

# Plot average gains by predicted decile
deciles %>%
  ggplot(aes(factor(decile), gain, fill = data)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(title = 'Average gain by predicted decile', x = 'Decile', y = 'Minutes')


# Select data from an out of time sample
data_2008 <- flights08_spark %>%
  filter(!is.na(arrdelay) & !is.na(depdelay) & !is.na(distance) & !is.na(airtime) & airtime>0)%>%
  filter(year == 2008)%>%
  mutate(gain = depdelay - arrdelay)%>%
  mutate(speed = distance/airtime)%>%
  select(year, month, arrdelay, depdelay, distance, uniquecarrier, origin,dest, gain,speed)%>%
  left_join(airlines_spark, by = c('uniquecarrier' = 'carrier'))%>%  
  inner_join(y=airports_spark, by = c('origin'='faa'))%>%  
  inner_join(y=airports_spark, by = c('dest'='faa'))%>% 
  select(year, month, arrdelay, depdelay, distance, uniquecarrier, 
         origin,dest, gain,speed,name, name.x,name.y)%>%
  rename(dest_airport=name)%>%
  rename(airline_name=name.x)%>%  # Bug in dplyr causes us to do this thrice
  rename(origin_airport=name.y)
  
  
# Summarize data by carrier
carrier <- sdf_predict(ml1, data_2008) %>%
  group_by(airline_name) %>%
  summarize(gain = mean(gain), 
            prediction = mean(prediction), 
            freq = n()) %>%
  filter(freq > 10000) %>%
  collect

# Plot actual gains and predicted gains by airline carrier
ggplot(carrier, aes(gain, prediction)) + 
  geom_point(alpha = 0.75, color = 'red', shape = 3) +
  geom_abline(intercept = 0, slope = 1, alpha = 0.15, color = 'blue') +
  geom_text(aes(label = substr(airline_name, 1, 20)), size = 3, alpha = 0.75, vjust = -1) +
  labs(title='Average Gains Forecast', x = 'Actual', y = 'Predicted')


# read from aws s3 data
tbl <- spark_read_parquet(sc,"s3://us-east-1.elasticmapreduce.samples/flightdata/input/")


