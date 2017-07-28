
## Using Spark with h2o

library(sparklyr)
library(rsparkling)
library(h2o)
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

###########################################################
##                    H2O Machine Learning
###########################################################

h2o.init(port = 54321,nthreads = -1)

# Push the data into h2o
model_data_spk_h2o<-as.h2o(model_data_spk)
model_data_spk_h2o$uniquecarrier<-as.factor(model_data_spk_h2o$uniquecarrier)

# Split the data
data_spk <- h2o.splitFrame(model_data_spk_h2o,ratios=c(.8),seed = 1234)
names(data_spk)<- c("train","test")

# Fit the model
model_spk <-h2o.glm(x = c("speed","distance","depdelay","uniquecarrier"),
                    y = "gain",
                    training_frame = data_spk$train,
                    nfolds = 10,
                    family = "gaussian")

# Evaluate the performance
h2o.performance(model_spk)
# on test set
h2o.performance(model_spk,newdata = data_spk$test)

#Predictions on trainset
predictions_train<-h2o.predict(model_spk,data_spk$train)

# Predict on test set
predictions_test<-h2o.predict(model_spk,newdata = data_spk$test)


# Combining the predictions with the data and merging the frames
data_spk$train$prediction<-predictions_train
data_spk$train$data<-"train"
data_spk$test$prediction<-predictions_test
data_spk$test$data<-"test"

data_h2o<-h2o.rbind(data_spk$train,data_spk$test)

# Convert to tbl to process in plots (Now we need to get into R)
model_data_pred_spk<-as_tibble(data_h2o)

# Calculate average gains by predicted decile
model_deciles <- model_data_pred_spk%>%
    mutate(decile = ntile(desc(prediction),10))%>%
    group_by(decile,data) %>%
    summarize(gain = mean(gain)) %>%
    select(decile, gain,data)

# Plot average gains by predicted decile

model_deciles %>%
    ggplot(aes(factor(decile), gain, fill = data)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    labs(title = 'Average gain by predicted decile', x = 'Decile', y = 'Minutes')+
    theme_wsj()

# Summarize data by carrier
carrier<-model_data_pred_spk%>%
    group_by(airline_name)%>%
    summarize(gain = mean(gain), prediction = mean(prediction), freq = n())%>%
    filter(freq>10000)


# Plot actual gains and predicted gains by airline carrier
ggplot(carrier, aes(gain, prediction)) + 
    geom_point(alpha = 0.75, color = 'red', shape = 3) +
    geom_abline(intercept = 0, slope = 1, alpha = 0.15, color = 'blue') +
    geom_text(aes(label = substr(airline_name, 1, 20)), size = 3, alpha = 0.75, vjust = -1) +
    labs(title='Average Gains Forecast', x = 'Actual', y = 'Predicted')+
    theme_economist()