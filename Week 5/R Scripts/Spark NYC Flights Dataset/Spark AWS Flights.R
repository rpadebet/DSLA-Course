
## Code to run on AWS Spark Cluster

library(sparklyr)
library(dplyr)
library(ggplot2)


Sys.setenv(SPARK_HOME="/usr/lib/spark")
config <- spark_config()


sc <- spark_connect(master = "yarn-client", config = config)


# read from aws s3 data
tbl <- spark_read_parquet(sc,"s3://us-east-1.elasticmapreduce.samples/flightdata/input/")

# Cache flights Hive table into Spark
tbl_cache(sc, 'flights')
flights_tbl <- tbl(sc, 'flights')

# Cache airlines Hive table into Spark
tbl_cache(sc, 'airlines')
airlines_tbl <- tbl(sc, 'airlines')

# Cache airports Hive table into Spark
tbl_cache(sc, 'airports')
airports_tbl <- tbl(sc, 'airports')



# Filter records and create target variable 'gain'
model_data <- flights_tbl %>%
    filter(!is.na(arrdelay) & !is.na(depdelay) & !is.na(distance)) %>%
    filter(depdelay > 15 & depdelay < 240) %>%
    filter(arrdelay > -60 & arrdelay < 360) %>%
    filter(year >= 2003 & year <= 2007) %>%
    left_join(airlines_tbl, by = c("uniquecarrier" = "code")) %>%
    mutate(gain = depdelay - arrdelay) %>%
    select(year, month, arrdelay, depdelay, distance, uniquecarrier, description, gain)

# Summarize data by carrier
model_data %>%
    group_by(uniquecarrier) %>%
    summarize(description = min(description), gain=mean(gain), 
              distance=mean(distance), depdelay=mean(depdelay)) %>%
    select(description, gain, distance, depdelay) %>%
    arrange(gain)

# Partition the data into training and validation sets
model_partition <- model_data %>% 
    sdf_partition(train = 0.8, valid = 0.2, seed = 5555)

# Fit a linear model
ml1 <- model_partition$train %>%
    ml_linear_regression(gain ~ distance + depdelay + uniquecarrier)

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
data_2008 <- flights_tbl %>%
    filter(!is.na(arrdelay) & !is.na(depdelay) & !is.na(distance)) %>%
    filter(depdelay > 15 & depdelay < 240) %>%
    filter(arrdelay > -60 & arrdelay < 360) %>%
    filter(year == 2008) %>%
    left_join(airlines_tbl, by = c("uniquecarrier" = "code")) %>%
    mutate(gain = depdelay - arrdelay) %>%
    select(year, month, arrdelay, depdelay, distance, uniquecarrier, description, gain, origin,dest)

# Summarize data by carrier
carrier <- sdf_predict(ml1, data_2008) %>%
    group_by(description) %>%
    summarize(gain = mean(gain), prediction = mean(prediction), freq = n()) %>%
    filter(freq > 10000) %>%
    collect

# Plot actual gains and predicted gains by airline carrier
ggplot(carrier, aes(gain, prediction)) + 
    geom_point(alpha = 0.75, color = 'red', shape = 3) +
    geom_abline(intercept = 0, slope = 1, alpha = 0.15, color = 'blue') +
    geom_text(aes(label = substr(description, 1, 20)), size = 3, alpha = 0.75, vjust = -1) +
    labs(title='Average Gains Forecast', x = 'Actual', y = 'Predicted')




