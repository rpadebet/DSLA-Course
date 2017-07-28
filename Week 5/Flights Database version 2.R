# Database version 2

# What if instead of just flights from NYC, all flights data was given ?
# Lets download the 2008 data - 108MB zipped 700MB unzipped

url<- "http://stat-computing.org/dataexpo/2009/2008.csv.bz2"
download.file(url = url,destfile = "./Week 5/2008.csv.bz2",method="curl")

library(readr)

flights_2008<-read_csv(file = "./Week 5/2008.csv.bz2",progress = T,trim_ws = T)
glimpse(flights_2008)

# 7 million records with 29 columns

# Renaming columns to lowercase to make database queries easier
colnames<-names(flights_2008)
names(flights_2008)<-tolower(colnames)

library(dplyr)

library(RPostgreSQL)

myPostgres<- src_postgres(dbname="spam",
                          host="rohit-lubuntu",
                          port=5432,
                          user="rohit",
                          password="rohit123")

dbListTables(myPostgres$con)

# Imported the file as a table into database using the createTable.sql script

flights.db<-tbl(src = myPostgres, from = "flights_2008" )

# Perform the best worst analysis again inside our Postgres database
flights_bestworst.db <- flights.db %>%
    select(year,month,uniquecarrier,depdelay) %>% 
    group_by(year,month) %>%
    filter(depdelay == min(depdelay) | depdelay == max(depdelay) )%>%
    arrange(month)

flights_bestworst.db


# Speed va Memory: system.time() shows the time it takes to run the query and bring it into R memory
system.time(collect(flights_bestworst.db))
#'        user  system elapsed 
#'        0.056   0.004   8.725 


###############################################################################
#                        Machine Learning
#'          We now do something more interesting like in local R
###############################################################################


# Get a reference to it into R
airlines.db.own<-tbl(src = myPostgres, from = "airlines" )
airports.db.own<-tbl(src = myPostgres, from = "airports" )

## Machine Learning Steps

## Clean up the data 
#           Remove all the NA rows

## Create New Features
#'          1. We create a feature called 'gain' defined as 'dep_delay'-'arr_delay'
#'          2. We also create another feature called 'speed' defined as 'distance' by 'airtime'

## Join it with other lookup tables

model_data.2008<-flights.db%>%
    filter(!is.na(arrdelay) & !is.na(depdelay) & !is.na(distance) & !is.na(airtime) & airtime>0)%>%
    mutate(gain = depdelay - arrdelay)%>%
    mutate(speed = distance/airtime)%>%
    select(year, month, arrdelay, depdelay, distance, uniquecarrier, origin,dest, gain,speed)%>%
    left_join(airlines.db.own, by = c('uniquecarrier' = 'carrier'))%>%  
    inner_join(y=airports.db.own, by = c('origin'='faa'))%>%  
    inner_join(y=airports.db.own, by = c('dest'='faa'))%>% 
    select(year, month, arrdelay, depdelay, distance, uniquecarrier, 
           origin,dest, gain,speed,name, name.x,name.y)%>%
    rename(dest.airport=name)%>%
    rename(airline.name=name.x)%>%  # Bug in dplyr causes us to do this thrice
    rename(origin.airport=name.y)


glimpse(model_data.2008)

# Fit a model using H2o
library(h2o)

h2o.init(port = 54321,nthreads = -1)

# Push the data into h2o
model_data.db.h2o<-as.h2o(model_data.2008)
model_data.db.h2o$uniquecarrier<-as.factor(model_data.db.h2o$uniquecarrier)

# Split the data
data_db <- h2o.splitFrame(model_data.db.h2o,ratios=c(.8),seed = 1234)
names(data_db)<- c("train","test")

# Fit the model
model_db <-h2o.glm(x = c("speed","distance","depdelay","uniquecarrier"),
                   y = "gain",
                   training_frame = data_db$train,
                   nfolds = 10,
                   family = "gaussian")

# Evaluate the performance
h2o.performance(model_db)
# on test set
h2o.performance(model_db,newdata = data_db$test)

#Predictions on trainset
predictions_train<-h2o.predict(model_db,data_db$train)

# Predict on test set
predictions_test<-h2o.predict(model_db,newdata = data_db$test)

# Combining the predictions with the data and merging the frames
data_db$train$prediction<-predictions_train
data_db$train$data<-"train"
data_db$test$prediction<-predictions_test
data_db$test$data<-"test"
data_h2o<-h2o.rbind(data_db$train,data_db$test)

# Convert to tbl to process in plots (Now we need to get into R)
model_data_pred.db<-as_tibble(data_h2o)

# Calculate average gains by predicted decile
model_deciles <- model_data_pred.db%>%
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
carrier<-model_data_pred.db%>%
    group_by(airline.name)%>%
    summarize(gain = mean(gain), prediction = mean(prediction), freq = n())%>%
    filter(freq>10000)

# Plot actual gains and predicted gains by airline carrier
ggplot(carrier, aes(gain, prediction)) + 
    geom_point(alpha = 0.75, color = 'red', shape = 3) +
    geom_abline(intercept = 0, slope = 1, alpha = 0.15, color = 'blue') +
    geom_text(aes(label = substr(airline.name, 1, 20)), size = 3, alpha = 0.75, vjust = -1) +
    labs(title='Average Gains Forecast', x = 'Actual', y = 'Predicted')+
    theme_economist()

