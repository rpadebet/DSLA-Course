# Database version

# If the same data is located in some database, how do we access it?
library(RMySQL)

# Get the driver for database type
drv<-dbDriver(drvName = "MySQL")

# Get the connection to the specific database
con = dbConnect(drv, 
                dbname = "dplyr", 
                host = "courses.csrrinzqubik.us-east-1.rds.amazonaws.com", 
                port = 3306, 
                user = "student",
                password = "datacamp")

# List tables
dbListTables(con)

# Read tables to get all the data into R
flights_ext_db<-dbReadTable(con, "dplyr")
glimpse(as.tbl(flights_ext_db))

# Or Query tables to get a few lines into R
# Query tables to get a few rows
flts_sample<-dbGetQuery(con,"SELECT * FROM dplyr limit 10;")

############################################################
#           Alternative : Better to not get the data into R
############################################################

# Query the table & run the SQL in the database itself &
# then get the results into R

library(dplyr)

flights.db<-tbl(con,"dplyr")
glimpse(flights.db)

#'   Another short cut without using driver or connection is src_<database>
mySQL = src_mysql( dbname = "dplyr", 
                   host = "courses.csrrinzqubik.us-east-1.rds.amazonaws.com", 
                   port = 3306, 
                   user = "student",
                   password = "datacamp")

flights.db<-tbl(src = mySQL, from = "dplyr" )

# Perform the best worst analysis again
flights_bestworst.db <- flights.db %>%
    select(year,month,day,carrier,flight,dep_delay,dep_time) %>% 
    group_by(year,month) %>%
    filter(dep_delay == min(dep_delay) | dep_delay == max(dep_delay) )%>%
    arrange(month)

flights_bestworst.db

# We get an error that min() function we need is not supported by this MySQL database.
# There is a workaround within MySQL, but let us just move this to our local Postgres database
library(RPostgreSQL)
myPostgres<- src_postgres(dbname="spam",
                          host="192.168.15.254",#192.168.15.254
                          port=5432,
                          user="rohit",
                          password="rohit123")

dbListTables(myPostgres$con)

dbGetQuery(myPostgres$con,"DROP TABLE nyc_flights_2013;") #Alternatively use dbRemoveTable()

# Now let us copy the table from the MySQL database to our Postgres Database
copy_to(dest = myPostgres,
        df = as.data.frame(flights.db), # needs to be a data.frame and not tbl
        name = 'nyc_flights_2013', # name of the new table in postgres
        temporary = F)   # to create a permanent table which persists after connection is lost


dbListTables(myPostgres$con)
dbGetQuery(myPostgres$con,"SELECT count(*) FROM nyc_flights_2013")

flights.db.own<-tbl(src = myPostgres, from = "nyc_flights_2013" )

# Perform the best worst analysis again inside our Postgres database
flights_bestworst.db.own <- flights.db.own %>%
    select(year,month,day,carrier,flight,dep_delay,dep_time) %>% 
    group_by(year,month) %>%
    filter(dep_delay == min(dep_delay) | dep_delay == max(dep_delay) )%>%
    arrange(month)

# to bring this into R memory use collect() [ You can also just type the name, but collect is good practice]
collect(flights_bestworst.db.own)

# We can get the underlying query via show_query() or explain()
show_query(flights_bestworst.db.own)

# Speed va Memory: system.time() shows the time it takes to run the query and bring it into R memory
system.time(collect(flights_bestworst.db.own))
#'        user  system elapsed 
#'        0.066   0.000   0.663 


###############################################################################
#                        Machine Learning
#'          We now do something more interesting like in local R
###############################################################################

# But first we need the look up tables in our database too!
library(nycflights13)
data("airports")  # metadata about airports
data("airlines")  # lookup table for airline with carrier codes

# Copy a 'airports' table
copy_to(dest = myPostgres,
        df = as.data.frame(airports), 
        name = 'airports', # name of the new table in postgres
        temporary = F) 

# Copy a 'airlines' table
copy_to(dest = myPostgres,
        df = as.data.frame(airlines), 
        name = 'airlines', # name of the new table in postgres
        temporary = F) 

# Check if tables exist
dbListTables(myPostgres$con)

# Remove the local copies
rm(airlines)
rm(airports)

# Get a reference to it into R
airlines.db.own<-tbl(src = myPostgres, from = "airlines" )
airports.db.own<-tbl(src = myPostgres, from = "airports" )

## Now we can do the Machine Learning Steps

## Clean up the data 
#           Remove all the NA rows

## Create New Features
#'          1. We create a feature called 'gain' defined as 'dep_delay'-'arr_delay'
#'          2. We also create another feature called 'speed' defined as 'distance' by 'airtime'

## Join it with other lookup tables

model_data.db<-flights.db.own%>%
    filter(!is.na(arr_delay) & !is.na(dep_delay) & !is.na(distance) & !is.na(air_time) & air_time>0)%>%
    mutate(gain = dep_delay - arr_delay)%>%
    mutate(speed = distance/air_time)%>%
    select(year, month, day, arr_delay, dep_delay, distance, carrier, origin,dest, gain,speed)%>%
    left_join(airlines.db.own, by = c('carrier' = 'carrier'))%>%  
    inner_join(y=airports.db.own, by = c('origin'='faa'))%>%  
    inner_join(y=airports.db.own, by = c('dest'='faa'))%>% 
    select(year, month,day, arr_delay, dep_delay, distance, carrier, 
           origin,dest, gain,speed,name, name.x,name.y)%>%
    rename(dest.airport=name)%>%
    rename(airline.name=name.x)%>%  # Bug in dplyr causes us to do this thrice
    rename(origin.airport=name.y)


glimpse(model_data.db)

# Fit a model using H2o
library(h2o)

h2o.init(port = 54321,nthreads = -1)

# Push the data into h2o
model_data.db.h2o<-as.h2o(model_data.db)
h2o.summary(model_data.db.h2o)
model_data.db.h2o$uniquecarrier<-as.factor(model_data.db.h2o$uniquecarrier)

# Split the data
data_db <- h2o.splitFrame(model_data.db.h2o,ratios=c(.8),seed = 1234)
names(data_db)<- c("train","test")

# Fit the model
model_db <-h2o.glm(x = c("speed","distance","dep_delay","carrier"),
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

