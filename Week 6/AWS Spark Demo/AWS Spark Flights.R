## Using Spark with h2o

#remove.packages("h2o")
#install_version("h2o","3.10.5.2")
#install.packages("h2o", type="source", repos="http://h2o-release.s3.amazonaws.com/h2o/rel-vajda/4/R")

#install_version("rsparkling","0.2.1")

library(devtools)
library(sparklyr)
library(rsparkling)
library(h2o)
library(dplyr)
library(ggplot2)
library(ggthemes)


Sys.setenv(SPARK_HOME="/usr/lib/spark") # on aws cluster

config <- list(
    "spark.dynamicAllocation.enabled" = "false",
    "sparklyr.shell.driver-memory"= "12G",
    "sparklyr.shell.num-executors" = 3,
    "sparklyr.shell.executor-memory" = "12G",
    "sparklyr.shell.executor-cores" = 1
)

sc <- spark_connect(master = "yarn-client", config = config) # on aws cluster

#Connecting to sparkling water
h2o_context(sc)
h2o_flow(sc)

#View the status of H2O in cloud
h2o.clusterInfo()
h2o.clusterStatus()

# find the tables in spark (hive tables also show up here but may need caching for speed)
# Code to pre-load some tables into Spark - not necessary when hive tables are present
src_tbls(sc)


# Cache Hive tables into Spark and get reference
tbl_cache(sc, 'flights_hive')
flights_spark <- tbl(sc, 'flights_hive')

tbl_cache(sc, 'airlines_hive')
airlines_spark <- tbl(sc, 'airlines_hive')

tbl_cache(sc, 'airports_hive')
airports_spark <- tbl(sc, 'airports_hive')

test<-as_h2o_frame(sc,airlines_spark)

# Perform the best worst analysis

flights_bestworst.spark <- flights_spark %>%
    select(year,month,uniquecarrier,depdelay) %>%
    group_by(year,month) %>%
    filter(depdelay == min(depdelay) | depdelay == max(depdelay) )%>%
    arrange(month)

flights_bestworst.spark

# Speed va Memory: system.time() shows the time it takes to run the query and bring it into R memory
system.time(collect(flights_bestworst.spark))
#'        user  system elapsed
#'       0.088   0.000   4.475    # on real data




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
    filter(!is.na(arrdelay) & !is.na(depdelay) & !is.na(distance))%>%
    filter(year >=1987 & year<=2007)%>%
    mutate(gain = depdelay - arrdelay)%>%
    mutate(speed = distance/(arrtime-deptime))%>%   # Use CRSARRTIME/CRSDEPTIME which are scheduled times for actual data
    select(year, month, arrdelay, depdelay, distance, uniquecarrier, origin,dest, gain,speed)%>%
    left_join(airlines_spark, by = c('uniquecarrier' = 'code'))%>%
    inner_join(y=airports_spark, by = c('origin'='faa'))%>%
    inner_join(y=airports_spark, by = c('dest'='faa'))%>%
    select(year, month, arrdelay, depdelay, distance, uniquecarrier,
           origin,dest, gain,speed,description, name.x,name.y)%>%
    rename(dest_airport=name.y)%>%
    rename(airline_name=description)%>%  # Bug in dplyr causes us to do this thrice
    rename(origin_airport=name.x)%>%
    select(gain,distance,depdelay,speed,uniquecarrier,airline_name)

glimpse(model_data_spk)


###########################################################
##                    H2O Machine Learning
###########################################################



# Push the data into h2o
model_data_spk_h2o<-as_h2o_frame(sc,model_data_spk)
#model_data_spk_h2o<-as.h2o(model_data_spk)
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
h2o.performance(model_spk,newdata = data_spk$test) # on test set

#Predictions on trainset
predictions_train<-h2o.predict(model_spk,data_spk$train)

# Predict on test set
predictions_test<-h2o.predict(model_spk,newdata = data_spk$test)

# Download the model
h2o.saveModel(model_spk,path = "./",force = T)


