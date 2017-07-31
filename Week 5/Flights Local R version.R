## Local R version

# Load the required libraries
library(dplyr)
library(ggplot2)
library(nycflights13)
library(ggthemes)

# Load the data
data("flights")   # flights that departed NYC in 2013
data("airports")  # metadata about airports
data("airlines")  # lookup table for airline with carrier codes

# Explore the data 
dim(flights)
dim(airports)
dim(airlines)

head(flights)
head(airports)
head(airlines)

glimpse(flights)
str(flights)
glimpse(airports)
glimpse(airlines)

# Get the Best and Worst flights with delayed departure every year, month and day

# You can do this
flights_sub<-select(flights,year,month,day,carrier,flight,dep_delay,sched_dep_time,dep_time)
flights_group<-group_by(flights_sub,year,month)
flights_worst<-filter(flights_group,dep_delay==max(dep_delay,na.rm = T))
flights_best<-filter(flights_group,dep_delay==min(dep_delay,na.rm = T))
flights_bestworst <- bind_rows(flights_best,flights_worst)
flights_bw_ord<-arrange(flights_bestworst,month)
flights_bw_ord
View(flights_bw_ord)


# Or we can chain it together
flights_bestworst_ord <- flights %>%
    select(year,month,day,carrier,flight,dep_delay,sched_dep_time,dep_time) %>% 
    group_by(year,month) %>%
    filter(dep_delay == min(dep_delay,na.rm = T) | dep_delay == max(dep_delay, na.rm = T) )%>%
    arrange(month)

flights_bestworst_ord


###############################################################################
#                        Machine Learning
#'          We now do something more interesting
###############################################################################

## Clean up the data 
#           Remove all the NA rows

model_data<-flights%>%
    filter(!is.na(arr_delay) & !is.na(dep_delay) & !is.na(distance)) # remove rows with NAs

## Create New Features
#'          1. We create a feature called 'gain' defined as 'dep_delay'-'arr_delay'
#'          2. We also create another feature called 'speed' defined as 'distance' by 'airtime'

model_data<-flights%>%
    filter(!is.na(arr_delay) & !is.na(dep_delay) & !is.na(distance) & !is.na(air_time))%>%
    # new code
    mutate(gain = (dep_delay - arr_delay))%>% # calculate new column gain
    mutate(speed = (distance/air_time))%>% # calculate new column speed
    select(year, month,day, arr_delay, dep_delay, distance, carrier, origin,dest, gain,speed)

## Join it with other lookup tables
model_data<-flights%>%
    filter(!is.na(arr_delay) & !is.na(dep_delay) & !is.na(distance) & !is.na(air_time))%>%
    mutate(gain = (dep_delay - arr_delay))%>%
    mutate(speed = (distance/air_time))%>%
    select(year, month, day, arr_delay, dep_delay, distance, carrier, origin,dest, gain,speed)%>%
    # new code
    left_join(airlines,by = c('carrier' = 'carrier'))%>%  # join with airlines for airline name
    inner_join(y=airports, by = c('origin'='faa'))%>% # join with airport for origin airport name 
    inner_join(y=airports, by = c('dest'='faa'))%>% # join with airport for destination airport name
    select(year, month,day, arr_delay, dep_delay, distance, carrier, 
           origin,dest, gain,speed,name, name.x,name.y)%>%
    rename(dest.airport=name,airline.name=name.x,origin.airport=name.y)

glimpse(model_data)
    

## Split the data into training and test sets
library(caret)

# We use the dataset to create a partition (80% training 20% testing)
set.seed(1000)
index <- createDataPartition(model_data$gain, p=0.80, list=FALSE)
# select 20% of the data for testing
testset <- model_data[-index,]
# select 80% of data to train the models
trainset <- model_data[index,]

# Verify the partition
dim(trainset)
dim(testset)

# Fit a linear model
model_local<-train(form = gain~speed+distance+dep_delay+carrier,
                   data = trainset,
                   method = 'lm',
                   metric ='RMSE',
                   trControl = trainControl(method = "cv"))

print(model_local)
summary(model_local$finalModel)

# Predict using testdata
new_data<-subset(testset,select = -gain)

results<-predict(object = model_local,newdata = new_data)
test.results<-data.frame(obs=testset$gain,pred=results)
defaultSummary(test.results)


# Recombine the data with predictions for some analysis
trainset$prediction <-model_local$finalModel$fitted.values
trainset$data<-"train"
testset$prediction <-test.results$pred
testset$data<-"test"

model_data_pred<-bind_rows(trainset,testset)

# Calculate average gains by predicted decile
model_deciles <- model_data_pred%>%
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
carrier<-model_data_pred%>%
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


# Summarize data for a particular carrier
AA<-model_data_pred%>%
    filter(carrier=='AA')%>%
    select(dest.airport,gain,prediction,data)%>%
    group_by(dest.airport,data)%>%
    summarize(gain=mean(gain),prediction=mean(prediction),freq=n())%>%
    filter(freq>50)

# Plot actual gains and predicted gains for a carrier by destination city
ggplot(AA, aes(gain, prediction,color=data)) + 
    geom_point(alpha = 0.75, shape = 3) +
    geom_abline(intercept = 0, slope = 1, alpha = 0.15, color = 'blue') +
    geom_text(aes(label = substr(dest.airport, 1, 15)), size = 3, alpha = 1, vjust = -1,check_overlap = T) +
    labs(title='Average Gains Forecast', x = 'Actual', y = 'Predicted')+
    theme_gdocs()
