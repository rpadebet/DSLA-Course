
# More Data can be got from here
# https://packages.revolutionanalytics.com/datasets/AirOnTime87to12/

install.packages("RMySQL")
library(RMySQL)
drv<-dbDriver(drvName = "MySQL")
con = dbConnect(drv, 
                dbname = "dplyr", 
                host = "courses.csrrinzqubik.us-east-1.rds.amazonaws.com", 
                port = 3306, 
                user = "student",
                password = "datacamp")


# List tables
dbListTables(con)

# Read tables to get all the data into R
flts<-dbReadTable(con, "dplyr")

# Query tables to get a few rows
flts_data<-dbGetQuery(con,"SELECT * FROM dplyr limit 10;")

# alternatively just refer to it with tbl and perform operations
library(dplyr)
nyc_flights<-tbl(con,"dplyr")

glimpse(nyc_flights)  ## like str function but for tbl

nyc_flights%>%group_by(carrier)%>%
    summarise(n_flights = n(),avg_delay = mean(arr_delay))%>%
    arrange(desc(avg_delay))


# Connect to local postgres database and dumpt this table there
library(RPostgreSQL)

drv2<- dbDriver("PostgreSQL")
con_local<-dbConnect(drv2,
                     dbname="spam",
                     host="rohit-lubuntu",
                     port=5432,
                     user="rohit",
                     password="rohit123")

dbListTables(con_local)

dbWriteTable(con_local,'nyc_flights_2013',as.data.frame(nyc_flights), row.names=FALSE)

dbListTables(con_local)

pg_flts_count<-dbGetQuery(con_local,"SELECT count(*) FROM nyc_flights_2013;")
pg_flts<-dbGetQuery(con_local,"SELECT * FROM nyc_flights_2013 WHERE origin = 'EWR' limit 100;")

# Modern way of doing this

myPostgres<- src_postgres(dbname="spam",
                          host="rohit-lubuntu",
                          port=5432,
                          user="rohit",
                          password="rohit123")

flights<-tbl(myPostgres,"nyc_flights_2013")

dim(flights)
colnames(flights)
head(flights)

# For Sqllite, a database can be created in a folder with a csv file
library(RSQLite)

mySqlite<-src_sqlite("/Users/rohitpittu/R Projects/Big Data with R/Big-Data-Analytics-with-R/Chapter 5/need_data",create = F)
need <-tbl(mySqlite,"need")
head(need)

query.2 <- tbl(mySqlite,sql( "SELECT EE_BAND, PROP_AGE, PROP_TYPE, 
                       AVG(Econs2012) AS 'AVERAGE_ELEC_2012' 
                       FROM need 
                       GROUP BY EE_BAND, PROP_AGE, PROP_TYPE 
                       ORDER BY EE_BAND, PROP_TYPE ASC"))

query.2

copy_to(mySqlite,as.data.frame(query.2),"need_query2")

dbListTables(mySqlite$con)
glimpse(query.2)


# Copy the need table over to Postgres DB

dbListTables(myPostgres$con)
copy_to(myPostgres, as.data.frame(query.2),name = 'bigdata_need_query2',temporary=FALSE)
dbWriteTable(con_local,'bigdata_need',as.data.frame(need), row.names=FALSE)
dbListTables(con_local)

myPostgres<- src_postgres(dbname="spam",
                          host="rohit-lubuntu",
                          port=5432,
                          user="rohit",
                          password="rohit123")


library(readr)
flights_2014<-read_csv(file = "/Users/rohitpittu/Desktop/flights_2014.csv",progress = T)
flts_2014_db<-tbl(myPostgres,'flights_2014')


# R Query (Very quick but memory consumption is 200MB plus)
system.time(
flights_2014 %>%filter(DISTANCE<2000)%>%
    filter(ARR_DELAY>0 || DEP_DELAY>0)%>%
    group_by(DAY_OF_WEEK)%>%
    summarise(avg_dep_delay = mean(DEP_DELAY),
              avg_arr_delay = mean(ARR_DELAY),
              total_delay = mean((ARR_DELAY + DEP_DELAY)/AIR_TIME),
              avg_distance = mean(DISTANCE),
              flights = n())%>%
    arrange(desc(total_delay))
)


# Database Query (takes 100X long time but no memory is used locally )


    result_db<-flts_2014_db %>%
        filter(distance<2000)%>%
        filter(arr_delay>0 || dep_delay>0)%>%
        group_by(day_of_week)%>%
        summarise(avg_dep_delay = mean(dep_delay),
                  avg_arr_delay = mean(arr_delay),
                  total_delay = mean((arr_delay+dep_delay)/air_time),
                  avg_distance = mean(distance),
                  flights = n())%>%
        arrange(desc(total_delay))
    
    system.time(collect(result_db))

  
