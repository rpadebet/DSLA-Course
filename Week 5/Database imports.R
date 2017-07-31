library(RPostgreSQL)
library(dplyr)

myPostgres<- src_postgres(dbname="spam",
                          host="rohit-lubuntu",#192.168.15.254
                          port=5432,
                          user="rohit",
                          password="rohit123")

#dbListTables(myPostgres$con)

flights_tbl<-tbl(src = myPostgres, from = "flights_2008" )
airlines_tbl<-tbl(src = myPostgres, from = "airlines" )
airports_tbl<-tbl(src = myPostgres, from = "airports" )

flights_tbl<-flights_tbl%>%
    collect(n = 1000000)%>%
    mutate( year = 2007)

flights_tbl_08<-flights_tbl%>%
    collect(n = 200000)

library(sparklyr)
Sys.setenv(SPARK_HOME="/home/rohit/spark") # on linux ubuntu vm
config <- spark_config()
sc <- spark_connect(master = "local", config = config)


# Copy the tbls over to spark
copy_to(sc,flights_tbl, name='flights_spark')
copy_to(sc,airlines_tbl, name='airlines_spark')
copy_to(sc,airports_tbl, name='airports_spark')
copy_to(sc,flights_tbl_08, name='flights08_spark')

rm(flights_tbl,airlines_tbl,airports_tbl,flights_tbl_08,myPostgres)


