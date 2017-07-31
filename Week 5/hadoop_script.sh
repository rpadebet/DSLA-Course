#!/bin/bash

# Move Downloaded files into hadoop

# Create directories on hdfs for new user
hadoop fs -mkdir /user/rstudio-user
hadoop fs -chmod 777 /user/rstudio-user

# Copy flight data to HDFS
hadoop fs -mkdir /user/rstudio-user/flights/
hadoop fs -put /tmp/flights /user/rstudio-user/

# Copy airline data to HDFS
hadoop fs -mkdir /user/rstudio-user/airlines/
hadoop fs -put /tmp/airlines.csv /user/rstudio-user/airlines

# Copy airport data to HDFS
hadoop fs -mkdir /user/rstudio-user/airports/
hadoop fs -put /tmp/airports.csv /user/rstudio-user/airports

# Download the hiveSql.sql script from S3
aws s3 cp s3://rohit-folder/flights/hiveSql.sql /home/hadoop

# Run the script to create HIVE tables
hive -f /home/hadoop/hiveSql.sql

# Install R packages

sudo R --no-save <<EOF
install.packages(c('RJSONIO', 'itertools','digest', 'Rcpp', 'functional','httr', 'plyr', 'stringr', 'reshape2','caTools','rJava', 'devtools', 'DBI','ggplot2','dplyr','data.table','rsparkling','tidyr','RMySQL','readr','ggthemes','R.methodsS3','Hmisc', 'memoise','rjson','sparklyr','tidyr','lubridate','tidyquant','tidytext'),repos="http://cran.rstudio.com")
EOF



