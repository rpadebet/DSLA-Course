#!/bin/bash

# check for master node
IS_MASTER=false

if grep isMaster /mnt/var/lib/info/instance.json | grep true;then
   IS_MASTER=true
fi


echo "Starting hadoop script"

if [ "$IS_MASTER" = true ] ;
then
    echo "This is master, so moving files to hadoop"

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
    aws s3 cp s3://rohit-folder/hiveSql.sql /home/hadoop

    echo "moving files into Hive"

    # Run the script to create HIVE tables
    hive -f /home/hadoop/hiveSql.sql

    echo "hive completed"

fi

echo "Exited Master and exiting this step"




