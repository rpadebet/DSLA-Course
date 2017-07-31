#!/bin/bash

# check for master node
IS_MASTER=false
if grep isMaster /mnt/var/lib/info/instance.json | grep true;
then
    IS_MASTER=true
fi


if[ "$IS_MASTER" = true ];
  then
      sudo mkdir /tmp/flights
      sudo chmod -R 777 /tmp/flights

      # Begin Downloading the data
      echo "Beginning download of flight files ..."

      for i in {1987..1988}
      do
	  echo "$(date) $i Download"
	  fnam=$i.csv.bz2
	  sudo wget -O /tmp/flights/$fnam http://stat-computing.org/dataexpo/2009/$fnam
	  echo "$(date) $i Unzip"
	  sudo bunzip2 /tmp/flights/$fnam
      done

      # Download airline carrier data
      echo "Beginning download of carrier data ..."
      sudo wget -O /tmp/airlines.csv http://www.transtats.bts.gov/Download_Lookup.asp?Lookup=L_UNIQUE_CARRIERS

      # Download airports data
      echo "Beginning download of airports data ..."
      sudo wget -O /tmp/airports.csv https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat


      # Download the hadoop script from S3
      aws s3 cp s3://rohit-folder/hadoop_script.sh /home/hadoop/


fi #Master Node
