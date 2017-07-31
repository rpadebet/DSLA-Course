#!/bin/bash
set -x -e

# check for master node
IS_MASTER=false
if grep isMaster /mnt/var/lib/info/instance.json | grep true;
then
    IS_MASTER=true
fi


if [ "$IS_MASTER" = true ];
then
# Update Packages
sudo yum -y update
sudo yum -y install libcurl-devel openssl-devel # used for devtools

# Install RStudio Server
wget -P /tmp https://download2.rstudio.org/rstudio-server-rhel-1.0.153-x86_64.rpm
sudo yum -y install --nogpgcheck /tmp/rstudio-server-rhel-1.0.153-x86_64.rpm

# Make User rstudio-user with same password
sudo useradd -m rstudio-user
echo -e "rstudio-user\nrstudio-user"|sudo passwd rstudio-user

sudo mkdir /tmp/flights
sudo chmod -R 777 /tmp/flights

# Begin Downloading the data
echo "Beginning download of flight files ..."

for i in {1987..2008}
do echo "$(date) $i Download"
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

