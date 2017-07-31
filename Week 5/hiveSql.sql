--------------------------------------------
--  Create metadata for flights & load data
--------------------------------------------
CREATE EXTERNAL TABLE IF NOT EXISTS flights_hive
(
year int,
month int,
dayofmonth int,
dayofweek int,
deptime int,
crsdeptime int,
arrtime int,
crsarrtime int,
uniquecarrier string,
flightnum int,
tailnum string,
actualelapsedtime int,
crselapsedtime int,
airtime string,
arrdelay int,
depdelay int,
origin string,
dest string,
distance int,
taxiin string,
taxiout string,
cancelled int,
cancellationcode string,
diverted int,
carrierdelay string,
weatherdelay string,
nasdelay string,
securitydelay string,
lateaircraftdelay string
)
ROW FORMAT DELIMITED
FIELDS TERMINATED BY ','
LINES TERMINATED BY '\n'
TBLPROPERTIES("skip.header.line.count"="1");

-- Load data into table
LOAD DATA INPATH '/user/rstudio-user/flights' INTO TABLE flights_hive;

------------------------------------------
-- Create metadata for airlines & load data
------------------------------------------
CREATE EXTERNAL TABLE IF NOT EXISTS airlines_hive
(
Code string,
Description string
)
ROW FORMAT SERDE 'org.apache.hadoop.hive.serde2.OpenCSVSerde'
WITH SERDEPROPERTIES
(
"separatorChar" = '\,',
"quoteChar"     = '\"'
)
STORED AS TEXTFILE
TBLPROPERTIES("skip.header.line.count"="1");

-- Load data into table
LOAD DATA INPATH '/user/rstudio-user/airlines' INTO TABLE airlines_hive;


------------------------------------------
-- Create metadata for airports & load data
------------------------------------------
CREATE EXTERNAL TABLE IF NOT EXISTS airports_hive
(
id string,
name string,
city string,
country string,
faa string,
icao string,
lat double,
lon double,
alt int,
tz_offset double,
dst string,
tz_name string
)
ROW FORMAT SERDE 'org.apache.hadoop.hive.serde2.OpenCSVSerde'
WITH SERDEPROPERTIES
(
"separatorChar" = '\,',
"quoteChar"     = '\"'
)
STORED AS TEXTFILE;

-- Load data into table
LOAD DATA INPATH '/user/rstudio-user/airports' INTO TABLE airports_hive;


-- Query the data to verify it is accessible via HIVE

SELECT * FROM airports_hive limit 10;
SELECT * FROM airlines_hive limit 10;
SELECT * FROM flights_hive limit 10;
