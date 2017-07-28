
## Loading the libraries
library(sparklyr)
library(dplyr)
library(nycflights13)
library(ggplot2)

## Exploring the data
str(flights)
dim(flights)

str(airlines)
dim(airlines)

## Loading the data into Spark
sc <- spark_connect(master="local")

sc <- spark_connect(master="spark://54.174.31.86:4040")

flights <- copy_to(sc, flights, "flights")
airlines <- copy_to(sc, airlines, "airlines")
src_tbls(sc)

## dplyr package Verbs in sparklyr

# Select
select(flights, year:day, arr_delay, dep_delay)

# Filter
filter(flights, dep_delay > 1000)

# Arrange
arrange(flights, desc(dep_delay))

# Summarize
summarise(flights, mean_dep_delay = mean(dep_delay))

# Mutate
mutate(flights, speed = distance / air_time * 60)

# Piping
c4 <- flights %>%
  filter(month == 5, day == 17, carrier %in% c('UA', 'WN', 'AA', 'DL')) %>%
  select(carrier, dep_delay, air_time, distance) %>%
  arrange(carrier) %>%
  mutate(air_time_hours = air_time / 60)

# Grouping
c4 %>%
  group_by(carrier) %>%
  summarize(count = n(), mean_dep_delay = mean(dep_delay))

# Collecting (getting data into R memory)
carrierhours <- collect(c4)
# Test the significance of pairwise differences and plot the results (inside local R)
with(carrierhours, pairwise.t.test(air_time, carrier))
ggplot(carrierhours, aes(carrier, air_time_hours)) + geom_boxplot()

# Compute (calculate results but store results as table in spark)
compute(c4, 'carrierhours')
src_tbls(sc)

# SQL Queries
bestworst <- flights %>%
  group_by(year, month, day) %>%
  select(dep_delay) %>% 
  filter(dep_delay == min(dep_delay) || dep_delay == max(dep_delay))
# Get SQL query
sql_render(bestworst)
# Execute spark SQL query
bestworst

# Example 2: Rank each flight within a daily
ranked <- flights %>%
  group_by(year, month, day) %>%
  select(dep_delay) %>% 
  mutate(rank = rank(desc(dep_delay)))
sql_render(ranked)
ranked


# Joins
flights %>% left_join(airlines)
flights %>% left_join(airlines, by = "carrier")
flights %>% left_join(airlines, by = c("carrier", "carrier"))

# Getting sample data from spark
sample_n(flights, 10)
sample_frac(flights, 0.01)

# Writing and Reading Files from Spark and HDFS
spark_write_parquet(tbl, "hdfs://hdfs.company.org:9000/hdfs-path/data")
tbl <- spark_read_parquet(sc, "data", "hdfs://hdfs.company.org:9000/hdfs-path/data")


# Example: Reading and Writing files
temp_csv <- tempfile(fileext = ".csv")
temp_parquet <- tempfile(fileext = ".parquet")
temp_json <- tempfile(fileext = ".json")

# Copy IRIS data to Spark
iris_tbl <- copy_to(sc, iris)

spark_write_csv(iris_tbl, temp_csv)
iris_csv_tbl <- spark_read_csv(sc, "iris_csv", temp_csv)

spark_write_parquet(iris_tbl, temp_parquet)
iris_parquet_tbl <- spark_read_parquet(sc, "iris_parquet", temp_parquet)

spark_write_json(iris_tbl, temp_json)
iris_json_tbl <- spark_read_json(sc, "iris_json", temp_json)

src_tbls(sc)


# Hive and Hive functions such as datediff() annd current_date()
flights %>% 
  mutate(flight_date = paste(year,month,day,sep="-"),
         days_since = datediff(current_date(), flight_date)) %>%
  group_by(flight_date,days_since) %>%
  tally() %>%
  arrange(-days_since)


# Disconnect the session
spark_disconnect(sc)
