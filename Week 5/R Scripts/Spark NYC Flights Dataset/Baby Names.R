# Setup
library(sparklyr)
library(dplyr)
library(babynames)
library(ggplot2)
library(dygraphs)
library(rbokeh)

# Connect to spark and copy the data over
sc <- spark_connect(master = "local")
babynames_tbl <- copy_to(sc, babynames, "babynames")
applicants_tbl <- copy_to(sc, applicants, "applicants")


# Total US births
birthsYearly <- applicants_tbl %>%
  mutate(male = ifelse(sex == "M", n_all, 0), female = ifelse(sex == "F", n_all, 0)) %>%
  group_by(year) %>%
  summarize(Male = sum(male) / 1000000, Female = sum(female) / 1000000) %>%
  arrange(year) %>%
  collect

# Plot
birthsYearly %>%
  dygraph(main = "Total US Births (SSN)", ylab = "Millions") %>%
  dySeries("Female") %>%
  dySeries("Male") %>%
  dyOptions(stackedGraph = TRUE) %>%
  dyRangeSelector(height = 20)


# Create a topnames dataframe
topNames_tbl <- babynames_tbl %>%
  filter(year >= 1986) %>%  
  group_by(name, sex) %>%
  summarize(count = as.numeric(sum(n))) %>%
  filter(count > 1000) %>%
  select(name, sex)

# Join with original data frame
filteredNames_tbl <- babynames_tbl %>%
  filter(year >= 1986) %>%
  inner_join(topNames_tbl)

# Create a data frame with ranks
yearlyNames_tbl <- filteredNames_tbl %>%
  group_by(year, name, sex) %>%
  summarize(count = as.numeric(sum(n)))

# Register it as a table
sdf_register(yearlyNames_tbl, "yearlyNames")

# Cache the table
tbl_cache(sc, "yearlyNames")



## Most popular names in 1986
# Get the ranks for 1986
topNames1986_tbl <- yearlyNames_tbl %>%
  filter(year == 1986) %>%
  group_by(name, sex) %>%
  summarize(count = sum(count)) %>%
  group_by(sex) %>%
  mutate(rank = min_rank(desc(count))) %>%
  filter(rank < 5) %>%
  arrange(sex, rank) %>%
  select(name, sex, rank) %>%
  sdf_register("topNames1986")

# Cache the table
tbl_cache(sc, "topNames1986")

# Join table with original yearly names table
topNames1986Yearly <- yearlyNames_tbl %>%
  inner_join(select(topNames1986_tbl, sex, name)) %>%
  collect

# Plot the data
ggplot(topNames1986Yearly, aes(year, count, color=name)) +
  facet_grid(~sex) +
  geom_line() +
  ggtitle("Most Popular Names of 1986")

## Most popular names for 2014
topNames2014_tbl <- yearlyNames_tbl %>%
  filter(year == 2014) %>%
  group_by(name, sex) %>%
  summarize(count = sum(count)) %>%
  group_by(sex) %>%
  mutate(rank = min_rank(desc(count))) %>%
  filter(rank < 5) %>%
  arrange(sex, rank) %>%
  select(name, sex, rank) %>%
  sdf_register("topNames2014")
tbl_cache(sc, "topNames2014")
topNames2014Yearly <- yearlyNames_tbl %>%
  inner_join(select(topNames2014_tbl, sex, name)) %>%
  collect
ggplot(topNames2014Yearly, aes(year, count, color=name)) +
  facet_grid(~sex) +
  geom_line() +
  ggtitle("Most Popular Names of 2014")


## Shared names between Males and Females and popularity
sharedName <- babynames_tbl %>%
  mutate(male = ifelse(sex == "M", n, 0), female = ifelse(sex == "F", n, 0)) %>%
  group_by(name) %>%
  summarize(Male = as.numeric(sum(male)), 
            Female = as.numeric(sum(female)),
            count = as.numeric(sum(n)),
            AvgYear = round(as.numeric(sum(year * n) / sum(n)),0)) %>%
  filter(Male > 30000 & Female > 30000) %>%
  collect

# Plot
rbokeh::figure(width = NULL, height = NULL, 
       xlab = "Log10 Number of Males", 
       ylab = "Log10 Number of Females",
       title = "Top shared names (1880 - 2014)") %>%
  ly_points(log10(Male), log10(Female), data = sharedName,
            color = AvgYear, size = scale(sqrt(count)),
            hover = list(name, Male, Female, AvgYear), legend = FALSE)


spark_disconnect(sc)
