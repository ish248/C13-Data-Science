continents <- read.csv("~/Documents/C13-Data-Science/data/processed/continents_clean.csv")
gdp_per_capita <- read.csv("~/Documents/C13-Data-Science/data/processed/gdp_per_capita_clean.csv")
neet <-  read.csv("~/Documents/C13-Data-Science/data/processed/neet_rate_tidy.csv")
population <- read.csv("~/Documents/C13-Data-Science/data/devstatus_population_gdp.csv")

library(dplyr)



# Rename columns so they match across all datasets
gdp_per_capita <- gdp_per_capita %>%
  rename(
    country = Entity,
    year    = Year
  )

neet <- neet %>%
  rename(
    country   = Country,
    year      = Year,
    neet_rate = Neet_rate
  )

population <- population %>%
  rename(
    country = Entity,
    year    = Year
  )

continents <- continents %>%
  rename(
    country   = Country,
    continent = Continent
  )

# Now create the master dataset
master <- gdp_per_capita %>%
  left_join(neet,       by = c("country", "year")) %>%
  left_join(population, by = c("country", "year")) %>%
  left_join(continents, by = "country")

View(master)


master <- gdp_per_capita %>%
  left_join(neet, by = c("country", "year")) %>%
  left_join(population, by = c("country", "year")) %>%
  left_join(continents, by = "country")

View(master)

master_clean <- master %>%
  select(country, year, GDP.per.capita, pop_total, neet_rate)

View(master_clean)

master_clean <- master %>%
  select(country, year, GDP.per.capita, pop_total, neet_rate) %>%
  rename(
    gdp_pc     = GDP.per.capita,
    population = pop_total
  )

master_clean <- master_clean %>%
  mutate(
    gdp = gdp_pc * population
  )

master_clean <- master_clean %>%
  mutate(gdp_billion = gdp / 1e9)


library(readr)

#Adding the LDCSs 

url <- "https://raw.githubusercontent.com/ish248/C13-Data-Science/refs/heads/main/data/processed/master_dataset.csv" 
master <- read_csv(url)
library(ggplot2)

ldc_list <- c(
  
  "Angola", "Benin", "Burkina Faso", "Burundi", "Central African Republic",
  "Chad", "Comoros", "Democratic Republic of Congo", "Djibouti", "Eritrea",
  "Ethiopia", "Gambia", "Guinea", "Guinea-Bissau", "Lesotho", "Liberia",
  "Madagascar", "Malawi", "Mali", "Mauritania", "Mozambique", "Niger",
  "Rwanda", "Senegal", "Sierra Leone", "Somalia", "South Sudan", "Sudan",
  "Togo", "Uganda", "Tanzania", "Zambia",
  
  
  "Afghanistan", "Bangladesh", "Cambodia", "Laos",
  "Myanmar", "Nepal", "East Timor", "Yemen",
  
  
  "Haiti",
  
  
  "Kiribati", "Solomon Islands", "Tuvalu"
)

master_clean <- master_clean %>%
  mutate(
    is_ldc = country %in% ldc_list
  )


head(master_clean)

write_csv(master_clean, "master_clean_with_pop.csv")







