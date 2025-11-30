library(dplyr)
library(readr)

continents_raw <-  read.csv("data/raw/data sets/continents-according-to-our-world-in-data.csv")


gdp_raw <-  read_csv("data/raw/data sets/gdp-per-capita-worldbank.csv")


wb_countries <- gdp_raw |>
  filter(!grepl("^OWID_", Code)) |>  # drop aggregates
  pull(Entity) |>
  unique()

continents_clean2 <- continents_raw |>
  filter(Entity %in% wb_countries) |>   # keep only WB countries
  filter(Continent != "Antarctica") |> 
  select(Entity, Continent) |>
  rename(
    country = Entity,
    continent = Continent
  ) |>
  distinct()

View(continents_clean2)

# Verdict -- Continents Clean 2 is an official list of recognised countries by the World Bank, and is also more compatible with the other datasets
# Therefore we will use attempt 2 for our script 

# Sanity check for attempt 2 

nrow(continents_clean2)                     # number of countries you ended with
any(duplicated(continents_clean2$country))  # should be FALSE
unique(continents_clean2$continent)         # should be 6 continents

write_csv(continents_clean2, "data/processed/continents_clean.csv")

