setwd("C:/Users/ishaa/OneDrive/Y1 Uni/Introduction to Data Science/Group Project/data sets")
library(tidyverse)

gdp <- read_csv("gdp-per-capita-worldbank.csv")

gdp_long <- gdp %>%
  rename(`GDP per capita PPP`  = `GDP per capita, PPP (constant 2017 international $)`) %>%
  select(Country = Entity, Code, Year, `GDP per capita PPP`) %>%
  filter(!is.na(`GDP per capita PPP`)) %>%
  arrange(Year, Country)


write_csv(gdp_long, "gdp_per_capita_clean.csv")
