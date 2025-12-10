library(tidyverse)

gdp <- read_csv("gdp-per-capita-worldbank.csv", show_col_types = FALSE)

gdp$Code <- NULL

gdp <- gdp %>% 
  rename(`GDP per capita` = `GDP per capita, PPP (constant 2017 international $)`)


write_csv(gdp, "gdp_per_capita_clean.csv")
