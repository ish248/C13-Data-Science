

library(tidyverse)

df <- read_csv("~/Downloads/data sets/youth-not-in-education-employment-training.csv")

df_tidy <- df %>%
  rename(
    Country = Entity,
    Year = Year,
    Neet_rate = `Share of youth not in education, employment or training, total (% of youth population)`
  ) %>%
  select(Country, Year, Neet_rate)
write_csv(df_tidy, "~/Desktop/neet_rate_tidy.csv")

