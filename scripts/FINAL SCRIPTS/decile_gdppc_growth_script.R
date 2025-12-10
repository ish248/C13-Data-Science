library(tidyverse)
library(dplyr)
library(ggplot2)

full_dataset <- read.csv("master_dataset.csv")
avg_gdp <- read.csv("master_dataset.csv")


avg_gdp <- full_dataset %>%
  group_by(country) %>%
  summarise(avg_gdp_per_capita = mean(gdp_per_capita, na.rm = TRUE)) %>%
  arrange(desc(avg_gdp_per_capita)) %>%
  mutate(
    decile = ntile(desc(avg_gdp_per_capita), 10),
    decile = case_when(
      decile == 1 ~ "1st",
      decile == 2 ~ "2nd",
      decile == 3 ~ "3rd",
      decile == 4 ~ "4th",
      decile == 5 ~ "5th",
      decile == 6 ~ "6th",
      decile == 7 ~ "7th",
      decile == 8 ~ "8th",
      decile == 9 ~ "9th",
      decile == 10 ~ "10th"
    )
  )

# 1. Identify the decile 10 countries
decile10_countries <- avg_gdp %>%
  filter(decile == "10th") %>%
  pull(country)

# 2. Filter the full dataset to only those countries
decile10_data <- full_dataset %>%
  filter(country %in% decile10_countries)

# 3. Calculate GDP growth and remove NA rows
gdp_growth_table <- decile10_data %>%
  arrange(country, year) %>% 
  group_by(country) %>%
  mutate(
    gdp_growth = (gdp_per_capita - lag(gdp_per_capita)) /
      lag(gdp_per_capita) * 100
  ) %>%
  ungroup() %>%
  filter(!is.na(gdp_growth)) %>%        # ← remove first-year NA rows
  select(country, year, gdp_growth, continent)

ggplot(gdp_growth_table, aes(x = year, 
                             y = gdp_growth, 
                             colour = continent, 
                             group = country)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 7, linetype = "dashed", linewidth = 1) +   # ← added line at 7%
  scale_colour_manual(values = c(
    "Africa" = "#E69F00",
    "Asia" = "#D55E00",
    "Europe" = "#0072B2",
    "North America" = "#009E73",
    "South America" = "#CC79A7",
    "Oceania" = "#F0E442"
  )) +
  labs(
    title = "GDP Per Capita Growth Over Time (Decile 10 Countries)",
    x = "Year",
    y = "GDP Growth (%)",
    colour = "Continent"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )
