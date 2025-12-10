library(readr)
library(dplyr)

url <- "https://raw.githubusercontent.com/ish248/C13-Data-Science/refs/heads/main/data/processed/master_dataset.csv" 
master <- read_csv(url)
library(ggplot2)

continent_gdp <- master %>%
  group_by(continent, year) %>%
  summarise(mean_gdp = mean(gdp_per_capita, na.rm = TRUE)) %>%
  ungroup()

continent_colors <- c(
  "Africa" = "#E69F00",
  "Asia" = "#D55E00",
  "Europe" = "#0072B2",
  "North America" = "#009E73",
  "South America" = "#CC79A7",
  "Oceania" = "#F0E442"
)

growth_data <- continent_gdp %>%
  arrange(continent, year) %>%                
  group_by(continent) %>%                     
  mutate(
    growth_rate = (mean_gdp - lag(mean_gdp)) / lag(mean_gdp) * 100
  ) %>%
  ungroup()

library(ggplot2)

ggplot(growth_data, aes(x = year, y = growth_rate, colour = continent)) +
  geom_line(size = 1.1) +
  scale_colour_manual(values = continent_colors) +
  geom_hline(yintercept = 7, linetype = "dashed") +   # SDG target: 7% growth
  labs(
    title = "GDP per Capita Growth Rate by Continent",
    x = "Year",
    y = "GDP per Capita Growth Rate (%)",
    colour = "Continent"
  ) +
  theme_minimal()
