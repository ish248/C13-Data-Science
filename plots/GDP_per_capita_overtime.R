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


ggplot(continent_gdp, aes(x = year, y = mean_gdp, colour = continent)) +
  geom_line(size = 1.2) +
  scale_colour_manual(values = continent_colors) +
  labs(
    title = "GDP per Capita Over Time by Continent",
    x = "Year",
    y = "Mean GDP per Capita (PPP, USD)",
    colour = "Continent"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

