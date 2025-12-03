#https://unctad.org/topic/least-developed-countries/list 
#this is the current list of all ldc determined by the UN

library(readr)
library(dplyr)

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

master <- master %>%
  mutate(
    is_ldc = ifelse(country %in% ldc_list, "LDC", "Non-LDC")
  )


master_ldc_growth <- master %>%
  filter(is_ldc == "LDC") %>%                  # only LDC countries
  group_by(country) %>%                        # work country by country
  arrange(year, .by_group = TRUE) %>%         # sort years within each country
  mutate(
    gdp_growth = (gdp_per_capita - lag(gdp_per_capita)) /
      lag(gdp_per_capita) * 100    # year-to-year % growth
  ) %>%
  ungroup()

asia_ldc <- master_ldc_growth %>%
  filter(continent == "Asia", !is.na(gdp_growth))

ggplot(asia_ldc,
       aes(x = year, y = gdp_growth, group = country, colour = country)) +
  geom_hline(yintercept = 7, linetype = "dashed") +
  geom_line(alpha = 0.7) +
  geom_point(size = 0.8) +
  labs(
    title = "GDP per capita growth in Asian LDCs",
    x = "Year",
    y = "GDP per capita growth (%)",
    colour = "Country"
  ) +
  theme_minimal()

oceania_ldc <- master_ldc_growth %>%
  filter(continent == "Oceania", !is.na(gdp_growth))

ggplot(oceania_ldc,
       aes(x = year, y = gdp_growth, group = country, colour = country)) +
  geom_hline(yintercept = 7, linetype = "dashed") +
  geom_line(alpha = 0.7) +
  geom_point(size = 0.8) +
  labs(
    title = "GDP per capita growth in Oceanian LDCs",
    x = "Year",
    y = "GDP per capita growth (%)",
    colour = "Country"
  ) +
  theme_minimal()

na_ldc <- master_ldc_growth %>%
  filter(continent == "North America", !is.na(gdp_growth))

ggplot(na_ldc,
       aes(x = year, y = gdp_growth, group = country, colour = country)) +
  geom_hline(yintercept = 7, linetype = "dashed") +
  geom_line(alpha = 0.7) +
  geom_point(size = 0.8) +
  labs(
    title = "GDP per capita growth in North American LDCs",
    x = "Year",
    y = "GDP per capita growth (%)",
    colour = "Country"
  ) +
  theme_minimal()

africa_ldc <- master_ldc_growth %>%
  filter(continent == "Africa", !is.na(gdp_growth))

ggplot(africa_ldc,
       aes(x = year, y = gdp_growth, group = country, colour = country)) +
  geom_hline(yintercept = 7, linetype = "dashed") +
  geom_line(alpha = 0.7) +
  geom_point(size = 0.8) +
  labs(
    title = "GDP per capita growth in African LDCs",
    x = "Year",
    y = "GDP per capita growth (%)",
    colour = "Country"
  ) +
  theme_minimal()






