
library(tidyverse)
df <- read_csv("~/Downloads/master_dataset.csv")

#clean the data
neet_clean <- df %>%
  mutate(
    year = as.integer(year),
    neet_rate = as.numeric(neet_rate)
  ) %>%
  filter(!is.na(neet_rate),
         !is.na(continent),
         continent != "Antarctica")

neet_trimmed <- neet_clean %>%
  group_by(continent, year) %>%
  summarise(neet_trimmed = mean(neet_rate, trim = 0.1, na.rm = TRUE)) %>%
  ungroup()

continent_colors <- c(
  "Africa" = "#E69F00",        
  "Asia" = "#D55E00",          
  "Europe" = "#0072B2",        
  "North America" = "#009E73", 
  "South America" = "#CC79A7", 
  "Oceania" = "#F0E442"        
)

print(ggplot(neet_trimmed, aes(x = year, y = neet_trimmed, color = continent)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = continent_colors) +
  labs(
    title = "NEET Rate by Continent (10% Trimmed Mean)",
    x = "Year",
    y = "Trimmed Mean NEET Rate (%)",
    color = "Continent"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5)
  ))
