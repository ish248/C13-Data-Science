setwd("C:/Users/ishaa/OneDrive/Y1 Uni/Introduction to Data Science/Group Project/data sets")
library(tidyverse)
library(dplyr)
library(ggplot2)

master_df <- read.csv("master_dataset.csv")

master_df <- master_df %>%
  group_by(continent, year) %>%
  mutate(neet_avg = mean(neet_rate, na.rm = TRUE)) %>%
  ungroup()


continent_cols <- c(
  "Asia"         = "#D55E00",  
  "Europe"       = "#0072B2",
  "Africa"       = "#E69F00",
  "North America"= "#009E73",  
  "South America"= "#CC79A7",  
  "Oceania"      = "#F0E442")   


NEET_Graph <- ggplot(master_df,
                     aes(x = year,
                         y = neet_avg,
                         colour = continent,
                         group = continent)) +
  
  geom_line(size = 2) +
  scale_colour_manual(values = continent_cols)+
  
  xlim(1990,2020)+
  ylim(10,40)+
    
  labs(title = "Average NEET over years by continent",
       x = "Year",
       y = "Average NEET (%)",
       colour = "Continent")+
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5)
  )

  
print(NEET_Graph)

