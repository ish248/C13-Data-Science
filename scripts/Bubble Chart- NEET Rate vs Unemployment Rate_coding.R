
library(tidyverse)
library(ggplot2)

df <- read_csv("~/Downloads/master_dataset.csv") %>%
  mutate(
    neet_rate = as.numeric(neet_rate),
    unemployment_rate = as.numeric(unemployment_rate),
    gdp_per_capita = as.numeric(gdp_per_capita)
  ) %>%
  filter(!is.na(neet_rate),
         !is.na(unemployment_rate),
         !is.na(gdp_per_capita),
         !is.na(continent),
         continent != "Antarctica")


stats <- df %>%
  group_by(continent) %>%
  summarise(
    corr = cor(neet_rate, unemployment_rate, use = "complete.obs"),
    slope = coef(lm(neet_rate ~ unemployment_rate))[2]
  )


annotation_text <- paste0(
  "Correlation & Slope by Continent:\n",
  paste(stats$continent, 
        " | r = ", sprintf("%.2f", stats$corr), 
        " | slope = ", sprintf("%.2f", stats$slope),
        collapse = "\n")
)


continent_colors <- c(
  "Africa" = "#E69F00",
  "Asia" = "#D55E00",
  "Europe" = "#0072B2",
  "North America" = "#009E73",
  "South America" = "#CC79A7",
  "Oceania" = "#F0E442"
)


p <- ggplot(df, aes(x = unemployment_rate, y = neet_rate,
                    size = gdp_per_capita, color = continent)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = continent_colors) +
  scale_size(range = c(2, 12)) +
  labs(
    title = "NEET Rate vs Unemployment Rate with GDP per Capita",
    x = "Unemployment Rate (%)",
    y = "NEET Rate (%)",
    color = "Continent",
    size = "GDP per Capita"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5)
  ) +
  annotate("text", x = 25, y = 60, label = annotation_text, hjust = 0, size = 4.5)

print(p)
