library(dplyr)
library(readr)
library(ggplot2)

master_clean <- read_csv("data/processed/master_clean_with_pop.csv")
continents   <- read_csv("data/processed/continents_clean.csv")

master_clean <- master_clean %>%
  left_join(
    continents %>%
      rename(country = Country,
             continent = Continent),
    by = "country"
  )

names(master_clean)

master_clean <- master_clean %>%
  group_by(country) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    gdp_growth = (gdp / lag(gdp) - 1) * 100,
    hit_target = gdp_growth >= 7
  ) %>%
  ungroup()

growth_cont <- master_clean %>%
  filter(!is.na(continent)) %>%   # drop aggregates + unknowns
  group_by(year, continent) %>%
  summarise(mean_growth = mean(gdp_growth, na.rm = TRUE), .groups = "drop")


master_clean <- master_clean %>%
  filter(!is.na(continent)) 


library(dplyr)
library(ggplot2)
library(zoo)   # for rollmean()

# 1. Average GDP growth by year & continent (same as before, but drop NA continents)
growth_cont <- master_clean %>%
  filter(!is.na(continent)) %>%
  group_by(year, continent) %>%
  summarise(mean_growth = mean(gdp_growth, na.rm = TRUE), .groups = "drop")

# 2. Smooth with a 3-year moving average within each continent
growth_cont_smooth <- growth_cont %>%
  arrange(continent, year) %>%
  group_by(continent) %>%
  mutate(
    mean_growth_smooth = rollmean(mean_growth, k = 3,
                                  fill = NA, align = "center")
  ) %>%
  ungroup()

# 3. Colour palette for continents
continent_cols <- c(
  "Africa"        = "#E69F00",
  "Asia"          = "#D55E00",
  "Europe"        = "#0072B2",
  "North America" = "#009E73",
  "South America" = "#CC79A7",
  "Oceania"       = "#F0E442"
)

# 4. Smoothed graph
p_growth_cont_smooth <- ggplot(growth_cont_smooth,
       aes(x = year, y = mean_growth_smooth, colour = continent)) +
  geom_line(linewidth = 1., na.rm = TRUE) +   # use smoothed series
  # 7% UN target line
  geom_hline(yintercept = 7, linetype = "dashed", colour = "black") +
  annotate("text",
           x = min(growth_cont_smooth$year, na.rm = TRUE) + 2,
           y = 7.4,
           label = "UN target rate (7%)",
           hjust = 0,
           size = 4,
           colour = "black") +
  scale_colour_manual(values = continent_cols) +
  scale_y_continuous(limits = c(-2, 10)) +     # zoom in on relevant range
  scale_x_continuous(breaks = seq(1990, 2022, by = 5)) +
  labs(
    title  = "Average Annual GDP Growth by Continent (3-year Moving Average)",
    x      = "Year",
    y      = "Average GDP Growth (%)",
    colour = "Continent"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position   = "right",
    panel.grid.minor  = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title        = element_text(face = "bold", size = 16)
  )


# --- 1. Define the LDC list (same as before) ---
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

# Add LDC flag
master_clean <- master_clean %>%
  mutate(is_ldc = country %in% ldc_list)

# --- 2. Average GDP growth by year & continent for LDCs only ---
growth_cont_ldc <- master_clean %>%
  filter(is_ldc, !is.na(continent)) %>%       # LDCs + real continents only
  group_by(year, continent) %>%
  summarise(mean_growth = mean(gdp_growth, na.rm = TRUE), .groups = "drop")

# --- 3. Smooth with 3-year moving average ---
growth_cont_ldc_smooth <- growth_cont_ldc %>%
  arrange(continent, year) %>%
  group_by(continent) %>%
  mutate(
    mean_growth_smooth = zoo::rollmean(mean_growth, k = 3,
                                       fill = NA, align = "center")
  ) %>%
  ungroup()

# --- 4. Plot: LDCs only ---
p_growth_ldc_smooth <- ggplot(growth_cont_ldc_smooth,
       aes(x = year, y = mean_growth_smooth, colour = continent)) +
  geom_line(linewidth = 1, na.rm = TRUE) +
  geom_hline(yintercept = 7, linetype = "dashed", colour = "black") +
  annotate("text",
           x = min(growth_cont_ldc_smooth$year, na.rm = TRUE) + 2,
           y = 7.4,
           label = "UN target rate (7%)",
           hjust = 0,
           size = 4,
           colour = "black") +
  scale_colour_manual(values = continent_cols) +
  scale_y_continuous(limits = c(-2, 10)) +
  scale_x_continuous(breaks = seq(1990, 2022, by = 5)) +
  labs(
    title  = "Average Annual GDP Growth in LDCs by Continent (3-year Moving Average)",
    x      = "Year",
    y      = "Average GDP Growth (%)",
    colour = "Continent"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position    = "right",
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title         = element_text(face = "bold", size = 16)
  )


# All countries with continent + gdp_growth
growth_all_box <- master_clean %>%
  filter(!is.na(continent))

p_box_continent <- ggplot(growth_all_box,
       aes(x = continent, y = gdp_growth, fill = continent)) +
  geom_boxplot(outlier.alpha = 0.3) +
  geom_hline(yintercept = 7, linetype = "dashed", colour = "black") +
  annotate("text",
           x = 0.6, y = 7.6,
           label = "UN target rate (7%)",
           hjust = 0,
           size = 3.5,
           colour = "black") +
  scale_fill_manual(values = continent_cols, guide = "none") +
  coord_cartesian(ylim = c(-10, 20)) +
  labs(
    title = "Distribution of Annual GDP Growth by Continent",
    x     = "Continent",
    y     = "Annual GDP Growth (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

growth_ldc_box <- master_clean %>%
  filter(is_ldc, !is.na(continent))
growth_ldc_box <- master_clean %>%
  filter(is_ldc, !is.na(continent))

p_box_ldc <- ggplot(growth_ldc_box,
       aes(x = continent, y = gdp_growth, fill = continent)) +
  geom_boxplot(outlier.alpha = 0.3) +
  geom_hline(yintercept = 7, linetype = "dashed", colour = "black") +
  annotate("text",
           x = 0.6, y = 7.4,
           label = "UN target rate (7%)",
           hjust = 0,
           size = 3.5,
           colour = "black") +
  scale_fill_manual(values = continent_cols, guide = "none") +
  coord_cartesian(ylim = c(-10, 20)) +
  labs(
    title = "Distribution of Annual GDP Growth in LDCs by Continent",
    x     = "Continent",
    y     = "Annual GDP Growth (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

library(dplyr)
library(ggplot2)
library(zoo)
library(scales)

# Step 1: proportion by year × continent
prop_cont <- master_clean %>%
  filter(!is.na(continent)) %>%
  group_by(year, continent) %>%
  summarise(
    prop_target = mean(hit_target, na.rm = TRUE),
    .groups = "drop"
  )

# Step 2: apply 3-year centred moving average
prop_cont_smooth <- prop_cont %>%
  arrange(continent, year) %>%
  group_by(continent) %>%
  mutate(
    prop_smooth = rollmean(prop_target, k = 3, fill = NA, align = "center")
  ) %>%
  ungroup()

# Step 3: plot
p_prop_continent_smooth <- ggplot(prop_cont_smooth,
       aes(x = year, y = prop_smooth, colour = continent)) +
  geom_line(linewidth = 1.1, na.rm = TRUE) +
  scale_colour_manual(values = continent_cols) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  scale_x_continuous(breaks = seq(1990, 2022, 5)) +
  labs(
    title  = "Share of Countries Achieving ≥7% Annual GDP Growth\n(3-Year Moving Average)",
    x      = "Year",
    y      = "Share achieving ≥7% GDP growth",
    colour = "Continent"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 16)
  )

# Step 1: proportion by year × LDC continent
prop_cont_ldc <- master_clean %>%
  filter(is_ldc, !is.na(continent)) %>%
  group_by(year, continent) %>%
  summarise(
    prop_target = mean(hit_target, na.rm = TRUE),
    .groups = "drop"
  )

# Step 2: 3-year moving average smoothing
prop_cont_ldc_smooth <- prop_cont_ldc %>%
  arrange(continent, year) %>%
  group_by(continent) %>%
  mutate(
    prop_smooth = rollmean(prop_target, k = 3, fill = NA, align = "center")
  ) %>%
  ungroup()

# Step 3: plot
p_prop_ldc_smooth <- ggplot(prop_cont_ldc_smooth,
       aes(x = year, y = prop_smooth, colour = continent)) +
  geom_line(linewidth = 1.1, na.rm = TRUE) +
  scale_colour_manual(values = continent_cols) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  scale_x_continuous(breaks = seq(1990, 2022, 5)) +
  labs(
    title  = "Share of LDCs Achieving ≥7% Annual GDP Growth\n(3-Year Moving Average)",
    x      = "Year",
    y      = "Share of LDCs achieving ≥7% GDP growth",
    colour = "Continent"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 16)
  )

dir.create("plots", showWarnings = FALSE)

plots <- list(
  "continent_gdp_growth_smoothed" = p_growth_cont_smooth,
  "ldc_gdp_growth_smoothed"       = p_growth_ldc_smooth,
  "gdp_growth_boxplot_continent"  = p_box_continent,
  "gdp_growth_boxplot_ldc"        = p_box_ldc,
  "prop_7pct_continent_smooth"    = p_prop_continent_smooth,
  "prop_7pct_ldc_smooth"          = p_prop_ldc_smooth
)

for (name in names(plots)) {
  ggsave(
    filename = file.path("plots", paste0(name, ".png")),
    plot = plots[[name]],
    width = 10, height = 6, dpi = 300
  )
}

dir.create("plots", showWarnings = FALSE)

plots <- list(
  "continent_gdp_growth_smoothed" = p_growth_cont_smooth,
  "ldc_gdp_growth_smoothed"       = p_growth_ldc_smooth,
  "gdp_growth_boxplot_continent"  = p_box_continent,
  "gdp_growth_boxplot_ldc"        = p_box_ldc,
  "prop_7pct_continent_smooth"    = p_prop_continent_smooth,
  "prop_7pct_ldc_smooth"          = p_prop_ldc_smooth
)

for (name in names(plots)) {
  ggsave(
    filename = file.path("plots", paste0(name, ".png")),
    plot = plots[[name]],
    width = 10, height = 6, dpi = 300
  )
}



