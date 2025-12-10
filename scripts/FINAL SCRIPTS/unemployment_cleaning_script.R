library(tidyverse)
library(dbplyr)

unemp_data <- read.csv("SDG_0852_SEX_AGE_RT_A-filtered-2025-11-30.csv")

unemp_data_clean <- unemp_data %>% 
  select(-source.label, -indicator.label, -obs_status.label, -note_classif.label, -note_indicator.label, -note_source.label) %>% 
  filter(classif1.label != "Age (Youth, adults): 15-24", classif1.label != "Age (Youth, adults): 25+") %>% 
  filter(sex.label != "Male", sex.label != "Female")

unemp_data_clean <- unemp_data_clean %>% 
  select(-sex.label, -classif1.label) %>% 
  rename(Year = time) %>%
  rename(Country = ref_area.label) %>%
  rename('Unemployment Rate' = obs_value)

View(unemp_data_clean)
