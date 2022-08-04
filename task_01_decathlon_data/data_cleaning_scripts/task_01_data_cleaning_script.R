# Read in the required libraries
library(tidyverse)
library(janitor)
library(readr)
library(assertr)
library(stringr)
library(here)

# Read in the raw data
decathlon <- read_rds(here("raw_data/decathlon.rds"))

# Change row_names to first column 'athlete_name'
decathlon <- tibble::rownames_to_column(decathlon, var = "athlete_name")

# Change athletes' names to title
decathlon <- decathlon %>% 
  mutate(athlete_name = str_to_title(athlete_name))

# Run `clean names` on the column names
decathlon_clean <- clean_names(decathlon)

# Remove the 'x' before any column names
decathlon_clean <- decathlon_clean %>% 
  rename_with(~ gsub("^x", "", .x))

# Relocate the rank, points, and competition columns to after athletes' names
decathlon_clean <- decathlon_clean %>% 
  relocate(c(rank:competition), .after = athlete_name)

# Source in column names
source(here("data_cleaning_scripts/event_columns.R"))

# Add in seconds units to the race columns
decathlon_clean <- decathlon_clean %>% 
  rename_with(~str_replace(., "$", "_(s)"), .cols = starts_with(race_columns))

# Add in meters units to the athletics event columns
decathlon_clean <- decathlon_clean %>% 
  rename_with(~str_replace(., "$", "_(m)"), .cols = starts_with(athletics_columns))

# Create an assertive program to check values against current world records
decathlon_clean %>% 
  verify(`100m_(s)` > 9.85) %>% 
  verify(`long_jump_(m)` < 8.95) %>% 
  verify(`shot_put_(m)` < 23.37) %>% 
  verify(`high_jump_(m)` < 2.45) %>% 
  verify(`400m_(s)` > 43.03) %>% 
  verify(`110m_hurdle_(s)` > 12.8) %>% 
  verify(`discus_(m)` < 74.08) %>% 
  verify(`pole_vault_(m)` < 6.21) %>% 
  verify(`javeline_(m)` < 98.48) %>% 
  verify(`1500m_(s)` > 230)

# View dataset
# view(decathlon_clean)

write_csv(decathlon_clean, "clean_data/decathlon_clean.csv")