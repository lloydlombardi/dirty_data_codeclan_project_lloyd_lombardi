# Read in the required libraries
library(tidyverse)
library(janitor)
library(readr)
library(assertr)
library(stringr)

# Read in the raw data
decathlon <- read_rds("raw_data/decathlon.rds")

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
source("data_cleaning_scripts/event_columns.R")

# Add in seconds units to the race columns
decathlon_clean <- decathlon_clean %>% 
  rename_with(~str_replace(., "$", "_(s)"), .cols = starts_with(race_columns))

# Add in meters units to the athletics event columns
decathlon_clean <- decathlon_clean %>% 
  rename_with(~str_replace(., "$", "_(m)"), .cols = starts_with(athletics_columns))

# View dataset
view(decathlon_clean)
