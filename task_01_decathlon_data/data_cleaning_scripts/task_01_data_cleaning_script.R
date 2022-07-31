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

# View the data with cleaned names
# view(decathlon_clean)


decathlon <- decathlon %>% 
  pivot_wider(names_from = Competition,
              values_from = Points)

# Run `clean names` on the column names
decathlon_clean <- clean_names(decathlon)

# Remove
decathlon_clean <- decathlon_clean %>% 
  rename_with(~ gsub("^x", "", .x))

view(decathlon_clean)

