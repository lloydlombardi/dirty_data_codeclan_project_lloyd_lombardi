# Read in the required libraries
library(tidyverse)
library(janitor)
library(readr)
library(assertr)

# Read in the raw data
decathlon <- read_rds("raw_data/decathlon.rds")

# View the raw data
# view(decathlon)

# Run `clean names` on the column names
decathlon_clean <- clean_names(decathlon)

# View the data with cleaned names
# view(decathlon_clean)

# Change row_names to first column 'athlete_name'
decathlon_clean <- tibble::rownames_to_column(decathlon_clean, var = "athlete_name")