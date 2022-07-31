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
view(decathlon_clean)

