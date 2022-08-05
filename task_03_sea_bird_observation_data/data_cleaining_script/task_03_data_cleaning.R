# Load in libs
library(readxl)
library(tidyverse)
library(stringr)
library(janitor)

# Read in excel sheets
bird_data <- read_xls("raw_data/seabirds.xls", sheet = "Bird data by record ID", guess_max = 30000)
ship_data <- read_xls("raw_data/seabirds.xls", sheet = "Ship data by record ID")

# Run clean names
bird_data <- clean_names(bird_data)
ship_data <- clean_names(ship_data)

# Left join tables by ID
seabird_data <- bird_data %>% 
  left_join(ship_data, by = "record_id")

# Select relevant columns
seabird_data <- seabird_data %>% 
  select(record_id,
         species_common_name_taxon_age_sex_plumage_phase,
         species_scientific_name_taxon_age_sex_plumage_phase,
         species_abbreviation,
         count,
         lat)

# Rename column
seabird_data <- seabird_data %>% 
  rename("common_name" = "species_common_name_taxon_age_sex_plumage_phase")

# Rename column
seabird_data <- seabird_data %>% 
  rename("scientific_name" = "species_scientific_name_taxon_age_sex_plumage_phase")

# Rename column
seabird_data <- seabird_data %>% 
  rename("abbreviation" = "species_abbreviation")

#Write csv
write_csv(seabird_data, "clean_data/seabird_data.csv")
