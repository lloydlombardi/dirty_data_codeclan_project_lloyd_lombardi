# Read in the required libraries
library(tidyverse)
library(janitor)
library(readxl)
library(assertr)
library(stringr)

# Read in the 3 xlsx sheets
candy_2015 <- read_excel("raw_data/boing-boing-candy-2015.xlsx")
candy_2016 <- read_excel("raw_data/boing-boing-candy-2016.xlsx")
candy_2017 <- read_excel("raw_data/boing-boing-candy-2017.xlsx")


# Run `clean_names` function on datasets
candy_2015 <- clean_names(candy_2015)
candy_2016 <- clean_names(candy_2016)
candy_2017 <- clean_names(candy_2017)

################################################################################
                            # 2015 data clean
################################################################################

# Rename columns in 2015 data
candy_2015 <- candy_2015 %>% 
  rename(boxo_raisins = box_o_raisins,
         hersheys_kisses = hershey_s_kissables,
         licorice_yes_black = licorice)


# Clean up the `age` column
candy_2015_ages <- candy_2015 %>% 
  mutate(how_old_are_you = str_remove_all(how_old_are_you, "[0-9][0-9][0-9]"),
         how_old_are_you = str_remove_all(how_old_are_you, "3.14%"),
         how_old_are_you = str_replace(how_old_are_you, ">39", "39"),
         how_old_are_you = str_replace(how_old_are_you, "７１＋", "71"),
         how_old_are_you = str_replace(how_old_are_you, "Good Lord!  I'm 43!", "43"))
# Remove any values with 3 or more digits
# Change some values to only return the age as a character


# Pivot longer 
candy_2015_longer <- candy_2015_ages %>% 
  pivot_longer(cols = c(butterfinger:york_peppermint_patties, 
                        sea_salt_flavored_stuff_probably_chocolate_since_this_is_the_it_flavor_of_the_year, 
                        necco_wafers),
               names_to = "candies",
               values_to = "rating")
# Use a vector to input names of candies
# Create a column for candy names
# Create a column for candy rating


# Column wrangling
candy_2015_clean <- candy_2015_longer %>% 
  mutate(year = 2015) %>% 
  relocate(c(candies:year), .after = timestamp) %>% 
  select(c(candies:are_you_going_actually_going_trick_or_treating_yourself)) %>% 
  rename(age = how_old_are_you,
         trick_or_treating = are_you_going_actually_going_trick_or_treating_yourself)
# Add a `year` column
# Move new columns
# Select necessary columns
# Rename necessary columns


# Change `age` column to numeric 
candy_2015_clean <- candy_2015_clean %>% 
  mutate(age = str_extract(age, "^[0-9]+")) %>% 
  mutate(age = as.numeric(age))
# Extract age that starts with one or more number
# Convert age to numeric


# Filter the ages
candy_2015_clean <- candy_2015_clean %>% 
  filter(age > 0,
         age < 100)
# It was decided to filter the ages between 0 - 100


# Add a `country` and `gender` column to data
candy_2015_clean <- candy_2015_clean %>% 
  mutate(country = "",
         country = na_if(country, ""),
         gender = "",
         gender = na_if(gender, ""))
# Give the data no values
# Convert the data to NA for ease of analysis


################################################################################
                        # 2016 data clean
################################################################################

# Clean up the `age` column  
candy_2016_ages <- candy_2016 %>% 
  mutate(how_old_are_you = str_remove_all(how_old_are_you, "[0-9][0-9][0-9]"),
         how_old_are_you = str_replace(how_old_are_you, "49 11/12ths", "49"),
         how_old_are_you = str_replace(how_old_are_you, "Fifty.  Nine.  Ish.", "59"))
# Remove any values with 3 or more digits
# Clean up some values


# Pivot longer
candy_2016_longer <- candy_2016_ages %>% 
  pivot_longer(cols = c(x100_grand_bar:york_peppermint_patties),
               names_to = "candies",
               values_to = "rating")
# Use a vector to input names of candies
# Create a column for candy names
# Create a column for candy rating


# Column wrangling
candy_2016_clean <- candy_2016_longer %>% 
  mutate(year = 2016) %>% 
  relocate(c(candies:year), .after = timestamp) %>% 
  select(c(candies:which_country_do_you_live_in)) %>% 
  rename(age = how_old_are_you,
         trick_or_treating = are_you_going_actually_going_trick_or_treating_yourself,
         gender = your_gender,
         country = which_country_do_you_live_in)
# Add a `year` column
# Move new columns
# Select necessary columns
# Rename necessary columns


# Relocate age and gender columns to match 2015
candy_2016_clean <- candy_2016_clean %>% 
  relocate(age, .after = year)

candy_2016_clean <- candy_2016_clean %>% 
  relocate(gender, .after = country)


# Change `age` column to numeric
candy_2016_clean <- candy_2016_clean %>% 
  mutate(age = str_extract(age, "^[0-9]+")) %>% 
  mutate(age = as.numeric(age))
# Extract age that starts with one or more number
# Convert age to numeric


# Filter the `age` column
candy_2016_clean <- candy_2016_clean %>% 
  filter(age > 0,
         age < 100)



################################################################################
                            # 2017 data clean
################################################################################

# Initial column renaming to make pivoting easier
candy_2017 <- candy_2017 %>% 
  rename_with(~ gsub("^q[0-9]_", "", .x))
# Remove the "q1" style prefix to column names

candy_2017 <- candy_2017 %>% 
  rename_with(~str_replace(., "^", "x"), .cols = starts_with("100"))
# Add an "x" onto one candy name to keep it consistent

candy_2017 <- candy_2017 %>% 
  rename(mary_janes = anonymous_brown_globs_that_come_in_black_and_orange_wrappers_a_k_a_mary_janes)
# Rename a candy to keep it consistent


# Clean up `age` column 
candy_2017_ages <- candy_2017 %>% 
  mutate(age = str_remove_all(age, "[0-9][0-9][0-9]"),
         age = str_replace(age, "45-55", "50"),
         age = str_replace(age, "24-50", "37"),
         age = str_replace(age, "sixty-nine", "69"))
# Remove any values with 3 or more digits
# Clean up some values


# Pivot longer 
candy_2017_longer <- candy_2017_ages %>% 
  pivot_longer(cols = c(x100_grand_bar:york_peppermint_patties),
               names_to = "candies",
               values_to = "rating")
# Use a vector to input names of candies
# Create a column for candy names
# Create a column for candy rating


# Column wrangling
candy_2017_clean <- candy_2017_longer %>% 
  mutate(year = 2017) %>% 
  relocate(c(candies:year), .after = internal_id) %>% 
  select(c(candies:country)) %>% 
  rename(trick_or_treating = going_out)
# Add a `year` column
# Move new columns
# Select necessary columns
# Rename necessary columns


# Relocate age and gender columns to match 2015 & 2016
candy_2017_clean <- candy_2017_clean %>% 
  relocate(age, .after = year)

candy_2017_clean <- candy_2017_clean %>% 
  relocate(gender, .after = country)


# Change `age` column to numeric
candy_2017_clean <- candy_2017_clean %>% 
  mutate(age = str_extract(age, "^[0-9]+")) %>% 
  mutate(age = as.numeric(age))
# Extract age that starts with one or more number
# Convert age to numeric


# Filter the `age` column 
candy_2017_clean <- candy_2017_clean %>% 
  filter(age > 0,
         age < 100)


################################################################################
                              # Join tables
################################################################################

candy <- bind_rows(candy_2015_clean, candy_2016_clean, candy_2017_clean)
# Bind rows was used as the 2015, 16 & 17 datasets have the same column names



################################################################################
                            # Clean country column
################################################################################

# Change all countries to title
candy <- candy %>% 
  mutate(country = str_to_title(country))
# This will help merge some characters together


#Create some variables to detect USA and UK countries
states <- c("North Carolina", "Pittsburgh", "New York", "California", "New Jersey", "Alaska")
uk <- c("England", "Scotland", "Endland")
# These were found by using distinct(countries)


# Sort country column
candy_country_sorted <- candy %>% 
  mutate(country = case_when(
    str_detect(country,"((?i)[a-z]* *[:punct:])*((?i)^u+ *[:punct:]*(?i)[ns]+ *[:punct:]*(?i)s* *[:punct:]*(?i)a* *[:punct:]*)+((?i)[a-z]* *[:punct:])*") ~ "USA",
    str_detect(country, "(?i)a+mer") ~ "USA",
    str_detect(country, "(?i)t+[a-z]* *(?i)u+[a-z]* *(?i)s+[a-z]* *(?i)o*[a-z]* *(?i)a*[a-z]*") ~ "USA",
    str_detect(country, " +U+[sa]*") ~ "USA",
    str_detect(country, "T+[a-z]* Y+[a-z]* E+[a-z]* O+[a-z]* A+[a-z]*") ~ "USA",
    str_detect(country, "[:punct:]*M+[ue]+[r]+") ~ "USA",
    country %in% states ~ "USA",
    str_detect(country, "^(?i)u+[:punct:]*(?i)k+") ~ "UK",
    country %in% uk ~ "UK",
    str_detect(country, "(^(?i)can)+[a-z]*[:punct:]*") ~ "Canada",
    str_detect(country, "ñ+") ~ "Spain",
    str_detect(country, "The N+") ~ "Netherlands",
    TRUE ~ country
  ))
# Sort all variations of USA, UK, Canada, Spain and Netherlands


# Create a vector of country outliers 
country_outliers = c("A tropical island south of the equator", "Ud", "Atlantis",
                     "Canae", "cascadia ", "Cascadia", "Denial", "Earth", "1", 
                     "god's country", "I Don't Know Anymore", "insanity lately", 
                     "There Isn't One For Old Men", "Soviet Canuckistan", "Narnia", "Neverland",
                     "one of the best ones", "See above", "Somewhere", "subscribe to dm4uz3 on youtube",
                     "The Republic Of Cascadia", "this one", "Eua", "Europe")


# Change these outlying countries to NAs
candy_country_sorted <- candy_country_sorted %>% 
  mutate(country = if_else(country %in% country_outliers, NA_character_, country))


# Rename dataset
candy_clean <- candy_country_sorted

candy_clean %>% 
  distinct(country)
################################################################################
                                # Write csv
################################################################################

write_csv(candy_clean, "clean_data/candy_clean.csv")