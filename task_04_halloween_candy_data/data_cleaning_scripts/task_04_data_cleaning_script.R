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

candy_2015 %>%
  names()

# candy_2016 %>% 
#   names()

# candy_2017 %>%
#   names()


# candy_2016 %>% 
#   distinct(which_country_do_you_live_in) %>% 
#   pull()

# view(candy_2015)
# view(candy_2016)
# view(candy_2017)


candy_2015 <- candy_2015 %>% 
  pivot_longer(c(4:96, 115),
               names_to = "candies",
               values_to = "rating")
