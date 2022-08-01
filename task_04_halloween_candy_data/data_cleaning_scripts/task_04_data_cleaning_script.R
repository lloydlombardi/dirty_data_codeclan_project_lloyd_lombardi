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

view(candy_2015)
view(candy_2016)
view(candy_2017)