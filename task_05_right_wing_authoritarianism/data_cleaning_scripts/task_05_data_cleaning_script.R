


library(readxl)
library(tidyverse)
library(stringr)
library(janitor)


rwa <- read_csv("raw_data/rwa.csv")


rwa <- clean_names(rwa)


rwa <- rwa %>% 
  select(c(q3:q22), c(introelapse:surveyelapse), education, gender, hand, age, familysize, urban)


rwa <- rwa %>% 
  mutate(education = as.character(education),
         gender = as.character(gender),
         hand = as.character(hand),
         urban = as.character(urban))


rwa <- rwa %>% 
  mutate(education = recode(education,
                            "1" = "Less than high school",
                            "2" = "High school",
                            "3" = "University degree",
                            "4" = "Graduate degree"),
         gender    = recode(gender,
                            "1" = "Male",
                            "2" = "Female",
                            "3" = "Other"),
         hand      = recode(hand,
                            "1" = "Right",
                            "2" = "Left",
                            "3" = "Both"),
         urban     = recode(urban,
                            "1" = "Rural",
                            "2" = "Suburban",
                            "3" = "Urban"))


rwa <- rwa %>% 
  mutate(across(q3:q22, as.character)) %>% 
  mutate(q4 =  recode(q4,
                      "1" = "9",
                      "2" = "8",
                      "3" = "7",
                      "4" = "6",
                      "6" = "4",
                      "7" = "3",
                      "8" = "2",
                      "9" = "1"),
         q6 =  recode(q6,
                      "1" = "9",
                      "2" = "8",
                      "3" = "7",
                      "4" = "6",
                      "6" = "4",
                      "7" = "3",
                      "8" = "2",
                      "9" = "1"),
         q8 =  recode(q8,
                      "1" = "9",
                      "2" = "8",
                      "3" = "7",
                      "4" = "6",
                      "6" = "4",
                      "7" = "3",
                      "8" = "2",
                      "9" = "1"),
         q9 =  recode(q9,
                      "1" = "9",
                      "2" = "8",
                      "3" = "7",
                      "4" = "6",
                      "6" = "4",
                      "7" = "3",
                      "8" = "2",
                      "9" = "1"),
         q11 =  recode(q11,
                       "1" = "9",
                       "2" = "8",
                       "3" = "7",
                       "4" = "6",
                       "6" = "4",
                       "7" = "3",
                       "8" = "2",
                       "9" = "1"),
         q13 =  recode(q13,
                       "1" = "9",
                       "2" = "8",
                       "3" = "7",
                       "4" = "6",
                       "6" = "4",
                       "7" = "3",
                       "8" = "2",
                       "9" = "1"),
         q15 =  recode(q15,
                       "1" = "9",
                       "2" = "8",
                       "3" = "7",
                       "4" = "6",
                       "6" = "4",
                       "7" = "3",
                       "8" = "2",
                       "9" = "1"),
         q18 =  recode(q18,
                       "1" = "9",
                       "2" = "8",
                       "3" = "7",
                       "4" = "6",
                       "6" = "4",
                       "7" = "3",
                       "8" = "2",
                       "9" = "1"),
         q20 =  recode(q20,
                       "1" = "9",
                       "2" = "8",
                       "3" = "7",
                       "4" = "6",
                       "6" = "4",
                       "7" = "3",
                       "8" = "2",
                       "9" = "1"),
         q21 =  recode(q21,
                       "1" = "9",
                       "2" = "8",
                       "3" = "7",
                       "4" = "6",
                       "6" = "4",
                       "7" = "3",
                       "8" = "2",
                       "9" = "1")) %>% 
  mutate(across(q3:q22, as.numeric))


rwa <- rwa %>% 
  mutate(total_score = q3+q4+q5+q6+q7+q8+q9+q10+q11+q12+q13+q14+q15+q16+q17+q18+q19+q20+q21+q22) %>% 
  mutate(rwa_score = total_score / 22) %>% 
  mutate(total_time = introelapse + testelapse + surveyelapse)

rwa <- rwa %>% 
  filter(education == c("Less than high school", "High school", "University degree", "Graduate degree"))

rwa <- rwa %>% 
  filter(urban == c("Rural", "Suburban", "Urban"))

write_csv(rwa, "clean_data/rwa.csv")
