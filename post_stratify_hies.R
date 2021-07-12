# Author: Kelvin Gorospe kdgorospe@gmail.com
# Post-stratification example

rm(list=ls())
library(haven)
library(tidyverse)
library(magrittr)
source("Data_cleaning/cleaning_functions.R")

# MacOS
datadir <- "/Volumes/jgephart/Kiribati/Outputs"
metadatadir <- "/Volumes/jgephart/Kiribati/Data/HIES-NSF-files"


# Read in sampling metadata
# contains one row for each "ea" (n = 159)
# 5 unique "divisions" corresponding to each of the 5 unique nb_ea_strata 
meta_sample <- read_dta(file.path(metadatadir, "KIR_2019_HIES_eaInfo.dta")) %>% clean_data(return = "df")
meta_sample_labels <- read_dta(file.path(metadatadir, "KIR_2019_HIES_eaInfo.dta")) %>% clean_data(return = "var_labels")

meta_house <- read_dta(file.path(metadatadir, "KIR_2020_PHC_households.dta")) %>% clean_data(return = "df")
meta_house_labels <- read_dta(file.path(metadatadir, "KIR_2020_PHC_households.dta")) %>% clean_data(return = "var_labels")

meta_individ <- read_dta(file.path(metadatadir, "KIR_2020_PHC_persons.dta")) %>% clean_data(return = "df") 
meta_individ_labels <- read_dta(file.path(metadatadir, "KIR_2020_PHC_persons.dta")) %>% clean_data(return = "var_labels") 

# Read in HIES data
house_tidy <- read.csv(file.path(datadir, "2021-07-09_hies_tidy_household-level.csv")) 
individ_tidy <- read.csv(file.path(datadir, "2021-07-09_hies_tidy_individual-level.csv"))
hies_labels <- read.csv(file.path(datadir, "2021-07-09_hies_question-id-to-label-key.csv"))

# Subset just the sampling info:
house_sample <- house_tidy %>%
  select(all_of(c("division", "ea", "island", "village", "fweight", "hhld_id", "hhsize"))) 

individ_sample <- individ_tidy %>%
  select(all_of(c("division", "island", "village", "fweight"))) 

# How to apply fweights to get strata-level metrics:
# EXAMPLE: apply fweight to household size to get the total population per strata (i.e., division)
# i.e., fweight is the representativeness of each household within a division (accounting for sampling probability, as well as age and sex structure)
# when dealing with house-level data, the sum of hhsize * fweight gives you total population within a division
house_sample %>%
  group_by(division) %>%
  summarise(total_population = sum(hhsize * fweight))

# Correct numbers according to census should be:
# South Tarawa 63140
# Northern 21360
# Central 8810
# Southern 16320
# Line Is. & Phoenix 8850

# for individual-level data, the sum of weights gives you the total population within a division
individ_sample %>%
  group_by(division) %>%
  summarise(sample_n = n(),
            total_population = sum(fweight))

# for household-level data, the sum of fweights gives you the total number of households
house_sample %>%
  group_by(division) %>%
  summarise(total_households = sum(fweight))

# EXAMPLE CALCULATIONS

# Examples for calculating a metric for division-level from household data
# EXAMPLE: h1110__1 - amount paid in AUD in the last month for electricity
house_tidy %>%
  select(all_of(c("division", "ea", "island", "village", "fweight", "hhld_id", "hhsize", "h1110__1"))) %>%
  filter(is.na(h1110__1)==FALSE) %>% 
  group_by(division) %>%
  summarise(electricity_aud = sum(h1110__1 * fweight, na.rm = TRUE),
            total_households = sum(fweight),
            electricity_per_hhld = electricity_aud / total_households)

# Use this for identifying candidate individual variables  
# hies_labels %>%
#   filter(col.names %in% names(individ_tidy)) %>%
#   filter(col.labels %in% c("coicop code", "item description")==FALSE) %>%
#   filter(str_detect(col.labels, "paid"))

# Example for calculating a metric for division-level from individual data
# EXAMPLE: p214__1 - amount paid in AUD for school fees or tuition in the last 12 months
individ_tidy %>%
  select(all_of(c("division", "island", "village", "fweight", "p214__1"))) %>%
  #filter(is.na(p214__1)==FALSE) %>%
  group_by(division) %>%
  summarise(tuition_aud = sum(p214__1 * fweight, na.rm = TRUE),
            total_population = sum(fweight),
            tuition_per_person = tuition_aud / total_population)

# Calculation of fweight is done within STATA
# It's based on the probability of sampling a household in a two stage sampling design
# i.e., probability of sampling a household within an EA and probability of sampling an EA within a division using census information
# But then adjusts this probability by the age and sex structure of the total population

# FIX IT: might be able to use R package for more sophisticated post-stratification? https://stats.idre.ucla.edu/r/faq/how-do-i-analyze-survey-data-with-stratification-after-sampling-poststratification/

# Below follows a simpler weighting scheme for village and island-level data, based only on the census information
# Spatial scales are EA_number within Village within Island within Division

# FIX IT - confirm with Mike are these the full census numbers?
# Get numbers at each scale
ea_n <- meta_individ %>%
  group_by(division, island, village, ea_number) %>%
  summarise(ea_n = n()) %>%
  arrange(division)

village_n <- meta_individ %>%
  group_by(division, island, village) %>%
  summarise(village_n = n()) %>%
  arrange(division)

island_n <- meta_individ %>%
  group_by(division, island) %>%
  summarise(island_n = n()) %>%
  arrange(division)

division_n <- meta_individ %>%
  group_by(division) %>%
  summarise(division_n = n()) %>%
  arrange(division)
