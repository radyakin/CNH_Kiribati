# Author: Kelvin Gorospe kdgorospe@gmail.com
# Provide examples for using fweight (calculated by SPC team) to get division-level estimates
# Calculate weights for HIES data for other spatial scales

rm(list=ls())
library(haven)
library(tidyverse)
library(magrittr)
source("Data_cleaning/cleaning_functions.R")

# MacOS
rawdatadir <- "/Volumes/jgephart/Kiribati/Data"
datadir <- "/Volumes/jgephart/Kiribati/Outputs"
outdir <- "/Volumes/jgephart/Kiribati/Outputs" # same as datadir
metadatadir <- "/Volumes/jgephart/Kiribati/Data/HIES-NSF-files"

# Read in HIES data
house_tidy <- read.csv(file.path(datadir, "2021-07-29_hies_tidy_household-level.csv")) 
individ_tidy <- read.csv(file.path(datadir, "2021-07-29_hies_tidy_individual-level.csv"))

# Subset just the sampling info:
house_sample <- house_tidy %>%
  select(all_of(c("interview__id", "division", "ea", "island", "village", "fweight", "hhld_id", "hhsize"))) 

individ_sample <- individ_tidy %>%
  select(all_of(c("interview__id", "division", "island", "village", "fweight"))) %>%
  # Join individual-level HIES data with HIES-level to get houseehold ID (hhld_id) - need this variable for calculating individual-level weights
  left_join(house_sample, by = c("interview__id", "division", "island", "village", "fweight"))

# How to apply fweights to get strata-level metrics:
# EXAMPLE: apply fweight to household size to get the total population per strata (i.e., division)
# i.e., fweight is the representativeness of each household within a division (accounting for sampling probability, as well as age and sex structure)
# when dealing with house-level data, the sum of hhsize * fweight gives you total population within a division
house_sample %>%
  group_by(division) %>%
  summarise(total_population = sum(hhsize * fweight)) %>%
  arrange(desc(total_population))

# Correct numbers according to census should be:
# South Tarawa 63140
# Northern 21360
# Southern 16320
# Line Is. & Phoenix 8850
# Central 8810

# for individual-level data, the sum of weights gives you the total population within a division
individ_sample %>%
  group_by(division) %>%
  summarise(sample_n = n(),
            total_population = sum(fweight)) %>%
  arrange(desc(total_population))

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
# STATA function also adjusts this probability by the age and sex structure of the total population

################################################################################################################
# Create household level weights for village and island-level data
# Unlike fweight, these weights are based only on the census information (not two-stage, and not accounting for age and sex structure)
# FIX IT: might be able to use R package for more sophisticated post-stratification? https://stats.idre.ucla.edu/r/faq/how-do-i-analyze-survey-data-with-stratification-after-sampling-poststratification/

# Read in raw data file to get key to matcy ID number to division, island, village names
cover_dta <- read_dta(file.path(rawdatadir, "20210301_HIES_FINAL", "SPC_KIR_2019_HIES_0-Cover_v01.dta"))
island_key <- data.frame(island_id = attributes(cover_dta[["island"]])$labels) %>%
  rownames_to_column("island")
village_key <- data.frame(village_id = attributes(cover_dta[["village"]])$labels) %>%
  rownames_to_column("village")
division_key <- data.frame(division_id = attributes(cover_dta[["division"]])$labels) %>%
  rownames_to_column("division")

# Read in sampling metadata: contains one row for each "EA" (n = 159), and numbers of people and households sampled in each EA
# Note: could also use house_tidy or individ_tidy below to generate these numbers
hies_sampling <- read_dta(file.path(metadatadir, "KIR_2019_HIES_eaInfo.dta")) %>% clean_data(return = "df") %>%
  # Mutate to character, otherwise it will get converted to scientific notation (e.g., 2.01 e6)
  mutate(ea = as.character(ea)) %>%
  rename(division_id = division,
         island_id = island,
         village_id = village) %>%
  # Join hies_sampling with division, island, village number key to get their full names
  left_join(village_key, by = "village_id") %>%
  left_join(island_key, by = "island_id") %>%
  left_join(division_key, by = "division_id") %>%
  # Clean village_name to match format of data
  mutate(village = gsub("^.*- ", "", village)) %>%
  # hies_sampling only has ea sampling numbers, calculate these numbers for the villageb island, and division levels
  group_by(village, island, division) %>%
  mutate(person_int_village = sum(person_int_ea),
         hh_int_village = sum(hh_int_ea)) %>%
  ungroup() %>%
  group_by(island, division) %>%
  mutate(person_int_island = sum(person_int_ea),
         hh_int_island = sum(hh_int_ea))

# Read in household-level census data
census_house <- read_dta(file.path(metadatadir, "KIR_2020_PHC_households.dta")) %>% clean_data(return = "df") %>%
  # Mutate to character for later joining with sampling info
  mutate(across(where(is.factor), as.character)) %>%
  # Standardize division names to match sampling info
  mutate(division = case_when(division == "Sth. Tarawa" ~ "South Tarawa",
                              TRUE ~ division)) %>% 
  # As per Mike: the following CSV matches HIES EA number with Census EA number
  left_join(read.csv(file.path(metadatadir, "EA HIES and census mapping.csv")) %>%
              mutate(across(where(is.numeric), as.character)), by = c("ea_number" = "Census.EA")) %>%
  # Clean village_name to match format of data
  mutate(village = gsub("^.*- ", "", village))

# Spatial scales are EA_number within Village within Island within Division
# Calcualte population numbers at each scale
# Calculate number of EA's per village and number of EA's per island
# Note: division-level numbers do not exactly match the numbers generated using the HIES fweights
# as per Mike: HIES fweights were generated using population projections, not actual 2020 Census numbers
census_summary <- census_house %>%
  group_by(division) %>%
  mutate(division_hh_census = n(),
         division_pop_census = sum(hhsize, na.rm = TRUE)) %>%
  group_by(division, island) %>%
  mutate(island_hh_census = n(),
         island_pop_census = sum(hhsize, na.rm = TRUE),
         n_ea_per_island = length(unique(ea_number))) %>%
  group_by(division, island, village) %>%
  mutate(village_hh_census = n(),
         village_pop_census = sum(hhsize, na.rm = TRUE),
         n_ea_per_village = length(unique(ea_number))) %>%
  group_by(division, island, village, ea_number) %>%
  mutate(ea_hh_census = n(),
         ea_pop_census = sum(hhsize, na.rm = TRUE)) %>%
  select(-c(interview__key, interview__id, urbrur, hhsize)) %>%
  distinct() %>%
  arrange(division, island, village, ea_number) %>%
  ungroup()

# Read in individual-level census data
# Generate same breakdown of population numbers using individual-level census data and check that they match those generated from household-level census data
census_individ <- read_dta(file.path(metadatadir, "KIR_2020_PHC_persons.dta")) %>% clean_data(return = "df") %>%
# clean census_individ same as census_house
# Mutate to character for later joining with sampling info
  mutate(across(where(is.factor), as.character)) %>%
  # Standardize division names to match sampling info
  mutate(division = case_when(division == "Sth. Tarawa" ~ "South Tarawa",
                              TRUE ~ division)) %>%
  # As per Mike: the following CSV matches HIES EA number with Census EA number
  left_join(read.csv(file.path(metadatadir, "EA HIES and census mapping.csv")) %>%
              mutate(across(where(is.numeric), as.character)), by = c("ea_number" = "Census.EA")) %>%
  # Clean village_name to match format of data
  mutate(village = gsub("^.*- ", "", village))

census_summary_2 <- census_individ %>%
  group_by(division) %>%
  mutate(division_pop_census = n()) %>%
  group_by(division, island) %>%
  mutate(island_pop_census = n()) %>%
  group_by(division, island, village) %>%
  mutate(village_pop_census = n()) %>%
  group_by(division, island, village, ea_number) %>%
  mutate(ea_pop_census = n()) %>%
  select(division, island, village, ea_number, division_pop_census, island_pop_census, village_pop_census, ea_pop_census) %>%
  distinct() %>%
  arrange(division, island, village, ea_number)

######################################################################################
# DATA CHECK: does census_summary (generated from household data) matches census_summary_2 (generated from individuald data)
census_summary_check <- census_summary %>%
  select(division, island, village, ea_number, division_pop_census, island_pop_census, village_pop_census, ea_pop_census) %>%
  distinct() %>%
  arrange(division, island, village, ea_number)

# The two datasets do not match:
# There's one extra EA in the household level census data
# as per Mike, this EA corresponds to a hotel and can be ignored for generating population numbers
setdiff(census_summary_check$ea_number, census_summary_2$ea_number)

######################################################################################
# CALCULATE HOUSEHOLD LEVEL WEIGHTS:
# Join sampling info (hies_sampling) with census_summary (household level census data)
# Calculate weights as total census number / sampled number
# Note: all households within an EA will get the same weight, all EA's within a village get the same weight, and all villages within an island get the same weight
hies_hh_weights <- hies_sampling %>%
  # Remove extra columns
  select(-c(division_id, island_id, village_id)) %>%
  # Note join just by "EA", don't join by island/village names, some of these names in the census are slightly different (e.g., island Betio and village Betio_East are in census but not hies_sampling)
  left_join(census_summary %>% select(-c(island, village)), by = c("division", "ea" = "HIES.EA")) %>% 
  # Rearrange columns, remove column "ea_number" (specific to census), keep column "ea" (specific to HIES data)
  select(division, island, village, ea, # ID cols
         person_int_ea, ea_pop_census, 
         person_int_village, village_pop_census,
         person_int_island, island_pop_census) %>%
  mutate(hh_to_ea_w = ea_pop_census / person_int_ea,
         hh_to_village_w = village_pop_census / person_int_village,
         hh_to_island_w = island_pop_census / person_int_island)

######################################################################################
# DATA CHECK: Does applying household-level weights to variable "hhsize" generate the correct population numbers according to census?

# Join hies_hh_weights with household-level hies data (house_sample)
house_sample_w <- house_sample %>% 
  mutate(ea = as.character(ea)) %>%
  left_join(hies_hh_weights)

# Test weights by applying them to hhsize and compare this to census data of total population size:
house_sample_w %>%
  group_by(division, island, village, ea) %>%
  mutate(ea_pop = sum(hhsize * hh_to_ea_w)) %>%
  group_by(division, island, village) %>%
  mutate(village_pop = sum(hhsize * hh_to_village_w)) %>%
  group_by(division, island) %>%
  mutate(island_pop = sum(hhsize * hh_to_island_w)) %>%
  select(division, island, village, ea, ea_pop, village_pop, island_pop) %>%
  distinct() %>%
  arrange(division, island, village, ea) %>%
  print(n = 159)

# Above calculation matches with actual census numbers, i.e. - household-level weights are correct
census_summary %>% filter(is.na(HIES.EA)==FALSE) %>% 
  select(division, island, village, HIES.EA, ea_pop_census, village_pop_census, island_pop_census) %>%
  print(n = 159)

######################################################################################
# CALCULATE INDIVIDUAL LEVEL WEIGHTS:

# Use housholed level weights (house_sample_w) to generate individual-level weights
# Each individual represents each household as 1 / hhsize, so divide household-level weights by hhsize to get individual level weights
hies_all_weights <- house_sample_w %>%
  mutate(individ_to_ea_w = hh_to_ea_w / hhsize,
         individ_to_village_w = hh_to_village_w / hhsize,
         individ_to_island_w = hh_to_island_w / hhsize)

######################################################################################
# DATA CHECK: Does summing individual-level weights generate the correct population numbers according to census?
# Test weights?? Not sure there is an equivalent for ground-truthing the calculated individual-level weights

######################################################################################
# Output all weights

file.date <- Sys.Date() # add file.date to all outputs to keep track of most recent versions
write.csv(hies_all_weights, file = file.path(outdir, paste(file.date, "_hies_all_weights-DRAFT.csv", sep = "")), row.names = FALSE)





### OLD CODE:
# The following was an attempt to follow the math used by SPC to produce strata-level estimates as a two-stage sampling design
# But calculation does not pass the test of using the weights to generate island pop numbers (resulting numbers were about 60% of the true numbers)
# hies_hh_weights <- hies_sampling %>%
#   # Remove extra columns, remove individual-level columns
#   select(-c(division_id, island_id, village_id, person_int_ea, person_int_village, person_int_island)) %>%
#   # Note join just by "EA" some of the island/village names in the census are slightly different (e.g., island Betio and village Betio_East are in census but not hies_sampling)
#   left_join(census_summary %>% select(-c(island, village)), by = c("division", "ea" = "HIES.EA")) %>% 
#   # Rearrange columns, remove ea_number (specific to census)
#   select(division, island, village, ea, occ, # ID cols
#          hh_int_ea, ea_hh_census, # EA hh sample, EA hh total
#          n_ea_per_village,
#          hh_int_village, village_hh_census,
#          n_ea_per_island,
#          hh_int_island, island_hh_census,
#          division_hh_census) %>%
#   mutate(island_prob_1 = ea_hh_census / island_hh_census * n_ea_per_island,
#          island_prob_2 = hh_int_ea / ea_hh_census, 
#          island_w = 1 / (island_prob_1 * island_prob_2) * occ) %>%
#   ungroup()