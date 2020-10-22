# Clean market data

rm(list=ls())
library(haven)
library(tidyverse)
library(magrittr)

# MacOS
datadir <- "/Volumes/jgephart/Kiribati/Data"
outdir <- "/Volumes/jgephart/Kiribati/Outputs"

# Windows
#datadir <- "K:/Kiribati/Data"
#outdir <- "K:/Kiribati/Outputs"

source("R_scripts/cleaning_functions.R")

#___________________________________________________________________________________________#
# Clean market data
#___________________________________________________________________________________________#
market.files <- list.files(file.path(datadir, "20201013_Market_Survey")) 
market.files <- market.files[grep(market.files, pattern = ".dta")]

# ID unique food items
food.files <- unique(gsub("_.*", '', market.files))
food.files <- food.files[food.files != "KIR"]

food.items <- food.files
food.items[food.items == "veges"] <- "vege" # make singular (have to keep plural to read in file)

marketSurvey <- data.frame("interview__key" = character(), "interview__id"  = character(),
                           "roster__id" = character(), "other_units" = character(), "availability" = character(),
                           "availability_other" = character(),
                           "unit_roster__id"  = character(), "price"  = numeric(), "weight" = numeric(), "unit_other" = character())

for(i in 1:length(food.items)){
  food.roster <- read_dta(file.path(datadir, "20201013_Market_Survey", 
                                    paste(food.files[i], "roster.dta", sep ="_")))
  unit.roster <- read_dta(file.path(datadir, "20201013_Market_Survey", 
                                    paste(food.files[i], "unit", "roster.dta", sep ="_")))
  
  # Standardize column names
  colnames(food.roster) <- gsub(paste(food.items[i], "s", sep = ""), paste(food.items[i]), colnames(food.roster))
  colnames(unit.roster) <- gsub(paste(food.items[i], "s", sep = ""), paste(food.items[i]), colnames(unit.roster))
  
  # Merge by roster__id column (but not unit_roster__id column)
  food.unit.roster <- full_join(food.roster, unit.roster, by = c("interview__key", "interview__id", paste(food.items[i], "roster__id", sep ="_")))
  
  # Clean and extract variable label attributes
  food.unit.roster <- clean_data(food.unit.roster)[[1]]
  
  
  # Remove y/n units columns and rename others
  select.cols <- c("interview__key", "interview__id", 
                   paste(food.items[i], "roster__id", sep = "_"),
                   paste("other", food.items[i], "units", sep = "_"),
                   paste(food.items[i], "available", sep = "_"),
                   paste(food.items[i], "avail_oth", sep = "_"),
                   paste(food.items[i], "unit_roster__id", sep = "_"),
                   paste(food.items[i], "price", sep = "_"),
                   paste(food.items[i], "weight", sep = "_"),
                   paste(food.items[i], "unit_oth", sep = "_")
  )
  food.unit.roster <- food.unit.roster %>% 
    select_at(all_of(select.cols))
  
  # Standardize column names
  colnames(food.unit.roster) <- c("interview__key", "interview__id", "roster__id","other_units","availability","availability_other",
                                  "unit_roster__id","price","weight", "unit_other")
  
  # Join with marketSurvey to create join all foods
  marketSurvey <- bind_rows(marketSurvey, food.unit.roster)
}

# Merge with survey location info
survey.info <- read_dta(file.path(datadir, "20201013_Market_Survey", "KIR_MARKET_SURVEY.dta"))
survey.info <- clean_data(survey.info)[[1]]

survey.info <- survey.info %>%
  select("interview__key", "interview__id", "ms_island", "ms_village", "ea_market")

# Already Tidy:
marketSurveyTidy <- full_join(survey.info, marketSurvey, by = c("interview__key", "interview__id"))

write.csv(marketSurveyTidy, file.path(outdir, "marketSurveyTidy.csv"), row.names=FALSE)

#___________________________________________________________________________________________#
# Other market survey outputs
#___________________________________________________________________________________________#

# Output sample sizes:
market_survey_sample_per_village <- marketSurveyTidy %>% 
  group_by(ms_island, ms_village) %>%
  summarise(n_market = length(unique(ea_market)), n_foods = length(unique(roster__id)))

write.csv(market_survey_sample_per_village, file.path(outdir, "market_survey_sample_per_village.csv"), row.names=FALSE)

# Output short answer responses for translation
other_answers <- marketSurveyTidy %>%
  filter(availability_other != "")

write.csv(other_answers, file.path(outdir, "marketSurvey_other_answers.csv"), row.names = FALSE)

# Question for Mike: why are their duplicates in price information within marketSurveyTidy?
market_survey_price_duplicates <- marketSurveyTidy %>% group_by(interview__id, roster__id, price, weight) %>% summarise(n=n()) %>% filter(n>1) %>% left_join(marketSurvey, by = c("interview__id", "roster__id", "price", "weight")) %>% ungroup() %>% select(-n) %>% arrange(interview__id)
write.csv(market_survey_price_duplicates, file = file.path(outdir, "market_survey_price_duplicates.csv"), row.names = FALSE) # n = 68

# FOODS WITH NO WEIGHT UNITS:
market_survey_no_weight_unit <- marketSurveyTidy %>%
  mutate(unit_roster__id = as.character(unit_roster__id)) %>%
  filter(unit_roster__id %in% c("Bundle / Bunch / Pack", "Each / Piece", "Can / Bottle", "Box / Carton", "Bag", "Other units", "Bucket", "Plate / Bowl", "Basket") & is.na(unit_other))
write.csv(market_survey_no_weight_unit, file = file.path(outdir, "market_survey_no_weight_unit.csv"), row.names = FALSE) # n = 1

# FOODS WITH NO WEIGHT MEASUREMENT (usually things that are measured with unit_roster__id == Each/Piece usually also has an entry for weight and unit_other, but these do not)
market_survey_no_weight <- marketSurveyTidy %>% 
  filter(weight == 0)
write.csv(market_survey_no_weight, file = file.path(outdir, "market_survey_no_weight.csv"), row.names = FALSE) # n = 17

# FOODS with mixed units: note, can get this list by allowing print(food) to run in plot.food.prices function
# mixed_units <- c("Cordial, syrup, not further specified",
#                  "Cooking oil &amp; fats",
#                  "Canned meat, corned beef",
#                  "Chocolate in bars or in slabs",
#                  "Toffees, pastilles and other confectionery products",
#                  "Other non alcoholic beverages",
#                  "Noodles, pasta",
#                  "Other diary and oil product",
#                  "Brown coconut",
#                  "Chewing gum",
#                  "Ice block, icies, ice frubu etc",
#                  "Chinese sweets (mango skin, pawpaw skin etc",
#                  "Ice cream",
#                  "Bottled water/spring water/mineral water",
#                  "Cola flavour soft drink eg. Coca cola/Pepsi",
#                  "Soft drinks (Fizzy), eg. Sprite, 7 Up, Tonic, Fanta,",
#                  "Fruit juice (apple, pineapple, tropical etc...)",
#                  "Lollies",
#                  "Ice cream cones",
#                  "Other fruit (specify)"
# )
# 
# market_survey_diff_units <- market_survey_price %>%
#   filter(roster__id %in% mixed_units) %>%
#   arrange(roster__id)
# 
# write.csv(market_survey_diff_units, file = file.path(outdir, "market_survey_diff_units.csv"), row.names = FALSE)
