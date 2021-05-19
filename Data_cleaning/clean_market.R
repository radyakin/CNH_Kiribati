# Clean and output tidy Market Survey data:
# Clean, format, and output data as the starting point for all data requests
# Reminder: keep cleaning and plotting code separate (for plotting, see prelim_VRS_and_market_survey_review.Rmd, etc)

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

source("Data_cleaning/cleaning_functions.R")

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

# Tidy version
market_tidy <- full_join(survey.info, marketSurvey, by = c("interview__key", "interview__id"))

# Long version
market_long <- pivot_dat_i(market_tidy, id_cols = c("interview__key", "interview__id", "ms_island", "ms_village", "ea_market")) 

market_alpha <- market_long %>% 
  filter(str_detect(value, "[:alpha:]")) %>% 
  # Try sort(unique(market_alpha$question_id) to figure out which questions to filter
  filter(question_id %in% c("availability_other", "other_units")) %>%
  arrange(question_id) %>%
  select(question_id, value) %>%
  unique()


# Outputs:
# Final long format of all uniquely identified questions: market_long
# Final tidy format: market_tidy
# "Alpha" responses for translation: market_alpha

write.csv(market_tidy, file.path(outdir, "market_tidy.csv"), row.names=FALSE)
write.csv(market_long, file.path(outdir, "market_long.csv"), row.names=FALSE)
write.csv(market_alpha, file.path(outdir, "market_text-responses-for-translation.csv"), row.names=FALSE)

