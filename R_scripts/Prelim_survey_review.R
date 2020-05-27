# Title: prelim_survey_review
# Author: Jessica Gephart
# Date: 20-May-20
# Notes for Mike:
# - Where is the market availability data that we had in the last iteration? 
# - What is the meaning of NA in the multiselect variable columns?
# - Why does VRS have household info and anemia info? 

# Note to self
# - Push up to github and ping Kelvin

library(haven)
library(tidyverse)
library(magrittr)
library(ggplot2)
source("cleaning_functions.R") # FIX IT: This probably isn't the best way to do this
#___________________________________________________________________________________________#
# Read in dta files
#___________________________________________________________________________________________#
datadir <- file.path(getwd(), "Data", "HIES_raw_19May20")
outdir <- file.path(getwd(), "Outputs")

fisheries <- read_dta(file.path(datadir, "Fisheries", "Fisheries.dta"))

# Dylan woring on health data, so leave alone for now
dietaryRecall <- read_dta(file.path(datadir, "Health", "dietaryRecall.dta"))
health <- read_dta(file.path(datadir, "Health", "health.dta"))
healthyLiving <- read_dta(file.path(datadir, "Health", "healthyLiving.dta"))

# Just look at fish variables from market surveys for now
fishRoster <- read_dta(file.path(datadir, "Market Survey", "fish_roster.dta"))
fishUnitRoster <- read_dta(file.path(datadir, "Market Survey", "fish_unit_roster.dta"))

# availability <- read_dta(file.path(datadir, "marketSurveyAvailability.dta")) # Not in this folder

# Village Resource Survey data
eventRoster <- read_dta(file.path(datadir, "VRS", "event_roster.dta"))
fishassetRoster <- read_dta(file.path(datadir, "VRS", "fish_asset_roster.dta"))
outsideRoster <- read_dta(file.path(datadir, "VRS", "outside_roster.dta"))
shareRoster <- read_dta(file.path(datadir, "VRS", "share_roster.dta"))
vrsRoster <- read_dta(file.path(datadir, "VRS", "vrs_roster.dta"))
vrs <- read_dta(file.path(datadir, "VRS", "VRS.dta"))
withinRoster <- read_dta(file.path(datadir, "VRS", "within_roster.dta"))

#___________________________________________________________________________________________#
# Clean fisheries data
#___________________________________________________________________________________________#
fisheries <- clean_data(fisheries)[[1]]

# Extract variable label attributes
var_labels <- clean_data(fisheries)[[2]]

fisheriesTidy <- fisheries %>% 
  gather(question, response, p902:p922n3) %>%
  filter(!is.na(response)) %>%
  left_join(var_labels, by = c("question" = "col.names")) %>%
  separate(col.labels, into = c("question.no", "question", "option"), sep = ":")


#___________________________________________________________________________________________#
# Visualize fisheries data
#___________________________________________________________________________________________#  
# Histograms of single/multi-select questions
plotDF <- fisheriesTidy %>% 
  filter(!is.na(option)) %>% 
  filter(response == "1") 

# Plot histogram for each question
# FIX IT: Not working as a loop. Need to decide best way to make many of these
i = 1
ggplot(plotDF %>% filter(question == unique(plotDF$question)[i]), aes(x = option)) +
    geom_bar() + 
    labs(title = paste(unique(plotDF$question)[i]), y = "Number of yes responses", x = "") +
    coord_flip()

i = 2
ggplot(plotDF %>% filter(question == unique(plotDF$question)[i]), aes(x = option)) +
  geom_bar() + 
  labs(title = paste(unique(plotDF$question)[i]), y = "Number of yes responses", x = "") +
  coord_flip()

# Histograms of single response questions
plotDF <- fisheriesTidy %>% 
  filter(is.na(option)) %>% 
  filter(str_detect(question.no, pattern = "n", negate = TRUE)) %>% # remove "other" responses
  mutate(option = as.numeric(option))
plotDF$response <- as.numeric(plotDF$response)

# There are 11 questions, so choose i between 1 and 11
# FIX IT: Check it theses are including responses from non-fisheres
i = 5
ggplot(plotDF %>% filter(question == unique(plotDF$question)[i]), aes(x = response)) +
  geom_histogram() + 
  labs(title = paste(unique(plotDF$question)[i]), y = "", x = "") 

#___________________________________________________________________________________________#
# Clean VRS data
#___________________________________________________________________________________________#



#___________________________________________________________________________________________#
# Write out csv files
#___________________________________________________________________________________________#