# Title: prelim_survey_review
# Author: Jessica Gephart
# Date: 20-May-20
# Notes for Mike:
# - Where is the market availability data that we had in the last iteration? 

library(haven)
library(tidyverse)
library(magrittr)
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
# Clean VRS data
#___________________________________________________________________________________________#
vrs <- vrs %<>% 
  mutate_if(is.labelled, funs(as_factor(.)))

vrsRoster <- vrsRoster %<>% 
  mutate_if(is.labelled, funs(as_factor(.)))

#___________________________________________________________________________________________#
# Write out csv files
#___________________________________________________________________________________________#
#write.csv(fisheries, file.path(outdir, "fisheries_200520.csv"), row.names = FALSE)
#write.csv(market, file.path(outdir, "marketSurvey_200520.csv"), row.names = FALSE)
#write.csv(availability, file.path(outdir, "marketSurveyAvailability_200520.csv"), row.names = FALSE)
#write.csv(VRS, file.path(outdir, "villageResourceSurvey_200520.csv"), row.names = FALSE)