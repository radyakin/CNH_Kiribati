# Title: prelim_survey_review
# Author: Jessica Gephart
# Date: 8-Jul-19
# Purpose: look through initial survey responses to give feedback to Mike for the refresher

library(haven)
#___________________________________________________________________________________________#
# Read in dta files
#___________________________________________________________________________________________#
datadir <- file.path(getwd(), "Data", "survey_prelim_7Jul19")
outdir <- file.path(getwd(), "Outputs")

fisheries <- read_dta(file.path(datadir, "fisheries.dta"))
market <- read_dta(file.path(datadir, "marketSurvey.dta"))
availability <- read_dta(file.path(datadir, "marketSurveyAvailability.dta"))
VRS <- read_dta(file.path(datadir, "villageResourceSurvey.dta"))

#___________________________________________________________________________________________#
# Look at data 
#___________________________________________________________________________________________#
str(fisheries)
colnames(fisheries)

str(market)
colnames(market)

str(availability)
colnames(availability)

str(VRS)
colnames(VRS)
#___________________________________________________________________________________________#
# Write out csv files
#___________________________________________________________________________________________#
write.csv(fisheries, file.path(outdir, "fisheries.csv"), row.names = FALSE)
write.csv(market, file.path(outdir, "marketSurvey.csv"), row.names = FALSE)
write.csv(availability, file.path(outdir, "marketSurveyAvailability.csv"), row.names = FALSE)
write.csv(VRS, file.path(outdir, "villageResourceSurvey.csv"), row.names = FALSE)