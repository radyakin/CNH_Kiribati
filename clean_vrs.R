# Clean VRS
# FIX IT - update .RMD files to have data cleaned using these scripts:


library(haven)
library(tidyverse)
library(magrittr)

datadir <- "/Volumes/jgephart/Kiribati/Data"
outdir <- "/Volumes/jgephart/Kiribati/Outputs"



source("R_scripts/cleaning_functions.R")
source("R_scripts/plotting_functions.R")
#___________________________________________________________________________________________#
# Clean VRS data
#___________________________________________________________________________________________#
vrs <- read_dta(file.path(datadir, "20201013_VRS", "KIR_VILLAGE_RESOURCE_SURVEY.dta"))

vrs <- clean_data(vrs)[[1]]

# Extract variable label attributes
var_labels <- clean_data(vrs)[[2]]

# Change class to character to allow left_join without warning below
var_labels <- var_labels %>%
  mutate(col.names = as.character(col.names))

# Make tidy
vrsTidy <- tidy_data(df = vrs, pivot_col_1 = "respondent_names__0", pivot_col_last = "villageGPS__Timestamp", var_labels = var_labels, question_no = FALSE)

write.csv(vrsTidy, file.path(outdir, "vrsTidy.csv"), row.names = FALSE)

# Output question list
write.csv(unique(vrsTidy$question), file.path(outdir, "vrs_question_list.csv"), row.names = FALSE)



#___________________________________________________________________________________________#
# Clean event roster 
#___________________________________________________________________________________________#

## FIX IT - need Mike to output another version of vrs with interview key or interview id columns

eventRoster <- read_dta(file.path(datadir, "20201013_VRS", "event_roster.dta"))

eventRoster <- clean_data(eventRoster)[[1]]

# Join back with vrsTidy to get island info
vrs_islands <- vrsTidy %>%
  select(interview__key, interview__id, vrs_island, vrs_village) %>%
  unique()

eventRoster<- eventRoster %>%
  left_join(vrs_islands, by = c("interview__key", "interview__id"))

# Extract variable label attributes
var_labels <- clean_data(eventRoster)[[2]]

# Change class to character to allow left_join without warning below
var_labels <- var_labels %>%
  mutate(col.names = as.character(col.names))

# Make tidy
eventRosterTidy <- tidy_data(df = eventRoster, pivot_col_1 = "event_occurrence", pivot_col_last = "event_nonfinfish", var_labels = var_labels, question_no = FALSE)

# Output question list
write.csv(unique(eventRosterTidy$question), "outputs/eventRoster_question_list.csv", row.names = FALSE)


# Clean responses for "Time of occurrence of event" should be years
eventRosterTidy <- eventRosterTidy %>%
  mutate(response = case_when(response == "0" ~ "2000",
                              response == "1" ~ "2001",
                              response == "2" ~ "2002",
                              response == "4" ~ "2004",
                              response == "7" ~ "2007",
                              response == "8" ~ "2008",
                              response == "10" ~ "2010",
                              TRUE ~ response))


write.csv(eventRosterTidy, "Outputs/eventRosterTidy.csv", row.names = FALSE)

