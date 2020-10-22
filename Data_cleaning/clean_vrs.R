# Clean VRS

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

# Tidy data output
write.csv(vrsTidy, file.path(outdir, "vrsTidy.csv"), row.names = FALSE)

#___________________________________________________________________________________________#
# Other VRS Outputs (for confirmation, translation, etc.)
#___________________________________________________________________________________________#

# Output question list
write.csv(unique(vrsTidy$question), file.path(outdir, "vrs_question_list.csv"), row.names = FALSE)

# List short-answer, fill-in response questions here:
fill_in_questions <- vrsTidy %>%
  filter(str_detect(question, pattern = "Other") & question != "Other meeting restrictions") %>% 
  pull(question) %>% 
  unique()

# Additional fill-in response questions:
fill_in_questions <- c(fill_in_questions, "Gender attending meeting", "New village rules")

# Output short answer responses for translation
other_answers <- vrsTidy %>% 
  filter(question %in% fill_in_questions) %>%
  filter(response != "") %>% 
  unique() %>%
  arrange(question)

write.csv(other_answers, file.path(outdir, "vrs_other_answers.csv"), row.names = FALSE)

# Output sample sizes:
vrs_sample_per_village <- vrsTidy %>% 
  group_by(question, vrs_island, vrs_village) %>%
  summarise(n = n(), n_blank = sum(response == "")) %>%
  mutate(all_blank = if_else(n_blank == n, true = "yes", false = "no")) 

write.csv(vrs_sample_per_village, file.path(outdir, "vrs_sample_per_village.csv"), row.names = FALSE)

# What if we remove all_blank==yes, all multi-select questions (whenever option = NA), and all fill-in-the-blank questions
vrs_sample_per_village_cleaned <- vrsTidy %>% 
  filter(is.na(option)) %>%
  filter(question %in% fill_in_questions == FALSE) %>%
  group_by(question, vrs_island, vrs_village) %>%
  summarise(n = n(), n_blank = sum(response == "")) %>%
  mutate(all_blank = if_else(n_blank == n, true = "yes", false = "no")) %>%
  filter(all_blank != "yes")

write.csv(vrs_sample_per_village_cleaned, file.path(outdir, "vrs_sample_per_village_cleaned.csv"), row.names = FALSE)

vrs_sample_per_island <- vrsTidy %>% 
  group_by(question, vrs_island) %>%
  summarise(n = n(), n_blank = sum(response == "")) %>%
  mutate(all_blank = if_else(n_blank == n, true = "yes", false = "no"))

write.csv(vrs_sample_per_island, file.path(outdir, "vrs_sample_per_island.csv"), row.names = FALSE)

vrs_sample_overall <- vrsTidy %>% 
  group_by(question) %>%
  summarise(n = n(), n_blank = sum(response == "")) %>%
  mutate(all_blank = if_else(n_blank == n, true = "yes", false = "no"))

write.csv(vrs_sample_overall, file.path(outdir, "vrs_sample_overall.csv"), row.names = FALSE)

# Output questions with zero answers for confirmation
vrs_no_responses <- vrsTidy %>% 
  group_by(question) %>%
  summarise(n = n(), n_blank = sum(response == "")) %>%
  mutate(all_blank = if_else(n_blank == n, true = "yes", false = "no")) %>%
  filter(all_blank == "yes")

write.csv(vrs_no_responses, file.path(outdir, "vrs_no_responses.csv"), row.names = FALSE)

#___________________________________________________________________________________________#
# Clean event roster 
#___________________________________________________________________________________________#
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

# Output tidy data:
write.csv(eventRosterTidy, file.path(outdir, "eventRosterTidy.csv"), row.names = FALSE)

# Output question list
write.csv(unique(eventRosterTidy$question), file.path(outdir, "eventRoster_question_list.csv"), row.names = FALSE)

#___________________________________________________________________________________________#
# Clean fish roster
#___________________________________________________________________________________________#
fishassetRoster <- read_dta(file.path(datadir, "20201013_VRS", "fish_asset_roster.dta"))

fishassetRoster <- clean_data(fishassetRoster)[[1]]

# Join back with vrsTidy to get island info
vrs_islands <- vrsTidy %>%
  select(interview__key, interview__id, vrs_island, vrs_village) %>%
  unique()

fishassetRoster<- fishassetRoster %>%
  left_join(vrs_islands, by = c("interview__key", "interview__id"))

# Extract variable label attributes
var_labels <- clean_data(fishassetRoster)[[2]]

# Change class to character to allow left_join without warning below
var_labels <- var_labels %>%
  mutate(col.names = as.character(col.names))

# Make tidy and output data:
fishassetRosterTidy <- tidy_data(df = fishassetRoster, pivot_col_1 = "num_assets", pivot_col_last = "num_assets10", var_labels = var_labels, question_no = FALSE)
write.csv(fishassetRosterTidy, file.path(outdir, "fishassetRosterTidy.csv"), row.names = FALSE)

# Output question list
write.csv(unique(fishassetRosterTidy$question), file.path(outdir, "fishassetRoster_question_list.csv"), row.names = FALSE)

#___________________________________________________________________________________________#
# Clean outside roster
#___________________________________________________________________________________________#
outsideRoster <- read_dta(file.path(datadir, "20201013_VRS", "outside_roster.dta"))

outsideRoster <- clean_data(outsideRoster)[[1]]

# Join back with vrsTidy to get island info
vrs_islands <- vrsTidy %>%
  select(interview__key, interview__id, vrs_island, vrs_village) %>%
  unique()

outsideRoster<- outsideRoster %>%
  left_join(vrs_islands, by = c("interview__key", "interview__id"))

# Extract variable label attributes
var_labels <- clean_data(outsideRoster)[[2]]

# Change class to character to allow left_join without warning below
var_labels <- var_labels %>%
  mutate(col.names = as.character(col.names))

# Make tidy and output data:
outsideRosterTidy <- tidy_data(df = outsideRoster, pivot_col_1 = "village_distance_out", pivot_col_last = "travel_time_out", var_labels = var_labels, question_no = FALSE)

write.csv(outsideRosterTidy, file.path(outdir, "outsideRosterTidy.csv"), row.names = FALSE)

# Output question list
write.csv(unique(outsideRosterTidy$question), file.path(outdir, "outsideRosterTidy_question_list.csv"), row.names = FALSE)

# Output short answers
short_answers <- c("Other transport mode outside boundary", "Travel time outside boundary")
# Short answers - units need to be standardized and some responses need to be translated

# Output short answer responses for translation
other_answers <- outsideRosterTidy %>% 
  filter(question %in% short_answers) %>%
  filter(response != "") %>% 
  filter((str_detect(response, pattern = "[[:alpha:]]") & str_detect(response, pattern = "[[:digit:]]"))==FALSE) %>% # filter out responses that are mixed alpha/digits (e.g., 1 minute) - unit already given
  unique() %>%
  arrange(question)

write.csv(other_answers, file.path(outdir, "outsideRoster_other_answers.csv"), row.names = FALSE)

#___________________________________________________________________________________________#
# Clean share roster
#___________________________________________________________________________________________#
shareRoster <- read_dta(file.path(datadir, "20201013_VRS", "share_roster.dta"))

shareRoster <- clean_data(shareRoster)[[1]]

# Join back with vrsTidy to get island info
vrs_islands <- vrsTidy %>%
  select(interview__key, interview__id, vrs_island, vrs_village) %>%
  unique()

shareRoster<- shareRoster %>%
  left_join(vrs_islands, by = c("interview__key", "interview__id"))

# Extract variable label attributes
var_labels <- clean_data(shareRoster)[[2]]

# Change class to character to allow left_join without warning below
var_labels <- var_labels %>%
  mutate(col.names = as.character(col.names))

# Make tidy
shareRosterTidy <- tidy_data(df = shareRoster, pivot_col_1 = "catch_receiver__1", pivot_col_last = "share_other", var_labels = var_labels, question_no = FALSE)

write.csv(shareRosterTidy, file.path(outdir, "shareRosterTidy.csv"), row.names = FALSE)

# Output QUESTION list:
write.csv(unique(shareRosterTidy$question), file.path(outdir, "shareRoster_question_list.csv"), row.names = FALSE)

# List short answer questions
short_answers <- c("Other giver of the sharing of catches", "Other receiver from the sharing of catches", "Other relationship from the sharing of catches",
                   "Other sharer of the catches")
# Short answers - units need to be standardized and some responses need to be translated

# Output short answer responses for translation
other_answers <- shareRosterTidy %>% 
  filter(question %in% short_answers) %>%
  filter(response != "") %>% 
  unique() %>%
  arrange(question)

write.csv(other_answers, file.path(outdir, "shareRoster_other_answers.csv"), row.names = FALSE)

#___________________________________________________________________________________________#
# Clean VRS roster
#___________________________________________________________________________________________#
vrsRoster <- read_dta(file.path(datadir, "20201013_VRS", "vrs_roster.dta"))

vrsRoster <- clean_data(vrsRoster)[[1]]

# Join back with vrsTidy to get island info
vrs_islands <- vrsTidy %>%
  select(interview__key, interview__id, vrs_island, vrs_village) %>%
  unique()

vrsRoster<- vrsRoster %>%
  left_join(vrs_islands, by = c("interview__key", "interview__id"))

# Extract variable label attributes
var_labels <- clean_data(vrsRoster)[[2]]

# Change class to character to allow left_join without warning below
var_labels <- var_labels %>%
  mutate(col.names = as.character(col.names))

# Make tidy and output data
vrsRosterTidy <- tidy_data(df = vrsRoster, pivot_col_1 = "vrs_sex", pivot_col_last = "live_years", var_labels = var_labels, question_no = FALSE)

write.csv(vrsRosterTidy, file.path(outdir, "vrsRosterTidy.csv"), row.names = FALSE)

# Output QUESTION list:
write.csv(unique(vrsRosterTidy$question), file.path(outdir, "vrsRoster_question_list.csv"), row.names = FALSE)

# List short answer questions
short_answers <- c("Other position of the respondent")
# Short answers - units need to be standardized and some responses need to be translated

# Output short answer responses for translation
other_answers <- vrsRosterTidy %>% 
  filter(question %in% short_answers) %>%
  filter(response != "") %>% 
  unique() %>%
  arrange(question)

write.csv(other_answers, file.path(outdir, "vrsRoster_other_answers.csv"), row.names = FALSE)

#___________________________________________________________________________________________#
# Clean within roster
#___________________________________________________________________________________________#
withinRoster <- read_dta(file.path(datadir, "20201013_VRS", "within_roster.dta"))

withinRoster <- clean_data(withinRoster)[[1]]

# Join back with vrsTidy to get island info
vrs_islands <- vrsTidy %>%
  select(interview__key, interview__id, vrs_island, vrs_village) %>%
  unique()

withinRoster<- withinRoster %>%
  left_join(vrs_islands, by = c("interview__key", "interview__id"))

# Extract variable label attributes
var_labels <- clean_data(withinRoster)[[2]]

# Change class to character to allow left_join without warning below
var_labels <- var_labels %>%
  mutate(col.names = as.character(col.names))

# Make tidy
withinRosterTidy <- tidy_data(df = withinRoster, pivot_col_1 = "transport_mode_within", pivot_col_last = "travel_time_within", var_labels = var_labels, question_no = FALSE)

write.csv(withinRosterTidy, file.path(outdir, "withinRosterTidy.csv"), row.names = FALSE)


# Output QUESTION list:
write.csv(unique(withinRosterTidy$question), file.path(outdir, "withinRoster_question_list.csv"), row.names = FALSE)

# List short answer questions:
short_answers <- c("Other transport mode within boundary", "Travel time within boundary")
# Short answers - units need to be standardized and some responses need to be translated

# Output short answer responses for translation
other_answers <- withinRosterTidy %>% 
  filter(question %in% short_answers) %>%
  filter(response != "") %>% 
  filter((str_detect(response, pattern = "[[:alpha:]]") & str_detect(response, pattern = "[[:digit:]]"))==FALSE) %>% # filter out responses that are mixed alpha/digits (e.g., 1 minute) - unit already given
  unique() %>%
  arrange(question)

write.csv(other_answers, file.path(outdir, "withinRoster_other_answers.csv"), row.names = FALSE)

