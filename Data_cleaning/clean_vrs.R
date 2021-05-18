# Clean and output tidy VRS data:
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

# Looks to be already tidy
vrs_tidy <- read_dta(file.path(datadir, "20201013_VRS", "KIR_VILLAGE_RESOURCE_SURVEY.dta"))

id_cols <- c("interview__key", "interview__id")

vrs_long <- pivot_dat_i(vrs_tidy, id_cols = id_cols) %>%
  filter(value != "##N/A##")

vrs_labels <- get_var_labels(vrs_tidy)

# Assume that fill in the blank responses will be those that have no other matching
vrs_fill_in_the_blank <- vrs_long %>%
  filter(str_detect(value, pattern = "[:alpha:]")) 

# FIX IT - move to top once finalized
# Outputs:
# Final long format of all uniquely identified questions: vrs_long
# Final tidy format: vrs_tidy
# Key for matching col.names (question_id) to col.labels: vrs_labels
# Short answers for translation: vrs_fill_in_the_blank

write.csv(vrs_long, file = file.path(outdir, "vrs_long.csv"), row.names = FALSE)
write.csv(vrs_tidy, file = file.path(outdir, "vrs_tidy.csv"), row.names = FALSE)
write.csv(vrs_labels, file = file.path(outdir, "vrs_question-id-to-label-key.csv"), row.names = FALSE)
write.csv(vrs_fill_in_the_blank, file = file.path(outdir, "vrs_fill-in-the-blanks-for-translation.csv"), row.names = FALSE)

