# Clean and output tidy HIES data:
# Reminder: keep cleaning and plotting code separate - the code below should only clean and output tidy data,
# All plotting should be done in the prelim_HIES_review.Rmd file

# QUESTIONS TO EMAIL TO MIKE:
# What is the zip files within the zip file for? "SPC_KIR_2019_HIES_ASCII_v01.zip"

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

hies_files <- list.files(file.path(datadir, "20210301_HIES_FINAL"))
dta_files <- hies_files[grep(pattern = "\\.dta", hies_files)] # only want the STATA .dta files

# HIES DATA REQUEST:
hies_files <- list.files(file.path(datadir, "20210301_HIES_FINAL"))
dta_files <- hies_files[grep(pattern = "\\.dta", hies_files)] # only want the STATA .dta files


# get full list of individual HIES data files and each file's data columns and labels
# Use data_cols, data_labels, etc for fielding data requests
data_cols <- NULL
data_labels <- NULL
data_cols_and_labels <- NULL
data_cols_vector <- NULL
# Get column names and labels
for (i in 1:length(dta_files)) {
  hies_i <- read_dta(file.path(datadir, "20210301_HIES_FINAL", dta_files[i]))
  # hies_i_clean <- clean_data(hies_i)[[1]] # this gets the data frame
  # Extract variable label attributes
  var_labels <- clean_data(hies_i)[[2]] # this gets the column names and column labels
  
  data_cols[[i]] <- paste(var_labels$col.names, collapse = ", ") # Just column names
  data_labels[[i]] <- paste(var_labels$col.labels, collapse = ", ") # Just column labels
  data_cols_and_labels[[i]] <- var_labels # Get column names and labels
  
  # Pull data cols as a vector to get common ID columns across all data frames
  data_cols_vector[[i]] <- var_labels %>%
    pull(col.names)
}

# LEFT OFF HERE: Find common data_cols
# intersect(intersect(data_cols_vector[[1]], data_cols_vector[[2]]), data_cols_vector[[3]])
# etc etc



## From market and vrs code - reuse functions?

# Make tidy
fisheriesTidy <- pivot_data_long(df = hies_i_clean, pivot_col_1 = "sex", pivot_col_last = "p922n3", var_labels = var_labels, question_no = TRUE) # question_no = TRUE applies to fisheries data where col.labels includes a third column for question.no

write.csv(fisheriesTidy, file.path(outdir, "fisheriesTidy.csv"), row.names = FALSE)

# Output QUESTION list:
#write.csv(unique(fisheriesTidy$question), file.path(outdir, "fisheries_question_list.csv"), row.names = FALSE)






## FROM OTHER CODE:
## Fisheries data
### *Note: no island information in current dataset
# FIX IT - if island information becomes available for fisheries data, will also need to incorporate p903r and p922.dta files (in Fisheries data folder): just copy code from prelim_survey_review.Rmd

#___________________________________________________________________________________________#
# Clean fisheries data
#___________________________________________________________________________________________#
fisheries <- read_dta(file.path(datadir, "20210301_HIES_FINAL", "Fisheries.dta"))

fisheries <- clean_data(fisheries)[[1]]

# Extract variable label attributes
var_labels <- clean_data(fisheries)[[2]]

# Change class to character to allow left_join without warning below
var_labels <- var_labels %>%
  mutate(col.names = as.character(col.names))

# Make tidy
fisheriesTidy <- pivot_data_long(df = fisheries, pivot_col_1 = "sex", pivot_col_last = "p922n3", var_labels = var_labels, question_no = TRUE) # question_no = TRUE applies to fisheries data where col.labels includes a third column for question.no

write.csv(fisheriesTidy, file.path(outdir, "fisheriesTidy.csv"), row.names = FALSE)

# Output QUESTION list:
write.csv(unique(fisheriesTidy$question), file.path(outdir, "fisheries_question_list.csv"), row.names = FALSE)


#___________________________________________________________________________________________#
# Visualize fisheries data
#___________________________________________________________________________________________#
# Bar graphs and Histograms of single/multi-select questions

# Plot bar graph for each multi-select question
plotDF_multiselect <- fisheriesTidy %>%
  filter(!is.na(option)) %>%
  filter(response == "1")

plot_multi_response(plotDF_multiselect)

## Fisheries continued...
### *Single response questions*


# List short-answer, fill-in response questions here:
fill_in_questions <- c("p903n", "p921n", "p922n1", "p922n2")

# Plot histogram for each single response question while removing zeroes
plotDF_single <- fisheriesTidy %>%
  filter(is.na(option)) %>%
  filter(question.no %in% fill_in_questions == FALSE) # remove "other" responses

plot_single_response(plotDF_single, bin_n = 20)


# Output short answer responses for translation
other_answers <- fisheriesTidy %>% 
  filter(question.no %in% fill_in_questions) %>%
  filter(response != "") %>% 
  unique() %>%
  arrange(question)

write.csv(other_answers, file.path(outdir, "fisheries_other_answers.csv"), row.names = FALSE)

