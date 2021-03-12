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

# LEFT OFF HERE - are there universal columns to be used as ID columns? and the rest pivot longer to columns: response and question, and if applicable "option" 
data_cols <- NULL
# Just get column names for now:
for (i in 1:length(dta_files)) {
  hies_i <- read_dta(file.path(datadir, "20210301_HIES_FINAL", dta_files[i]))
  hies_i_clean <- clean_data(hies_i)[[1]]
  # Extract variable label attributes
  var_labels <- clean_data(hies_i)[[2]]
  
  # Change class to character to allow left_join without warning below
  var_labels <- var_labels %>%
    mutate(col.names = as.character(col.names))
  
  data_cols[[i]] <- paste(names(hies_i_clean), collapse = ", ")
}

#########################################
# Create separate folder for data requests
# Find p922, p922n1, p922n2, p922n3 for Indie's data request:
lapply(data_cols, str_detect, "p922")
i = 40
hies_i <- read_dta(file.path(datadir, "20210301_HIES_FINAL", dta_files[i]))
hies_i_clean <- clean_data(hies_i)[[1]]

# Extract variable label attributes
var_labels <- clean_data(hies_i)[[2]]

# Change class to character to allow left_join without warning below
var_labels <- var_labels %>%
  mutate(col.names = as.character(col.names)) %>%
  # NOT SURE WHY, but p922n3 was relabelled as p922n1, mutating back to p922n3
  mutate(col.labels = if_else(col.labels == "p922n1: other hunted animal", true = "p922n3: other hunted animal", false = col.labels))
  

hies_40_tidy <- pivot_data_long(df = hies_i_clean, pivot_col_1 = "sex", pivot_col_last = "p922n3", var_labels = var_labels, question_no = FALSE)

question_labels <- var_labels %>%
  filter(str_detect(col.names, "p922|p922n1|p922n2|p922n3")) %>%
  pull(col.labels)

hies_p922 <- hies_40_tidy %>%
  filter(question %in% c(question_labels, "p922n1", "p922n2", "p922n3")) # Need add these back in because var_labels for these gets split by colon; not sure this is relevant like it was for other datasets

write.csv(hies_p922, file.path(outdir, "Data-request_IRS_p922.csv"), row.names = FALSE)


#########################################

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

