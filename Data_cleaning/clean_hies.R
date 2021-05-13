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

# Find common data_cols to bind all data frames
# Intersection of common data cols for first three data files
id_cols <- intersect(intersect(data_cols_vector[[1]], data_cols_vector[[2]]), data_cols_vector[[3]])

# Confirm id_cols are in all data files
lapply(data_cols_vector, FUN = intersect, id_cols)

# Original IRS data request
# hies_40 <- read_dta(file.path(datadir, "20210301_HIES_FINAL", dta_files[unlist(lapply(data_cols, str_detect, "p907"))]))
# Current output format of read_dta (hies_40) WILL write out to a CSV but only the value (not the label) e.g., 1, 2, 3, 4, etc but not the corresponding text label S Tarawa, Northern, Central, Southern, etc.

# Pivot long based on id_cols
# hies_40_long <- hies_40 %>%
#   mutate(across(where(is.numeric), as.character)) %>% # Mutate all columns to be character so all questions (numeric or character) can be combined into a single long pivot column
#   pivot_longer(cols = !all_of(c(id_cols)), names_to = "question_id") %>% 
#   filter(is.na(value)==FALSE) # Remove questions with NA responses

# lpply read_dta 
hies_all <- lapply(dta_files, function(i){read_dta(file.path(datadir, "20210301_HIES_FINAL", i))})

# FUNCTION: Pivot long based on id_cols
pivot_hies_i <- function(hies_i, id_cols){
  hies_i_long <- hies_i %>%
  mutate(across(where(is.numeric), as.character)) %>% # Mutate all columns to be character so all questions (numeric or character) can be combined into a single long pivot column
  pivot_longer(cols = !all_of(c(id_cols)), names_to = "question_id") %>% 
  filter(is.na(value)==FALSE) # Remove questions with NA responses}
}

# lapply pivot function to all hies_all
hies_long_list <- lapply(hies_all, function(i){pivot_hies_i(hies_i = i, id_cols = id_cols)})

hies_tidy <- bind_rows(hies_long_list, .id = "dta_file") # Column dta_file corresponds to filename in dta_files   

# lapply function get_var_labels to get corresponding col.names and col.labels
hies_labels_list <- lapply(hies_all, get_var_labels)
hies_labels <- bind_rows(hies_labels_list)
hies_labels <- unique(hies_labels) 

# NEXT: check that all non-ID columns are unique
# interview__key is one of the id_cols but these aren't consistently labelled; See: hies_labels %>% filter(col.names == "interview__key")
# Just keep names of id_cols as is, don't replace with col.labels since these are inconsistent

# NEXT STEPS:
# Join labels with hies_tidy
# Test hies_tidy with IRS last roster request

# Outputs:
# hies_tidy - all dataframes in list dta_file, pivot long (tidy format) and combined
# dta_file - list of dta_files that correspond to column dta_file number in hies_tidy
#