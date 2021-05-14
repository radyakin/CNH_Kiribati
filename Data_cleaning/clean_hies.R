# Clean and output tidy HIES data:
# Reminder: keep cleaning and plotting code separate - the code below should only clean and output tidy data,
# All plotting should be done in the prelim_HIES_review.Rmd file

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

# Only want the STATA .dta files
# Some files have v01 and v02 - these are the same datasets but organized differently (v01 - household level and v02 - transaction level?)
# Keep only v01 for now - household level seems to be how data is organized throughout
dta_files <- hies_files[grep(pattern = "\\v01.dta", hies_files)] 

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
# add hm_basic__id as an id column
#id_cols <- c(id_cols, "hm_basic__id")

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
    filter(is.na(value)==FALSE) %>% # Remove questions with NA responses
    filter(value != "") # Remove questions with blank responses
}

# lapply pivot function to all hies_all
hies_long_list <- lapply(hies_all, function(i){pivot_hies_i(hies_i = i, id_cols = id_cols)})

# Column dta_file corresponds to filename in dta_files; keep this column until we're sure all col.names and col.labels are standardized (see code in manual cleaning section below)
hies_long <- bind_rows(hies_long_list, .id = "dta_file") 

# hies_long was created by joining columns that were unique to ALL data files, but there are still columns that are unique to just subsets of data files
# i.e., for a given interview, there are still duplicate question_id's
# Compare non-unique and unique question_id's
hies_long %>%
  filter(interview__key == "00-01-31-37") %>%
  pull(question_id) %>%
  length()

hies_long %>%
  filter(interview__key == "00-01-31-37") %>%
  pull(question_id) %>%
  unique() %>%
  length()

# lapply function get_var_labels to get corresponding col.names and col.labels
hies_labels_list <- lapply(hies_all, get_var_labels)
hies_labels <- bind_rows(hies_labels_list, .id = "dta_file") # add .id so that if there are common col.names across dta files these can be matched to the specific col.label from that dta file

# Which col.names have duplicate col.labels
hies_no_dta_file <- hies_labels %>% 
  select(-dta_file) %>%
  unique() %>%
  arrange(col.names)
non_standard_col_names <- as.data.frame(table(hies_no_dta_file$col.names)) %>%
  filter(Freq>1) %>%
  pull(Var1)

hies_no_dta_file %>%
  filter(col.names %in% non_standard_col_names) %>%
  unique()

hies_labels %>%
  filter(col.names == "annual_amount_clean")

hies_labels %>%
  filter(col.names == "fweight")

hies_labels %>%
  filter(col.names == "hm_basic__id")

hies_labels %>%
  filter(col.names == "interview__key")

########## IMPORTANT: MANUAL CLEANING SECTION
# MANUALLY CLEAN non_standard_col_names to be consistent throughout both hies_labels and hies_long
# fweight, hm_basic__id, and interview__key should all be the same col.label
# annual_amount_clean looks like the only col.name that is duplicated and should be renamed (expenditure vs income)

hies_labels_clean <- hies_labels %>%
  mutate(col.labels = case_when(col.names == "fweight" ~ "final sample hh weight",
                                col.names == "hm_basic__id" ~ "id in hm_basic",
                                col.names == "interview__key" ~ "interview key (identifier in xx-xx-xx-xx format)",
                                TRUE ~ col.labels)) %>%
  mutate(col.names = case_when(col.names == "annual_amount_clean" & col.labels == "cleaned annualised expenditure" ~ "annual_amount_clean_expenditure",
                               col.names == "annual_amount_clean" & col.labels == "cleaned annualised income" ~ "annual_amount_clean_income",
                               TRUE ~ col.names))

# Mutate question_id in hies_long to match the changes made in hies_labels_clean col.names
hies_long_clean <- hies_long %>%
  mutate(question_id = case_when(question_id == "annual_amount_clean" & dta_file == "25" ~ "annual_amount_clean_expenditure",
                                 question_id == "annual_amount_clean" & dta_file == "26" ~ "annual_amount_clean_income",
                                 TRUE ~ question_id))

# Check that all col.names have a unique col.label
hies_clean_no_dta_file <- hies_labels_clean %>% 
  select(-dta_file) %>%
  unique() %>%
  arrange(col.names)

non_standard_col_names <- as.data.frame(table(hies_clean_no_dta_file$col.names)) %>%
  filter(Freq>1) %>%
  pull(Var1)

non_standard_col_names # EMPTY

# Now that hies_labels_clean and hies_long_clean are standardized, we can remove dta_file and use distinct() to remove duplicate questions across data files
hies_labels_distinct <- hies_labels_clean %>%
  select(-dta_file) %>%
  distinct() 

hies_long_distinct <- hies_long_clean %>%
  select(-dta_file) %>%
  distinct() # Shrinks from 9,000,000 to 3,000,000 rows

# Attempt to pivot hies_long_distinct to get tidy format results in non-unique values
hies_long_distinct %>%
  pivot_wider(names_from = question_id, values_from = value)
# Suspect these are all the fill in the blank questions, which can have multiple answers by same respondent
# Use values_fn to identify
hies_unique_counts <- hies_long_distinct %>%
  pivot_wider(names_from = question_id, values_from = value, values_fn = length) 

# How many questions have non-unique values
hies_test <- hies_unique_counts != 1
# hies_test[1:10, 1:10] matrix of TRUE/FALSE
hies_test_df <- data.frame(non_unique_count = colSums(hies_test, na.rm = TRUE)) %>%
  rownames_to_column("question_id") 

# This is the number of question_id's with non-unique values
hies_test_df %>%
  filter(question_id %in% id_cols == FALSE) %>% # Remove ID cols - these don't have to be unique since they aren't included in pivot wide
  filter(non_unique_count > 0) %>% # Which columns contain one or more non-unique value
  nrow() 

# These are the qs that cannot be uniquely identified for tidy format (one observation per row)
qs_with_non_unique_ids <- hies_test_df %>%
  filter(question_id %in% id_cols == FALSE) %>% # Remove ID cols - these don't have to be unique since they aren't included in pivot wide
  filter(non_unique_count > 0) %>% 
  pull(question_id)

## LEFT OFF HERE: add hm_basic__id (one of the qs_with_non_unique_ids) back into id_cols


# Remove these questions from hies_long_distinct so that pivot_wide can work and come back to them later
hies_unique_qs <- hies_long_distinct %>% 
  filter(question_id %in% qs_with_non_unique_ids==FALSE)
  
# Attempt to pivot hies_long_distinct to get tidy format results in non-unique values
hies_tidy <- hies_unique_qs %>%
  pivot_wider(names_from = question_id, values_from = value)



# Outputs:
# hies_long_distinct - read in all dataframes in list dta_file: pivot long, combine by columns unique to ALL data files, then remove additional duplicate questions (common to subsets of data files)
# OR - write out hies_unique_qs instead so that the two data frames TIDY vs LONG are equivalent?
# hies_tidy


########################
# OLD CODE:
# Join clean labels to hies_tidy
hies_tidy_labels <- hies_long %>%
  left_join(hies_labels_clean, by = c("question_id" = "col.names", "dta_file")) %>%
  rename(question_label = col.labels)

# NEXT STEPS:
# Test hies_tidy_labels with IRS last roster request (Example: question P907_)
# For example, see interview__key = "00-57-18-79"
hies_tidy_labels %>%
  filter(str_detect(question_id, "p907_")) %>%
  arrange(interview__key)

# Needs to be joined with roster contextual question
# Answer to p9075 and p907_11 (number of hours) corresponds to p903_5 (spearfishing) and p903_11 (other fishing methods)
hies_tidy_labels %>%
  filter(str_detect(question_id, "p903_")) %>%
  arrange(interview__key) %>%
  print(n = 11)

# Furthermore needs to be joined with fill-in-the-blanks
hies_tidy_labels %>%
  filter(str_detect(question_id, "p903n")) 
