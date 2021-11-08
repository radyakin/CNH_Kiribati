# Indie Reid-Shaw data request
# Email 2021/04/12

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

# P907.
# In the last 7 days, how many hours did %hmName% spend using the %rostertitle% Method?
# Get the name of the data file that matches p907
# P907a.
# In the last 7 days, how many fishing trips did %hmName% undertake using the %rostertitle% Method?
# P908.
# How many hours does %hmName% usually use for a typical trip while engaging in the %rostertitle% method?  
# P909.
# In the last 7 days, what was the total weight (in pounds) of %hmName%'s catch using the %rostertitle% method? 
# P910. 
# How would %hmName% rate this week's catch compared to %hisHer% usual catch while using the %rostertitle% method?.
# P923.
# In the last 7 days, how much %sCatch% (in pounds) did %hmName% catch?
# All the same data file:
dta_files[unlist(lapply(data_cols, str_detect, "p907"))]
df_dat_request <- read_dta(file.path(datadir, "20210301_HIES_FINAL", dta_files[unlist(lapply(data_cols, str_detect, "p907"))]))

# df_clean_tmp <- clean_data(df)[[1]] 
# First, extract variable label attributes
var_labels_dat_request <- clean_data(df_dat_request)[[2]]

# Note: requested data are ROSTERS, so will have to retain original col.names; otherwise will lose the specific roster ID
# i.e., don't use pivot_dat_longer which replaces col.names with col.labels
# Example: p907 has a different column name for each fishing method 1 through 11 (e.g., p907__1, p907__2, p907__3, etc)
# Fishing method comes from question p903
var_labels_dat_request %>%
  filter(str_detect(col.names, "p907_")) # use p907_ vs p907a to differentiate

var_labels_dat_request %>%
  filter(str_detect(col.names, "p907a")) # use p907_ vs p907a to differentiate

var_labels_dat_request %>%
  filter(str_detect(col.names, "p908"))

var_labels %>%
  filter(str_detect(col.names, "p909"))

var_labels_dat_request %>%
  filter(str_detect(col.names, "p910"))

var_labels_dat_request %>%
  filter(str_detect(col.names, "p923"))

# Process one data request questions individually
# Test function of p907
data_col_match <- "p907_"
roster_match <- "p903_"
fill_in_the_blank_match <- "p903n"
var_labels <- var_labels_dat_request
df <- df_dat_request

# OLD VERSION: only one write-in question
# Create function for roster data requests:
# clean_roster_data_request <- function(df, var_labels, data_col_match, roster_match, fill_in_the_blank_match) {
#   
#   dat_request_cols <- var_labels %>%
#     filter(str_detect(col.names, data_col_match)) %>%
#     pull(col.names) %>%
#     unique() %>%
#     sort()
#   
#   # Notice p907, p907a, p908, p910 all have version 1 through 11
#   # This corresponds to question p903: FISHING METHOD
#   # NEED TO KEEP THIS COLUMN AS ONE OF THE ID COLUMNS 
#   
#   roster_cols <- var_labels %>%
#     filter(str_detect(col.names, roster_match)) %>% # See note below Re: p903
#     pull(col.names) %>%
#     unique()
#   
#   # Later, need to bring back p903n (this is the fill in the blank respionse for if p903_11 == 1)
#   id_cols <- c("interview__key",
#                "fweight",
#                "division",
#                "island",
#                "village",
#                "rururb",
#                "hm_basic__id",
#                "sex",
#                "age",
#                roster_cols)
# 
#   # ORIGINAL working code for single fill_in_the_blank column  
#   df_roster <- clean_data(df)[[1]] %>%
#     select(all_of(c(id_cols, dat_request_cols, fill_in_the_blank_match))) %>% # Bring back column p903n here (fill in the blank for when p903_11 == 1 - i.e., "other fishing method")
#     # Turn roster_cols into an ID column by pivotting longer and left_joining with var_labels
#     pivot_longer(cols = all_of(roster_cols), names_to = "roster", values_to = "response") %>%
#     # Remove response == 0
#     filter(response == 1) %>%
#     left_join(var_labels, by = c("roster" = "col.names")) %>%
#     rename(roster_label = col.labels) %>%
#     rename(roster_write_in = !!as.symbol(fill_in_the_blank_match)) %>%
#     select(-response)
#   
#   # Use this as an example of why roster info needs to match data request columns
#   not_all_na <- function(x) any(!is.na(x))
#   # EXAMPLE: df_roster  %>% filter(interview__key == "01-43-71-19") %>% select_if(not_all_na)
#   # EXAMPLE: df_roster %>% filter(str_detect(fishing_method, "other"))
#   
#   # Now clean data_request columns by pivotting longer and left_joining with var_labels
#   df_dat_request <- df_roster %>%
#     pivot_longer(all_of(dat_request_cols), names_to = "question", values_to = "response") %>%
#     filter(is.na(response) == FALSE) %>%
#     left_join(var_labels, by = c("question" = "col.names"))
#   
#   # EXAMPLE: df_dat_request %>% filter(interview__key == "01-43-71-19")
#   
#   # Match data request column to the fishing method column using the roster and question columns
#   # Example p907__5 corresponds to p903__5, remove all other p907__X
#   df_match_roster <- df_dat_request %>%
#     mutate(roster = gsub(x = roster, pattern = ".*__", replacement = "")) %>%
#     mutate(question = gsub(x = question, pattern = ".*__", replacement = "")) %>%
#     # Remove non-matches
#     filter(roster == question)
#   
#   # EXAMPLE: df_match_roster %>% filter(interview__key == "01-43-71-19")
#   
#   # Final cleaning
#   df_clean <- df_match_roster %>%
#     select(-c(roster, question)) %>%
#     rename(question = col.labels) %>%
#     arrange(interview__key, hm_basic__id)
#   
#   # EXAMPLE: df_clean %>% filter(interview__key == "01-43-71-19")
#   return(df_clean)
# }

# New version works with multiple write-in roster questions:
# Create function for roster data requests:
clean_roster_data_request <- function(df, var_labels, data_col_match, roster_match, fill_in_the_blank_match) {
  
  dat_request_cols <- var_labels %>%
    filter(str_detect(col.names, data_col_match)) %>%
    pull(col.names) %>%
    unique() %>%
    sort()
  
  roster_cols <- var_labels %>%
    filter(str_detect(col.names, roster_match)) %>% # See note below Re: p903
    pull(col.names) %>%
    unique()
  
  # Later, need to bring back p903n (this is the fill in the blank respionse for if p903_11 == 1)
  id_cols <- c("interview__key",
               "fweight",
               "division",
               "island",
               "village",
               "rururb",
               "hm_basic__id",
               "sex",
               "age",
               roster_cols)
  
  # ORIGINAL working code for single fill_in_the_blank column  
  df_roster <- clean_data(df)[[1]] %>%
    select(all_of(c(id_cols, dat_request_cols, fill_in_the_blank_match))) %>% # Bring back column p903n here (fill in the blank for when p903_11 == 1 - i.e., "other fishing method")
    # Turn roster_cols into an ID column by pivotting longer and left_joining with var_labels
    pivot_longer(cols = all_of(roster_cols), names_to = "roster", values_to = "response") %>%
    # Remove response == 0
    filter(response == 1) %>%
    left_join(var_labels, by = c("roster" = "col.names")) %>%
    rename(roster_label = col.labels) %>%
    unite("roster_write_in", all_of(fill_in_the_blank_match), sep = "") %>%
    select(-response)
  
  # Use this as an example of why roster info needs to match data request columns
  # not_all_na <- function(x) any(!is.na(x))
  # EXAMPLE: df_roster  %>% filter(interview__key == "01-43-71-19") %>% select_if(not_all_na)
  # EXAMPLE: df_roster %>% filter(str_detect(fishing_method, "other"))
  
  # Now clean data_request columns by pivotting longer and left_joining with var_labels
  df_dat_request <- df_roster %>%
    pivot_longer(all_of(dat_request_cols), names_to = "question", values_to = "response") %>%
    filter(is.na(response) == FALSE) %>%
    left_join(var_labels, by = c("question" = "col.names"))
  
  # EXAMPLE: df_dat_request %>% filter(interview__key == "01-43-71-19")
  
  # Match data request column to the fishing method column using the roster and question columns
  # Example p907__5 corresponds to p903__5, remove all other p907__X
  df_match_roster <- df_dat_request %>%
    mutate(roster = gsub(x = roster, pattern = ".*__", replacement = "")) %>%
    mutate(question = gsub(x = question, pattern = ".*__", replacement = "")) %>%
    # Remove non-matches
    filter(roster == question)
  
  # EXAMPLE: df_match_roster %>% filter(interview__key == "01-43-71-19")
  
  # Final cleaning
  df_clean <- df_match_roster %>%
    select(-c(roster, question)) %>%
    rename(question = col.labels) %>%
    arrange(interview__key, hm_basic__id)
  
  # EXAMPLE: df_clean %>% filter(interview__key == "01-43-71-19")
  return(df_clean)
}

dta_files[unlist(lapply(data_cols, str_detect, "p907"))]
df_dat_request <- read_dta(file.path(datadir, "20210301_HIES_FINAL", dta_files[unlist(lapply(data_cols, str_detect, "p907"))]))

# df_clean_tmp <- clean_data(df)[[1]] 
# First, extract variable label attributes
var_labels_dat_request <- clean_data(df_dat_request)[[2]]

p907_dat_request <- clean_roster_data_request(df = df_dat_request, var_labels = var_labels_dat_request, data_col_match = "p907_", roster_match = "p903_", fill_in_the_blank_match = "p903n")
p907a_dat_request <- clean_roster_data_request(df = df_dat_request, var_labels = var_labels_dat_request, data_col_match = "p907a", roster_match = "p903_", fill_in_the_blank_match = "p903n") 
p908_dat_request <- clean_roster_data_request(df = df_dat_request, var_labels = var_labels_dat_request, data_col_match = "p908", roster_match = "p903_", fill_in_the_blank_match = "p903n")
p909_dat_request <- clean_roster_data_request(df = df_dat_request, var_labels = var_labels_dat_request, data_col_match = "p909", roster_match = "p903_", fill_in_the_blank_match = "p903n")
p910_dat_request <- clean_roster_data_request(df = df_dat_request, var_labels = var_labels_dat_request, data_col_match = "p910", roster_match = "p903_", fill_in_the_blank_match = "p903n")

write.csv(p907_dat_request, file.path(outdir, "Data Requests", "2021-04-19_Data-request_IRS_p907.csv"), row.names = FALSE)
write.csv(p907a_dat_request, file.path(outdir, "Data Requests", "2021-04-19_Data-request_IRS_p907a.csv"), row.names = FALSE)
write.csv(p908_dat_request, file.path(outdir, "Data Requests", "2021-04-19_Data-request_IRS_p908.csv"), row.names = FALSE)
write.csv(p909_dat_request, file.path(outdir, "Data Requests", "2021-04-19_Data-request_IRS_p909.csv"), row.names = FALSE)
write.csv(p910_dat_request, file.path(outdir, "Data Requests", "2021-04-19_Data-request_IRS_p910.csv"), row.names = FALSE)

# p923 has multiple fill in the blank columns

# data request columns
var_labels_dat_request %>%
  filter(str_detect(col.names, "p923"))

# roster for p923 is p922 (4 main catches)
var_labels_dat_request %>%
  filter(str_detect(col.names, "p922_"))

# fill in the blank columns
var_labels_dat_request %>%
  filter(str_detect(col.names, "p922n"))

# Test function:
# data_col_match <- "p923"
# roster_match <- "p922_"
# fill_in_the_blank_match <- c("p922n1", "p922n2", "p922n3")
# var_labels <- var_labels_dat_request
# df <- df_dat_request

p923_dat_request <- clean_roster_data_request(df = df_dat_request, var_labels = var_labels_dat_request, data_col_match = "p923", roster_match = "p922_", fill_in_the_blank_match = c("p922n1", "p922n2", "p922n3"))
write.csv(p923_dat_request, file.path(outdir, "Data Requests", "2021-04-20_Data-request_IRS_p923.csv"), row.names = FALSE)



# FIX IT - QUESTION FOR JG/MIKE: Where's this data? Continue to next section of Indie's request
# H1503b. In the last 7 days, how much (unit of quantity) (all for 2 decimal pts) %rostertitle% did your household CONSUME?
# H1503c. What is the unit of quantity CONSUMED? 
# Need to investigate further h1503.... no matches for h1503b
dta_files[unlist(lapply(data_cols, str_detect, "h1503b"))]
# Look at column labels that match h1503
data_cols_and_labels[names(data_cols_and_labels)==dta_files[unlist(lapply(data_cols, str_detect, "h1503"))]]


# MARKET SURVEY DATA REQUEST:   
# MS_FS2. Select all the FISH products that are available today ?
# MS_FS3a. How many different units of %rostertitle% are available today? (pounds, ounces)
# MS_FS3b. How often is the %rostertitle% available? (single select)
# MS_FS5. Record the weight (number) for a%rostertitle%?
# MS_FS6. Record the unit for a %rostertitle%? (lbs/kgs)

# Market Data:
# MacOS
datadir <- "/Volumes/jgephart/Kiribati/Data"
outdir <- "/Volumes/jgephart/Kiribati/Outputs"
marketSurveyTidy <- read.csv(file.path(outdir, "marketSurveyTidy.csv"))
# Already cleaned: just send marketSurveyTidy.csv to Indie

# VRS Data:
# VRS4_10.Overall, compared to 10 years ago, has the level of fish stock in your sea/lagoon .....?
# VRS4_12.Overall, compared to 10 years ago, has consumption of fish and seafoods......?
# VRS4_13.Overall, compared to 10 years ago, has the physical condition of your sea/lagoon...?
vrsTidy <- read.csv(file.path(outdir, "vrsTidy.csv"))


# Based on matching column names with corresponding column labels, IRS needs:
#"Level of fish stock"
#"Consumption of ocean foods"
#"Physical condition of sea"

vrs4_10 <- vrsTidy %>%
  filter(question == "Level of fish stock") %>%
  select(-option)

vrs4_12 <- vrsTidy %>%
  filter(question == "Consumption of ocean foods") %>%
  select(-option)

vrs4_13 <- vrsTidy %>%
  filter(question == "Physical condition of sea") %>%
  select(-option)

write.csv(vrs4_10, file.path(outdir, "Data Requests", "2021-04-20_Data-request_IRS_vrs4-10.csv"), row.names = FALSE)
write.csv(vrs4_12, file.path(outdir, "Data Requests", "2021-04-20_Data-request_IRS_vrs4-12.csv"), row.names = FALSE)
write.csv(vrs4_13, file.path(outdir, "Data Requests", "2021-04-20_Data-request_IRS_vrs4-13.csv"), row.names = FALSE)