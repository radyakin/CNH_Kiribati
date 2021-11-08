# Chris Golden data request
# Email 2021/04/09

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

# HIES DATA REQUEST:
hies_files <- list.files(file.path(datadir, "20210301_HIES_FINAL"))
dta_files <- hies_files[grep(pattern = "\\.dta", hies_files)] # only want the STATA .dta files

# get full list of individual HIES data files and each file's data columns and labels
data_cols <- NULL
data_labels <- NULL
data_cols_and_labels <- NULL
# Get column names and labels
for (i in 1:length(dta_files)) {
  hies_i <- read_dta(file.path(datadir, "20210301_HIES_FINAL", dta_files[i]))
  # hies_i_clean <- clean_data(hies_i)[[1]] # this gets the data frame
  # Extract variable label attributes
  var_labels <- clean_data(hies_i)[[2]] # this gets the column names and column labels
  
  data_cols[[i]] <- paste(var_labels$col.names, collapse = ", ") # Just column names
  data_labels[[i]] <- paste(var_labels$col.labels, collapse = ", ") # Just column labels
  data_cols_and_labels[[i]] <- var_labels # Get column names and labels
}

# Request List:
# 1) Island name (and code if it exists) 
# i = 1; col.name = island
hies_1 <- read_dta(file.path(datadir, "20210301_HIES_FINAL", dta_files[1]))
hies_1_clean <- clean_data(hies_1)[[1]]
var_labels_1 <- clean_data(hies_1)[[2]]

# 2) Neighborhood name (and code if it exists;  I believe that each island was segmented into various geogprahical areas) 
lapply(data_labels, str_detect, "neighborhood")
lapply(data_labels, str_detect, "geo") # match to i = 40 
data_cols_and_labels[[40]] %>%
  filter(str_detect(col.labels, "geo")) # false match (surgeonfish and pigeon)
lapply(data_labels, str_detect, "area") # match to i = 1, 33, 34
data_cols_and_labels[[1]] %>%
  filter(str_detect(col.labels, "area")) # i = 1; col.name = ea (col.label = enumeration area); get village too just in case

# 3) Household name (and code if it exists) 
# i = 1; col.name = hhld_id (col.label = household id)

# 4) Household size
# i = 1; col.name = hhsize (col.label = household size

# 5) Geo ID 
# i = 1; col.name = division (col.label = group of islands)
hies_2 <- read_dta(file.path(datadir, "20210301_HIES_FINAL", dta_files[2]))
hies_2_clean <- clean_data(hies_2)[[1]]
var_labels_2 <- clean_data(hies_2)[[2]]

# 6) Individual name 
# i = 2; Unique combinations of col.name = interview__id and hm_basic__id?

# 7) Individual Date of birth 
# p104b Day of birth; p104a Month of birth; p104c Year of birth 
# Doesn't exist in our data?
lapply(data_cols, str_detect, "p104")
lapply(data_labels, str_detect, "birth") # false match: have celebrations on special occassions such as birthdays?
# Just get age/sex?

# Join relevant columns in hies_1_clean and hies_2_clean
# First get relevant join columns
intersect(names(hies_1_clean), names(hies_2_clean))
#Use: "interview__key", "interview__id"

# Get relevant columns from each dataframe
data_request_1 <- hies_1_clean %>%
  select(interview__key, interview__id, island, village, ea, hhld_id, hhsize, division) %>%
  unique()

# Get relevant join columns
data_request_2 <- hies_2_clean %>%
  select(interview__key, interview__id, hm_basic__id, age, sex) %>%
  unique()

cdg_data_request <- data_request_1 %>%
  full_join(data_request_2, by = c("interview__key", "interview__id"))

write.csv(cdg_data_request, file.path(outdir, "Data Requests", "2021-04-09_Data-request_CDG.csv"), row.names = FALSE)
