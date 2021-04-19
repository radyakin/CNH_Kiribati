# Indie Reid-Shaw data request
# Email 2021/02/26

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

# get full list of individual HIES data files and each file's data columns and search for question p922
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
  filter(question %in% c(question_labels, "p922n1", "p922n2", "p922n3")) %>% # Need to add these back in because var_labels for these gets split by colon; not sure this is relevant like it was for other datasets
  filter(response != "") %>%
  filter(response != 0)

write.csv(hies_p922, file.path(outdir, "Data Requests", "2021-02-26_Data-request_IRS_p922.csv"), row.names = FALSE)

#########################################
# VRS data request
vrs <- read_dta(file.path(datadir, "20201013_VRS", "KIR_VILLAGE_RESOURCE_SURVEY.dta"))

vrs <- clean_data(vrs)[[1]]

# Extract variable label attributes
var_labels <- clean_data(vrs)[[2]]

# Change class to character to allow left_join without warning below
var_labels <- var_labels %>%
  mutate(col.names = as.character(col.names))

# Output var_labels (need this to link column names with their labels; col.names is what is listed in the questionnaire)
write.csv(var_labels, file.path(outdir, "vrsTidy_var-labels.csv"), row.names = FALSE)

# Make tidy
vrs_long <- pivot_data_long(df = vrs, pivot_col_1 = "respondent_names__0", pivot_col_last = "villageGPS__Timestamp", var_labels = var_labels, question_no = FALSE)

# Filter out specific data request:
vrs_4_2 <- vrs_long %>%
  filter(question %in% c("Items consumed by village", "Other fish consumed description", "Other non-finfish seafood consumed")) %>%
  filter(response != "") %>%
  filter(response != 0)

vrs_4_3 <- vrs_long %>%
  filter(question %in% c("Items sold by village", "Other fish sold description", "Other non-finfish seafood sold")) %>%
  filter(response != "") %>%
  filter(response != 0)



# Tidy data output
write.csv(vrs_4_2, file.path(outdir, "Data Requests", "2021-02-26_Data-request_IRS_vrs4-2.csv"), row.names = FALSE)
write.csv(vrs_4_3, file.path(outdir, "Data Requests", "2021-02-26_Data-request_IRS_vrs4-3.csv"), row.names = FALSE)

