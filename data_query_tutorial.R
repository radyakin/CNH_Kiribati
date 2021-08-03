# Examples for querying HIES data
# Change file.path directories as needed
rm(list=ls())
library(tidyverse)
datadir <- "/Volumes/jgephart/Kiribati/Data"
outdir <- "/Volumes/jgephart/Kiribati/Outputs"

##############################################################################################################
# First begin by downloading NSF team's Google Folder: Kiribati/Data/Starting Point for Data Pulls
# Read through the _INTRO and _README files in the folder

##############################################################################################################
# Start by loading all relevant data files needed for the two example data queries below

# Metadata for all question IDs and descriptions:
var_labels <- read.csv(file = file.path(outdir, "2021-07-29_hies_question-id-to-label-key.csv"))

# HIES household and individual-level tidy data
hies_tidy <- read.csv(file = file.path(outdir, "2021-07-29_hies_tidy_individual-level.csv"))
hies_tidy_house <- read.csv(file = file.path(outdir, "2021-07-29_hies_tidy_household-level.csv"))

# Other HIES special-format data that doesn't pivot tidy with the rest of the dataset
hies_expenditure <- read.csv(file.path(outdir, "2021-07-29_hies_expenditures-standard-units.csv"))
root_recall <- read.csv(file = file.path(outdir, "2021-07-29_special-roster-hies_root-crop-details.csv"))

# VRS tidy
vrs_tidy <- read.csv(file = file.path(outdir, "vrs_tidy.csv"))

# VRS metadata
vrs_labels <- read.csv(file = file.path(outdir, "vrs_question-id-to-label-key.csv"))

##############################################################################################################
# Katy's examples:

# P901-P925
# P801-P803
# P810
# MIG_P9 == migrant_prev_work
# Food recall from p. 81
# Dietary recall from p. 124
# HIES 1503 (all subquestions)

# We'll split these into three sections below: HIES, food recall, and diet recall queries

#########################################################
# (a) Katy's HIES query 
# Create a search pattern for all the question IDs that you're interested in
p9s <- paste("p", 901:925, sep = "")
p8s <- c(paste("p", 801:803, sep = ""), "p810")
search_pattern <- paste(c(p9s, p8s, "migrant_prev_work"), collapse = "|")

var_labels %>% 
  filter(str_detect(col.names, search_pattern))

hies_query <- var_labels %>% 
  filter(str_detect(col.names, search_pattern)) %>%
  pull(col.names)

# When I attempt to subset "all_of" these col.names, I get an error message that some columns don't exist (e.g., p905__1_4`, `p905__1_7, `p905__10_4`, `p905__10_7`, `p905__11_4`)
# These might correspond to questions that were never applicable (e.g., p905__1_4 means someone fished in January using method night spearfishing)
hies_tidy %>%
  select(all_of(hies_query))

hies_data <- hies_tidy %>%
  select(any_of(hies_query))

#########################################################
# (b) Katy's food recall query from page 81 and Fish and seafood recall (h1503 and all subquestions)
# For food recall questions, query the expenditures dataset
# Unlike hies_tidy, expenditures data (including food recall questions) are not tagged by question ID

# Notice there are several sections included here:
sort(unique(hies_expenditure$section))

food_recall <- hies_expenditure %>%
  filter(section == "21_foodrecall")

# To get specific types of food recall:
# (1) Filter by specific coicop_class
unique(food_recall$coicop_class)

# For food recall from page 81 of HIES, Roster: "Details on snacks, candies and confectionery", filter to coicop_class == "Sugar, jam, honey, chocolate and confectionery"
h1508_all <- food_recall %>%
  #filter(description %in% list_of_descriptions) %>% # if filtering by "Description"
  filter(coicop_class == "Sugar, jam, honey, chocolate and confectionery")

# The "description" column gives you the individual food recall items, including fill in the blank responses
unique(h1508_all$description)

# The food_desc_pndb is metadata joined to expenditures data that assigns category to each item (can serve as rough translation of some of the fill in the blank response)
h1508_all %>%
  select(description, food_desc_pndb, coicop_class) %>%
  arrange(description) %>%
  distinct()

# Use var_labels to get descriptions of columns in food_recall - can use this to help decide which are relevant to your data query
var_labels %>%
  filter(col.names %in% names(h1508_all)) %>%
  select(-dta_filename) %>%
  distinct() %>%
  filter(str_detect(col.labels, "quantity|gift|expenditure"))

h1508_query <- var_labels %>%
  filter(col.names %in% names(h1508_all)) %>%
  select(-dta_filename) %>%
  distinct() %>%
  filter(str_detect(col.labels, "quantity|gift|expenditure")) %>%
  pull(col.names)

# Subset h1508_all to just the data columns you're interested in
h1508_all %>%
  select(all_of(h1508_query))

# Follow the same steps to get the Fish and seafood data
h1503_all <- food_recall %>%
  #filter(description %in% list_of_descriptions) %>% # if filtering by "Description"
  filter(coicop_class == "Fish and sea food")

# The "description" column gives you the individual food recall items, including fill in the blank responses
unique(h1503_all$description)

# The food_desc_pndb is metadata joined to expenditures data that assigns category to each item (can serve as rough translation of some of the fill in the blank response)
h1503_all %>%
  select(description, food_desc_pndb, coicop_class) %>%
  arrange(description) %>%
  distinct()

# Use var_labels to get descriptions of columns in food_recall - can use this to help decide which are relevant to your data query
var_labels %>%
  filter(col.names %in% names(h1503_all)) %>%
  select(-dta_filename) %>%
  distinct() %>%
  filter(str_detect(col.labels, "quantity|gift|expenditure"))

h1503_query <- var_labels %>%
  filter(col.names %in% names(h1508_all)) %>%
  select(-dta_filename) %>%
  distinct() %>%
  filter(str_detect(col.labels, "quantity|gift|expenditure")) %>%
  pull(col.names)

# Subset h1508_all to just the data columns you're interested in
h1503_all %>%
  select(all_of(c(h1503_query, "interview__key", "description", "food_desc_pndb", "coicop_class")))

#########################################################
# (c) Katy's diet recall query - p 124

# Search var_labels for possible col.names
root_qs <- paste("h19", 12:16, sep = "")
root_search <- paste(root_qs, collapse = "|")

var_labels %>% 
  filter(str_detect(col.names, root_search))

root_query <- var_labels %>% filter(str_detect(col.names, root_search)) %>% pull(col.names)

# Notice: none of these are in hies_tidy
hies_tidy %>%
  select(any_of(root_query))

# That's because all diet recall questions do not pivot tidy and are found in the special roster data files
names(root_recall)

# according to var_labels, these all come from the file: SPC_KIR_2019_HIES_19a-AgricParcel_v01.dta - i.e., should be in either hies_tidy or hies_tidy_house
# so far, we've only checked hies_tidy, check hies_tidy_house

# check household-level data
hies_root_recall <- hies_tidy_house %>%
  select(any_of(root_query))

# i.e., diet-recall questions are split between the special rosters and the house-hold level hies data

##############################################################################################################
# Jacob's examples:
# HIES H13b1 (specifically responses 05, 06 and 07)
# VRS VRS4_1 with associated VRS4_1a and VRS4_1b

#########################################################
# (a) Jacob's HIES query

var_labels %>% 
  filter(str_detect(col.names, "h13b1"))

# Create a query for h13b1 to h13b20 and only for __5, __6, and __7
h13bs <- paste("h13b", 1:20, sep = "")
search_pattern <- paste(h13bs, collapse = "|")

var_labels %>% 
  filter(str_detect(col.names, search_pattern))

# Now filter down to just 5, 6, 7
var_labels %>%
  filter(str_detect(col.names, search_pattern)) %>%
  filter(str_detect(col.names, c("__5|__6|__7")))

h13b_query <- var_labels %>%
  filter(str_detect(col.names, search_pattern)) %>%
  filter(str_detect(col.names, c("__5|__6|__7"))) %>%
  pull(col.names)

# Remember to query both individual and household-level datasets
hies_tidy %>%
  select(any_of(h13b_query))

# None of these pertain to individual-level dataset
hies_tidy_house %>%
  select(all_of(h13b_query))

# Some (not all) are in the household-level dataset
hies_tidy_house %>%
  select(any_of(h13b_query))

# Which ones are missing:
h13b_found <- hies_tidy_house %>%
  select(any_of(h13b_query)) %>%
  names()

# Questions about spare parts and insurance are not available
setdiff(h13b_query, h13b_found)
# [1] "h13b13__5" "h13b13__6" "h13b13__7" "h13b17__5" "h13b17__6" "h13b17__7"

# Can use var_labels to also figure out how to dig back into the clean_hies.R just to make sure it wasn't lost
# According to var_labels, these all come from this dta file: SPC_KIR_2019_HIES_13b-Vehicles_v01.dta
# Run clean_hies through creation of hies_long
# Notice dta_files[7] corresponds to "Vehicles" data
# Notice no data available for these missing columns: hies_long_list[[7]] %>% arrange(question_id) %>% pull(question_id) %>% unique()

#########################################################
# (b) Jacob's VRS query
# VRS VRS4_1 with associated VRS4_1a and VRS4_1b

# None of the VRS questions are part of the HIES
var_labels %>%
  filter(str_detect(col.names, "vrs|fish_asset"))

sort(names(vrs_tidy))

# Create query for all fish_asset_list questions
fish_asset_query <- paste("fish_asset_list__", 1:17, sep = "")

# subset from vrs_tidy, including the fill-in-the-blank response fishing_other
vrs_tidy %>%
  select(all_of(c(fish_asset_query, "fishing_other"))) %>% as_tibble()

# What about num_assets and num_assets10? Check vrs_labels - can't find these and will email Mike about it
vrs_labels %>% filter(str_detect(col.names, "num"))
vrs_labels %>% filter(str_detect(col.names, "asset"))
vrs_labels %>% filter(str_detect(col.labels, "num"))
vrs_labels %>% filter(str_detect(col.labels, "asset"))
vrs_labels %>% filter(str_detect(col.names, "__1"))

#########################################################
# OLD: Data query code from Indy's first data request
id_cols <- c("interview__key",
             "hm_basic__id")

# For non-roster questions, select column name
# example using p319
dat_col_match = "p319"

# Check var_labels to see these match to the correct question_id's
var_labels %>%
  filter(str_detect(col.names, dat_col_match))

# Subset df
df_query <- df_tidy %>%
  select(all_of(id_cols), contains(dat_col_match))

# save df_query
# write.csv(df_query, file = "insert-filename-here.csv")

# For roster questions (i.e., questions that are generated based on responses to other questions):
# example using p923, roster question with a linked question that also contains multiple fill in the blank columns

dat_col_match = "p923" # matches to p923__1, p923__2, p923__3

link_match = "p922_"

fill_in_the_blank_match = "p922n"

# Check var_labels to see these match to the correct question_id's
var_labels %>%
  filter(str_detect(col.names, dat_col_match))

var_labels %>%
  filter(str_detect(col.names, link_match))

var_labels %>%
  filter(str_detect(col.names, fill_in_the_blank_match))

df_query <- df_tidy %>%
  select(all_of(id_cols), contains(c(dat_col_match, link_match, fill_in_the_blank_match)))

# save df_query
# write.csv(df_query, file = "insert-filename-here.csv")

# For even more context can add in p901 (notice p922 only applies if respondent answered 1 to p901)

# Always check var_labels to see these match to the correct question_id's
var_labels %>%
  filter(str_detect(col.names, "p901"))

df_query <- df_tidy %>%
  select(all_of(id_cols), contains(c(dat_col_match, link_match, fill_in_the_blank_match, "p901")))

# Removing all responses that didn't go fishing makes the dataset more manageable
df_query %>%
  filter(p901 == 1) %>%
  dim()

# Make sure to trace back all relevant layers of roster questions
# p318 and p318n relates to respondents who answer "no" to p314
# p315, p316, p317, and p317n relates to respondents who answer "yes" to p314
# p314 only applies to respondents who answered yes to any of the p308
# For multiple layers of roster questions:

dat_col_match <- c("p308", "p314", "p315", "p315n", "p316", "p317", "p317n", "p318", "p318n")

# Check column labels
var_labels %>%
  filter(str_detect(col.names, paste(dat_col_match, collapse = "|")))

df_query <- df_tidy %>%
  select(all_of(id_cols), contains(dat_col_match))

# save df_query
# write.csv(df_query, file = "insert-filename-here.csv")

# For food recall questions, query the expenditures dataset
hies_expenditure <- read.csv(file.path(outdir, "hies_expenditures-standard-units.csv"))

# Notice there are several sections included here:
sort(unique(hies_expenditure$section))

food_recall <- hies_expenditure %>%
  filter(section == "21_foodrecall")

# To get specific types of food recall:
# (1) Filter by specific coicop_class
unique(food_recall$coicop_class)

# For question h1503b and h1503c, filter to coicop_class == "Fish and sea food"

# Or (2) Filter by specific responses in the desciprtion column
# list_of_descriptions <- c("Oceanic fish (Tuna, Wahoo, Mahi-mahi, etc.)",
#                           "Reef fish (fresh or frozen)",
#                           "Lagoon and sandflat fish (fresh or frozen)",
#                           "Deep sea fish (fresh or frozen)",
#                           "Fresh water fish",
#                           "Sharks (fresh or frozen)",
#                           "Sea worm",
#                           "Lobster",
#                           "Cockles",
#                           "Clams",
#                           "Sea snails",
#                           "Tinned tuna",
#                           "Tinned mackerel",
#                           "Other dried, canned or salted fish",
#                           "Land crab (mud crab, coconut crab, etc.)"
#                           )

# Use var_labels to get descriptions of columns in food_recall - cn use this to help decide which are relevant to your data query
var_labels %>%
  filter(col.names %in% names(food_recall))

h1503b_and_c <- food_recall %>%
  #filter(description %in% list_of_descriptions) %>% # if filtering by "Description"
  filter(coicop_class == "Fish and sea food")

write.csv(h1503b_and_c, file.path(outdir, "Data Requests", "2021-06-24_Data-request_IRS_h1503b_and_c.csv"), row.names = FALSE)

