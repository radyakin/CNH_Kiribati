# Examples for querying hies data
# Change file.path directories as needed
rm(list=ls())
library(tidyverse)
datadir <- "/Volumes/jgephart/Kiribati/Data"
outdir <- "/Volumes/jgephart/Kiribati/Outputs"

# Start by loading df_tidy and/or var_labels
hies_tidy <- read.csv(file = file.path(outdir, "2021-07-29_hies_tidy_individual-level.csv"))
hies_tidy_house <- read.csv(file = file.path(outdir, "2021-07-29_hies_tidy_household-level.csv"))
var_labels <- read.csv(file = file.path(outdir, "2021-07-29_hies_question-id-to-label-key.csv"))
hies_long <- read.csv(file = file.path(outdir, "2021-07-29_hies_long_qs-with-unique-ids.csv")) # df long includes both household (hm_basic__id == NA) and individual-level responses
hies_expenditure <- read.csv(file.path(outdir, "2021-07-29_hies_expenditures-standard-units.csv"))

##############################################################################################################
# Katy's examples:
# P901-P925
# P801-P803
# P810
# MIG_P9 == migrant_prev_work
# Food recall from p. 81
# Dietary recall from p. 124
# HIES 1503 (all subquestions)

# Create a search pattern for all the question IDs that you're interested in
p9s <- paste("p", 901:925, sep = "")
p8s <- c(paste("p", 801:803, sep = ""), "p810")
search_pattern <- paste(c(p9s, p8s, "migrant_prev_work"), collapse = "|")

var_labels %>% 
  filter(str_detect(col.names, search_pattern))

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
# Use var_labels to get descriptions of columns in food_recall - cn use this to help decide which are relevant to your data query
var_labels %>%
  filter(col.names %in% names(food_recall))

h1508 <- food_recall %>%
  #filter(description %in% list_of_descriptions) %>% # if filtering by "Description"
  filter(coicop_class == "Sugar, jam, honey, chocolate and confectionery")

# The "description" column gives you the individual food recall items, including fill in the blank responses
unique(h1508$description)

# The food_desc_pndb is metadata joined to expenditures data that assigns category to each item (can serve as rough translation of some of the fill in the blank response)
h1508 %>%
  select(description, food_desc_pndb, coicop_class) %>%
  arrange(description) %>%
  distinct()

##############################################################################################################
# Jacob's examples:
# HIES H13b1 (specifically responses 05, 06 and 07)
# VRS VRS4_1 with associated VRS4_1a and VRS4_1b

############################################################
# Data query code from Indy's first data request
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

