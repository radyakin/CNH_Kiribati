# This begins to clean some of the non-uniquely identified question_ids - still not finished
# Clean and output tidy HIES data:
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

hies_files <- list.files(file.path(datadir, "20210301_HIES_FINAL"))

# Only want the STATA .dta files
# Some files have v01 and v02 - these are the same datasets but organized differently (v01 - household level and v02 - transaction level?)
# Keep only v01 for now - household level seems to be how data is organized throughout
all_dta_files <- hies_files[grep(pattern = "\\v01.dta", hies_files)] 

# Deal with these files separately
dta_files <- all_dta_files[!grepl(pattern = "AgricVegetables|AgricRootCrop|AgricFruit|IncomeAggreg|ExpenditureAggreg", all_dta_files)]
dta_files_not_pivoting_tidy <- all_dta_files[grepl(pattern = "AgricVegetables|AgricRootCrop|AgricFruit|IncomeAggreg|ExpenditureAggreg", all_dta_files)]

# for all dta files
# dta_files <- hies_files[grep(pattern = "\\.dta", hies_files)] 

# lapply read_dta 
hies_all <- lapply(dta_files, function(i){read_dta(file.path(datadir, "20210301_HIES_FINAL", i))})

# lapply function get_var_labels to get corresponding col.names and col.labels
hies_labels_list <- lapply(hies_all, get_var_labels)

hies_labels <- bind_rows(hies_labels_list, .id = "dta_file") # add .id so that if there are common col.names across dta files these can be matched to the specific col.label from that dta file

# Find common data_cols to bind all data frames
# Intersection of common data cols for first three data files
id_cols <- intersect(intersect(hies_labels_list[[1]]$col.names, hies_labels_list[[2]]$col.names), hies_labels_list[[3]]$col.names)

# add hm_basic__id - any questions with NA for this is at the household level
id_cols <- c(id_cols, "hm_basic__id")

# Reminder - Don't add additional columns like below, this creates duplicate rows of NAs for interview__id in downstream tidy format
# add other columns as ID columns: hm_basic__id, interview__id, hhld_id, team_id, interviewer_id
#id_cols <- c(id_cols, "hm_basic__id", "interview__id", "hhld_id", "team_id", "interviewer_id")

# Original IRS data request
# hies_40 <- read_dta(file.path(datadir, "20210301_HIES_FINAL", dta_files[unlist(lapply(data_cols, str_detect, "p907"))]))
# Current output format of read_dta (hies_40) WILL write out to a CSV but only the value (not the label) e.g., 1, 2, 3, 4, etc but not the corresponding text label S Tarawa, Northern, Central, Southern, etc.

# Pivot long based on id_cols
# hies_40_long <- hies_40 %>%
#   mutate(across(where(is.numeric), as.character)) %>% # Mutate all columns to be character so all questions (numeric or character) can be combined into a single long pivot column
#   pivot_longer(cols = !all_of(c(id_cols)), names_to = "question_id") %>% 
#   filter(is.na(value)==FALSE) # Remove questions with NA responses

# FIX IT - return pivot_dat_i function to filtering out "" and NA's
# Pivot long with pivot_dat_i
# lapply pivot function to all hies_all
hies_long_list <- lapply(hies_all, function(i){pivot_dat_i(hies_i = i, id_cols = id_cols)})

# Column dta_file corresponds to filename in dta_files; keep this column until we're sure all col.names and col.labels are standardized (see code in manual cleaning section below)
hies_long <- bind_rows(hies_long_list, .id = "dta_file") 


# Exploring the data:
# hies_long was created by joining columns that were unique to ALL data files, but there are still columns that are unique to just subsets of data files
# i.e., for a given interview, there are still duplicate question_id's
# Compare non-unique and unique question_id's
# hies_long %>%
#   filter(interview__key == "00-01-31-37") %>%
#   pull(question_id) %>%
#   length()
# 
# hies_long %>%
#   filter(interview__key == "00-01-31-37") %>%
#   pull(question_id) %>%
#   unique() %>%
#   length()


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
                                TRUE ~ col.labels)) #%>%
# FIX IT - delete if dealing with IncomeAggreg and ExpenditureAggreg separately
  # mutate(col.names = case_when(col.names == "annual_amount_clean" & col.labels == "cleaned annualised expenditure" ~ "annual_amount_clean_expenditure",
  #                              col.names == "annual_amount_clean" & col.labels == "cleaned annualised income" ~ "annual_amount_clean_income",
  #                              TRUE ~ col.names))

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
  distinct() # Shrinks from 8,900,000 to 4,500,000 rows

# Now deal with dta_files_not_pivoting_tidy separately

agric_veg <- read_dta(file.path(datadir, "20210301_HIES_FINAL", "SPC_KIR_2019_HIES_19b-AgricVegetables_v01.dta"))

# LEFT OFF HERE: go over this with JG
# If h1901r__id is changed to h1901r__id_0, h1901r__id_1, and h1901r__id_2 and values changed to NA or 1 then this should allow it to pivot wide 
unique(agric_veg$h1901r__id)

agric_veg_fix <- agric_veg %>% 
  mutate()
  

agric_root <- read_dta(file.path(datadir, "20210301_HIES_FINAL", "SPC_KIR_2019_HIES_19c-AgricRootCrop_v01.dta"))
agric_fruit <- read_dta(file.path(datadir, "20210301_HIES_FINAL", "SPC_KIR_2019_HIES_19d-AgricFruit_v01.dta"))
expend_agg <- read_dta(file.path(datadir, "20210301_HIES_FINAL", "SPC_KIR_2019_HIES_30-ExpenditureAggreg_v01.dta"))
income_agg <- read_dta(file.path(datadir, "20210301_HIES_FINAL", "SPC_KIR_2019_HIES_40-IncomeAggreg_v01.dta"))


lapply(dta_files, function(i){read_dta(file.path(datadir, "20210301_HIES_FINAL", i))})

# lapply function get_var_labels to get corresponding col.names and col.labels
hies_labels_list <- lapply(hies_all, get_var_labels)



# FIX IT - before pivotting tidy, deal with AgricVegetables, AgricRootCrop, AgricFruit, IncomeAggreg, and ExpenditureAggreg
# FINAL TIDY FORMAT (each row is a single observation at the HOUSEHOLD LEVEL):
hies_house_tidy <- hies_long_distinct %>%
  filter(is.na(hm_basic__id)) %>%
  pivot_wider(names_from = question_id, values_from = value) %>%
  arrange(interview__key)

# FINAL TIDY FORMAT (each row is a single observation at the INDIVIDUAL LEVEL):
hies_individ_tidy <- hies_long_distinct %>%
  filter(is.na(hm_basic__id)==FALSE) %>%
  pivot_wider(names_from = question_id, values_from = value) %>%
  arrange(interview__key)

# SHORT ANSWERS FOR TRANSLATION:
# FIRST, create FULL long format
# Join with hies_labels_distinct for question context (helpful for translation)
hies_alpha <- hies_long_distinct %>%
  filter(str_detect(value, pattern = "[:alpha:]")) %>% # response has text (i.e., alpha) 
  # "FIND" the word "text" in the HIES survey to get all the question_id's for the fill-in-the-blanks
  # ...and question_id ends with either an "n", "n1", "n2", "n3", "n4", "e" (e.g., p903n, h1801n2, h1122e) OR contains an "oth"
  filter((str_detect(question_id, pattern = "n$|n1$|n2$|n3$|n4$|e$") | str_detect(question_id, pattern = "oth") | question_id %in% c("nonfinfish_consump", "travel_time_within"))) %>% 
  # REMOVE FALSE MATCHES:
  filter(question_id %in% c("desc_2001e", "description", "ind_desc_main", "occ_desc_main")==FALSE) %>%
  left_join(hies_labels_distinct, by = c("question_id" = "col.names")) %>%
  select(question_id, value, col.labels) %>%
  unique() %>%
  arrange(question_id)

# FIX IT - move to top once finalized
# Outputs:
# Final long format of all uniquely identified questions: hies_unique_qs - read in all dataframes in list dta_file: pivot long, combine by columns unique to ALL data files, remove duplicate questions (common to subsets of data files), remove qs_with_non_unique_ids
# Final long format of all NON-uniquely identified questions: hies_non_unique_qs
# Final tidy formats at the household (hies_house_tidy) and individual (hies_individ_tidy) levels
# Key for matching col.names (question_id in hies) to col.labels: hies_labels_distinct
# "Alpha" responses for translation: hies_fill_in_the_blank

write.csv(hies_unique_qs, file = file.path(outdir, "hies_long_qs-with-unique-ids.csv"), row.names = FALSE)
write.csv(hies_non_unique_qs, file = file.path(outdir, "hies_long_qs-with-non-unique-ids.csv"), row.names = FALSE)
write.csv(hies_house_tidy, file = file.path(outdir, "hies_tidy_household-level.csv"), row.names = FALSE)
write.csv(hies_individ_tidy, file = file.path(outdir, "hies_tidy_individual-level.csv"), row.names = FALSE)
write.csv(hies_labels_distinct, file = file.path(outdir, "hies_question-id-to-label-key.csv"), row.names = FALSE)
write.csv(hies_alpha, file = file.path(outdir, "hies_text-responses-for-translation.csv"), row.names = FALSE)


##############################################################################################################################
# OLD CODE:

# Use values_fn to identify all the responses with non-unique values
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

# Explore qs_with_non_unique_ids
# hies_unique_counts %>%
#   select(any_of(c(id_cols, "h1901r__id"))) %>%
#   filter(h1901r__id > 1) %>%
#   arrange(desc(h1901r__id))
# 
# hies_long_distinct %>%
#   filter(interview__key == "83-74-86-59" & question_id == "h1901r__id")
# 
# hies_all[[19]] %>% filter(interview__key == "83-74-86-59")
# 
# # Try binding to col.labels for more context
# hies_long_distinct %>%
#   left_join(hies_labels_distinct, by = c("question_id" = "col.names")) %>%
#   filter(question_id == "recall") %>%
#   arrange(interview__key)

# Option 1:
# Remove qs_with_non_unique_ids from hies_long_distinct so that pivot_wide can work
# Output qs_with_non_unique_ids as a separate data file

# Option 2: # Append qs_with_non_unique_ids with dta file metadata so they become unique
# Go back to hies_long_clean (before dta file number was removed): These are the corresponding dta_file numbers for all the qs_with_non_unique_ids
look_up_metadata <- hies_long_clean %>% 
  filter(question_id %in% qs_with_non_unique_ids) %>% 
  select(dta_file, question_id) %>% 
  distinct() %>% 
  arrange(question_id) %>% 
  print(n=96)

# Get metadata from each dta file name
dta_files
dta_meta_1 <- str_split(dta_files, pattern = '-', simplify = TRUE)[,2]
dta_meta_2 <- str_split(dta_meta_1, pattern = '_', simplify = TRUE)[,1]
dta_meta_df <- data.frame(filename_metadata = dta_meta_2, dta_file = 1:length(dta_meta_2)) %>%
  mutate(dta_file = as.character(dta_file)) # required to join with look_up_metadata

# Join metadata to qs_with_non_unique_ids
# This df shows that some question_ids are shared for different surveys (dta_files) and should have a more descriptive name (new_question_id) so they can be uniquely identified
look_up_metadata_df <- look_up_metadata %>%
  left_join(dta_meta_df, by = "dta_file") %>%
  mutate(new_question_id = paste(question_id, "_", filename_metadata, sep = ""))

# Go back to hies_long_clean and add metadata before dropping duplicate data
hies_long_with_metadata <- hies_long_clean %>%
  left_join(look_up_metadata_df, by = c("dta_file", "question_id")) %>%
  # substitute question_id with new_question_id # Reminder: only doing this for the qs_with_non_unique_ids so that duplicate data can still be dropped
  mutate(question_id = if_else(is.na(new_question_id)==FALSE, true = new_question_id, false = question_id)) %>%
  select(-c(dta_file, filename_metadata, new_question_id)) %>%
  distinct()

# NOW: individual level surveys can pivot to Tidy format but not household level

# Repeat process of finding which responses are non-unique
# Use values_fn to identify all the responses with non-unique values
hies_long_with_metadata %>%
  filter(is.na(hm_basic__id)) %>%
  pivot_wider(names_from = question_id, values_from = value)

hies_with_metadata_unique_counts <- hies_long_with_metadata %>%
  filter(is.na(hm_basic__id)) %>%
  pivot_wider(names_from = question_id, values_from = value, values_fn = length)

# Wait until BOTH individ and household level can pivot to tidy to do this  
# hies_individ_tidy <- hies_long_with_metadata %>%
#   filter(is.na(hm_basic__id)==FALSE) %>%
#   pivot_wider(names_from = question_id, values_from = value) %>% 
#   arrange(interview__key)

# How many questions have non-unique values
hies_with_metadata_test <- hies_with_metadata_unique_counts != 1
# hies_test[1:10, 1:10] matrix of TRUE/FALSE
hies_with_metadata_test_df <- data.frame(non_unique_count = colSums(hies_with_metadata_test, na.rm = TRUE)) %>%
  rownames_to_column("question_id") 

# This is the number of question_id's with non-unique values
hies_with_metadata_test_df %>%
  filter(question_id %in% id_cols == FALSE) %>% # Remove ID cols - these don't have to be unique since they aren't included in pivot wide
  filter(non_unique_count > 0) %>% # Which columns contain one or more non-unique value
  nrow() 

# These are the qs that still cannot be uniquely identified for tidy format (one observation per row)
qs_with_non_unique_ids_2 <- hies_with_metadata_test_df %>%
  filter(question_id %in% id_cols == FALSE) %>% # Remove ID cols - these don't have to be unique since they aren't included in pivot wide
  filter(non_unique_count > 0) %>% 
  pull(question_id)

# Why are qs_with_non_unique_ids_2 still Not unique:
# Explore qs_with_non_unique_ids_2
hies_with_metadata_unique_counts %>%
  select(any_of(c(id_cols, "h1901r__id_AgricVegetables"))) %>%
  filter(h1901r__id_AgricVegetables>1) %>%
  arrange(interview__key) 

# Why do these interviews have multiple rows for h1901r__id_AgricVegetables?
# Go back to hies_long_with_metadata
hies_long_with_metadata %>%
  filter(interview__key == "36-96-00-31" & question_id == "h1901r__id_AgricVegetables")

# FIX IT: question_id with the word "ID" in them should be replaced with the VALUE just like with other rosters:
# Rembmer p904 turns into p904_1 to p904_2 depending on "ID" generated by p903

# What about "h1912__2_AgricRootCrop"
hies_with_metadata_unique_counts %>%
  select(any_of(c(id_cols, "h1912__2_AgricRootCrop"))) %>%
  filter(h1912__2_AgricRootCrop>1) %>%
  arrange(interview__key) 

# Go back to hies_long_with_metadata
# Respondent replied with "2" on question 1911
# This generated question_id h1912__2, but there should really only be one of these
hies_long_with_metadata %>%
  filter(interview__key == "33-97-85-65" & question_id == "h1912__2_AgricRootCrop")
# FIX IT: This looks like an error: Two different answers to the same question - take the mean?

# How many of the different qs_with_non_unique_ids_2 are just isolated errors like this
hies_errors <- hies_with_metadata_unique_counts %>%
  select(all_of(c(id_cols, qs_with_non_unique_ids_2))) %>%
  rowwise() %>%
  mutate()

View(hies_errors)
# Sort by different columns

# Sort by line_id_ExpenditureAggreg
# One interview has 263 of these responses
# Interview: 29-46-67-68
hies_long_with_metadata %>%
  filter(interview__key == "29-46-67-68" & question_id == "line_id_ExpenditureAggreg")


# hies_long_with_metadata %>%
#   filter(question_id == "h1901r__id") %>%
#   filter(value > 0) %>%
#   arrange(interview__key)
# 
# hies_long_with_metadata %>%
#   filter(question_id == "recall") %>%
#   arrange(interview__key)
# 
# # Try binding to col.labels for more context
# hies_long_with_metadata %>%
#   left_join(hies_labels_distinct, by = c("question_id" = "col.names")) %>%
#   filter(question_id == "recall") %>%
#   arrange(interview__key)


# TO DO: Add filename_metadata to hies_labels_distinct so they are part of the question_id to label key
# Use this new version for downstream joins

# TO DO: Output final long format 
# TO DO: Output final tidy formats for household and individual level


# FIX IT - move this to "Option1" above (pivot after filtering)
# FINAL LONG FORMAT (includes household and individual-level data):
hies_unique_qs <- hies_long_distinct %>%
  filter(question_id %in% qs_with_non_unique_ids==FALSE) %>%
  arrange(interview__key) 

# FINAL LONG FORMAT for non-unique-qs (note: all are at the household level)
hies_non_unique_qs <- hies_long_distinct %>%
  filter(question_id %in% qs_with_non_unique_ids) %>%
  arrange(interview__key)

# FINAL TIDY FORMAT (each row is a single observation at the HOUSEHOLD LEVEL):
hies_house_tidy <- hies_unique_qs %>%
  filter(is.na(hm_basic__id)) %>%
  pivot_wider(names_from = question_id, values_from = value) %>%
  arrange(interview__key)

# FINAL TIDY FORMAT (each row is a single observation at the INDIVIDUAL LEVEL):
hies_individ_tidy <- hies_unique_qs %>%
  filter(is.na(hm_basic__id)==FALSE) %>%
  pivot_wider(names_from = question_id, values_from = value) %>%
  arrange(interview__key)

# SHORT ANSWERS FOR TRANSLATION:
# Join with hies_labels_distinct for question context (helpful for translation)
hies_alpha <- hies_long_distinct %>%
  filter(str_detect(value, pattern = "[:alpha:]")) %>% # response has text (i.e., alpha) 
  # "FIND" the word "text" in the HIES survey to get all the question_id's for the fill-in-the-blanks
  # ...and question_id ends with either an "n", "n1", "n2", "n3", "n4", "e" (e.g., p903n, h1801n2, h1122e) OR contains an "oth"
  filter((str_detect(question_id, pattern = "n$|n1$|n2$|n3$|n4$|e$") | str_detect(question_id, pattern = "oth") | question_id %in% c("nonfinfish_consump", "travel_time_within"))) %>% 
  # REMOVE FALSE MATCHES:
  filter(question_id %in% c("desc_2001e", "description", "ind_desc_main", "occ_desc_main")==FALSE) %>%
  left_join(hies_labels_distinct, by = c("question_id" = "col.names")) %>%
  select(question_id, value, col.labels) %>%
  unique() %>%
  arrange(question_id)
  
# FIX IT - move to top once finalized
# Outputs:
# Final long format of all uniquely identified questions: hies_unique_qs - read in all dataframes in list dta_file: pivot long, combine by columns unique to ALL data files, remove duplicate questions (common to subsets of data files), remove qs_with_non_unique_ids
# Final long format of all NON-uniquely identified questions: hies_non_unique_qs
# Final tidy formats at the household (hies_house_tidy) and individual (hies_individ_tidy) levels
# Key for matching col.names (question_id in hies) to col.labels: hies_labels_distinct
# "Alpha" responses for translation: hies_fill_in_the_blank

write.csv(hies_unique_qs, file = file.path(outdir, "hies_long_qs-with-unique-ids.csv"), row.names = FALSE)
write.csv(hies_non_unique_qs, file = file.path(outdir, "hies_long_qs-with-non-unique-ids.csv"), row.names = FALSE)
write.csv(hies_house_tidy, file = file.path(outdir, "hies_tidy_household-level.csv"), row.names = FALSE)
write.csv(hies_individ_tidy, file = file.path(outdir, "hies_tidy_individual-level.csv"), row.names = FALSE)
write.csv(hies_labels_distinct, file = file.path(outdir, "hies_question-id-to-label-key.csv"), row.names = FALSE)
write.csv(hies_alpha, file = file.path(outdir, "hies_text-responses-for-translation.csv"), row.names = FALSE)

