# Author: Kelvin Gorospe kdgorospe@gmail.com
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
nsfdatadir <- "/Volumes/jgephart/Kiribati/Data/HIES-NSF-files"

source("Data_cleaning/cleaning_functions.R")

############################################################## 
# Step 1: Read in and combine all HIES dta files
hies_files <- list.files(file.path(datadir, "20210301_HIES_FINAL"))

# Only want the STATA .dta files
# For some files .dta files there is a v01 and v02 - these are the same datasets but organized differently (v01 - household level and v02 - transaction level?)
# Keep only v01 - household level
all_dta_files <- hies_files[grep(pattern = "\\v01.dta", hies_files)] 

# Deal with these files separately (transaction-level data, therefore does not pivot tidy with household and individual-level data)
dta_files <- all_dta_files[!grepl(pattern = "AgricVegetables|AgricRootCrop|AgricFruit|IncomeAggreg|ExpenditureAggreg", all_dta_files)]
dta_files_not_pivoting_tidy <- all_dta_files[grepl(pattern = "AgricVegetables|AgricRootCrop|AgricFruit|IncomeAggreg|ExpenditureAggreg", all_dta_files)]

# lapply read_dta 
hies_all <- lapply(dta_files, function(i){read_dta(file.path(datadir, "20210301_HIES_FINAL", i))})

# Clean data (extract attributes)
hies_all_clean <- lapply(hies_all, clean_data, return = "df")

# Get labels for all column names
hies_labels_list <- lapply(hies_all, clean_data, return = "var_labels")

hies_labels <- bind_rows(hies_labels_list, .id = "dta_file") # add .id so that if there are common col.names across dta files these can be matched to the specific col.label from that dta file

# Join hies_labels with name of original DTA file
dta_df <- data.frame(dta_filename = dta_files, dta_file = as.character(1:length(dta_files)))
hies_labels <- hies_labels %>%
  left_join(dta_df, by = "dta_file")

# For joining NSF data, use only interview__key and hm_basic__id
# any questions with NA for hm_basic__id is a household level question
# Reminder - adding additional ID columns (e.g.,"interview__id", "hhld_id", "team_id", "interviewer_id") creates duplicate rows of NAs in downstream tidy format
id_cols <- c("interview__key", "hm_basic__id")

# Pivot long with pivot_dat_i
# lapply pivot function to all hies_all
hies_long_list <- lapply(hies_all_clean, function(i){pivot_dat_i(hies_i = i, id_cols = id_cols)})

# Column dta_file corresponds to filename in dta_files; keep this column until we're sure all col.names and col.labels are standardized (see code in manual cleaning section below)
hies_long <- bind_rows(hies_long_list, .id = "dta_file") 

##############################################################
# Step 2: Read in NSF-specific questions and add to HIES dataset:
# Incorporate NSF data sent in a separate email from Mike (stored in nsfdatadir) - add to hies_labels, hies_long, AND hies_tidy

####
# Anthropometry data: 
# Note: removing sensitive PII data: birth_d, birth_m, birth_y, and name!
# All other columns (p105 through p114) are already in hies_long, so only need to get age_m column
anthropometry_filename <- "KIR_2019_HIES_s1Demo_characteristics.dta"
anthropometry <- read_dta(file.path(nsfdatadir, anthropometry_filename)) %>% 
  clean_data(return = "df") %>%
  # Assemble birthday columns into a single column
  mutate(birthday_paste = paste(birth_y, birth_m, birth_d, sep = "-")) %>% 
  # Clean birthdates - some dates returned as NA because either month or day entered as "99" 
  mutate(birthday_paste_cleaned = str_replace(birthday_paste, "-99-", "-6-"), # replaces all "99" months with "6" (estimate = June)
         birthday_paste_cleaned = str_replace(birthday_paste_cleaned, "99$", "15"))  %>% # "$" specifies end of the string - i.e., replaces all days with "15" (estimate = 15th of the month)
  mutate(birthday_yyyy_mm_dd = as.Date(birthday_paste_cleaned, format = "%Y-%m-%d")) %>% 
  # Extract start and end dates - everything before "T" - i.e., before timestamp
  mutate(end_date = str_extract(end_pers_details, ".+?(?=T)")) %>%
  mutate(start_date = str_extract(start_pers_details, ".+?(?=T)")) %>%
  mutate(end_date = as.Date(end_date, "%Y-%m-%d")) %>%
  mutate(start_date = as.Date(start_date, "%Y-%m-%d")) %>%
  # NOTE: In some cases there are as many as four years between the start and end dates. In addition, there appears to be errors in start and end dates (sometimes end date comes before start date)
  # Related to the issues with start/end date, will calculate age as both start date minus birthday and end date minus birthday
  # As per Simone, calculate both age in months and age in days as required for certain anthropometry analyses
  # Difference in dates is reported in "days"; multiply by 12/365 to get number of months
  mutate(age_d_start = as.numeric(start_date - birthday_yyyy_mm_dd),
         age_d_end = as.numeric(end_date - birthday_yyyy_mm_dd),
         age_mo_start = as.numeric((start_date - birthday_yyyy_mm_dd)*12/365),
         age_mo_end = as.numeric((end_date - birthday_yyyy_mm_dd)*12/365)) %>%
  # Drop all PII columns
  select(any_of(c("interview__key", "hm_basic__id", "age_m", "age_mo_start", "age_mo_end", "start_pers_details", "end_pers_details"))) %>%
  # note: anthropometry only contains interview__key and hm_basic__id - Bind to hies_long to get other id_cols 
  mutate(hm_basic__id = as.character(hm_basic__id)) %>%
  left_join(hies_long %>% select(all_of(id_cols)) %>% distinct(), by = c("interview__key", "hm_basic__id"))

# Data check: make sure all of the interview__key ID's can match (be joined with) to the standard HIES dataset
# unique(anthropometry$interview__key)[unique(anthropometry$interview__key) %in% unique(hies_long$interview__key)==FALSE] # should be empty

anthropometry_long <- pivot_dat_i(hies_i = anthropometry, id_cols = id_cols)

anthro_labels <- clean_data(anthropometry, return = "var_labels") %>%
  # Mutate str_to_lower to match all other var_labels
  mutate(col.labels = str_to_lower(col.labels)) %>%
  # Define labels for calculated columns
  mutate(col.labels = case_when(col.names == "age_d_start" ~ "calculated age in days from interview start date",
                                col.names == "age_d_end" ~ "calculated age in days from interview end date",
                                col.names=="age_mo_start" ~ "calculated age in months from interview start date",
                                col.names=="age_mo_end" ~ "calculated age in months from interview end date",
                                TRUE ~ col.labels)) %>%
  mutate(dta_filename = anthropometry_filename)

####
# Anaemia data: 
anaemia_filename <- "anaemia.dta"
anaemia <- read_dta(file.path(nsfdatadir, anaemia_filename)) %>% 
  clean_data(return = "df")

# Data check: make sure all of the interview__key ID's can match (be joined with) to the standard HIES dataset
anaemia_no_match <- unique(anaemia$interview__key)[unique(anaemia$interview__key) %in% unique(hies_long$interview__key)==FALSE]

# For now, remove these from the dataset: since they don't match to the HIES dataset
anaemia <- anaemia %>%
  filter(interview__key %in% anaemia_no_match==FALSE)

anaemia_long <- pivot_dat_i(hies_i = anaemia, id_cols = id_cols)
anaemia_labels <- clean_data(anaemia, return = "var_labels") %>%
  mutate(dta_filename = anaemia_filename)

####
# Diet recall data: 
diet_filename <- "dietary_recall.dta"
diet <- read_dta(file.path(nsfdatadir, diet_filename)) %>% 
  clean_data(return = "df")

# Data check: make sure all of the interview__key ID's can match (be joined with) to the standard HIES dataset
diet_no_match <- unique(diet$interview__key)[unique(diet$interview__key) %in% unique(hies_long$interview__key)==FALSE]
# For now, remove these from the dataset
diet <- diet %>%
  filter(interview__key %in% anaemia_no_match==FALSE)

diet_long <- pivot_dat_i(hies_i = diet, id_cols = id_cols)
diet_labels <- clean_data(diet, return = "var_labels") %>%
  mutate(dta_filename = diet_filename)

#### Other files in the NSF folder:
# Healthy living data: Note: already part of SPC_KIR_2019_HIES_o26-HealthyLiving_v01.dta so no need to incorporate this into hies_tidy
# health_filename <- "healthy_living.dta"
# health <- read_dta(file.path(nsfdatadir, health_filename)) %>% 
#   clean_data(return = "df")
# health_long <- pivot_dat_i(hies_i = health, id_cols = id_cols)
# health_labels <- clean_data(health, return = "var_labels") %>%
#   mutate(dta_filename = health_filename)

####
# Time use data: Note: already part of SPC_KIR_2019_HIES_o26-HealthyLiving_v01.dta so no need to incorporate this into hies_tidy
# time_filename <- "time_use.dta"
# time <- read_dta(file.path(nsfdatadir, time_filename)) %>% 
#   clean_data(return = "df")
# time_long <- pivot_dat_i(hies_i = time, id_cols = id_cols)
# time_labels <- clean_data(time, return = "var_labels")  %>%
#   mutate(dta_filename = time_filename)

# Add NSF Team data to hies_long
hies_long_all <- hies_long %>% 
  bind_rows(anthropometry_long) %>%
  bind_rows(anaemia_long) %>%
  bind_rows(diet_long)

# apply distinct() to hies_long and clean village names
hies_long_distinct <- hies_long_all %>%
  select(-dta_file) %>%
  distinct() %>%
  mutate(value = if_else(question_id == "village", true = gsub("^.*- ", "", value), false = value)) # Remove the ID number and hyphen before each village name

##############################################################
# Step 3: Pivot hies_long_distinct dataset to tidy format

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

##############################################################
# Step 4: Clean data files that do not pivot to tidy:
# These are the files in dta_files_not_pivoting_tidy
# Note: these are not incorporated into hies_long

veg_filename <- "SPC_KIR_2019_HIES_19b-AgricVegetables_v01.dta"
root_filename <- "SPC_KIR_2019_HIES_19c-AgricRootCrop_v01.dta"
fruit_filename <- "SPC_KIR_2019_HIES_19d-AgricFruit_v01.dta"
expend_filename <- "SPC_KIR_2019_HIES_30-ExpenditureAggreg_v01.dta"
income_filename <- "SPC_KIR_2019_HIES_40-IncomeAggreg_v01.dta"
agric_veg <- read_dta(file.path(datadir, "20210301_HIES_FINAL", veg_filename)) %>% clean_data(return = "df")
agric_root <- read_dta(file.path(datadir, "20210301_HIES_FINAL", root_filename)) %>% clean_data(return = "df")
agric_fruit <- read_dta(file.path(datadir, "20210301_HIES_FINAL", fruit_filename)) %>% clean_data(return = "df")
expend_agg <- read_dta(file.path(datadir, "20210301_HIES_FINAL", expend_filename)) %>% clean_data(return = "df")
income_agg <- read_dta(file.path(datadir, "20210301_HIES_FINAL", income_filename)) %>% clean_data(return = "df")
# NOTE: None of the data in the dta_files_not_pivoting_tidy files require translations:
# veg, root, and fruit do not have fill-in-the-blank answers
# and expend and income data are joined with PNDB code below which serves as a translation

# SPC data team have already worked to standardize units in the Expenditure and Income files - remove all non-standard unit columns
# These columns should be removed as per Mike Sharpe
non_standard_columns <- c("qty",
                          "unit",
                          "raw_amount",
                          "annual_amount",
                          "annual_amount_clean",
                          "rent_act",
                          "rent_est",
                          "rent_imp",
                          "net_rent",
                          "usevalue_assets2",
                          "cpi",
                          "fpi",
                          "mean_fpi",
                          "fpi_def",
                          "amount_def",         
                          "qty_corrected",
                          "amount_corrected",
                          "outlier",
                          "amount_new",
                          "kcal_cost",
                          "kcal_cost_snack",
                          "kcal_cost_bev",
                          "raw_amount_est",
                          "other_unit",
                          "year_acq",
                          "nmonth_owned",
                          "paid_by",
                          "location",
                          "num_owned",
                          "amount_resell",      
                          "purpose",
                          "h1201_sale_"
                          )
expend_agg <- expend_agg %>%
  select(!all_of(non_standard_columns))
income_agg <- income_agg %>%
  select(!all_of(non_standard_columns))

# Pivot all dta_files_not_pivoting_tidy to long and combine with hies_long_distinct
veg_long <- pivot_dat_i(hies_i = agric_veg, id_cols = id_cols)
root_long <- pivot_dat_i(hies_i = agric_root, id_cols = id_cols)
fruit_long <- pivot_dat_i(hies_i = agric_fruit, id_cols = id_cols)
expend_long <- pivot_dat_i(hies_i = expend_agg, id_cols = id_cols)
income_long <- pivot_dat_i(hies_i = income_agg, id_cols = id_cols)

#### ADDENDUM to income and expenditure data
# Incorporate PNDB metadata
# Left join pndb_meta to expend_agg and income_agg (pndb_meta contains nutritional info and descriptions for ALL pndb codes)
# Columns: Food_description and food_desc_pndb can be used as coded "translations" of the fill-in-the-blank responses
# Some of the entries in pndb_meta are blank PNDB codes... filter these out before left-joining
pndb_filename <- "PNDB_17May2020.dta"
pndb_meta <- read_dta(file.path(nsfdatadir, pndb_filename)) %>% clean_data(return = "df") %>% rename(pndbcode = PNDBCode)
pndb_long <- pivot_dat_i(hies_i = pndb_meta, id_cols = id_cols)

income_agg <- income_agg %>%
  left_join(pndb_meta %>% filter(pndbcode %in% c("", "..")==FALSE) %>% distinct(), by = "pndbcode") # need distinct() for pndb_meta - some rows are duplicates

expend_agg <- expend_agg %>%
  left_join(pndb_meta %>% filter(pndbcode %in% c("", "..")==FALSE) %>% distinct(), by = "pndbcode") # need distinct() for pndb_meta - some rows are duplicates

# get variable labels for each of the files in dta_file_not_pivoting_tidy
veg_labels <- clean_data(agric_veg, return = "var_labels") %>%
  mutate(dta_filename = veg_filename)
root_labels <- clean_data(agric_root, return = "var_labels") %>%
  mutate(dta_filename = root_filename)
fruit_labels <- clean_data(agric_fruit, return = "var_labels") %>%
  mutate(dta_filename = fruit_filename)
expend_labels <- clean_data(expend_agg, return = "var_labels") %>%
  mutate(dta_filename = expend_filename)
income_labels <- clean_data(income_agg, return = "var_labels") %>%
  mutate(dta_filename = income_filename)

# Get var_labels for pndb columns
pndb_labels <- clean_data(pndb_meta, return = "var_labels") %>%
  mutate(dta_filename = pndb_filename)

############################################################## 
# Step 5: Bind all long-format datasets together for final output

# FINAL LONG FORMAT
hies_long_full <- hies_long_distinct %>%
  bind_rows(veg_long) %>%
  bind_rows(root_long) %>%
  bind_rows(fruit_long) %>%
  bind_rows(expend_long) %>%
  bind_rows(income_long) %>%
  bind_rows(pndb_long) %>%
  distinct()

############################################################## 
# Step 6: Bind all column names and labels together for final output

hies_labels_all <- hies_labels %>%
  bind_rows(anthro_labels) %>%
  bind_rows(anaemia_labels) %>%
  bind_rows(diet_labels) %>%
  bind_rows(veg_labels) %>%
  bind_rows(root_labels) %>%
  bind_rows(fruit_labels) %>%
  bind_rows(expend_labels) %>%
  bind_rows(income_labels) %>%
  bind_rows(pndb_labels) %>%
  # Standardize col.labels as lowercase
  mutate(col.labels = str_to_lower(col.labels))

# Which col.names have duplicate col.labels
hies_no_dta_file <- hies_labels_all %>% 
  select(-c(dta_file, dta_filename)) %>%
  unique() %>%
  arrange(col.names)

non_standard_col_names <- as.data.frame(table(hies_no_dta_file$col.names)) %>%
  filter(Freq>1) %>%
  pull(Var1)

# These are the col.names with inconsistent col.labels
# hies_no_dta_file %>%
#   filter(col.names %in% non_standard_col_names) %>%
#   unique()

# MANUALLY CLEAN non_standard_col_names
hies_labels_clean <- hies_labels_all %>%
  mutate(col.labels = case_when(col.names == "fweight" ~ "final sample hh weight",
                                col.names == "hm_basic__id" ~ "id in hm_basic",
                                col.names == "interview__key" ~ "interview key (identifier in xx-xx-xx-xx format)",
                                col.names == "pndbcode" ~ "pacific nutrient database code (string)",
                                col.names == "rururb" ~ "urban - rural",
                                TRUE ~ col.labels)) 

# Check that all col.names have a unique col.label
hies_clean_no_dta_file <- hies_labels_clean %>% 
  select(-c(dta_file, dta_filename)) %>%
  unique() %>%
  arrange(col.names)

non_standard_col_names <- as.data.frame(table(hies_clean_no_dta_file$col.names)) %>%
  filter(Freq>1) %>%
  pull(Var1)

non_standard_col_names # EMPTY

# FINAL VARIABLE DESCRIPTIONS OUTPUT
# Now that hies_labels_clean is standardized, we can remove dta_file and use distinct() to remove duplicate questions across data files
hies_labels_distinct <- hies_labels_clean %>%
  select(-dta_file) %>%
  distinct() %>%
  arrange(col.names)

##############################################################
# Step 7: Identify fill-in-the-blank answers that may require translation

# Only do this for hies_long_distinct - before hies_long was joined with data files that do not pivot tidy (only these have fill in the blank answers)
# Then join with hies_labels_distinct for question context (helpful for translation)
hies_alpha <- hies_long_distinct %>%
  filter(str_detect(value, pattern = "[:alpha:]")) %>% # response has text (i.e., alpha) 
  # "FIND" the word "text" in the PDF HIES survey to get all the question_id's for the fill-in-the-blanks
  # ...and question_id ends with either an "n", "n1", "n2", "n3", "n4", "e" (e.g., p903n, h1801n2, h1122e) OR contains an "oth"
  filter((str_detect(question_id, pattern = "n$|n1$|n2$|n3$|n4$|e$") | str_detect(question_id, pattern = "oth") | question_id %in% c("nonfinfish_consump", "travel_time_within"))) %>% 
  # REMOVE FALSE MATCHES:
  filter(question_id %in% c("copra_increase", "copra_motivation", "desc_2001e", "interview_type", "reason_replace", "sitting_at_home")==FALSE) %>%
  left_join(hies_labels_distinct, by = c("question_id" = "col.names")) %>%
  select(question_id, value, col.labels) %>%
  unique() %>%
  arrange(question_id)

############################################################## 
# Step 8: Outputs
# Final long format of all uniquely identified questions: hies_long_distinct - read in all dataframes in list dta_file: pivot long, combine by columns unique to ALL data files, remove duplicate questions (common to subsets of data files)
# Final tidy formats at the household (hies_house_tidy) and individual (hies_individ_tidy) levels
# Key for matching col.names (question_id in hies) to col.labels: hies_labels_final
# Fill-in-the-blank responses for translation: hies_alpha
# All "special roster" files that do not pivot to tidy (because questions do not have unique IDs)

file.date <- Sys.Date() # add file.date to all outputs to keep track of most recent versions

# Standard HIES dataset that is able to pivot tidy
write.csv(hies_long_full, file = file.path(outdir, paste(file.date, "_hies_long_qs-with-unique-ids.csv", sep = "")), row.names = FALSE)
write.csv(hies_house_tidy, file = file.path(outdir, paste(file.date, "_hies_tidy_household-level.csv", sep = "")), row.names = FALSE)
write.csv(hies_individ_tidy, file = file.path(outdir, paste(file.date, "_hies_tidy_individual-level.csv", sep = "")), row.names = FALSE)
write.csv(hies_labels_distinct, file = file.path(outdir, paste(file.date, "_hies_question-id-to-label-key.csv", sep = "")), row.names = FALSE)
write.csv(hies_alpha, file = file.path(outdir, paste(file.date, "_hies_text-responses-for-translation.csv", sep = "")), row.names = FALSE)

# HIES questions related to expenditures and income (pre-processed by SPC data team)
write.csv(expend_agg, file = file.path(outdir, paste(file.date, "_hies_expenditures-standard-units.csv", sep = "")), row.names = FALSE)
write.csv(income_agg, file = file.path(outdir, paste(file.date, "_hies_income-standard-units.csv", sep = "")), row.names = FALSE)

# Data rosters that do not have unique IDs:
write.csv(agric_veg, file = file.path(outdir, paste(file.date, "_special-roster-hies_vegetable-details.csv", sep = "")), row.names = FALSE)
write.csv(agric_root, file = file.path(outdir, paste(file.date, "_special-roster-hies_root-crop-details.csv", sep = "")), row.names = FALSE)
write.csv(agric_fruit, file = file.path(outdir, paste(file.date, "_special-roster-hies_fruit-details.csv", sep = "")), row.names = FALSE)

