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

# Clean data (extract attributes)
hies_all_clean <- lapply(hies_all, clean_data, return = "df")

# Get labels for all column names
hies_labels_list <- lapply(hies_all, clean_data, return = "var_labels")

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

# Pivot long with pivot_dat_i
# lapply pivot function to all hies_all
hies_long_list <- lapply(hies_all_clean, function(i){pivot_dat_i(hies_i = i, id_cols = id_cols)})

# Column dta_file corresponds to filename in dta_files; keep this column until we're sure all col.names and col.labels are standardized (see code in manual cleaning section below)
hies_long <- bind_rows(hies_long_list, .id = "dta_file") 



##### ADDENDUM to HIES dataset:
# Incorporate anthropometry data sent in a separate email from Mike - add to hies_labels, hies_long, AND hies_tidy
# Note: removing sensitive PII data: birth_d, birth_m, birth_y, and name!
# All other columns (p105 through p114) are already in hies_long, so only need to get age_m column
anthropometry <- read_dta(file.path(datadir, "20210624_HIES-metadata", "KIR_2019_HIES_s1Demo_characteristics.dta")) %>% 
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
  # Related to the issues with start/end date, will calculate age in months as both start date minus birthday and end date minus birthday
  # Difference in dates is reported in "days"; multiply by 12/365 to get number of months
  mutate(age_mo_start = as.numeric((start_date - birthday_yyyy_mm_dd)*12/365),
         age_mo_end = as.numeric((end_date - birthday_yyyy_mm_dd)*12/365)) %>%
  # Drop all PII columns
  select(any_of(c("interview__key", "hm_basic__id", "age_m", "age_mo_start", "age_mo_end", "start_pers_details", "end_pers_details"))) %>%
  # note: anthropometry only contains interview__key and hm_basic__id - Bind to hies_long to get other id_cols 
  mutate(hm_basic__id = as.character(hm_basic__id)) %>%
  left_join(hies_long %>% select(id_cols) %>% distinct(), by = c("interview__key", "hm_basic__id"))

anthropometry_long <- pivot_dat_i(hies_i = anthropometry, id_cols = id_cols)

anthro_labels <- clean_data(anthropometry, return = "var_labels") %>%
  # Mutate str_to_lower to match all other var_labels
  mutate(col.labels = str_to_lower(col.labels)) %>%
  # Define labels for calculated columns
  mutate(col.labels = case_when(col.names=="age_mo_start" ~ "calculated age in months from interview start date",
                                col.names=="age_mo_end" ~ "calculated age in months from interview end date",
                                TRUE ~ col.labels))

hies_long <- hies_long %>% 
  bind_rows(anthropometry_long)

hies_labels <- hies_labels %>%
  bind_rows(anthro_labels)

######

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
# No longer need to clean these columns if dealing with IncomeAggreg and ExpenditureAggreg separately
  # mutate(col.names = case_when(col.names == "annual_amount_clean" & col.labels == "cleaned annualised expenditure" ~ "annual_amount_clean_expenditure",
  #                              col.names == "annual_amount_clean" & col.labels == "cleaned annualised income" ~ "annual_amount_clean_income",
  #                              TRUE ~ col.names))

# Mutate question_id in hies_long to match the changes made in hies_labels_clean col.names
# hies_long_clean <- hies_long %>%
#   mutate(question_id = case_when(question_id == "annual_amount_clean" & dta_file == "25" ~ "annual_amount_clean_expenditure",
#                                  question_id == "annual_amount_clean" & dta_file == "26" ~ "annual_amount_clean_income",
#                                  TRUE ~ question_id))

# Check that all col.names have a unique col.label
hies_clean_no_dta_file <- hies_labels_clean %>% 
  select(-dta_file) %>%
  unique() %>%
  arrange(col.names)

non_standard_col_names <- as.data.frame(table(hies_clean_no_dta_file$col.names)) %>%
  filter(Freq>1) %>%
  pull(Var1)

non_standard_col_names # EMPTY

# Now that hies_labels_clean is standardized, we can remove dta_file and use distinct() to remove duplicate questions across data files
hies_labels_distinct <- hies_labels_clean %>%
  select(-dta_file) %>%
  distinct() 

hies_long_distinct <- hies_long %>%
  select(-dta_file) %>%
  distinct() # Shrinks from 8,900,000 to 4,500,000 rows

######

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


########################################
# DATA CHECK: 
# Check hies_house_tidy
# data.frame(table(interview__key = hies_house_tidy$interview__key)) %>% filter(Freq > 1)
# 
# # Check hies_individ_tidy: i.e., any rows with repeated interview__key should have different hm_basic__id
# # Unique length of full_id should be same as nrows (hies_individ_tidy)
# hies_individ_tidy %>%
#   mutate(full_id = paste(interview__key, hm_basic__id, sep = "")) %>%
#   pull(full_id) %>%
#   unique() %>%
#   length()
########################################

# SHORT ANSWERS FOR TRANSLATION:
# FIRST, create FULL long format
# Join with hies_labels_distinct for question context (helpful for translation)
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

# Output each of the files in dta_files_not_pivoting_tidy separately - extract attributes with clean_data
agric_veg <- read_dta(file.path(datadir, "20210301_HIES_FINAL", "SPC_KIR_2019_HIES_19b-AgricVegetables_v01.dta")) %>% clean_data(return = "df")
agric_root <- read_dta(file.path(datadir, "20210301_HIES_FINAL", "SPC_KIR_2019_HIES_19c-AgricRootCrop_v01.dta")) %>% clean_data(return = "df")
agric_fruit <- read_dta(file.path(datadir, "20210301_HIES_FINAL", "SPC_KIR_2019_HIES_19d-AgricFruit_v01.dta")) %>% clean_data(return = "df")
expend_agg <- read_dta(file.path(datadir, "20210301_HIES_FINAL", "SPC_KIR_2019_HIES_30-ExpenditureAggreg_v01.dta")) %>% clean_data(return = "df")
income_agg <- read_dta(file.path(datadir, "20210301_HIES_FINAL", "SPC_KIR_2019_HIES_40-IncomeAggreg_v01.dta")) %>% clean_data(return = "df")
# NOTE: Already checked, and none of the data in the dta_files_not_pivoting_tidy files require translations (no fill-in-the-blanks)

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


#### ADDENDUM to income and expenditure data
# Incorporate PNDB metadata sent in a separate email from Mike
# Left join pndb_meta to expend_agg and income_agg (pndb_meta contains nutritional info and descriptions for ALL pndb codes, don't need to full join all of these, just left join to the data points in expend_agg and income_agg)
# Columns: Food_description and food_desc_pndb can be used as coded "translations" of the fill-in-the-blank responses
# Some of the entries in pndb_meta are blank PNDB codes... filter these out before left-joining
pndb_meta <- read_dta(file.path(datadir, "20210624_HIES-metadata", "PNDB_17May2020.dta")) %>% clean_data(return = "df") %>% rename(pndbcode = PNDBCode)

income_agg <- income_agg %>%
  left_join(pndb_meta %>% filter(pndbcode %in% c("", "..")==FALSE) %>% distinct(), by = "pndbcode") # need distinct() for pndb_meta - some rows are duplicates

expend_agg <- expend_agg %>%
  left_join(pndb_meta %>% filter(pndbcode %in% c("", "..")==FALSE) %>% distinct(), by = "pndbcode") # need distinct() for pndb_meta - some rows are duplicates

# Get fill in the blank responses for expenditure and income datasets ("description" column)
# No longer necessary since these are "translated" / coded in the PNDB addendum
# expend_alpha <- expend_agg %>%
#   select(all_of(c("description", "coicop"))) %>%
#   distinct()
# 
# income_alpha <- income_agg %>%
#   select(all_of(c("description", "coicop"))) %>%
#   distinct()


# get variable labels for each of the files in dta_file_not_pivoting_tidy
veg_labels <- clean_data(agric_veg, return = "var_labels")
root_labels <- clean_data(agric_root, return = "var_labels")
fruit_labels <- clean_data(agric_fruit, return = "var_labels")
expend_labels <- clean_data(expend_agg, return = "var_labels")
income_labels <- clean_data(income_agg, return = "var_labels")

# Get var_labels for pndb columns
pndb_labels <- clean_data(pndb_meta, return = "var_labels")

# Bind all column names and labels together for final output
hies_labels_final <- hies_labels_distinct %>%
  bind_rows(veg_labels) %>%
  bind_rows(root_labels) %>%
  bind_rows(fruit_labels) %>%
  bind_rows(expend_labels) %>%
  bind_rows(income_labels) %>%
  distinct() %>%
  arrange(col.names)

# Outputs:
# Final long format of all uniquely identified questions: hies_long_distinct - read in all dataframes in list dta_file: pivot long, combine by columns unique to ALL data files, remove duplicate questions (common to subsets of data files)
# Final tidy formats at the household (hies_house_tidy) and individual (hies_individ_tidy) levels
# Key for matching col.names (question_id in hies) to col.labels: hies_labels_final
# Fill-in-the-blank responses for translation: hies_alpha
# All "special roster" files that do not pivot to tidy (because questions do not have unique IDs)

file.date <- Sys.Date() # add file.date to all outputs to keep track of most recent versions

# Standard HIES dataset
write.csv(hies_long_distinct, file = file.path(outdir, paste(file.date, "_hies_long_qs-with-unique-ids.csv", sep = "")), row.names = FALSE)
write.csv(hies_house_tidy, file = file.path(outdir, paste(file.date, "_hies_tidy_household-level.csv", sep = "")), row.names = FALSE)
write.csv(hies_individ_tidy, file = file.path(outdir, paste(file.date, "_hies_tidy_individual-level.csv", sep = "")), row.names = FALSE)
write.csv(hies_labels_final, file = file.path(outdir, paste(file.date, "_hies_question-id-to-label-key.csv", sep = "")), row.names = FALSE)
write.csv(hies_alpha, file = file.path(outdir, paste(file.date, "_hies_text-responses-for-translation.csv", sep = "")), row.names = FALSE)

# HIES questions related to expenditures and income (pre-processed by SPC data team)
write.csv(expend_agg, file = file.path(outdir, paste(file.date, "_hies_expenditures-standard-units.csv", sep = "")), row.names = FALSE)
write.csv(income_agg, file = file.path(outdir, paste(file.date, "_hies_income-standard-units.csv", sep = "")), row.names = FALSE)

# Data rosters that do not have unique IDs:
write.csv(agric_veg, file = file.path(outdir, paste(file.date, "_special-roster-hies_vegetable-details.csv", sep = "")), row.names = FALSE)
write.csv(agric_root, file = file.path(outdir, paste(file.date, "_special-roster-hies_root-crop-details.csv", sep = "")), row.names = FALSE)
write.csv(agric_fruit, file = file.path(outdir, paste(file.date, "_special-roster-hies_fruit-details.csv", sep = "")), row.names = FALSE)

