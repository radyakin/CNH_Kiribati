# Code for finding questions by question_ID
# Goal is to turn this into a function
# Uses CSV file started by JAG that identifies how questions are linked together
# Author: Kelvin D. Gorospe

rm(list=ls())
library(tidyverse)
datadir <- "/Volumes/Dept/CAS/jgephart/Kiribati/Data/Metadata"
outdir <- "/Volumes/Dept/CAS/jgephart/Kiribati/Outputs"

# Read in metadata
metadat <- read.csv(file.path(datadir, "hies_question_key_20210611.csv"))

metadat_clean <- metadat %>%
  # Change to lowercase to match hies question ID key
  mutate(question_no = str_to_lower(question_no),
         variable = str_to_lower(variable)) %>%
  # Remove last line: blank
  filter(variable != "") 

# Read in all hies data files and question ID key
hies_expenditure <- read.csv(file.path(outdir, "hies_expenditures-standard-units.csv"))
hies_income <- read.csv(file.path(outdir, "hies_income-standard-units.csv"))
hies_household <- read.csv(file.path(outdir, "hies_tidy_household-level.csv"))
hies_individual <- read.csv(file.path(outdir, "hies_tidy_individual-level.csv"))
hies_fruit <- read.csv(file.path(outdir, "special-roster-hies_fruit-details.csv"))
hies_root <- read.csv(file.path(outdir, "special-roster-hies_root-crop-details.csv"))
hies_vegetable <- read.csv(file.path(outdir, "special-roster-hies_vegetable-details.csv"))
hies_key <- read.csv(file.path(outdir, "hies_question-id-to-label-key.csv"))

# Number of exact matches: column "variable" has more exact matches with hies_key
length(intersect(metadat_clean$question_no, hies_key$col.names))
length(intersect(metadat_clean$variable, hies_key$col.names))

# And so far, none of the matches in column "question_no" are different from what matches in column "variable" - i.e., column "variable" has more matches and inclusive of column "question_no"
question_no_match <- intersect(metadat_clean$question_no, hies_key$col.names)
variable_match <- intersect(metadat_clean$variable, hies_key$col.names)
setdiff(question_no_match, variable_match)
setdiff(variable_match, question_no_match)

# Which are the questions in metadata that cannot be linked to the raw data?
metadat_clean %>%
  filter(variable %in% hies_key$col.names == FALSE) %>% 
  select(variable) 

# Deep dive into "h1502" - Hypothetically, if someone wanted to pull all the questions in the Food recall: Meat section
metadat_clean %>%
  filter(variable %in% hies_key$col.names == FALSE) %>% 
  filter(str_detect(variable, "h1502"))

hies_key %>% filter(str_detect(col.names, "h1502")) # Note: in hies_key, the only match is for a question about remittances

# Strategy Search for the section and subsection name in expenditure data
names(hies_expenditure)[str_detect(names(hies_expenditure), "section|coicop")] # use column "section" or any of the "coicop" columns as potential filter columns

# Get search terms
search_section <- metadat_clean %>%
  filter(variable %in% hies_key$col.names == FALSE) %>% 
  filter(str_detect(variable, "h1502")) %>%
  pull(section_name) %>%
  unique()

search_subsection <- metadat_clean %>%
  filter(variable %in% hies_key$col.names == FALSE) %>% 
  filter(str_detect(variable, "h1502")) %>%
  pull(subsection_name) %>%
  unique()

# Search for metadata's section_name within hies_expenditure column "section"
str_to_lower(unique(hies_expenditure$section))[str_detect(str_to_lower(unique(hies_expenditure$section)), pattern = str_to_lower(search_section))] # Matches food recall and non food recall

# Search for metadata's subsection_name within hies_expenditure's coicop columns
str_to_lower(unique(hies_expenditure$coicop))[str_detect(str_to_lower(unique(hies_expenditure$coicop)), pattern = str_to_lower(search_subsection))]
str_to_lower(unique(hies_expenditure$coicop_subclass))[str_detect(str_to_lower(unique(hies_expenditure$coicop_subclass)), pattern = str_to_lower(search_subsection))]
str_to_lower(unique(hies_expenditure$coicop_class))[str_detect(str_to_lower(unique(hies_expenditure$coicop_class)), pattern = str_to_lower(search_subsection))]
str_to_lower(unique(hies_expenditure$coicop_group))[str_detect(str_to_lower(unique(hies_expenditure$coicop_group)), pattern = str_to_lower(search_subsection))]
str_to_lower(unique(hies_expenditure$coicop_division))[str_detect(str_to_lower(unique(hies_expenditure$coicop_division)), pattern = str_to_lower(search_subsection))]

# LEFT OFF HERE: subsection_name uniquely matches column coicop_class - this might be the best way to filter out all food recall queries
