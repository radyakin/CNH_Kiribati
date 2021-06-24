# Indie Reid-Shaw data request
# Email 2021/06/24

rm(list=ls())
library(tidyverse)
datadir <- "/Volumes/jgephart/Kiribati/Data"
outdir <- "/Volumes/jgephart/Kiribati/Outputs"


# For food recall questions: i.e., any questions beginning with "In the last 7 days....", query the expenditures dataset
hies_expenditure <- read.csv(file.path(outdir, "hies_expenditures-standard-units.csv"))
var_labels <- read.csv(file = file.path(outdir, "hies_question-id-to-label-key.csv"))

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

