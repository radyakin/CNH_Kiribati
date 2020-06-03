# Title: prelim_survey_review
# Author: Jessica Gephart
# Date: 20-May-20
# Notes for Mike:
# - Where is the market availability data that we had in the last iteration?
# -- [name of product]_roster is the availability data
# - What is the meaning of NA in the multiselect variable columns?
# -- . and .a in STATA both mean missing. Refer to Mike's May 27 email for example (and to check for consistency with R)
# - Why does VRS have household info and anemia info?
# - Why are these output as different files (vrs, market, etc. each broken into multiple separate files)
#   It is easiest to have the fewest number possible
# - Are they going to translate the HIES iKiribati responses?


# Suggested path forward:
# 1. Write out tidy versions of each data set and share csv
# 2. Write functions to visualize each question type:
# - Multi- and single-select, produce bar chart
# - Integer, produce bar chart
# - Continuous, produce histogram and box and whisker
# - Free response, compile unique answers with unique IDs for translation, question, island, possibly role
# 3. Loop through data and produce pdf of all plots
# 4. Start to creat summaries by village/island

# Questions for the team
# - How do they envision engaging with the data for this checking process?
# - Timeline for compiling feedback for Mike by June 19 (suggest getting #3 to team by June 8)
# - Need to coordinate cleaning across teams and centralizing code and data 

library(haven)
library(tidyverse)
library(magrittr)
library(ggplot2)

#___________________________________________________________________________________________#
# Read in dta files
#___________________________________________________________________________________________#
datadir <- file.path(getwd(), "Data", "HIES_raw_19May20")
outdir <- file.path(getwd(), "Outputs")

source("R_scripts/cleaning_functions.R")
source("R_scripts/tidy_data.R")
# FIXED? I just made the file structure look more like an R Project. 
# Go to the root directory of your local repo for this project and make sure there are three folders: Data, Outputs, and R_scripts. 
# Then place the cleaning_function.R inside the "R_scripts" folder. 
# Set working directory to the root directory of your local repo and run script


fisheries <- read_dta(file.path(datadir, "Fisheries", "Fisheries.dta"))

# Dylan woring on health data, so leave alone for now
dietaryRecall <- read_dta(file.path(datadir, "Health", "dietaryRecall.dta"))
health <- read_dta(file.path(datadir, "Health", "health.dta"))
healthyLiving <- read_dta(file.path(datadir, "Health", "healthyLiving.dta"))

# Just look at fish variables from market surveys for now
# FIX IT: Look for easy way to combine all market survey data
fishRoster <- read_dta(file.path(datadir, "Market Survey", "fish_roster.dta"))
fishUnitRoster <- read_dta(file.path(datadir, "Market Survey", "fish_unit_roster.dta"))

# Village Resource Survey data
eventRoster <- read_dta(file.path(datadir, "VRS", "event_roster.dta"))
fishassetRoster <- read_dta(file.path(datadir, "VRS", "fish_asset_roster.dta"))
outsideRoster <- read_dta(file.path(datadir, "VRS", "outside_roster.dta"))
shareRoster <- read_dta(file.path(datadir, "VRS", "share_roster.dta"))
vrsRoster <- read_dta(file.path(datadir, "VRS", "vrs_roster.dta"))
vrs <- read_dta(file.path(datadir, "VRS", "VRS.dta"))
withinRoster <- read_dta(file.path(datadir, "VRS", "within_roster.dta"))

#___________________________________________________________________________________________#
# Clean fisheries data
#___________________________________________________________________________________________#
fisheries <- clean_data(fisheries)[[1]]

# Extract variable label attributes
var_labels <- clean_data(fisheries)[[2]]

# Change class to character to allow left_join without warning below
var_labels <- var_labels %>%
  mutate(col.names = as.character(col.names))

# Make tidy
fisheriesTidy <- tidy_data(df = fisheries, pivot_col_1 = "sex", pivot_col_last = "p922n3", var_labels = var_labels, question_no = TRUE) # question_no = TRUE applies to fisheries data where col.labels includes a third column for question.no

write.csv(fisheriesTidy, "Outputs/fisheriesTidy.csv", row.names = FALSE)
#___________________________________________________________________________________________#
# Visualize fisheries data
#___________________________________________________________________________________________#
# Histograms of single/multi-select questions
plotDF_multiselect <- fisheriesTidy %>%
  filter(!is.na(option)) %>%
  filter(response == "1")

# Plot histogram for each question
for (i in 1:length(unique(plotDF_multiselect$question))){
  question_i <- unique(plotDF_multiselect$question)[i]

  p <- ggplot(plotDF_multiselect %>% filter(question == question_i), aes(x = option)) +
    geom_bar() +
    labs(title = paste(question_i), y = "Number of yes responses", x = "") +
    coord_flip()
  
  # Print out each plot within R
  print(p)
  
  # Save each plot
  file_i <- paste("plot_", question_i, ".png", sep="")
  #ggsave(filename = file.path(outdir, file_i))
}


# Histograms of single response questions
plotDF_single <- fisheriesTidy %>%
  filter(is.na(option)) %>%
  filter(question.no %in% c("p903n", "p921n", "p922n1", "p922n2")==FALSE) # remove "other" responses

for (i in 1:length(unique(plotDF_single$question))){
  question_i <- unique(plotDF_single$question)[i]

  plot_i <- plotDF_single %>%
    filter(question == question_i)


  if (str_detect(plot_i$response[1], pattern = "[[:digit:]]")){ # TEST: look at first row and see if response is a numeric
    plot_i <- plot_i %>%
      mutate(response = as.numeric(response))

    p <- ggplot(plot_i, aes(x = response)) +
      geom_histogram(binwidth = max(plot_i$response/5)) +
      labs(title = question_i, y = "", x = "")

    # Print out each plot within R
    print(p)
    
    # Save each plot
    question_i_no_slash <- str_remove(question_i, pattern = "/")
    file_i <- paste("plot_", question_i_no_slash, ".png", sep="")

    ggsave(filename = file.path(outdir, file_i))


  }

  if (str_detect(plot_i$response[1], pattern = "[[:alpha:]]")) {
    p <- ggplot(plot_i, aes(x = response)) +
      geom_bar() +
      labs(title = question_i, y = "", x = "")

    # Print out each plot within R
    print(p)
    
    # Save each plot
    question_i_no_slash <- str_remove(question_i, pattern = "/")
    file_i <- paste("plot_", question_i_no_slash, ".png", sep="")

    ggsave(filename = file.path(outdir, file_i))

  }

}


# LOOP but remove zero-inflation:
for (i in 1:length(unique(plotDF_single$question))){
  question_i <- unique(plotDF_single$question)[i]

  # FIX IT - if desired, allow zeroes to be filtered out and re-run entire loop
  plot_i <- plotDF_single %>%
    filter(question == question_i) %>%
    filter(response != 0)



  if (str_detect(plot_i$response[1], pattern = "[[:digit:]]")){ # TEST: look at first row and see if response is a numeric
    plot_i <- plot_i %>%
      mutate(response = as.numeric(response))

    ggplot(plot_i, aes(x = response)) +
      geom_histogram(binwidth = max(plot_i$response/5)) +
      labs(title = paste(question_i, "(no zeroes)", sep = " "), y = "", x = "")
    
    # Print out each plot within R
    print(p)
    
    # Save each plot
    question_i_no_slash <- str_remove(question_i, pattern = "/")
    file_i <- paste("plot_", question_i_no_slash, "_no_zeroes.png", sep="")

    ggsave(filename = file.path(outdir, file_i))


  }

  if (str_detect(plot_i$response[1], pattern = "[[:alpha:]]")) {
    ggplot(plot_i, aes(x = response)) +
      geom_bar() +
      labs(title = paste(question_i, "(no zeroes)", sep = " "), y = "", x = "")

    # Print out each plot within R
    print(p)
    
    # Save each plot
    question_i_no_slash <- str_remove(question_i, pattern = "/")
    file_i <- paste("plot_", question_i_no_slash, "_no_zeroes.png", sep="")

    ggsave(filename = file.path(outdir, file_i))

  }

}

#___________________________________________________________________________________________#
# Clean VRS data
#___________________________________________________________________________________________#
vrs <- clean_data(vrs)[[1]]

# Extract variable label attributes
var_labels <- clean_data(vrs)[[2]]

# Change class to character to allow left_join without warning below
var_labels <- var_labels %>%
  mutate(col.names = as.character(col.names))

# Make tidy
vrsTidy <- tidy_data(df = vrs, pivot_col_1 = "vrs_island", pivot_col_last = "assignment__id", var_labels = var_labels, question_no = FALSE)

write.csv(vrsTidy, "Outputs/vrsTidy.csv", row.names = FALSE)

#___________________________________________________________________________________________#
# Clean event roster 
#___________________________________________________________________________________________#
eventRoster <- clean_data(eventRoster)[[1]]

# Extract variable label attributes
var_labels <- clean_data(eventRoster)[[2]]

# Change class to character to allow left_join without warning below
var_labels <- var_labels %>%
  mutate(col.names = as.character(col.names))

# Make tidy
eventRosterTidy <- tidy_data(df = eventRoster, pivot_col_1 = "event_roster__id", pivot_col_last = "event_nonfinfish", var_labels = var_labels, question_no = FALSE)



write.csv(eventRosterTidy, "Outputs/eventRosterTidy.csv", row.names = FALSE)

#___________________________________________________________________________________________#
# Clean fish roster
#___________________________________________________________________________________________#
fishassetRoster <- clean_data(fishassetRoster)[[1]]

# Extract variable label attributes
var_labels <- clean_data(fishassetRoster)[[2]]

# Change class to character to allow left_join without warning below
var_labels <- var_labels %>%
  mutate(col.names = as.character(col.names))

# Make tidy
fishassetRosterTidy <- tidy_data(df = fishassetRoster, pivot_col_1 = "fish_asset_roster__id", pivot_col_last = "num_assets10", var_labels = var_labels, question_no = FALSE)

write.csv(fishassetRosterTidy, "Outputs/fishassetRosterTidy.csv", row.names = FALSE)

#___________________________________________________________________________________________#
# Clean outside roster
#___________________________________________________________________________________________#
outsideRoster <- clean_data(outsideRoster)[[1]]

# Extract variable label attributes
var_labels <- clean_data(outsideRoster)[[2]]

# Change class to character to allow left_join without warning below
var_labels <- var_labels %>%
  mutate(col.names = as.character(col.names))

# Make tidy
outsideRosterTidy <- tidy_data(df = outsideRoster, pivot_col_1 = "outside_roster__id", pivot_col_last = "travel_time_out", var_labels = var_labels, question_no = FALSE)

write.csv(outsideRosterTidy, "Outputs/outsideRosterTidy.csv", row.names = FALSE)

#___________________________________________________________________________________________#
# Clean share roster
#___________________________________________________________________________________________#
shareRoster <- clean_data(shareRoster)[[1]]

# Extract variable label attributes
var_labels <- clean_data(shareRoster)[[2]]

# Change class to character to allow left_join without warning below
var_labels <- var_labels %>%
  mutate(col.names = as.character(col.names))

# Make tidy
shareRosterTidy <- tidy_data(df = shareRoster, pivot_col_1 = "share_roster__id", pivot_col_last = "share_other", var_labels = var_labels, question_no = FALSE)

write.csv(shareRosterTidy, "Outputs/shareRosterTidy.csv", row.names = FALSE)

#___________________________________________________________________________________________#
# Clean VRS roster
#___________________________________________________________________________________________#
vrsRoster <- read_dta(file.path(datadir, "VRS", "vrs_roster.dta"))
vrsRoster <- clean_data(vrsRoster)[[1]]

# Extract variable label attributes
var_labels <- clean_data(vrsRoster)[[2]]

# Change class to character to allow left_join without warning below
var_labels <- var_labels %>%
  mutate(col.names = as.character(col.names))

# Make tidy
vrsRosterTidy <- tidy_data(df = vrsRoster, pivot_col_1 = "vrs_roster__id", pivot_col_last = "live_years", var_labels = var_labels, question_no = FALSE)

write.csv(vrsRosterTidy, "Outputs/vrsRosterTidy.csv", row.names = FALSE)

#___________________________________________________________________________________________#
# Clean within roster
#___________________________________________________________________________________________#
withinRoster <- clean_data(withinRoster)[[1]]

# Extract variable label attributes
var_labels <- clean_data(withinRoster)[[2]]

# Change class to character to allow left_join without warning below
var_labels <- var_labels %>%
  mutate(col.names = as.character(col.names))

# Make tidy
withinRosterTidy <- tidy_data(df = vrsRoster, pivot_col_1 = "within_roster__id", pivot_col_last = "travel_time_within", var_labels = var_labels, question_no = FALSE)

write.csv(withinRosterTidy, "Outputs/withinRosterTidy.csv", row.names = FALSE)
