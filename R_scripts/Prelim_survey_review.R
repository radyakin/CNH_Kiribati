# Title: prelim_survey_review
# Author: Jessica Gephart
# Date: 20-May-20
# Notes for Mike:
# - Where is the market availability data that we had in the last iteration?
# - What is the meaning of NA in the multiselect variable columns?
# - Why does VRS have household info and anemia info?

# Note to self
# - Push up to github and ping Kelvin

library(haven)
library(tidyverse)
library(magrittr)
library(ggplot2)
source("cleaning_functions.R") # FIX IT: This probably isn't the best way to do this
#___________________________________________________________________________________________#
# Read in dta files
#___________________________________________________________________________________________#
datadir <- file.path(getwd(), "Data", "HIES_raw_19May20")
outdir <- file.path(getwd(), "Outputs")

fisheries <- read_dta(file.path(datadir, "Fisheries", "Fisheries.dta"))

# Dylan woring on health data, so leave alone for now
dietaryRecall <- read_dta(file.path(datadir, "Health", "dietaryRecall.dta"))
health <- read_dta(file.path(datadir, "Health", "health.dta"))
healthyLiving <- read_dta(file.path(datadir, "Health", "healthyLiving.dta"))

# Just look at fish variables from market surveys for now
fishRoster <- read_dta(file.path(datadir, "Market Survey", "fish_roster.dta"))
fishUnitRoster <- read_dta(file.path(datadir, "Market Survey", "fish_unit_roster.dta"))

# availability <- read_dta(file.path(datadir, "marketSurveyAvailability.dta")) # Not in this folder

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

# NOTE: gather() is deprecated use pivot_longer instead
#fisheriesTidy <- fisheries %>%
#  gather(question, response, p902:p922n3) %>%
#  filter(!is.na(response)) %>%
#  left_join(var_labels, by = c("question" = "col.names")) %>%
#  separate(col.labels, into = c("question.no", "question", "option"), sep = ":")


fisheriesTidy <- fisheries %>%
  mutate_at(vars(contains('p9')), as.character) %>% # mutate class to character to avoid warning message attributes are not identical across measure variables; they will be dropped
  pivot_longer(p902:p922n3, names_to = "question", values_to = "response") %>%
  filter(!is.na(response)) %>%
  left_join(var_labels, by = c("question" = "col.names")) %>%
  separate(col.labels, into = c("question.no", "question", "option"), sep = ":") %>%
  mutate(question = str_trim(question))

# FIX IT-
# WHY NOT pivot the other question columns?
fisheriesTidy <- fisheries %>%
  mutate_all(as.character) %>% # mutate class to character to avoid warning message attributes are not identical across measure variables; they will be dropped
  pivot_longer(sex:p922n3, names_to = "question", values_to = "response") %>%
  filter(!is.na(response)) %>%
  left_join(var_labels, by = c("question" = "col.names")) %>%
  separate(col.labels, into = c("question.no", "question", "option"), sep = ":") %>%
  mutate(question = str_trim(question),
         question.no = tolower(question.no),
         question = case_when(is.na(question) ~ question.no,
                              TRUE ~ question)) # For sex, age, birthday/year, relationship, age in mo.; there is no ":" separator, so copy "question.no" to fill in for "question"


#___________________________________________________________________________________________#
# Visualize fisheries data
#___________________________________________________________________________________________#
# Histograms of single/multi-select questions
plotDF_multiselect <- fisheriesTidy %>%
  filter(!is.na(option)) %>%
  filter(response == "1")

# Plot histogram for each question
# FIX IT: Not working as a loop. Need to decide best way to make many of these
#i = 1
#ggplot(plotDF_multiselect %>% filter(question == unique(plotDF_multiselect$question)[i]), aes(x = option)) +
#    geom_bar() +
#    labs(title = paste(unique(plotDF_multiselect$question)[i]), y = "Number of yes responses", x = "") +
#    coord_flip()

#i = 2
#ggplot(plotDF_multiselect %>% filter(question == unique(plotDF_multiselect$question)[i]), aes(x = option)) +
#  geom_bar() +
#  labs(title = paste(unique(plotDF_multiselect$question)[i]), y = "Number of yes responses", x = "") +
#  coord_flip()

# LOOP:
for (i in 1:length(unique(plotDF_multiselect$question))){
  question_i <- unique(plotDF_multiselect$question)[i]

  p <- ggplot(plotDF_multiselect %>% filter(question == question_i), aes(x = option)) +
    geom_bar() +
    labs(title = paste(question_i), y = "Number of yes responses", x = "") +
    coord_flip()

  file_i <- paste("plot_", question_i, ".png", sep="")

  ggsave(filename = file.path(outdir, file_i))
}



# Histograms of single response questions
plotDF_single <- fisheriesTidy %>%
  filter(is.na(option)) %>%
  filter(question.no %in% c("p903n", "p921n", "p922n1", "p922n2")==FALSE) # remove "other" responses
  # mutate(option = as.numeric(option)) # Why? Delete?
  # mutate(response = as.numeric(response)) # contains mix of characters and numerics, mutate within the loop


# There are 11 questions, so choose i between 1 and 11
#i = 5
#ggplot(plotDF_single %>% filter(question == unique(plotDF_single$question)[i]), aes(x = response)) +
#  geom_histogram() +
#  labs(title = paste(unique(plotDF_single$question)[i]), y = "", x = "")


# LOOP:
for (i in 1:length(unique(plotDF_single$question))){
  question_i <- unique(plotDF_single$question)[i]


  plot_i <- plotDF_single %>%
    filter(question == question_i)


  if (str_detect(plot_i$response[1], pattern = "[[:digit:]]")){ # TEST: look at first row and see if response is a numeric
    plot_i <- plot_i %>%
      mutate(response = as.numeric(response))

    ggplot(plot_i, aes(x = response)) +
      geom_histogram(binwidth = max(plot_i$response/5)) +
      labs(title = question_i, y = "", x = "")

    question_i_no_slash <- str_remove(question_i, pattern = "/")
    file_i <- paste("plot_", question_i_no_slash, ".png", sep="")

    ggsave(filename = file.path(outdir, file_i))


  }

  if (str_detect(plot_i$response[1], pattern = "[[:alpha:]]")) {
    ggplot(plot_i, aes(x = response)) +
      geom_bar() +
      labs(title = question_i, y = "", x = "")

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

    question_i_no_slash <- str_remove(question_i, pattern = "/")
    file_i <- paste("plot_", question_i_no_slash, "_no_zeroes.png", sep="")

    ggsave(filename = file.path(outdir, file_i))


  }

  if (str_detect(plot_i$response[1], pattern = "[[:alpha:]]")) {
    ggplot(plot_i, aes(x = response)) +
      geom_bar() +
      labs(title = paste(question_i, "(no zeroes)", sep = " "), y = "", x = "")

    question_i_no_slash <- str_remove(question_i, pattern = "/")
    file_i <- paste("plot_", question_i_no_slash, "_no_zeroes.png", sep="")

    ggsave(filename = file.path(outdir, file_i))

  }

}






# OLD: DELETE
# LOOP but remove zero-inflation
for (i in 1:length(unique(plotDF_single$question))){
  question_i <- unique(plotDF_single$question)[i]

  plot_i <- plotDF_single %>%
    filter(question == question_i) %>%
    filter(response != 0)


  ggplot(plot_i, aes(x = response)) +
    geom_histogram(binwidth = max(plot_i$response/10)) +
    labs(title = question_i, y = "", x = "")

  question_i_no_slash <- str_remove(question_i, pattern = "/")
  file_i <- paste("plot_", question_i_no_slash, "_no_zeroes.png", sep="")

  ggsave(filename = file.path(outdir, file_i))
}



#___________________________________________________________________________________________#
# Clean VRS data
#___________________________________________________________________________________________#



#___________________________________________________________________________________________#
# Write out csv files


#___________________________________________________________________________________________#
