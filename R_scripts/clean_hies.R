# Clean HIES data:

# This code was removed from the original RMD files: includes cleanning and plotting the fisheries data
# FIX IT - need to redo code to process data in batches for each HIES folder
# Reminder: keep cleaning and plotting code separate - the code below should only clean and output tidy data,
# All plotting should be done in the prelim_HIES_review.Rmd file

## Fisheries data
### *Note: no island information in current dataset
# FIX IT - if island information becomes available for fisheries data, will also need to incorporate p903r and p922.dta files (in Fisheries data folder): just copy code from prelim_survey_review.Rmd

#___________________________________________________________________________________________#
# Clean fisheries data
#___________________________________________________________________________________________#
fisheries <- read_dta(file.path(datadir, "Fisheries", "Fisheries.dta"))

fisheries <- clean_data(fisheries)[[1]]

# Extract variable label attributes
var_labels <- clean_data(fisheries)[[2]]

# Change class to character to allow left_join without warning below
var_labels <- var_labels %>%
  mutate(col.names = as.character(col.names))

# Make tidy
fisheriesTidy <- tidy_data(df = fisheries, pivot_col_1 = "sex", pivot_col_last = "p922n3", var_labels = var_labels, question_no = TRUE) # question_no = TRUE applies to fisheries data where col.labels includes a third column for question.no

write.csv(fisheriesTidy, file.path(outdir, "fisheriesTidy.csv"), row.names = FALSE)

# Output QUESTION list:
write.csv(unique(fisheriesTidy$question), file.path(outdir, "fisheries_question_list.csv"), row.names = FALSE)


#___________________________________________________________________________________________#
# Visualize fisheries data
#___________________________________________________________________________________________#
# Bar graphs and Histograms of single/multi-select questions

# Plot bar graph for each multi-select question
plotDF_multiselect <- fisheriesTidy %>%
  filter(!is.na(option)) %>%
  filter(response == "1")

plot_multi_response(plotDF_multiselect)

## Fisheries continued...
### *Single response questions*


# List short-answer, fill-in response questions here:
fill_in_questions <- c("p903n", "p921n", "p922n1", "p922n2")

# Plot histogram for each single response question while removing zeroes
plotDF_single <- fisheriesTidy %>%
  filter(is.na(option)) %>%
  filter(question.no %in% fill_in_questions == FALSE) # remove "other" responses

plot_single_response(plotDF_single, bin_n = 20)


# Output short answer responses for translation
other_answers <- fisheriesTidy %>% 
  filter(question.no %in% fill_in_questions) %>%
  filter(response != "") %>% 
  unique() %>%
  arrange(question)

write.csv(other_answers, file.path(outdir, "fisheries_other_answers.csv"), row.names = FALSE)

