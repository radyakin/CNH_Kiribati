# Cleaning Functions


###################################################################################################
clean_data <- function(df){
  var_labels <- data.frame(col.names = colnames(df), col.labels = NA)
  
  for(i in 1:length(colnames(df))){
    if(length(attributes(df[[i]])$label) == 1){
      var_labels$col.labels[i] <- attributes(df[[i]])$label
    }else{
      var_labels$col.labels[i] <- var_labels$col.names[i]
    }
  }
  
  df <- df %<>% 
    mutate_if(is.labelled, as_factor)
  
  return(list(df = df, var_labels = var_labels))
}


###################################################################################################

tidy_data <- function(df, pivot_col_1, pivot_col_last, var_labels, question_no = TRUE){
  
  not_all_na <- function(x) any(!is.na(x))
  
  # question_no = TRUE applies to fisheries data where col.labels includes a third column for question.no
  if (question_no == TRUE){
    # Make tidy
    df_tidy <- df %>%
      mutate_all(as.character) %>% # mutate class to character to avoid warning message attributes are not identical across measure variables; they will be dropped
      pivot_longer(all_of(pivot_col_1):all_of(pivot_col_last), names_to = "question", values_to = "response") %>% # NOTE: use all_of() to tidy_select variable names
      filter(!is.na(response)) %>%
      left_join(var_labels, by = c("question" = "col.names")) %>%
      separate(col.labels, into = c("question.no", "question", "option"), sep = ":") %>%
      mutate(question = str_trim(question),
             question.no = tolower(question.no),
             question = case_when(is.na(question) ~ question.no,
                                  TRUE ~ question))  %>% # For sex, age, birthday/year, relationship, age in mo.; there is no ":" separator, so copy "question.no" to fill in for "question"
      select_if(not_all_na)
    
    
    
  } else {
    
    
    # Make tidy
    df_tidy <- df %>%
      mutate_all(as.character) %>% # mutate class to character to avoid warning message attributes are not identical across measure variables; they will be dropped
      pivot_longer(all_of(pivot_col_1):all_of(pivot_col_last), names_to = "question", values_to = "response") %>%
      filter(!is.na(response)) %>%
      filter(response != "##N/A##") %>%
      left_join(var_labels, by = c("question" = "col.names")) %>%
      separate(col.labels, into = c("question", "option"), sep = ":") %>%
      mutate(question = str_trim(question)) %>%
      select_if(not_all_na)
    
  }
  
  return(df_tidy)
  
}