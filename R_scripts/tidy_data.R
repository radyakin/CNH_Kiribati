# For fisheries data:
# columns_to_pivot = sex:p922n3
# split_column_names = c("question.no", "question", "option"); question column must be named question


tidy_data <- function(df, pivot_col_1, pivot_col_last, var_labels, question_no = TRUE){
  
  not_all_na <- function(x) any(!is.na(x))
  
  if (question_no == TRUE){
    # Make tidy
    df_tidy <- df %>%
      mutate_all(as.character) %>% # mutate class to character to avoid warning message attributes are not identical across measure variables; they will be dropped
      pivot_longer(!!pivot_col_1:!!pivot_col_last, names_to = "question", values_to = "response") %>%
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
      pivot_longer(pivot_col_1:!!pivot_col_last, names_to = "question", values_to = "response") %>%
      filter(!is.na(response)) %>%
      left_join(var_labels, by = c("question" = "col.names")) %>%
      separate(col.labels, into = c("question", "option"), sep = ":") %>%
      mutate(question = str_trim(question)) %>%
      select_if(not_all_na)

  }

  return(df_tidy)
  
}