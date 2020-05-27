# Functions
clean_data <- function(df){
  var_labels <- data.frame(col.names = colnames(df), col.labels = NA)
  
  for(i in 1:length(colnames(df))){
    var_labels$col.labels[i] <- attributes(df[[i]])$label
  }
  
  df <- df %<>% 
    mutate_if(is.labelled, funs(as_factor(.)))
  
  return(list(df, var_labels))
}