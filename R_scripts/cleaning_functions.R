# Functions
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