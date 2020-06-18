# Plotting functions

###################################################################################################################################################

plot_multi_response <- function(plotDF_multiselect, incl_island = FALSE)
  
for (i in 1:length(unique(plotDF_multiselect$question))){
  question_i <- unique(plotDF_multiselect$question)[i]
  
  plot_i <- plotDF_multiselect %>% 
    filter(question == question_i)
  
  if (incl_island == TRUE){
    p <- ggplot(plot_i, aes(x = option)) +
      geom_bar() +
      labs(title = paste(question_i), x = "Number of yes responses", y = "") +
      theme(axis.text.x = element_text(size = 8, angle = 90, hjust = 1, vjust = 0.5)) +
      facet_wrap(~vrs_island)
  } else {
    p <- ggplot(plot_i, aes(x = option)) +
      geom_bar() +
      labs(title = paste(question_i), x = "Number of yes responses", y = "") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  }

  # Print out each plot within R
  print(p)
  # Go to new page before printing next figure
  cat("\n\n\\newpage\n")

  # Save each plot
  #file_i <- paste("plot_", question_i, ".png", sep="")
  #ggsave(filename = file.path(outdir, file_i))
  
  
}


###################################################################################################################################################

plot_single_response <- function(plotDF_single, bin_n = 20, incl_island = FALSE){
  
  
  for (i in 1:length(unique(plotDF_single$question))){
    question_i <- unique(plotDF_single$question)[i]
    
    plot_i <- plotDF_single %>%
      filter(question == question_i)  %>%
      filter(response != "") # E.g., needed for VRS - single response; question = "Gender attending meeting"
    
    
    # STRICT if statement (must contain digit and must not contain alpha) - i.e., will not proceed if response is blank
    if (str_detect(plot_i$response[1], pattern = "[[:alpha:]]")==FALSE & str_detect(plot_i$response[1], pattern = "[[:digit:]]")){ 
      plot_i <- plot_i %>%
        mutate(response = as.numeric(response))
      
      # For numeric responses, decide if it's zero inflated or not
      # loose definition - does zero have the largest count? (technically only relevant for integers, but for our data, i think this still works)
      # What is the response with the largest count?
      most_response <- names(sort(table(plot_i$response), decreasing = TRUE))[1]
      n_of_response <- sort(table(plot_i$response), decreasing = TRUE)[1]
      
      # set the title of the plot here, but use if statement to switch to new title if zero-inflated
      title <- question_i
      
      if (most_response == "0"){
        plot_i <- plot_i %>%
          filter(response != 0)
        
        title <- paste(question_i, " (zeroes removed; n = ", n_of_response, ")", sep = "")
      }
      
      if (incl_island == TRUE){
        p <- ggplot(plot_i, aes(x = response)) +
          geom_histogram(bins = bin_n) +
          labs(title = title, y = "", x = "") +
          facet_wrap(~vrs_island)
      } else {
        p <- ggplot(plot_i, aes(x = response)) +
          geom_histogram(bins = bin_n) +
          labs(title = title, y = "", x = "")
      }
      
      # Print out each plot within R
      print(p)
      # Go to new page before printing next figure
      cat("\n\n\\newpage\n")
      
      # Save each plot
      #question_i_no_slash <- str_remove(question_i, pattern = "/")
      #file_i <- paste("plot_", question_i_no_slash, ".png", sep="")
      #ggsave(filename = file.path(outdir, file_i))
    }
    
    if (str_detect(plot_i$response[1], pattern = "[[:alpha:]]")) {
      
      if (incl_island == TRUE){
        p <- ggplot(plot_i, aes(x = response)) +
          geom_bar() +
          labs(title = question_i, y = "", x = "") +
          theme(axis.text.x = element_text(size = 8, angle = 90, hjust = 1, vjust = 0.5)) +
          facet_wrap(~vrs_island)
      } else {
        p <- ggplot(plot_i, aes(x = response)) +
          geom_bar() +
          labs(title = question_i, y = "", x = "") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
      }

      
      # Print out each plot within R
      print(p)
      # Go to new page before printing next figure
      cat("\n\n\\newpage\n")
      
      # Save each plot
      #question_i_no_slash <- str_remove(question_i, pattern = "/")
      #file_i <- paste("plot_", question_i_no_slash, ".png", sep="")
      #ggsave(filename = file.path(outdir, file_i))
      
    }
    
  }
  
}


###################################################################################################################################################

plot.food.availability <- function(plotDF_food, incl_island = FALSE){
  plotDF_food$availability <- factor(plotDF_food$availability, 
                                     levels = c("A couple days per week", "A couple days per month", 
                                               "Seasonally",  "All the time", "Other times (specify)"))
  
  # Create plot for each food item
  for(i in 1:length(unique(plotDF_food$roster__id))){
    
    if (incl_island == TRUE){
      p <- ggplot(plotDF_food %>% filter(roster__id == unique(plotDF_food$roster__id)[i]), 
                  aes(x = availability)) +
        geom_bar() +
        labs(title = paste(unique(plotDF_food$roster__id)[i]), y = "", x = "") +
        theme(axis.text.x = element_text(size = 8, angle = 90, hjust = 1, vjust = 0.5)) +
        facet_wrap(~ms_island)
    } else {
      
      p <- ggplot(plotDF_food %>% filter(roster__id == unique(plotDF_food$roster__id)[i]), 
                  aes(x = availability)) +
        geom_bar() +
        labs(title = paste(unique(plotDF_food$roster__id)[i]), y = "", x = "") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
    }
 
    
    # Print out each plot within R
    print(p)
    # Go to new page before printing next figure
    cat("\n\n\\newpage\n")
  }
}



###################################################################################################################################################

plot.food.prices <- function(plotDF_food, incl_island = FALSE){

  
  # Create plot for each food item
  for(i in 1:length(unique(plotDF_food$roster__id))){
    
    
    food <- unique(plotDF_food$roster__id)[i]
    
    plotDF_food_i <- plotDF_food %>% filter(roster__id == food)
    
    unit <- plotDF_food_i %>% pull(unit_roster__id) %>% unique()
    
    
    #if (length(unit) != 1){
    #  print(food)
    #}
    #if (length(unit) == 1 ){
    #  x_axis <- paste("price per", unit)
    #  
    #  if (incl_island == TRUE){
    #    p <- ggplot(plotDF_food_i,
    #                aes(x = standardized_price)) +
    #      geom_histogram(bins = 20) +
    #      labs(title = food, y = "", x = x_axis) +
    #      facet_wrap(~ms_island)
    #  } else {
    #    
    #    p <- ggplot(plotDF_food_i,
    #                aes(x = standardized_price)) +
    #      geom_histogram(bins = 20) +
    #      labs(title = food, y = "", x = x_axis)
    #    
    #  }
    
    
    if (length(unit) != 1){ # Keep only the most common unit
      most_common_unit <- plotDF_food_i %>% count(unit_roster__id) %>% arrange(desc(n)) %>% filter(row_number()==1) %>% pull(unit_roster__id)
      plotDF_food_i <- plotDF_food_i %>%
        filter(unit_roster__id == most_common_unit)
    }
    
    
    x_axis <- paste("price per", unit)
    
    if (incl_island == TRUE){
      p <- ggplot(plotDF_food_i,
                  aes(x = standardized_price)) +
        geom_histogram(bins = 20) +
        labs(title = food, y = "", x = x_axis) +
        facet_wrap(~ms_island)
    } else {
      
      p <- ggplot(plotDF_food_i,
                  aes(x = standardized_price)) +
        geom_histogram(bins = 20) +
        labs(title = food, y = "", x = x_axis)
    }
    
    # Print out each plot within R
    print(p)
    # Go to new page before printing next figure
    cat("\n\n\\newpage\n")
    
  }

  
}


