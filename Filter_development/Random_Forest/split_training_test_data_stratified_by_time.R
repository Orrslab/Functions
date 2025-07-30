library(dplyr)

split_training_test_data_stratified_by_time <- function(
    data,
    train_frac = 0.8,
    species_col = "Species_id",
    tag_col = "TAG",
    time_col = "TIME",
    outlier_col = "Outliers",
    seed = 42) {
  
  set.seed(seed)
  
  # Make sure all the required columns exist in the data
  required_cols <- c(outlier_col, species_col, tag_col, time_col)
  if (!all(required_cols %in% names(data))) {
    stop("One or more required columns not found in the dataset.")
  }
  
  # Initiate lists for the training+validation set and test set
  train_list <- list()
  test_list <- list()
  
  # Get the Species id names
  species_ids <- unique(data[[species_col]])
  
  # Run on the species
  for (species in species_ids) {
    
    # Extract the species data
    species_data <- data %>% filter(.data[[species_col]] == species)
    
    # Get the tag numbers within the data
    tags <- unique(species_data[[tag_col]])
    
    # Run on the tag numbers
    for (tag in tags) {
      
      # Extract the tag data from each species data and sort by the time column
      tag_data <- species_data %>% filter(.data[[tag_col]] == tag) %>%
        arrange(.data[[time_col]])
      
      # If there are no data for this tag, move to the next tag
      n <- nrow(tag_data)
      if (n == 0) next
      
      # Calculate the fraction of the training + validation set
      split_point <- floor(train_frac * n)
      
      
      if (split_point > 0) {
        train_list[[length(train_list) + 1]] <- tag_data[1:split_point, ]
      }
      if (split_point < n) {
        test_list[[length(test_list) + 1]] <- tag_data[(split_point + 1):n, ]
      }
    }
  }
  
  train_set <- bind_rows(train_list)
  test_set <- bind_rows(test_list)
  
  return(list(
    training_set = train_set,
    test_set = test_set
  ))
}