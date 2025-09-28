
library(caret)
library(dplyr)

split_training_test_data_stratified_by_Outliers <- function(
    data, 
    train_frac = 0.8,
    outlier_col = "Outliers",
    species_col = "Species_id",
    seed = 42) {
  
  set.seed(seed)
  
  if (!(outlier_col %in% names(data))) stop("Outlier column not found.")
  if (!(species_col %in% names(data))) stop("Species column not found.")
  
  # Create a new column for stratification by interaction of species and outlier status
  data <- data %>%
    mutate(strata = interaction(data[[species_col]], data[[outlier_col]], drop = TRUE))
  
  # Stratified sampling using the interaction column
  train_index <- createDataPartition(data$strata, p = train_frac, list = FALSE)
  
  train_val_set <- data[train_index, ] %>% select(-strata)
  test_set <- data[-train_index, ] %>% select(-strata)
  
  return(list(
    training_and_validation_set = train_val_set,
    test_set = test_set
  ))
}