library(caret)
library(dplyr)

split_training_test_data_stratified_by_outliers <- function(
    data, 
    train_frac = 0.8,
    shared_frac = 0.1,  # Fraction of TAGs that appear in both sets
    outlier_col = "Outliers",
    species_col = "Species_id",
    tag_col = "TAG",
    seed = 42) {
  
  set.seed(seed)
  
  # Check input columns
  required_cols <- c(outlier_col, species_col, tag_col)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
  
  # Unique TAGs
  unique_tags <- unique(data[[tag_col]])
  n_tags <- length(unique_tags)
  
  # Determine the number of tag sets
  n_test <- round(n_tags * (1 - train_frac - shared_frac))
  n_shared <- round(n_tags * shared_frac)
  n_train <- n_tags - n_test - n_shared
  
  # Sample tag number for each set- training, shared, and test
  tag_test <- sample(unique_tags, n_test)
  remaining_tags <- setdiff(unique_tags, tag_test)
  tag_shared <- sample(remaining_tags, n_shared)
  tag_train <- setdiff(remaining_tags, tag_shared)
  
  # Extract the data with features for each group
  data_test <- data %>% filter(!!sym(tag_col) %in% tag_test)
  data_train <- data %>% filter(!!sym(tag_col) %in% tag_train)
  data_shared <- data %>% filter(!!sym(tag_col) %in% tag_shared)
  
  # Stratified split of shared data
  data_shared <- data_shared %>%
    mutate(strata = interaction(.[[species_col]], .[[outlier_col]], drop = TRUE))
  shared_index <- createDataPartition(data_shared$strata, p = train_frac, list = FALSE)
  
  # Split the shared tags' data between the training and test sets
  train_shared <- data_shared[shared_index, ] %>% select(-strata)
  test_shared <- data_shared[-shared_index, ] %>% select(-strata)
  
  # Add the splitted shared data between the training and test sets
  train_final <- bind_rows(data_train, train_shared)
  test_final <- bind_rows(data_test, test_shared)
  
  return(list(
    training_set = train_final,
    test_set = test_final
  ))
}