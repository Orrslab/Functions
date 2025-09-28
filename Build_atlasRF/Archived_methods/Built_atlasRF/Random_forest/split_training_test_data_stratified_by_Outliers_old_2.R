library(caret)
library(dplyr)

#' @title Split Data into Training and Test Sets Stratified by Outliers
#'
#' @description
#' Splits localization data into training and test sets while accounting for 
#' outlier labels and species identity. Data are divided at the level of tags 
#' into three groups: training-only tags, test-only tags, and shared tags 
#' (that contribute to both sets). Shared tags are further split 
#' in a stratified way based on species and outlier labels, ensuring that 
#' both training and test sets maintain representative class distributions.
#'
#' @param data A data frame containing at least the specified outlier, species, and tag columns.
#' @param train_frac Numeric (default = 0.8). Fraction of the data to allocate to the training set.
#'   Applies both globally and when splitting shared tags.
#' @param shared_frac Numeric (default = 0.1). Fraction of tags that should appear 
#'   in both training and test sets. These tags are stratified by species and outlier labels.
#' @param outlier_col Character (default = `"Outliers"`). Name of the column 
#'   containing outlier labels (e.g., 0/1 or factor).
#' @param species_col Character (default = `"Species_id"`). Name of the column 
#'   containing species identifiers.
#' @param tag_col Character (default = `"TAG"`). Name of the column containing individual tag identifiers.
#' @param seed Integer (default = 42). Random seed for reproducibility of tag sampling.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Checks that required columns exist in the dataset.
#'   \item Randomly assigns tags into three disjoint groups: 
#'         training-only, test-only, and shared.
#'   \item For shared tags, splits the data rows between training and test sets 
#'         in a stratified fashion (by species and outlier labels) 
#'         using \code{caret::createDataPartition}.
#'   \item Combines training-only tags with the training portion of shared tags, 
#'         and test-only tags with the test portion of shared tags.
#' }
#'
#' This approach ensures that:
#' \itemize{
#'   \item Species and outlier class distributions are preserved.
#'   \item Training and test sets remain disjoint at the tag level, 
#'         except for tags intentionally shared.
#' }
#'
#' @return A list with two data frames:
#'   \item{training_set}{The training subset of the data.}
#'   \item{test_set}{The test subset of the data.}
#'
#' @examples
#' \dontrun{
#' split_sets <- split_training_test_data_stratified_by_outliers(
#'   data = my_data,
#'   train_frac = 0.75,
#'   shared_frac = 0.15,
#'   outlier_col = "Outliers",
#'   species_col = "Species_id",
#'   tag_col = "TAG"
#' )
#'
#' training <- split_sets$training_set
#' testing  <- split_sets$test_set
#' }
#'
#' @seealso 
#'   \code{\link{split_training_test_data_stratified_by_time}}
#'
split_training_test_data_stratified_by_Outliers <- function(
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
  
  # Split shared data
  if (nrow(data_shared) < 2) {
    # Too small to split, assign all to train
    train_shared <- data_shared
    test_shared <- data.frame()
  } else {
    # Try species × outlier strata first
    data_shared <- data_shared %>%
      mutate(strata = interaction(.[[species_col]], .[[outlier_col]], drop = TRUE))
    
    if (all(table(data_shared$strata) > 1)) {
      # Enough rows per strata
      shared_index <- createDataPartition(data_shared$strata, p = train_frac, list = FALSE)
    } else if (length(unique(data_shared[[outlier_col]])) > 1 &&
               all(table(data_shared[[outlier_col]]) > 1)) {
      # Fallback: stratify by outlier only
      shared_index <- createDataPartition(data_shared[[outlier_col]], p = train_frac, list = FALSE)
    } else {
      # Last fallback: random split
      shared_index <- sample(seq_len(nrow(data_shared)), size = floor(train_frac * nrow(data_shared)))
    }
    
    train_shared <- data_shared[shared_index, ] %>% select(-strata)
    test_shared <- data_shared[-shared_index, ] %>% select(-strata)
  }
  
  # DELETE AFTER DEBUGGING
  # # Stratified split of shared data
  # data_shared <- data_shared %>%
  #   mutate(strata = interaction(.[[species_col]], .[[outlier_col]], drop = TRUE))
  # shared_index <- createDataPartition(data_shared$strata, p = train_frac, list = FALSE)
  # 
  # # Split the shared tags' data between the training and test sets
  # train_shared <- data_shared[shared_index, ] %>% select(-strata)
  # test_shared <- data_shared[-shared_index, ] %>% select(-strata)
  
  # Add the splitted shared data between the training and test sets
  train_final <- bind_rows(data_train, train_shared)
  test_final <- bind_rows(data_test, test_shared)
  
  return(list(
    training_set = train_final,
    test_set = test_final
  ))
}