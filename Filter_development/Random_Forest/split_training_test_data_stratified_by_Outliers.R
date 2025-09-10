library(caret)
library(dplyr)

#' @title Split Data into Training and Test Sets Stratified by Outliers labels.
#'
#' @description
#' Splits localization data into training and test sets, stratifying by outlier labels.
#'
#' @param data A data frame containing at least the specified outlier column.
#' @param train_frac Numeric (default = 0.8). Fraction of the data to allocate to the training set.
#' @param outlier_col Character (default = "Outliers"). Name of the column containing outlier labels.
#' @param seed Integer (default = 42). Random seed for reproducibility.
#'
#' @return A list with two data frames:
#'   \item{training_set}{The training subset of the data.}
#'   \item{test_set}{The test subset of the data.}
split_training_test_data_stratified_by_Outliers <- function(
    data,
    train_frac = 0.8,
    outlier_col = "Outliers",
    seed = 42
) {
  set.seed(seed)
  
  # Check input column
  if (!(outlier_col %in% names(data))) stop(paste("Missing column:", outlier_col))
  
  # If only one row, assign all to training
  if (nrow(data) < 2) {
    return(list(training_set = data, test_set = data.frame()))
  }
  
  # Stratified split by outlier labels
  partition_index <- createDataPartition(data[[outlier_col]], p = train_frac, list = FALSE)
  
  training_set <- data[partition_index, ]
  test_set <- data[-partition_index, ]
  
  return(list(
    training_set = training_set,
    test_set = test_set
  ))
}