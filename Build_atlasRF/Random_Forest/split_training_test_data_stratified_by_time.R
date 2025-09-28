library(dplyr)

#' @title Split Data into Training and Test Sets Stratified by Time
#'
#' @description
#' This function splits the cleaned localization data with the features into training and test sets 
#' while preserving temporal order within each species and tag. 
#' For each tag (nested within species), the data is ordered by the time column 
#' and split according to a specified training fraction. 
#' This ensures that the training set contains earlier observations and the 
#' test set contains later observations, avoiding temporal leakage.
#'
#' @param data A data frame containing at least the specified species, tag, time, and outlier columns.
#' @param train_frac Numeric (default = 0.8). Fraction of the data to allocate to the training set 
#'   (the remaining fraction goes to the test set).
#' @param species_col Character (default = `"Species_id"`). Name of the column containing species identifiers.
#' @param tag_col Character (default = `"TAG"`). Name of the column containing individual tag identifiers.
#' @param time_col Character (default = `"TIME"`). Name of the column containing temporal information 
#'   (must be sortable, e.g., POSIXct, numeric, or ordered factor).
#' @param outlier_col Character (default = `"Outliers"`). Name of the column containing outlier labels.
#' @param seed Integer (default = 42). Random seed for reproducibility. Note: affects only the reproducibility 
#'   of split order if stochasticity is introduced in preprocessing, though in this implementation 
#'   the split is deterministic given ordered times.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Checks that all required columns exist in the dataset.
#'   \item Iterates over each species and within each species, over each tag.
#'   \item Sorts the data by the specified time column.
#'   \item Splits the ordered data into a training portion (first \code{train_frac} fraction) 
#'         and a test portion (remaining rows).
#'   \item Binds together the split data across all species and tags.
#' }
#'
#' This design ensures that training always uses earlier observations 
#' and test sets use later ones, minimizing the risk of temporal leakage in modeling.
#'
#' @return A list with two data frames:
#'   \item{training_set}{The training subset of the data.}
#'   \item{test_set}{The test subset of the data.}
#'
#' @examples
#' \dontrun{
#' # Example usage
#' split_sets <- split_training_test_data_stratified_by_time(
#'   data = my_data,
#'   train_frac = 0.7,
#'   species_col = "Species_id",
#'   tag_col = "TAG",
#'   time_col = "Timestamp",
#'   outlier_col = "Outliers"
#' )
#'
#' training <- split_sets$training_set
#' testing  <- split_sets$test_set
#' }
#'
#' @seealso \code{\link{split_training_test_data_stratified_by_Outliers}}
#'
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