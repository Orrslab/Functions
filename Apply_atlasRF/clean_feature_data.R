library(dplyr)

#' @title Clean Feature Data for Random Forest Classification
#'
#' @description
#' Prepares ATLAS localization data for use in random forest models by handling 
#' structural `NA` values, removing non-feature columns, and filtering out 
#' incomplete rows. The function also preserves the original localization data 
#' for later use.
#'
#' @param localization_data data.frame or tibble  
#'   Localization dataset containing both features and non-feature metadata.  
#'   Must include the columns:  
#'   - `num_missed_bs`  
#'   - `closest_missed_bs_distance`  
#'   - `mean_missed_bs_distance`  
#'
#' @param non_feature_column_names character vector  
#'   Names of columns to exclude from the feature set (e.g., identifiers, 
#'   metadata, or non-numeric fields).
#'
#' @return list with three elements:  
#' \describe{
#'   \item{clean_data_for_rf}{data.frame of features only, with rows containing 
#'         `NA` values removed.}
#'   \item{full_localization_data}{Original `localization_data`, including 
#'         non-feature columns and unmodified rows.}
#'   \item{complete_rows}{Logical vector indicating which rows were retained in 
#'         the clean dataset.}
#' }
#'
#' @details
#' The function replaces structural `NA` values in certain features with zeros 
#' and creates indicator columns marking whether those values were imputed:  
#' - `closest_missed_bs_distance`: set to `0` when `num_missed_bs == 0`.  
#' - `mean_missed_bs_distance`: set to `0` when `num_missed_bs < 2`.  
#'
#' It then removes user-specified non-feature columns and filters out any rows 
#' with remaining missing values. The percentage of rows containing `NA` values 
#' is reported to the console.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cleaned <- clean_feature_data(localization_data = my_data,
#'                               non_feature_column_names = c("tag", "timestamp"))
#' }
#' 
clean_feature_data <- function(localization_data, non_feature_column_names) {
  
  message("*** Cleaning the feature data for atlasRF ***")
  
  # Create a copy for later restoration of non-feature columns
  original_localization_data <- localization_data
  
  ##  Handle columns with structural NA values- random forest does not accept NA values
  
  # "closest_missed_bs_distance"
  localization_data$closest_missed_bs_distance_is_na <- localization_data$num_missed_bs == 0
  localization_data$closest_missed_bs_distance[is.na(localization_data$closest_missed_bs_distance)] <- 0
  
  # "mean_missed_bs_distance"
  localization_data$mean_missed_bs_distance_is_na <- localization_data$num_missed_bs < 2
  localization_data$mean_missed_bs_distance[localization_data$mean_missed_bs_distance_is_na] <- 0
  
  ## Remove non-feature columns
  data_with_features_only <- localization_data %>%
    dplyr::select(-all_of(non_feature_column_names))
  
  ## Identify rows with no NA values
  complete_rows <- complete.cases(data_with_features_only)
  
  # Report NA rows
  na_percentage <- mean(rowSums(is.na(data_with_features_only)) > 0) * 100
  print(paste("Percentage of rows with NA values:", na_percentage))

  ## Return:
  list(
    clean_data_for_rf = data_with_features_only[complete_rows, ],
    full_localization_data = original_localization_data,
    complete_rows = complete_rows
  ) 
  
}