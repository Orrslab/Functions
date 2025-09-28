
source(file.path(getwd(), "Build_atlasRF/Random_Forest/Performance_analysis/add_confusion_categories_to_data.R"))
source(file.path(getwd(), "Build_atlasRF/Random_Forest/Performance_analysis/create_and_save_performance_maps_per_species_and_tag.R"))

#' Generate Performance Maps per Species and TAG
#'
#' This function creates visual performance maps for each species and TAG in a dataset
#' containing predicted and reference labels. It classifies predictions into confusion
#' categories (True Positive, True Negative, False Positive, False Negative) and then
#' generates interactive maps showing these categories.
#'
#' @param data_with_label_predictions A data frame containing predicted and reference
#'   labels for each observation. Must include columns for the reference labels and
#'   predicted labels.
#' @param reference_label_col Name of the column containing the true reference labels.
#'   Default is \code{"Outliers"}.
#' @param predicted_label_col Name of the column containing predicted labels. Default
#'   is \code{"predicted_outlier"}.
#' @param output_dir Directory where the interactive maps will be saved. If the directory
#'   does not exist, it will be created.
#'
#' @details
#' The function performs the following steps:
#' 1. Validates that the reference and predicted label columns exist in the data.
#' 2. Adds confusion categories (TP, TN, FP, FN) to each observation using
#'    \code{add_confusion_categories_to_data()}.
#' 3. Generates interactive maps for each unique species and TAG combination, with points
#'    colored by confusion category.
#' 4. Saves the maps as HTML files in the specified \code{output_dir}.
#'
#' @return
#' The function does not return an R object. Interactive maps are saved as HTML files
#' in the \code{output_dir}.
#'
#' @examples
#' \dontrun{
#' performance_mapping_pipeline(
#'   data_with_label_predictions = test_data,
#'   reference_label_col = "Outliers",
#'   predicted_label_col = "predicted_outlier",
#'   output_dir = "Performance_maps/"
#' )
#' }
#'
#' @import dplyr
#' @export
performance_mapping_pipeline <- function(data_with_label_predictions,
                                         reference_label_col = "Outliers",
                                         predicted_label_col = "predicted_outlier",
                                         output_dir) {
  
  message('Generating performance maps per species and tag:')
  
  # Check that required columns exist
  required_columns <- c(reference_label_col, predicted_label_col)
  missing_columns <- setdiff(required_columns, names(data_with_label_predictions))
  if (length(missing_columns) > 0) {
    stop(paste("Missing required columns:", paste(missing_columns, collapse = ", ")))
  }
  
  # Add confusion tables categories to the data (True Positive, True Negative, False Positive and False Negative)
  data_with_confusion_table_categories <- add_confusion_categories_to_data(data_with_label_predictions,
                                                                           reference_label_col = reference_label_col,
                                                                           predicted_label_col = predicted_label_col)
  
  # Create performance maps per species and TAG
  create_and_save_performance_maps_per_species_and_tag(
    data = data_with_confusion_table_categories,
    color_column = "confusion_category",
    output_dir = output_dir)
  
}

