
source(file.path(getwd(), "Filter_development/Random_Forest/Performance_analysis/evaluate_model_performance_vs_other_filter.R"))
source(file.path(getwd(), "Filter_development/Random_Forest/Performance_analysis/performance_mapping_pipeline.R"))

#' Compare atlasRF Outlier Detection with the Confidence Filter
#'
#' This wrapper function compares the performance of the atlasRF outlier detection
#' method against a Confidence Filter on localization data. It calculates confusion
#' matrices and performance metrics, and generates performance maps for visualization.
#'
#' @param localization_data_with_atlasRF_outlier_labels A data frame containing
#'   localization points with atlasRF outlier labels in the column \code{Outliers_atlasRF}.
#' @param config A list containing configuration settings, including parameters for
#'   the Confidence Filter and paths for saving results.
#'
#' @details
#' The function performs the following steps:
#' 1. Applies the Confidence Filter using \code{TrackConfidenceLevelcpp()} with parameters
#'    specified in \code{config$confidence_filter_settings}.
#' 2. Converts the label columns to factors for consistent comparison.
#' 3. Computes performance metrics comparing atlasRF predictions against the Confidence Filter
#'    using \code{evaluate_model_performance_vs_other_filter()}.
#' 4. Generates interactive performance maps and saves them in the specified output folder
#'    using \code{performance_mapping_pipeline()}.
#' 5. Ensures that all necessary output directories exist, creating them if needed.
#'
#' @return
#' The function does not return a value. All results (metrics and performance maps) are
#' saved to disk in folders specified by the configuration.
#'
#' @export
#' 
wrapper_compare_atlasRF_and_confidence_filter <- function(localization_data_with_atlasRF_outlier_labels, config) {
  
  message("*** Comparing atlasRF to the Confidence Filter. ***")
  
  source(file.path(getwd(),"Track_cpp.R"))
  
  localization_data_with_atlasRF_and_confidence_filter <- TrackConfidenceLevelcpp(localization_data_with_atlasRF_outlier_labels,
                                                                                  conectedVel = config$confidence_filter_settings$conectedVel,
                                                                                  conectedDist = config$confidence_filter_settings$conectedDist,
                                                                                  stdlim = config$confidence_filter_settings$stdlim,
                                                                                  minNBSforConf2 = config$confidence_filter_settings$minNBSforConf2,
                                                                                  minNBSforConf1 = config$confidence_filter_settings$minNBSforConf1,
                                                                                  Nconf1forConf2 = config$confidence_filter_settings$Nconf1forConf2)
  # If Conf == 2 then Outliers_Conf = 0, else Outliers_Conf = 1
  localization_data_with_atlasRF_and_confidence_filter$Outliers_Conf <- ifelse(localization_data_with_atlasRF_and_confidence_filter$Conf == 2, 0, 1)
  
  ## Calculate confusion matrix and metrics of atlasRF versus Confidence Filter ###
  
  # Convert the label columns to factor
  localization_data_with_atlasRF_and_confidence_filter$Outliers_atlasRF <- factor(
    localization_data_with_atlasRF_and_confidence_filter$Outliers_atlasRF,
    levels = c(0, 1),
    labels = c("valid", "outlier")
  )
  
  localization_data_with_atlasRF_and_confidence_filter$Outliers_Conf <- factor(
    localization_data_with_atlasRF_and_confidence_filter$Outliers_Conf,
    levels = c(0, 1),
    labels = c("valid", "outlier")
  )
  
  path_to_save_comparison_results <- config$confidence_filter_settings$folder_comparison_results
  
  if (!file.exists(path_to_save_comparison_results)) {
    dir.create(path_to_save_comparison_results, recursive = TRUE)
  }
  
  # Evaluate the confusion matrix and comparison metrics between atlasRF and the Confidence Filter
  evaluate_model_performance_vs_other_filter(data = localization_data_with_atlasRF_and_confidence_filter,
                                             reference_label_col = "Outliers_Conf",
                                             predicted_label_col = "Outliers_atlasRF",
                                             positive_class = "outlier",
                                             output_dir = paste0(path_to_save_comparison_results, "/atlasRF_vs_confidence_filter"))
  
  ### Generate and save Performance map ###
  
  # Get the specied id
  species_id <- config$animal_name_code
  
  # Add the species id to the data to match the format of the performance maps function
  localization_data_with_atlasRF_and_confidence_filter$Species_id <- species_id
  
  # Create a folder to save the performance maps
  maps_folder <- file.path(path_to_save_comparison_results, "Maps")
  
  if (!dir.exists(maps_folder)) {
    dir.create(maps_folder, recursive = TRUE)
  }
  
  performance_mapping_pipeline(localization_data_with_atlasRF_and_confidence_filter,
                               reference_label_col = "Outliers_Conf",
                               predicted_label_col = "Outliers_atlasRF",
                               output_dir = maps_folder)
  
  return(localization_data_with_atlasRF_and_confidence_filter)
  
}