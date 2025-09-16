
library(caret)
library(ranger)
library(ggplot2)

source(file.path(getwd(), "load_tables_from_sqlite_file.R"))
source(file.path(getwd(), "Filter_development/Random_Forest/split_training_test_data_stratified_by_Outliers.R"))
source(file.path(getwd(), "Filter_development/Random_Forest/split_training_test_data_stratified_by_time.R"))

#' @title Generate Training and Test Sets for Random Forest Model
#'
#' @description
#' This is the main script for creating stratified training and test sets 
#' for Random Forest model development for outlier filtering in ATLAS data. 
#' It loads the cleaned features data from an SQLite file, extracts the data 
#' of the desired species, splits the data into training and test sets, 
#' removes non-feature columns, and saves the resulting datasets to disk.
#'
#' @param config A configuration list that must include:
#'   \itemize{
#'     \item \code{paths$folder_of_feature_results} – Path to the folder where 
#'       the cleaned features SQLite file is stored.
#'     \item \code{paths$filename_cleaned_feature_data_all_species} – Filename 
#'       of the SQLite file containing cleaned features for all species.
#'     \item \code{training_and_test_settings$desired_species_id} – 
#'       ID of the species to use for model training/testing.
#'     \item \code{training_and_test_settings$non_feature_column_names} – 
#'       Character vector of column names to exclude from the training set 
#'       (e.g., metadata columns).
#'     \item \code{paths$atlasRF_results_folder} – Path to the folder 
#'       where the training and test datasets should be saved.
#'     \item \code{paths$training_set_filename} – Filename for saving the training dataset (RDS).
#'     \item \code{paths$test_set_filename} – Filename for saving the test dataset (RDS).
#'   }
#'
#' @details
#' - The function loads the features data from the specified SQLite file 
#'   using \code{load_tables_from_sqlite_file}.
#' - The dataset is filtered to include only the desired species.
#' - Data is split into training and test sets using stratification by time 
#'   (see \code{split_training_test_data_stratified_by_time}).
#' - Non-feature columns are removed from the training set to ensure that 
#'   only predictive features remain.
#' - If the Random Forest results folder does not yet exist, it will be created.
#' - The resulting datasets are saved as RDS files for later use in model training.
#'
#' @return
#' Saves two RDS files:
#'   \itemize{
#'     \item Training dataset (\code{training_set_filename})
#'     \item Test dataset (\code{test_set_filename})
#'   }
#' Returns nothing.
#'
#' @seealso 
#'   \code{\link{load_tables_from_sqlite_file}}, 
#'   \code{\link{split_training_test_data_stratified_by_time}}
#'
#' @examples
#' \dontrun{
#' config <- list(
#'   paths = list(
#'     folder_of_feature_results = "path/to/features",
#'     filename_cleaned_feature_data_all_species = "cleaned_features.sqlite",
#'     atlasRF_results_folder = "path/to/results",
#'     training_set_filename = "training_set.rds",
#'     test_set_filename = "test_set.rds"
#'   ),
#'   training_and_test_settings = list(
#'     desired_species_id = "LD",
#'     non_feature_column_names = c("ID", "Species_id", "Timestamp")
#'   )
#' )
#' main_generate_training_and_test_sets(config)
#' }
main_generate_training_and_test_sets <- function(config) {
  
  message("### STARTED THE MAIN SCRIPT OF GENERATE TRAINING AND TEST SETS FOR THE RANDOM FOREST MODEL. ###")
  
  features_filepath <- file.path(config$paths$folder_of_feature_results, config$paths$filename_cleaned_feature_data_all_species)
  
  # Load the features data from sqlite
  features_data <- load_tables_from_sqlite_file(
    sqlite_filepath = features_filepath,
    tables = "LOCALIZATIONS")
  
  localization_data <- features_data$LOCALIZATIONS
  
  #### EXTRACT THE DATA OF ONLY THE DESIRED SPECIES
  localization_data <- localization_data[localization_data$Species_id == config$training_and_test_settings$desired_species_id, ]
  ####
  
  # # Split the data into a training set and a test set- stratify by column "Outliers"
  # data_sets <- split_training_test_data_stratified_by_Outliers(
  #   data = localization_data,
  #   train_frac = config$training_and_test_settings$training_data_fraction)
  
  # Split the data into a training set and a test set- stratify by column "TIME"
  data_sets <- split_training_test_data_stratified_by_time(
    data = localization_data,
    train_frac = config$training_and_test_settings$training_data_fraction)
  
  training_set <- data_sets$training_set
  test_set <- data_sets$test_set
  
  # Remove the non-feature columns from the training set
  training_set <- training_set %>%
    dplyr::select(-all_of(config$training_and_test_settings$non_feature_column_names))
  
  # Create the results folder if it does not yet exist
  if (!dir.exists(config$paths$atlasRF_results_folder)) {
    dir.create(config$paths$atlasRF_results_folder, recursive = TRUE)
  }
  
  # save the datasets
  saveRDS(training_set, file = file.path(config$paths$atlasRF_results_folder, config$paths$training_set_filename))
  saveRDS(test_set, file = file.path(config$paths$atlasRF_results_folder, config$paths$test_set_filename))
  
}

