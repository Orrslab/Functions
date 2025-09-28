
library(DBI)
library(dplyr)
library(scales)
library(stringr)
library(lubridate)

source(file.path(getwd(), "Build_atlasRF/Labeled_DB_establishment/generate_metadata_for_tag_files.R"))
source(file.path(getwd(), "Build_atlasRF/Labeled_DB_establishment/plot_data_time_ranges.R"))
source(file.path(getwd(), "Build_atlasRF/Labeled_DB_establishment/combine_all_localizations_of_species.R"))
source(file.path(getwd(), "check_and_clean_duplicates_in_localizations.R"))
source(file.path(getwd(), "Build_atlasRF/Labeled_DB_establishment/generate_metadata_per_tag_from_species_localization_data.R"))
source(paste0(getwd(), "/save_ATLAS_data_to_sqlite.R"))

#' Process and Combine Species Data
#'
#' This function creates metadata for all the labeled data segment files of a given species,
#' plots the time ranges of the labeled data, combines all the localization data of the species,
#' removes duplicates, saves the cleaned data into an SQLite database,
#' and generates metadata per tag of this species.
#'
#' @param species_id Character. Identifier of the species being processed.
#' @param reviewer_name Character. Name of the reviewer who labeled the data.
#' @param data_source Character. Source of the data (e.g., project name or repository).
#' @param filter_applied Character. Filter applied to the data during labeling.
#' @param plot_resolution Character. Resolution for plotting time ranges
#'   (default = "1 month").
#' @param path_to_db Character. Path to the main database directory.
#' @param combined_species_data_folder Character. Path to the folder where
#'   combined species-level SQLite files should be saved.
#'
#' @return This function does not return a value; it produces side effects:
#'   \itemize{
#'     \item Saves a CSV file with metadata for the species' tag files.
#'     \item Plots the time ranges of the labeled data.
#'     \item Creates and saves an SQLite database with cleaned localizations and detections.
#'     \item Generates a per-tag metadata file summarizing the labeled data.
#'   }
#'
#' @import DBI dplyr scales stringr lubridate
#' @export
process_and_combine_species_data <- function(species_id, reviewer_name, 
                                             data_source, filter_applied,
                                             plot_resolution = "1 month",
                                             path_to_db, combined_species_data_folder) {
  
  # Path to the labeled segment files of the species
  path_to_species <- file.path(path_to_db, species_id)
  
  # Generate tag files metadata
  tag_files_metadata <- generate_metadata_for_tag_files(path_to_species,
                                                        species_id,
                                                        data_source,
                                                        reviewer_name,
                                                        filter_applied)
  
  # Save the files' metadata
  csv_path <- file.path(path_to_species, paste0(species_id, "_files_metadata.csv"))
  write.csv(tag_files_metadata, csv_path, row.names = FALSE)
  message(paste0("Metadata of ", species_id, " files saved to: ", csv_path))
  
  # Plot time ranges
  plot_data_time_ranges(tag_files_metadata, plot_resolution, species_id, path_to_species)
  
  # Combine all localizations data of the species
  combined_species_data <- combine_all_localizations_of_species(path_to_species)
  species_localization_data <- combined_species_data$LOCALIZATIONS
  species_detections_data <- combined_species_data$DETECTIONS
  
  # Check duplicates
  species_localization_data <- check_and_clean_duplicates_in_localizations(
    localization_data = species_localization_data,
    clean_duplicates = TRUE
  )
  
  # Save to SQLite
  sqlite_filepath <- file.path(combined_species_data_folder, paste0(species_id, "_labeled_data.sqlite"))
  save_ATLAS_data_to_sqlite(localization_data = species_localization_data,
                            detections_data = species_detections_data,
                            fullpath = sqlite_filepath)
  
  # Add metadata per tag
  generate_metadata_per_tag_from_species_localization_data(
    localization_data = species_localization_data,
    combined_species_data_folder = combined_species_data_folder,
    species_id = species_id,
    data_source = data_source
  )
}