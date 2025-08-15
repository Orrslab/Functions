# This function creates metadata for all the tag's data files sent by 
# the reviewers who labeled the data for a particular species.
# It also plots the time ranges of the data of the data's time ranges,
# combines all the species' data, checks and removes duplicates,
# and creates a metadata file that summarizes the data per tag.

library(DBI)
library(dplyr)
library(scales)
library(stringr)
library(lubridate)

source(file.path(getwd(), "Filter_development/Labeled_DB_establishment/generate_metadata_for_tag_files.R"))
source(file.path(getwd(), "Filter_development/Labeled_DB_establishment/plot_data_time_ranges.R"))
source(file.path(getwd(), "Filter_development/Labeled_DB_establishment/combine_all_localizations_of_species.R"))
source(file.path(getwd(), "check_and_clean_duplicates_in_localizations.R"))
source(file.path(getwd(), "Filter_development/Labeled_DB_establishment/generate_metadata_per_tag_from_species_localization_data.R"))
source(paste0(getwd(), "/save_ATLAS_data_to_sqlite.R"))

process_and_combine_species_data <- function(species_id, reviewer_name, 
                                             data_source, filter_applied,
                                             plot_resolution = "1 month",
                                             path_to_db, combined_species_data_folder) {
  
  # Path to the labeled segment files of the species
  path_to_species <- file.path(path_to_db, species_id)
  
  # Generate tag files metadata
  tag_files_metadata <- generate_metadata_for_tag_files(path_to_species,
                                                        species_id,
                                                        reviewer_name,
                                                        data_source,
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
    reviewer_name = reviewer_name,
    data_source = data_source,
    filter_applied = filter_applied
  )
}