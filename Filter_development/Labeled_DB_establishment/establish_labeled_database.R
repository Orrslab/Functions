
# This script creates metadata for all the tag's data files sent by 
# the reviewers who labeled the data for a particular species.
# It also plots the time ranges of the data of the data's time ranges,
# combines all the species' data, checks and removes duplicates,
# and creates a metadata file that summarizes the data per tag.

# After running this script, please run the script get_metadata_per_species.R

# Read the README file for more instructions

# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data, not

# Load required libraries
library(DBI)
library(dplyr)
library(scales)
library(stringr)
library(lubridate)

source(file.path(getwd(), "Filter_development/Labeled_DB_establishment/generate_metadata_for_tag_files.R"))
source(file.path(getwd(), "Filter_development/Labeled_DB_establishment/plot_data_time_ranges.R"))
source(file.path(getwd(), "Filter_development/Labeled_DB_establishment/combine_all_localizations_of_species.R"))
source(file.path(getwd(), "check_and_clean_duplicates_in_localizations.R"))
source(file.path(getwd(), "Filter_development/Labeled_DB_establishment/generate_metadata_per_tag_from_species_localizations_data.R"))
source(paste0(getwd(), "/save_ATLAS_data_to_sqlite.R"))

#######################################################################
# USER INPUT- set the species name
species_id <- "WB"

# USER INPUT- insert the name of the person who annotated the data
# reviewer_name <- "Shlomo Cain"
reviewer_name <- "Michal Handel"
# reviewer_name <- "Jehuda Samuel"

data_source <- "ATLAS system Harod"

filter_applied <- "Visual Filter"

# USER INPUT- adjust the database path if necessary
path_to_db <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Labeled_data_DB/Visual_Filter_DB"

# Time Ranges bar plot resolution
# plot_resolution <- "1 hour"
# plot_resolution <- "1 week"
plot_resolution <- "1 month"

# END OF USER INPUT

#######################################################################

# Full path to the species data
path_to_species <- file.path(path_to_db, species_id)

# Path to combined species data
combined_species_data_folder <- file.path(path_to_db, "Combined_species_data")

#######################################################################

# Generate the medatada of the files with single tags- the files received from the reviewers
tag_files_metadata <- generate_metadata_for_tag_files(path_to_species,
                                                      species_id,
                                                      reviewer_name,
                                                      data_source,
                                                      filter_applied)

# Save the tag files' metadata to CSV
csv_path <- file.path(path_to_species, paste0(species_id, "_files_metadata.csv"))
write.csv(tag_files_metadata, csv_path, row.names = FALSE)
message(paste0("Metadata of the ", species_id, " files saved to:", csv_path, "\n"))

#######################################################################

# Create a bar Plot of the time ranges- to check if there ore time overlaps within the tags of the same species
plot_data_time_ranges(tag_files_metadata, plot_resolution, species_id, path_to_species)

#######################################################################

# Combine all the location data from all tags of the species
combined_species_data <- combine_all_localizations_of_species(path_to_species)

# Save the combined data as sqlite
file_name_species <- paste0(species_id, "_labeled_data.sqlite")
sqlite_filepath <- file.path(combined_species_data_folder, file_name_species)

species_localizations_data <- combined_species_data$LOCALIZATIONS
species_detections_data <- combined_species_data$DETECTIONS

#######################################################################

# Check for duplicates in the combined species data
species_localizations_data <- check_and_clean_duplicates_in_localizations(
  localization_data = species_localizations_data,
  clean_duplicates = TRUE)

#######################################################################

# Save the combined species data
save_ATLAS_data_to_sqlite(localizations_data = species_localizations_data,
                          detections_data = species_detections_data,
                          fullpath = sqlite_filepath)

#######################################################################

# Add the metadata of the combined species file to the metatada of the other species' files
metadata_per_tag <- generate_metadata_per_tag_from_species_localizations_data(
  species_localizations_data,
  combined_species_data_folder)

#######################################################################
