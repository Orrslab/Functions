
# This script loads all the labeled data of each species and combines
# the data of all species into one file called labeled_data_db.sqlite.

# More details are in README.md

# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data, not

library(DBI)
library(RSQLite)

# source(file.path(getwd(), "load_localizations_data_from_sqlite_file.R"))
source(file.path(getwd(), "load_atlas_data_from_sqlite.R"))
source(file.path(getwd(), "save_ATLAS_data_to_sqlite.R"))

db_folder <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Labeled_data_DB/Visual_Filter_DB"
combined_species_data_folder <- file.path(db_folder, "Combined_species_data")

# Open all the sqlite files in the combined species folder
# List all sqlite files in the folder
sqlite_files <- list.files(combined_species_data_folder, pattern = "\\.sqlite$", full.names = TRUE)

# Initialize empty data frames before the loop
all_data_localizations <- data.frame()
all_data_detections <- data.frame()

for (sqlite_file in sqlite_files) {
  
  # Get just the file name without the path
  file_name <- basename(sqlite_file)
  
  # Load the localizations and detections from the sqlite file
  data <- load_atlas_data_from_sqlite(sqlite_file)
  
  # Get the localizations and detections data
  localization_data <- data$LOCALIZATIONS
  detection_data <- data$DETECTIONS
  
  # Extract the first two letters for species_id
  species_id <- substr(file_name, 1, 2)
  
  # Add the species_id as a column in the beginning of the data table
  localization_data <- cbind(species_id = species_id, localization_data)
  detection_data <- cbind(species_id = species_id, detection_data)
  
  # Append the localizations and detections to the big data frames
  all_data_localizations <- rbind(all_data_localizations, localization_data)
  all_data_detections <- rbind(all_data_detections, detection_data)
  
}

save_ATLAS_data_to_sqlite(localizations_data = all_data_localizations,
                          detections_data = all_data_detections,
                          fullpath = file.path(db_folder, "labeled_data_db.sqlite"))