# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data

library(dplyr)

source(file.path(getwd(), "load_atlas_data_from_sqlite.R"))

## USER INPUT BEGINNING 

path_to_db <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Annotated_data_DB/Visual_Filter_DB"
path_to_species_metadata <- file.path(path_to_db, "Combined_species_data", "metadata_per_species.csv")
folder_to_save_results <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Feature_Engineering/Data_with_features"

clean_duplicates <- TRUE

### USER INPUT END

# Load species metadata
species_metadata <- read.csv(path_to_species_metadata)

# all_data <- load_atlas_data_from_sqlite(file.path(path_to_db, "labeled_data_db.sqlite"))

# Run on the species
for (species_id in species_metadata$Species_id) {
  
  print(species_id)
  
  # Set the soecies' sqlite file name and path
  file_name <- paste0(species_id, "_labeled_data.sqlite")
  file_path <- file.path(path_to_db, "Combined_species_data", file_name)
  
  # Load the species data from the sqlite file
  data <- load_atlas_data_from_sqlite(file_path)
  
  # Extract the localizations and detections data
  localization_data <- data$LOCALIZATIONS
  detection_data <- data$DETECTIONS
  
  # Check for duplicates based on both TAG and TIME- returns a few wrong duplicates whose TIME values are different in just a few seconds
  duplicates <- localization_data %>%
    group_by(TAG, TIME) %>%
    filter(n() > 1) %>%
    ungroup()
  
  # Print message depending on whether duplicates were found
  if (nrow(duplicates) > 0) {
    cat("Number of duplicated (TAG, TIME) combinations:", nrow(duplicates), "\n")
    print(duplicates)
  } else {
    cat("There are no duplicates in the data.\n")
  }
  
  if (clean_duplicates) {
    # Clean the duplicates, while prioritizing leaving the Outliers = 1
    localization_data <- localization_data %>%
      arrange(TAG, TIME, desc(Outliers == 1), desc(Outliers == 2)) %>%
      distinct(TAG, TIME, .keep_all = TRUE)
  }
  
}