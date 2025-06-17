# This script gets the localizations_data with all the features,
# cleans, and formats the data to match the input format of the Random Forest function.
# It will also be used for the feature analyses, such as AUC calculation.
# The script unites the data of all species

# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data

library(readxl)
library(dplyr)

source(file.path(getwd(), "load_tables_from_sqlite_file.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/save_ATLAS_data_with_features_to_sqlite.R"))

## USER INPUT BEGINNING 

path_to_db <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Labeled_data_DB/Visual_Filter_DB"
path_to_data_with_features <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Feature_Engineering/Data_with_features"
path_to_species_metadata <- file.path(path_to_db, "Species_metadata.xlsx")
cleaned_feature_data_all_species_filename <- "Features_data_for_RF_all_species.sqlite"

### USER INPUT END

# Set the names of the relevant tables to load from the sqlite data file
tables_to_load <- c("LOCALIZATIONS", 
                    "DETECTIONS", 
                    "PARTICIPATING_BASE_STATIONS", 
                    "MISSED_BASE_STATIONS")

# Load species metadata
species_metadata <- read_excel(path_to_species_metadata)

# Initialize a data frames for the combined data of all species
combined_localizations_data <- data.frame()
combined_detections_data <- data.frame()
combined_participating_bs_data <- data.frame()
combined_missed_bs_data <- data.frame()

# Run on the species
for (species_id in species_metadata$Species_ID) {
 
  # For debug purposes
  # species_id <- "LD"
  
  print(species_id)
  
  # Name of the sqlite file with the localization_data + features of the species
  sqlite_file_name <- paste0(species_id, "_features_eng.sqlite")
  
  # Load the features' data tables from sqlite
  data <- load_tables_from_sqlite_file(
    sqlite_filepath = file.path(path_to_data_with_features, sqlite_file_name), 
    tables = tables_to_load)
  
  localization_data <- data$LOCALIZATIONS
  detections_data <- data$DETECTIONS
  participating_base_stations <- data$PARTICIPATING_BASE_STATIONS
  missed_base_stations <- data$MISSED_BASE_STATIONS
  
  ##  Handle columns with structural NA values- random forest does not accept NA values

  # "closest_missed_bs_distance"
  localization_data$closest_missed_bs_distance_is_na <- localization_data$num_missed_bs == 0
  localization_data$closest_missed_bs_distance[is.na(localization_data$closest_missed_bs_distance)] <- 0

  # "mean_missed_bs_distance"
  localization_data$mean_missed_bs_distance_is_na <- localization_data$num_missed_bs < 2
  localization_data$mean_missed_bs_distance[localization_data$mean_missed_bs_distance_is_na] <- 0

  # Calculate percentage of deleted rows
  na_percentage <- mean(rowSums(is.na(localization_data)) > 0) * 100

  print(paste("Percentage of rows with NA values:", na_percentage))

  # Delete rows wich have any NA values- the Random Forest function can't handle them
  localization_data <- na.omit(localization_data)

  # Delete all rows with Outliers == 2 or Outliers == NA
  localization_data <- localization_data %>%
    filter(!(Outliers == 2 | is.na(Outliers)))

  # Convert the Outliers column to factor type- required for the Random Forest function
  localization_data$Outliers <- factor(localization_data$Outliers, levels = c(0, 1), labels = c("valid", "outlier"))

  # Move the Outliers column to the end of the dataframe
  localization_data <- localization_data %>%
    dplyr::select(-Outliers, Outliers)
  
  # Append the prepared species data to the combined dataframe
  combined_localizations_data <- bind_rows(combined_localizations_data, localization_data)
  combined_detections_data <- bind_rows(combined_detections_data, detections_data)
  combined_participating_bs_data <- bind_rows(combined_participating_bs_data, participating_base_stations)
  combined_missed_bs_data <- bind_rows(combined_missed_bs_data, missed_base_stations)
  
}

save_ATLAS_data_with_features_to_sqlite(localizations_data = combined_localizations_data,
                                        detections_data = combined_detections_data,
                                        participating_base_stations = combined_participating_bs_data,
                                        missed_base_stations = combined_missed_bs_data,
                                        fullpath = file.path(path_to_data_with_features, cleaned_feature_data_all_species_filename))
