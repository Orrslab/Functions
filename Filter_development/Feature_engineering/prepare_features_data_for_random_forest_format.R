# This script gets the localization_data with all the features,
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
source(file.path(getwd(), "Filter_development/Stops_filter/apply_stops_rule_to_data.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/save_ATLAS_data_with_features_to_sqlite.R"))

## USER INPUT BEGINNING 

path_to_db <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Labeled_data_DB/Visual_Filter_DB"
path_to_data_with_features <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Feature_Engineering/Data_with_features"
path_to_species_metadata <- file.path(path_to_db, "Species_metadata.xlsx")
cleaned_feature_data_all_species_filename <- "Features_data_for_RF_all_species.sqlite"

identify_stops <- TRUE

stop_filter_rules <- list(
  # list(column = "distance_triangle_ratio", type = ">", value = 3, logic = "AND"),
  list(column = "Speed_m_s", type = "<", value = 3, logic = "AND"),
  list(column = "Speed_window_mean", type = "<", value = 2.5, logic = "AND"),
  list(column = "avg_dist_from_points_in_window", type = "<", value = 14, logic = "AND")
  # list(column = "var_x_window", type = "<", value = 56, logic = "AND"),
  # list(column = "var_y_window", type = "<", value = 124, logic = "AND"),
  # list(column = "diff_dist_mean_median", type = "<", value = 4, logic = "AND"),
  # list(column = "speed_diff_from_median", type = "<", value = 1.5, logic = "AND")
)

### USER INPUT END

# Set the names of the relevant tables to load from the sqlite data file
tables_to_load <- c("LOCALIZATIONS", 
                    "DETECTIONS", 
                    "PARTICIPATING_BASE_STATIONS", 
                    "MISSED_BASE_STATIONS")

# Load species metadata
species_metadata <- read_excel(path_to_species_metadata)

# Initialize a data frames for the combined data of all species
combined_localization_data <- data.frame()
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
  
  # Identify stops in the data
  if (identify_stops) {
    localization_data <- apply_stops_rule_to_data(localization_data, stop_filter_rules)
  }

  # Convert the Outliers column to factor type- required for the Random Forest function
  localization_data$Outliers <- factor(localization_data$Outliers, levels = c(0, 1), labels = c("valid", "outlier"))

  # Move the Outliers column to the end of the dataframe
  localization_data <- localization_data %>%
    dplyr::select(-Outliers, Outliers)
  
  # Append the prepared species data to the combined dataframe
  combined_localization_data <- bind_rows(combined_localization_data, localization_data)
  combined_detections_data <- bind_rows(combined_detections_data, detections_data)
  combined_participating_bs_data <- bind_rows(combined_participating_bs_data, participating_base_stations)
  combined_missed_bs_data <- bind_rows(combined_missed_bs_data, missed_base_stations)
  
}

save_ATLAS_data_with_features_to_sqlite(localization_data = combined_localization_data,
                                        detections_data = combined_detections_data,
                                        participating_base_stations = combined_participating_bs_data,
                                        missed_base_stations = combined_missed_bs_data,
                                        fullpath = file.path(path_to_data_with_features, cleaned_feature_data_all_species_filename))
