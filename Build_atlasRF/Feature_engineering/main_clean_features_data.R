
library(dplyr)

source(file.path(getwd(), "load_tables_from_sqlite_file.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/save_ATLAS_data_with_features_to_sqlite.R"))

#' Clean and Prepare the Features' Data for Random Forest and Feature Analyses
#'
#' This function loads localization and detection data (with pre-computed features) 
#' from SQLite files for all species, cleans and formats them to be compatible with 
#' Random Forest classification code, and saves a combined dataset back into an SQLite file. 
#' It also prepares the data for feature analyses such as AUC calculation.
#'
#' @param config A list containing configuration parameters and paths:
#'   \describe{
#'     \item{paths$species_metadata_file_path}{Path to the CSV file containing species metadata, including \code{Species_id}.}
#'     \item{paths$folder_of_feature_results}{Path to the folder containing species-specific SQLite feature files.}
#'     \item{paths$filename_cleaned_feature_data_all_species}{Filename for the combined cleaned dataset to be saved.}
#'   }
#'
#' @details
#' For each species:
#' \itemize{
#'   \item Loads localization, detection, participating base stations, and missed base stations tables from the species' SQLite file.
#'   \item Handles structural NA values by replacing them with zeros and creating indicator columns.
#'   \item Removes rows with any remaining NA values.
#'   \item Removes rows with \code{Outliers == 2} ("Uncertain") or \code{Outliers == NA}.
#'   \item Converts the \code{Outliers} column into a factor with levels "valid" (0) and "outlier" (1).
#'   \item Reorders columns so that \code{Outliers} is the last column.
#'   \item Appends cleaned data to the combined dataset across all species.
#' }
#'
#' Finally, the combined cleaned data (localizations, detections, participating and missed base stations) 
#' is saved to a new SQLite file.
#'
#' @return None. The function is called for its side effect of saving a cleaned SQLite file.
#'
#' @examples
#' \dontrun{
#' config <- list(
#'   paths = list(
#'     species_metadata_file_path = "path/to/species_metadata.csv",
#'     folder_of_feature_results = "path/to/feature_results/",
#'     filename_cleaned_feature_data_all_species = "all_species_cleaned.sqlite"
#'   )
#' )
#'
#' main_clean_features_data(config)
#' }
#'
#' @seealso \code{\link{load_tables_from_sqlite_file}}, 
#'          \code{\link{save_ATLAS_data_with_features_to_sqlite}}
#'
main_clean_features_data <- function(config) {
  
  message("### STARTED THE MAIN SCRIPT OF CLEANING THE FEATURES' DATA. ###")
  
  # Set the names of the relevant tables to load from the sqlite data file
  tables_to_load <- c("LOCALIZATIONS", 
                      "DETECTIONS", 
                      "PARTICIPATING_BASE_STATIONS", 
                      "MISSED_BASE_STATIONS")
  
  # Load species metadata
  species_metadata <- read.csv(config$paths$species_metadata_file_path)
  
  # Initialize a data frames for the combined data of all species
  combined_localization_data <- data.frame()
  combined_detections_data <- data.frame()
  combined_participating_bs_data <- data.frame()
  combined_missed_bs_data <- data.frame()
  
  # Run on the species
  for (species_id in species_metadata$Species_id) {
    
    # For debug purposes
    # species_id <- "LD"
    
    print(species_id)
    
    # Name of the sqlite file with the localization_data + features of the species
    sqlite_file_name <- paste0(species_id, "_features_eng.sqlite")
    
    ## Load the features' data tables from sqlite
    data <- load_tables_from_sqlite_file(
      sqlite_filepath = file.path(config$paths$folder_of_feature_results, sqlite_file_name), 
      tables = tables_to_load)
    
    localization_data <- data$LOCALIZATIONS
    detections_data <- data$DETECTIONS
    participating_base_stations <- data$PARTICIPATING_BASE_STATIONS
    missed_base_stations <- data$MISSED_BASE_STATIONS
    
    ######### Add Nesting Barn Owls data- Optional ###############
    
    # if (species_id == "BO") {
    #   
    #   # Add the labeled data of the nesting barn owls
    #   nesting_BO_data <- readRDS("C:/Users/netat/Documents/Movement_Ecology/Filter_development/Outliers_characterization/Nesting_barn_owls/Nesting_BO_DB/Nesting_BO_labeled_data/BO_0856_from_2024-03-31_00-01-00_to_2024-04-01_23-59-00_labeled.rds")
    #   
    #   # Add empty columns dateTime and DAY to nesting_BO data to match the structure of localization_data
    #   nesting_BO_data$dateTime <- 0
    #   nesting_BO_data$DAY <- 0
    #   
    #   # Reorder nesting_box_data to match localization_data column order
    #   nesting_BO_data <- nesting_BO_data[, names(localization_data)]
    #   
    #   # Drop the geometry column before binding of the dataframes due to format problems
    #   localization_data$geometry <- NULL
    #   nesting_BO_data$geometry <- NULL
    #   
    #   # Combine the two data frames
    #   localization_data <- rbind(localization_data, nesting_BO_data)
    #   
    # }
    
    #############
    
    ##  Handle columns with structural NA values- random forest does not accept NA values
    
    # "closest_missed_bs_distance"
    localization_data$closest_missed_bs_distance_is_na <- localization_data$num_missed_bs == 0
    localization_data$closest_missed_bs_distance[is.na(localization_data$closest_missed_bs_distance)] <- 0
    
    # "mean_missed_bs_distance"
    localization_data$mean_missed_bs_distance_is_na <- localization_data$num_missed_bs < 2
    localization_data$mean_missed_bs_distance[localization_data$mean_missed_bs_distance_is_na] <- 0
    
    ## Detlete rows with NA values
    
    # Calculate percentage of deleted rows
    na_percentage <- mean(rowSums(is.na(localization_data)) > 0) * 100
    
    print(paste("Percentage of rows with NA values:", na_percentage))
    
    # Delete rows which have any NA values- the Random Forest function can't handle them
    localization_data <- na.omit(localization_data)
    
    ## Delete all rows with Outliers == 2 ("Uncertain") or Outliers == NA
    
    localization_data <- localization_data %>%
      filter(!(Outliers == 2 | is.na(Outliers)))
    
    ## Convert the Outliers column to factor type- required for the Random Forest function
    localization_data$Outliers <- factor(localization_data$Outliers, levels = c(0, 1), labels = c("valid", "outlier"))
    
    ## Move the Outliers column to the end of the dataframe
    localization_data <- localization_data %>%
      dplyr::select(-Outliers, Outliers)
    
    ## Append the prepared species data to the combined dataframe
    combined_localization_data <- bind_rows(combined_localization_data, localization_data)
    combined_detections_data <- bind_rows(combined_detections_data, detections_data)
    combined_participating_bs_data <- bind_rows(combined_participating_bs_data, participating_base_stations)
    combined_missed_bs_data <- bind_rows(combined_missed_bs_data, missed_base_stations)
    
  }
  
  # Save the cleaned features' data
  save_ATLAS_data_with_features_to_sqlite(localization_data = combined_localization_data,
                                          detections_data = combined_detections_data,
                                          participating_base_stations = combined_participating_bs_data,
                                          missed_base_stations = combined_missed_bs_data,
                                          fullpath = file.path(config$paths$folder_of_feature_results, config$paths$filename_cleaned_feature_data_all_species))
  
}

