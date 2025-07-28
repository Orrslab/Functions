# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data

library(readxl)

### USER INPUT BEGIN
# species_id <- "BS"

path_to_species_metadata <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Annotated_data_DB/Visual_Filter_DB/Species_metadata.xlsx"
path_to_data <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Annotated_data_DB/Visual_Filter_DB/Annotated_data"
path_to_save_results <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Feature_Engineering"

### USER INPUT END

# Load the features' info
load(file.path(getwd(), "Filter_development", "Feature_engineering", "features_info.RData"))

# Load species metadata
species_metadata <- read_excel(path_to_species_metadata)

for (i in 1:nrow(species_metadata)) {
  
  species_id <- species_metadata$Species_ID[i]
  species_name <- species_metadata$Species_name[i]
  
  message(paste("Features evaluation for", species_id, "-", species_name))
  
  # Set path to save the results of the species
  path_to_save_results_species <- file.path(path_to_save_results, species_id)
  
  # Create the results path if it does not yet exist
  if (!dir.exists(path_to_save_results_species)) {
    # If it doesn't exist, create it
    dir.create(path_to_save_results_species, recursive = TRUE)  # recursive = TRUE ensures that all parent directories are created as needed
  }
  
  ### Load the locations and detections data from the species file
  
  # Set the file name and path
  species_data_file_name <- paste0(species_id, "_localizations_annotated.sqlite")
  species_data_path <- file.path(path_to_data, species_data_file_name)
  
  # Load the labeled data
  # TODO Add DETECITIONS to the combined data
  # source(file.path(getwd(), "load_atlas_data_from_sqlite.R"))
  # species_data <- load_atlas_data_from_sqlite(species_data_path)
  source(file.path(getwd(), "load_localization_data_from_sqlite_file.R"))
  species_data <- load_localization_data_from_sqlite_file(species_data_path)
  
  ### Feature Engineering
  source(file.path(getwd(), "atlas_metrics.R"))
  
  # Cosine of the turning angle
  species_data$cos_turning_angle <- calculate_cosine_turning_angle(X_column = species_data$X,
                                                                   Y_column = species_data$Y)
  
  # ### Save into sqlite the labeled data with all the added features
  # source(paste0(path_to_functions, "save_ATLAS_data_to_sqlite.R"))
  # fullpath <- paste0(data_folder, "/labeled_data_with_features.sqlite")
  # save_ATLAS_data_to_sqlite(localization_data=location_data_labeled, 
  #                           fullpath = fullpath)
  
  ### Evaluate outliers proportion
  outliers_proportion_table <- table(species_data$Outliers)
  filename_outliers_proportion <- "outliers_proportion.csv"
  write.csv(outliers_proportion_table, file = file.path(path_to_save_results_species, filename_outliers_proportion), row.names = FALSE)
  
  ### Data Statistics and Visualization
  source(file.path(getwd(), "Filter_development", "Feature_engineering", "plot_exploratory_analysis.R"))
  for (feature_info in features_info) {
    print(feature_info$feature_name)
    plot_exploratory_analysis(species_data = species_data,
                              feature_column_name = feature_info$feature_column_name,
                              feature_name = feature_info$feature_name,
                              feature_units = feature_info$feature_units)
  }
  
}

