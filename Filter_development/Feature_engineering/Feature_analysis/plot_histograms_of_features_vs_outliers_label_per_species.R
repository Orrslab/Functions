# Plot histograms of features and Outliers label

# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data

library(dplyr)
library(ggplot2)

source(file.path(getwd(), "load_tables_from_sqlite_file.R"))

###   USER'S INPUT BEGIN

path_to_data_with_features <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Feature_Engineering/Data_with_features"
cleaned_feature_data_all_species_filename <- "Features_data_for_RF_all_species.sqlite"
path_to_save_histograms <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Feature_Engineering/Features_analysis/Features_histograms_outliers_vs_valid"

# Define the columns that are not considered as features and should be excluded from the training set
non_feature_column_names <- c("TAG", "X", "Y", "Z", "lat", "lon",
                              "TIME", "dateTime", "DAY", "geometry",
                              "Species_id", "X_mean", "Y_mean", "X_median", "Y_median", "Is_stop")

### USER'S INPUT END

# Load the features data from sqlite
features_data <- load_tables_from_sqlite_file(
  sqlite_filepath = file.path(path_to_data_with_features, cleaned_feature_data_all_species_filename),
  tables = "LOCALIZATIONS")

localization_data <- features_data$LOCALIZATIONS

# Get unique species
species_list <- unique(localization_data$Species_id)

# Loop over species
for (species in species_list) {
  
  # Subset data for current species
  species_data <- localization_data %>% filter(Species_id == species)
  
  # Remove the non-feature columns again (in case)
  species_data <- species_data %>% dplyr::select(-all_of(non_feature_column_names))
  
  # Ensure Outliers is a factor
  species_data$Outliers <- factor(species_data$Outliers, levels = c("valid", "outlier"))
  
  # Get the feature columns
  feature_columns <- setdiff(names(species_data), "Outliers")
  
  # Create subfolder for this species
  species_folder <- file.path(path_to_save_histograms, species)
  if (!dir.exists(species_folder)) dir.create(species_folder, recursive = TRUE)
  
  # Loop through features for this species
  for (feature_name in feature_columns) {
    
    cat("Processing:", species, "-", feature_name, "\n")
    
    if (is.numeric(species_data[[feature_name]])) {
      p <- ggplot(species_data, aes(x = .data[[feature_name]], fill = Outliers)) +
        geom_histogram(position = "identity", alpha = 0.6, bins = 30) +
        scale_fill_manual(values = c("valid" = "green", "outlier" = "red")) +
        theme_minimal() +
        labs(title = paste("Histogram of", feature_name, "for", species),
             x = feature_name,
             y = "Count") +
        theme(plot.title = element_text(hjust = 0.5))
      
      # Save the histogram to species-specific folder
      ggsave(filename = file.path(species_folder, paste0(feature_name, ".png")),
             plot = p, width = 8, height = 6, bg = "white")
    }
  }
}