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

# Remove the non-feature columns from the localizations data
localization_data <- localization_data %>%
  dplyr::select(-all_of(non_feature_column_names))

# Ensure Outliers is a factor (optional, for consistent coloring)
localization_data$Outliers <- factor(localization_data$Outliers, levels = c("valid", "outlier"))

# Get the feature columns (excluding the Outliers column)
feature_columns <- setdiff(names(localization_data), "Outliers")

# Loop through each feature and create the histogram
for (feature_name in feature_columns) {
  
  print(feature_name)
  
  # Only plot numeric columns
  if (is.numeric(localization_data[[feature_name]])) {
    
    # Create histogram with ggplot
    p <- ggplot(localization_data, aes(x = .data[[feature_name]], fill = Outliers)) +
      geom_histogram(position = "identity", alpha = 0.6, bins = 30) +
      scale_fill_manual(values = c("valid" = "green", "outlier" = "red")) +
      theme_minimal() +
      labs(title = paste("Histogram of", feature_name),
           x = feature_name,
           y = "Count") +
      theme(plot.title = element_text(hjust = 0.5))
    
    # Save plot
    ggsave(filename = file.path(path_to_save_histograms, paste0(feature_name, ".png")),
           plot = p,
           width = 8, height = 6, bg = "white")
  }
}