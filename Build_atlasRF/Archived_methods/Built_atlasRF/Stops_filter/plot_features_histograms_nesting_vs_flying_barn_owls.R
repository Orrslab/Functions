# This script creates facet histograms which compare the feature of nesting versus flying barn owls

# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data

library(dplyr)
library(ggplot2)

source(file.path(getwd(), "load_tables_from_sqlite_file.R"))

## USER INPUT BEGINNING 

path_to_db <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Stops_Filter/Stops_DB/Nesting_Barn_Owls"
path_to_data_with_features <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Stops_Filter/Stops_analysis"
path_to_data_with_features_nesting_BO <- file.path(path_to_data_with_features, "Nesting_barn_owls/Prepared_data_with_features_nesting_barn_owls.sqlite")
path_to_data_with_features_flying_BO <- file.path(path_to_data_with_features, "Flying_barn_owls/Prepared_data_with_features_flying_barn_owls.sqlite")

non_feature_column_names <- c("TAG", "X", "Y", "Z", "lat", "lon",
                              "TIME", "dateTime", "DAY", "Is_stop")

### USER INPUT END

# Set the names of the relevant tables to load from the sqlite data file
tables_to_load <- c("LOCALIZATIONS")

# Load the nesting barn owls data
BO_data_nesting <- load_tables_from_sqlite_file(
  sqlite_filepath = path_to_data_with_features_nesting_BO, 
  tables = tables_to_load)

# Get the localization_data of the nesting barn owls
localization_data_nesting <- BO_data_nesting$LOCALIZATIONS

# Filter out the non-stopping points
localization_data_nesting <- localization_data_nesting %>% 
  filter(Is_stop != 0)

# Load the flying barn owls data
BO_data_flying <- load_tables_from_sqlite_file(
  sqlite_filepath = path_to_data_with_features_flying_BO, 
  tables = tables_to_load)

# Get the localization_data of the flying barn owls
localization_data_flying <- BO_data_flying$LOCALIZATIONS

# Filter out the stopping points
localization_data_flying <- localization_data_flying %>% 
  filter(Is_stop != 1)

# Remove non-features columns
localization_data_nesting <- localization_data_nesting %>%
  dplyr::select(-all_of(non_feature_column_names))

localization_data_flying <- localization_data_flying %>%
  dplyr::select(-all_of(non_feature_column_names))

# Create output folder if it doesn't exist
output_folder <- file.path(path_to_data_with_features, "Nesting_vs_flying_histograms")
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# Get common feature columns
features <- intersect(colnames(localization_data_nesting), colnames(localization_data_flying))

# Filter numeric columns only
features <- features[sapply(localization_data_nesting[features], is.numeric)]

# Loop through each feature and create histogram
for (feature in features) {
  
  # Get the feature's data
  nesting_values <- localization_data_nesting[[feature]]
  flying_values <- localization_data_flying[[feature]]
  
  # Remove NA values from the feature's data
  nesting_values <- nesting_values[!is.na(nesting_values)]
  flying_values <- flying_values[!is.na(flying_values)]
  
  # Combine data into one dataframe for ggplot
  combined_data <- data.frame(
    value = c(nesting_values, flying_values),
    state = factor(c(rep("Nesting", length(nesting_values)), 
                     rep("Flying", length(flying_values))))
  )
  
  # Create plot
  p <- ggplot(combined_data, aes(x = value, fill = state)) +
    geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
    scale_fill_manual(values = c("Nesting" = "red", "Flying" = "green")) +
    labs(
      x = feature,
      y = "Count",
      title = paste(feature)
    ) +
    theme_minimal()
  
  # Save plot
  file_name <- paste0("histogram_", feature, ".png")
  file_path <- file.path(output_folder, file_name)
  ggsave(file_path, plot = p, width = 8, height = 5, bg = "white")
}