# This script creates facet histograms which compare the feature of nesting versus flying barn owls

# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data

library(dplyr)
library(ggplot2)

source(file.path(getwd(), "load_tables_from_sqlite_file.R"))

## USER INPUT BEGINNING 

path_to_db <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Stops_Filter/Stops_DB"
path_to_data_with_features <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Stops_Filter/Stops_analysis/Monivg_vs_stopping_barn_owls"
features_data_filename <- "Prepared_data_with_features_BO.sqlite"
non_feature_column_names <- c("TAG", "X", "Y", "Z", "lat", "lon",
                              "TIME", "dateTime", "DAY", "Species_id", "Is_stop")
output_folder <- file.path(path_to_data_with_features, "Feature_histograms")

### USER INPUT END

# Set the names of the relevant tables to load from the sqlite data file
tables_to_load <- c("LOCALIZATIONS")

# Load the nesting barn owls data
BO_data <- load_tables_from_sqlite_file(
  sqlite_filepath = file.path(path_to_data_with_features, features_data_filename), 
  tables = tables_to_load)

# Get the localization_data of the nesting barn owls
localization_data <- BO_data$LOCALIZATIONS

# Filter out the Uncertain points
localization_data <- localization_data %>% 
  filter(Is_stop != 2)

# Remove non-feature columns
feature_data <- localization_data %>%
  dplyr::select(-all_of(non_feature_column_names))

# Add back Is_stop for filtering
feature_data$Is_stop <- localization_data$Is_stop

# Split into moving and stopping
stopping_BO_data <- feature_data %>% filter(Is_stop == 1)
moving_BO_data <- feature_data %>% filter(Is_stop == 0)

# Get feature names (all columns except Is_stop)
feature_names <- setdiff(colnames(feature_data), "Is_stop")

# Prepare list to store percentiles
percentile_summary <- list()

# Loop through each feature and create histograms
for (feature in feature_names) {
  
  print(feature)
  
  # Get feature values 
  stop_vals <- stopping_BO_data[[feature]]
  move_vals <- moving_BO_data[[feature]]
  
  # Remove NA values
  stop_vals <- stop_vals[!is.na(stop_vals)]
  move_vals <- move_vals[!is.na(move_vals)]
  
  # Skip if both are empty
  if (length(stop_vals) == 0 & length(move_vals) == 0) next
  
  # Combine into one data frame
  combined_data <- data.frame(
    value = c(stop_vals, move_vals),
    state = factor(c(rep("Stopping", length(stop_vals)), rep("Moving", length(move_vals))))
  )
  
  # Calculate percentiles for stopping data
  p90 <- quantile(stop_vals, 0.90, na.rm = TRUE)
  p95 <- quantile(stop_vals, 0.95, na.rm = TRUE)
  
  # Store percentiles in summary list
  percentile_summary[[feature]] <- data.frame(
    Feature_name = feature,
    Percentile_90 = p90,
    Percentile_95 = p95
  )
  
  # Create histogram
  p <- ggplot(combined_data, aes(x = value, fill = state)) +
    geom_histogram(position = "identity", alpha = 0.5, bins = 50) +
    geom_vline(xintercept = p90, linetype = "dashed", color = "blue", linewidth = 0.8) +
    geom_vline(xintercept = p95, linetype = "dotted", color = "blue", linewidth = 0.8) +
    scale_fill_manual(values = c("Stopping" = "red", "Moving" = "green")) +
    theme_minimal() +
    labs(
      x = feature,
      y = "Count",
      title = paste("Nesting versus flying -", feature),
      subtitle = "Blue dashed = 80th percentile | Blue dotted = 90th percentile (Stopping only)"
    )
  
  # Save plot
  histogram_name <- paste0("histogram_", feature, ".png")
  ggsave(filename = file.path(output_folder, histogram_name), plot = p, width = 8, height = 5, bg = "white")
}

# Combine all rows and save as CSV
percentile_table <- do.call(rbind, percentile_summary)

# Save CSV
percentile_csv_path <- file.path(output_folder, "stopping_features_percentiles.csv")
write.csv(percentile_table, percentile_csv_path, row.names = FALSE)