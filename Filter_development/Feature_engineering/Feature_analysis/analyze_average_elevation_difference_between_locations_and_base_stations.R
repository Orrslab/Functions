# Analyze the missed base stations, which were closer than the participating ones

# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data

library(ggplot2)
library(data.table)

source(file.path(getwd(), "Filter_development/Feature_engineering/load_data_with_features.R"))

# USER'S INPUT BEGIN

analysis_folder <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Feature_Engineering/Features_analysis/Location_base_stations_elevation_difference"

# tables_to_load <- c("LOCALIZATIONS", 
#                     "DETECTIONS", 
#                     "PARTICIPATING_BASE_STATIONS", 
#                     "MISSED_BASE_STATIONS", 
#                     "PROPERTIES")

tables_to_load <- c("LOCALIZATIONS")

# USER'S INPUT END

# Load the features data
features_data <- load_data_with_features(tables_to_load)

localization_data <- features_data$LOCALIZATIONS

# Ensure data is a data.table
localization_data <- as.data.table(localization_data)

# Ensure Outliers is a factor
localization_data[, Outliers := as.factor(Outliers)]

# Loop over each species and create a faceted histogram
species_list <- unique(localization_data$Species_id)

for (species in species_list) {
  species_data <- localization_data[Species_id == species & !is.na(avg_abs_elevation_diff)]
  
  if (nrow(species_data) == 0) next  # Skip empty subsets
  
  p <- ggplot(species_data, aes(x = avg_abs_elevation_diff, fill = Outliers)) +
    geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
    facet_wrap(~Outliers, ncol = 1) +
    theme_minimal() +
    labs(
      title = paste("Elevation Difference Histogram for Species", species),
      x = "Average Absolute Elevation Difference (m)",
      y = "Count"
    ) +
    scale_fill_manual(values = c("valid" = "#2ca02c", "outlier" = "#d62728")) +
    theme(legend.position = "none")
  
  # Save plot
  file_name <- paste0(analysis_folder, "/elevation_diff_histogram_species_", species, ".png")
  ggsave(file_name, plot = p, width = 8, height = 6, bg = "white")
}