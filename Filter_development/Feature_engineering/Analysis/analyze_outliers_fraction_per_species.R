
# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data

library(readxl)
library(dplyr)

source(file.path(getwd(), "load_tables_from_sqlite_file.R"))

path_to_db <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Annotated_data_DB/Visual_Filter_DB"
path_to_data_with_features <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Feature_Engineering/Data_with_features"
path_to_species_metadata <- file.path(path_to_db, "Species_metadata.xlsx")
output_filepath <- file.path("C:/Users/netat/Documents/Movement_Ecology/Filter_development/Feature_Engineering/Features_analysis", "species_outlier_summary.csv")

# Load species metadata
species_metadata <- read_excel(path_to_species_metadata)

# Initialize an empty list to store results
summary_list <- list()

# Run on the species
for (species_id in species_metadata$Species_ID) {
  
  print(species_id)
  
  # Name of the sqlite file with the localization_data + features of the species
  sqlite_file_name <- paste0(species_id, "_features_eng.sqlite")
  
  # Load the LOCALIZATIONS and PARTICIPATING_BASE_STATIONS data tables from sqlite
  data <- load_tables_from_sqlite_file(
    sqlite_filepath = file.path(path_to_data_with_features, sqlite_file_name), 
    tables = "LOCALIZATIONS")
  
  localizations_data <- data$LOCALIZATIONS
  
  # Total before removing NAs
  num_total_points <- nrow(localizations_data)
  num_na_outliers <- sum(is.na(localizations_data$Outliers))
  
  # Remove NA in Outliers for analysis
  localizations_clean <- localizations_data[!is.na(localizations_data$Outliers), ]
  
  # Count values
  num_valid <- sum(localizations_clean$Outliers == 0)
  num_outliers <- sum(localizations_clean$Outliers == 1)
  num_uncertain <- sum(localizations_clean$Outliers == 2)
  
  # Percentages
  num_used_points <- nrow(localizations_clean)
  percentage_uncertain <- (num_uncertain / num_used_points) * 100
  valid_plus_outliers <- num_valid + num_outliers
  percentage_outliers_without_uncertain <- if (valid_plus_outliers > 0) {
    (num_outliers / valid_plus_outliers) * 100
  } else {
    NA  # Avoid division by zero
  }
  
  # Save the summary
  summary_list[[species_id]] <- data.frame(
    Species_id = species_id,
    num_total_points = num_total_points,
    num_na_outliers = num_na_outliers,
    num_valid = num_valid,
    num_outliers = num_outliers,
    num_uncertain = num_uncertain,
    percentage_uncertain = round(percentage_uncertain, 2),
    percentage_outliers_without_uncertain = round(percentage_outliers_without_uncertain, 2)
  )
  
}

# Combine into one data frame
summary_table <- do.call(rbind, summary_list)

# Calculate weighted average without modifying summary_table
weights <- summary_table$num_valid + summary_table$num_outliers
percentages <- summary_table$percentage_outliers_without_uncertain

# Filter out rows where percentage is NA or weight is 0
valid_idx <- !is.na(percentages) & weights > 0

weighted_avg_outliers <- sum(percentages[valid_idx] * weights[valid_idx]) / sum(weights[valid_idx])

# Print result
cat("Weighted average of outliers across species (excluding uncertain and NA):",
    round(weighted_avg_outliers, 2), "%\n")

# Save as CSV
write.csv(summary_table, output_filepath, row.names = FALSE)