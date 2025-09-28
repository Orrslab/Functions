# Generate tables of the number of participating base stations 
# that had low detections of all the beacons, per hour

# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data

library(data.table)
library(officer)
library(flextable)

source(file.path(getwd(), "Build_atlasRF/Feature_engineering/load_data_with_features.R"))

# USER'S INPUT BEGIN

analysis_folder <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Feature_Engineering/Features_analysis/Beacons_data_analysis"

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

# Count by num_bs_with_all_low_beacons and Outliers
count_by_num_bs <- localization_data[
  , .N, by = .(num_bs_with_all_low_beacons, Outliers)
][order(num_bs_with_all_low_beacons, Outliers)]

# Print the result
cat("Counts by num_bs_with_all_low_beacons and Outliers:\n")
print(count_by_num_bs)

# Count by frac_bs_with_all_low_beacons and Outliers
count_by_frac_bs <- localization_data[
  , .N, by = .(frac_bs_with_all_low_beacons, Outliers)
][order(frac_bs_with_all_low_beacons, Outliers)]

# Print the result
cat("\nCounts by frac_bs_with_all_low_beacons and Outliers:\n")
print(count_by_frac_bs)

# Create a new Word document
doc <- read_docx()

# Convert the count tables to flextable
ft_num_bs <- flextable(count_by_num_bs)
ft_frac_bs <- flextable(count_by_frac_bs)

# Add title and table for num_bs_with_all_low_beacons
doc <- doc %>%
  body_add_par("Table: Counts by num_bs_with_all_low_beacons and Outliers", style = "heading 1") %>%
  body_add_flextable(ft_num_bs) %>%
  body_add_par("", style = "Normal") # space

# Add title and table for frac_bs_with_all_low_beacons
doc <- doc %>%
  body_add_par("Table: Counts by frac_bs_with_all_low_beacons and Outliers", style = "heading 1") %>%
  body_add_flextable(ft_frac_bs)

# Save the Word document
print(doc, target = file.path(analysis_folder, "Beacon_summary_num_low_detections.docx"))

### Feature analysis per species

# Create a new Word document
doc <- read_docx()

# Get list of species
species_list <- unique(localization_data$Species_id)

for (species in species_list) {
  # Filter the data for the current species
  species_data <- localization_data[Species_id == species]
  
  # Skip if there's no data
  if (nrow(species_data) == 0) next
  
  # Count of localizations per number of BS with all low beacons and outlier status
  summary_table <- species_data[
    , .N,
    by = .(num_bs_with_all_low_beacons, Outliers)
  ][order(num_bs_with_all_low_beacons, Outliers)]
  
  # Add a title to the Word document
  doc <- doc %>%
    body_add_par(value = paste("Species:", species), style = "heading 2") %>%
    body_add_flextable(flextable(summary_table)) %>%
    body_add_par("", style = "Normal")  # Add space
}

# Save the Word document
print(doc, target = file.path(analysis_folder, "Low_Beacons_Per_Species.docx"))
