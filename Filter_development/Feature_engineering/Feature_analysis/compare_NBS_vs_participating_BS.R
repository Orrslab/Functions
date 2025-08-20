## This script compares between the number of 
# matched detections (number of participating base stations), 
# and the NBS value in the localizations table.

# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data

library(dplyr)
library(data.table)

source(file.path(getwd(), "load_tables_from_sqlite_file.R"))

## USER INPUT BEGINNING 

path_to_db <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Annotated_data_DB/Visual_Filter_DB"
path_to_data_with_features <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Feature_Engineering/Data_with_features"
path_to_species_metadata <- file.path(path_to_db, "Combined_species_data", "metadata_per_species.csv")
output_folder <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Feature_Engineering/NBS_matched_bs_comparison"

### USER INPUT END

# Set the names of the relevant tables to load from the sqlite data file
tables_to_load <- c("LOCALIZATIONS", "PARTICIPATING_BASE_STATIONS")

# Load species metadata
species_metadata <- read.csv(path_to_species_metadata)

# Initialize tables which will be saved as files at the end of this script
all_discrepancies <- data.table()
error_summary <- data.table()

# Run on the species
for (species_id in species_metadata$Species_id) {
  
  # For debug purposes
  # species_id <- "LD"
  
  print(species_id)
  
  # Set the sqlite file name
  sqlite_file_name <- paste0(species_id, "_features_eng.sqlite")
  
  # Load the LOCALIZATIONS and PARTICIPATING_BASE_STATIONS data tables from sqlite
  data <- load_tables_from_sqlite_file(sqlite_filepath = file.path(path_to_data_with_features, sqlite_file_name), 
                                       tables = tables_to_load)
  
  localization_data <- data$LOCALIZATIONS
  participating_bs <- data$PARTICIPATING_BASE_STATIONS

  # Convert the data frames to data.table
  loc_dt <- as.data.table(localization_data)
  pbs_dt <- as.data.table(participating_bs)

  # Count the number of participating base stations per (TAG, TIME)
  pbs_counts <- pbs_dt[, .(num_participating_bs = .N), by = .(TAG, TIME)]

  # Merge with localization_data to compare
  merged_dt <- merge(loc_dt, pbs_counts, by = c("TAG", "TIME"), all.x = TRUE)

  # Replace NA in num_participating_bs with 0 (in case no matches were found)
  merged_dt[is.na(num_participating_bs), num_participating_bs := 0]

  # Compute the difference between the number of participating base stations and the reported NBS
  merged_dt[, diff := num_participating_bs - NBS]
  
  # Summarize discrepancies by diff value
  diff_summary <- merged_dt[, .N, by = diff]
  diff_summary[, species_id := species_id]
  all_discrepancies <- rbind(all_discrepancies, diff_summary, fill = TRUE)
  
  # Calculate error: 2 * sum of negative diffs / total number of points
  total_n <- nrow(merged_dt)
  sum_negative_diffs <- sum(abs(merged_dt[diff < 0, diff]))
  estimated_error <- (2 * sum_negative_diffs / total_n) * 100
  
  error_summary <- rbind(error_summary, data.table(species_id = species_id,
                                                   total_points = total_n,
                                                   sum_negative_diff = sum_negative_diffs,
                                                   estimated_error_percent = estimated_error))
  
  
  # num_exact_matches <- nrow(merged_dt[diff == 0])
  # 
  # # Check if all are zero (i.e., matches perfectly)
  # if (all(merged_dt$diff == 0)) {
  #   message("✅ All localizations have matching participating base station counts.")
  # } else {
  #   message("❌ Some localizations have mismatched participating base station counts.")
  # 
  #   # Summarize discrepancies
  #   discrepancy_summary <- merged_dt[diff != 0, .N, by = diff][order(diff)]
  #   print(discrepancy_summary)
  # 
  #   # # Optional: Show a few examples of mismatches
  #   # cat("\nExamples of mismatches:\n")
  #   # print(merged_dt[diff != 0][1:10])
  # }
  
}

# Save CSVs
fwrite(all_discrepancies, file.path(output_folder, "per_species_diff_summary.csv"))
fwrite(error_summary, file.path(output_folder, "per_species_error_estimate.csv"))
