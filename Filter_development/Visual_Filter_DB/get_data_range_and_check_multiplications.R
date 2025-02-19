# Load required libraries
library(DBI)
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(lubridate)

# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data, not

# USER INPUT- set the species name
species_id <- "SW"

# USER INPUT- insert the name of the person who annotated the data
reviewer_name <- "Michal Handel"

# USER INPUT- adjust the database path if necessary
path_to_db <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Annotated_data_DB/Visual_Filter_DB/"

# Full path to the species data
path_to_species <- paste0(path_to_db, species_id)

# List all SQLite files in the species folder
sqlite_files <- list.files(path_to_species, pattern = "*.sqlite", full.names = TRUE)

# Extract metadata from filenames
time_ranges <- data.frame(TAG = character(), start_time = as.POSIXct(character()), end_time = as.POSIXct(character()), stringsAsFactors = FALSE)

for (file in sqlite_files) {

  # Extract tag number, start time, and end time from filename
  matches <- str_match(basename(file), paste0(species_id, "_(\\d{4})_from_(\\d{4}-\\d{2}-\\d{2}_\\d{2}-\\d{2}-\\d{2})_to_(\\d{4}-\\d{2}-\\d{2}_\\d{2}-\\d{2}-\\d{2})"))
  
  if (!is.na(matches[1])) {
    Species_ID <- species_id
    TAG <- matches[2]  # Extract last 4 digits of TAG
    Start_time <- ymd_hms(str_replace_all(matches[3], "-", ":"))  # Convert to datetime
    End_time <- ymd_hms(str_replace_all(matches[4], "-", ":"))  # Convert to datetime
    Reviewer <- reviewer_name
    
    # Add to dataframe
    time_ranges <- rbind(time_ranges, data.frame(TAG = tag, start_time = start_time, end_time = end_time, stringsAsFactors = FALSE))
  }
}

# Save metadata to CSV
csv_path <- file.path(path_to_species, paste0(species_id, "_files_metadata.csv"))
write.csv(time_ranges, csv_path, row.names = FALSE)
message(paste0("Metadata saved to:", csv_path, "\n"))

# Create the horizontal bar plot
p <- ggplot(time_ranges, aes(x = start_time, xend = end_time, y = as.factor(TAG), yend = as.factor(TAG), color = TAG)) +
  geom_segment(size = 5) +  # Use geom_segment to create bars
  scale_x_datetime(labels = date_format("%Y-%m-%d"), breaks = date_breaks("1 month")) +  # Customize X-axis to show date breaks
  labs(x = "Time", y = "Tag Number", title = paste(species_id, "data:", "Time Ranges by Tag Number")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")  # Rotate X-axis labels and remove legend if desired
print(p)

ggsave(filename = paste0(path_to_species, "/time_ranges_plot_", species_id, ".png"), 
       plot = p, width = 10, height = 6, dpi = 300, bg = "white")

# Open all the sqlite files and unite all the data in one R dataframe
source(paste0(getwd(), "/load_data_from_all_sqlite_files_in_folder.R"))
combined_data <- load_data_from_all_sqlite_files_in_folder(path_to_species)

# Convert `TIME` from milliseconds to human-readable datetime
combined_data$dateTime <- as.POSIXct(combined_data$TIME / 1000, origin = "1970-01-01", tz = "UTC")

# Check for duplicates based on both TAG and TIME- returns a few wrong duplicates whose TIME values are different in just a few seconds
duplicates_temp <- combined_data %>%
  distinct(TAG, TIME, .keep_all = TRUE)

# Verify duplicates by printing
duplicates <- duplicates_temp %>%
  group_by(TAG, TIME) %>%
  filter(n() > 1) %>%
  ungroup()

# Check if duplicates exist
if (nrow(duplicates) == 0) {
  message(paste0("There are no duplicates in the ", species_id, " annotated data."))
} else {
  print(duplicates)
  
  # Save duplicates as a CSV file
  write.csv(duplicates, paste0(path_to_species, "/duplicates.csv"), row.names = FALSE)
  
  print("Duplicates saved as duplicates.csv")
}

# # Create a new dataframe to store time range information for the combined data
# time_ranges_combined <- combined_data %>%
#   group_by(TAG) %>%
#   arrange(dateTime) %>%
#   summarise(
#     Start_time = first(dateTime),        # The first time entry for each tag
#     End_time = last(dateTime)            # The last time entry for each tag
#   ) %>%
#   ungroup()
# 
# # Extract the last 4 digits of the TAG for better clarity in the plot
# time_ranges_combined <- time_ranges_combined %>%
#   mutate(TAG = substr(TAG, nchar(TAG)-3, nchar(TAG)))

species_metadata <-  

## Save the combined data as sqlite and add the information to the metadata file
# Load existing metadata if it exists
metadata_file_path <- paste0(path_to_db, "/Annotated_data/files_metadata.csv")
if (file.exists(metadata_file_path)) {
  existing_metadata <- read.csv(metadata_file_path)
  
  # Remove existing records of the same Species_ID before adding new ones
  updated_metadata <- existing_metadata %>%
    filter(Species_ID != species_id) %>%  # Keep all except the species being updated
    bind_rows(species_metadata)  # Add the new data
  
} else {
  # Create new metadata file
  updated_metadata <- species_metadata
}

# Save updated metadata
write.csv(updated_metadata, metadata_file, row.names = FALSE)
message("Metadata file updated successfully!")