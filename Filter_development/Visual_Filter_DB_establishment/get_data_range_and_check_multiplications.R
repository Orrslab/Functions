
# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data, not

# Load required libraries
library(DBI)
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(lubridate)

source(paste0(getwd(), "/Filter_development/Visual_Filter_DB_establishment/get_metadata_from_all_sqlite_files_in_folder.R"))
source(paste0(getwd(), "/load_localizations_data_from_all_sqlite_files_in_folder.R"))
source(file.path(getwd(), "load_atlas_data_from_multiple_sqlite_files.R"))
source(paste0(getwd(), "/save_ATLAS_data_to_sqlite.R"))

#######################################################################
# USER INPUT- set the species name
species_id <- "WB"

# USER INPUT- insert the name of the person who annotated the data
# reviewer_name <- "Shlomo Cain"
reviewer_name <- "Michal Handel"
# reviewer_name <- "Jehuda Samuel"

data_source <- "ATLAS system Harod"

filter_applied <- "Visual Filter"

# USER INPUT- adjust the database path if necessary
path_to_db <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Annotated_data_DB/Visual_Filter_DB/"

# Time Ranges bar plot resolution
# plot_resolution <- "1 hour"
# plot_resolution <- "1 week"
plot_resolution <- "1 month"

# END OF USER INPUT

# Full path to the species data
path_to_species <- paste0(path_to_db, species_id)

#######################################################################

### Get the metadata from the sqlite files ###
files_metadata <- get_metadata_from_all_sqlite_files_in_folder(path_to_species)

# Add the Species ID, Reviewer and data source
files_metadata <- files_metadata %>%
  mutate(
    Species_ID = species_id,    # Assuming species_id is a single value for all rows
    Reviewer = reviewer_name,         # Assuming Reviewer is a single value for all rows
    Data_source = data_source,
    Filter_applied = filter_applied
  )

# Re-order the column names
files_metadata <- files_metadata %>%
  select(Species_ID, TAG, Start_time, End_time, Num_records, Data_source, Reviewer, Filter_applied)

# Save the files' metadata to CSV
csv_path <- file.path(path_to_species, paste0(species_id, "_files_metadata.csv"))
write.csv(files_metadata, csv_path, row.names = FALSE)
message(paste0("Metadata of the ", species_id, " files saved to:", csv_path, "\n"))


### Bar Plot of the time ranges ###
# Create the horizontal bar plot of the time range in each sqlite file
p <- ggplot(files_metadata, aes(x = Start_time, xend = End_time, y = as.factor(TAG), yend = as.factor(TAG), color = substr(TAG, nchar(TAG) - 3, nchar(TAG)))) +
  geom_segment(size = 5) +  # Use geom_segment to create bars
  scale_x_datetime(labels = date_format("%Y-%m-%d"), breaks = date_breaks(plot_resolution)) +  # Customize X-axis to show date breaks
  labs(x = "Time", y = "Tag Number", title = paste(species_id, "data:", "Time Ranges by Tag Number")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")  # Rotate X-axis labels and remove legend if desired
print(p)

ggsave(filename = paste0(path_to_species, "/time_ranges_plot_", species_id, ".png"),
       plot = p, width = 10, height = 6, dpi = 300, bg = "white")

### Combine all the location data of the species

# Get all SQLite files in the folder
sqlite_files_in_species_folder <- list.files(path_to_species, pattern = "\\.sqlite$", full.names = TRUE)

# Open all the sqlite files and unite all the data in one R dataframe
# combined_data <- load_localizations_data_from_all_sqlite_files_in_folder(path_to_species)
combined_data <- load_atlas_data_from_multiple_sqlite_files(sqlite_files_in_species_folder)

localization_data <- combined_data$LOCALIZATIONS
detection_data <- combined_data$DETECTIONS

# Convert `TIME` from milliseconds to human-readable datetime
localization_data$dateTime <- as.POSIXct(localization_data$TIME / 1000, origin = "1970-01-01", tz = "UTC")

# Save the combined data as sqlite
file_name_species <- paste0(species_id, "_labeled_data.sqlite")
sqlite_filepath <- paste0(path_to_db, "/Combined_species_data/", file_name_species)

save_ATLAS_data_to_sqlite(localizations_data = localization_data,
                          detections_data = detection_data,
                          fullpath = sqlite_filepath)

## Check for duplicates ##

# Check for duplicates based on both TAG and TIME- returns a few wrong duplicates whose TIME values are different in just a few seconds
duplicates <- localization_data %>%
  group_by(TAG, TIME) %>%
  filter(n() > 1) %>%
  ungroup()

# Print message depending on whether duplicates were found
if (nrow(duplicates) > 0) {
  cat("Number of duplicated (TAG, TIME) combinations:", nrow(duplicates), "\n")
  print(duplicates)
} else {
  cat("There are no duplicates in the data.\n")
}

# Clean the duplicates, while prioritizing leaving the Outliers = 1
localization_data <- localization_data %>%
  arrange(TAG, TIME, desc(Outliers == 1), desc(Outliers == 2)) %>%
  distinct(TAG, TIME, .keep_all = TRUE)

### Add the metadata of the combined species file to the metatada of the other files

# Create a dataframe with the metadata of the file
metadata_per_tag <- localization_data %>%
  group_by(TAG) %>%
  arrange(dateTime) %>%
  summarise(
    Start_time = first(dateTime),         # The first time entry for each tag
    End_time = last(dateTime),            # The last time entry for each tag
    Num_records = n()                     # The count of rows for each TAG
  ) %>%
  ungroup()

# Add the Species ID, Reviewer and data source
metadata_per_tag <- metadata_per_tag %>%
  mutate(
    Species_ID = species_id,    # Assuming species_id is a single value for all rows
    Reviewer = reviewer_name,         # Assuming Reviewer is a single value for all rows
    Data_source = data_source,
    Filter_applied = filter_applied
  )

# Re-order the column names
metadata_per_tag <- metadata_per_tag %>%
  select(Species_ID, TAG, Start_time, End_time, Num_records, Data_source, Reviewer, Filter_applied)

# If the metadata file exists add the current metadata to the file and replace the relevant row if exists
metadata_file_path <- paste0(path_to_db, "/Combined_species_data/metadata_per_tag.csv")
if (file.exists(metadata_file_path)) {
  existing_metadata_per_tag <- read.csv(metadata_file_path)
  
  # Ensure datetime columns have the same format as new_species_metadata (POSIXct)
  existing_metadata_per_tag <- existing_metadata_per_tag %>%
    mutate(
      Start_time = as.POSIXct(Start_time, tz = "UTC"),
      End_time = as.POSIXct(End_time, tz = "UTC")
    )

  # Remove existing records of the same Species_ID before adding new ones
  updated_metadata_per_tag <- existing_metadata_per_tag %>%
    filter(Species_ID != species_id) %>%  # Keep all except the species being updated
    bind_rows(metadata_per_tag)  # Add the new data

} else {
  # Create new metadata file and insert the current metadata
  updated_metadata_per_tag <- metadata_per_tag
}

# Save updated metadata
write.csv(updated_metadata_per_tag, metadata_file_path, row.names = FALSE)
message("Metadata per Tag file updated successfully!")