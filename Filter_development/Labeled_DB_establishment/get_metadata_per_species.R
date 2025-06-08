# Get the metadata per species from the metadata_per_tag.csv file

library(dplyr)

# Open the metadata per tag file
metadata_file_path <- paste0(path_to_db, "/Combined_species_data/metadata_per_tag.csv")
if (file.exists(metadata_file_path)) {
  metadata_per_tag <- read.csv(metadata_file_path)
  
  # Convert the Start and End times to POSIXct format
  metadata_per_tag <- metadata_per_tag %>%
    mutate(
      Start_time = as.POSIXct(Start_time, tz = "UTC"),
      End_time = as.POSIXct(End_time, tz = "UTC")
    )
  
  # Summarize metadata per species
  metadata_per_species <- metadata_per_tag %>%
    group_by(Species_ID) %>%
    summarise(
      Num_tags = n_distinct(TAG),
      Start_time = min(Start_time, na.rm = TRUE),
      End_time = max(End_time, na.rm = TRUE),
      Num_records = sum(Num_records, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Save the dataframe as metadata_per_species.csv
  output_file_path <- paste0(path_to_db, "/Combined_species_data/metadata_per_species.csv")
  write.csv(metadata_per_species, output_file_path, row.names = FALSE)
  
  message("Metadata per species saved to: ", output_file_path)
  
} else {
  stop(paste("Error: File", metadata_file_path, "does not exist."))
}


