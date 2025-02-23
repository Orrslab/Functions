# Helper function to save the annotated data
save_filtered_data <- function(tag_number, 
                               start_time, 
                               end_time, 
                               filtered_data_path, 
                               segment_location_data,
                               segment_detection_data,
                               save_as_csv = FALSE) {
  
  # Create the file name and full path to save the filtered data
  full_path_filtered_data <- create_sqlite_filepath(animal_name_code,
                                                    tag_number, 
                                                    start_time, 
                                                    end_time, 
                                                    filtered_data_path)
  
  # Add _filtered to the file name
  # Find the position of the last dot (before the extension)
  pos <- regexpr("\\.sqlite$", full_path_filtered_data)
  
  # If a dot followed by 'sqlite' is found, insert '_raw' before it
  if (pos > 0) {
    full_path_filtered_data <- paste0(substr(full_path_filtered_data, 1, pos - 1), "_annotated", substr(full_path_filtered_data, pos, nchar(full_path_filtered_data)))
  }
  
  # Remove geometry if the location data is an sf object
  if (!is.null(segment_location_data) && inherits(segment_location_data, "sf")) {
    segment_location_data <- st_drop_geometry(segment_location_data)
  }
  
  
  # Save the current data segment as sqlite
  source(paste0(getwd(), "/save_ATLAS_data_to_sqlite.R"))
  save_ATLAS_data_to_sqlite(localizations_data = segment_location_data,
                            detections_data = segment_detection_data,
                            fullpath = full_path_filtered_data)
  
  if (save_as_csv) {
    full_path_filtered_data_csv <- sub("\\.sqlite$", ".csv", full_path_filtered_data)
    write.csv(segment_location_data, full_path_filtered_data_csv, row.names = FALSE)
  }
}