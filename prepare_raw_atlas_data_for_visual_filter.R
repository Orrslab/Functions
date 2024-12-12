# Get ATLAS data and prepare it for using the visual filter

prepare_raw_atlas_data_for_visual_filter <- function(tag_number,
                                                     start_time,
                                                     end_time,
                                                     retrieve_data_from_server,
                                                     save_data_to_sqlite_file,
                                                     raw_data_folder_path)
{
  
  # # Install the required R packages- in not yet installed
  # source(paste0(path_to_scripts,"install_required_R_packages.R"))
  
  data_request <- list(
    list(tag = tag_number, 
    start_time = start_time, 
    end_time = end_time)
  )
  
  # Generate the file names from the tag numbers and dates
  source(paste0(path_to_atlas_data_analysis_repo, "create_sqlite_filepath.R"))
  fullpath_to_sqlite_file <- create_sqlite_filepath(tag_numbers = tag_number,
                                                    start_time = start_time,
                                                    end_time = end_time,
                                                    folder_path_to_sqlite_files = raw_data_folder_path)
  
  # Add _raw to the file name
  # Find the position of the last dot (before the extension)
  pos <- regexpr("\\.sqlite$", fullpath_to_sqlite_file)
  
  # If a dot followed by 'sqlite' is found, insert '_raw' before it
  if (pos > 0) {
    fullpath_to_sqlite_file <- paste0(substr(fullpath_to_sqlite_file, 1, pos - 1), "_raw", substr(fullpath_to_sqlite_file, pos, nchar(fullpath_to_sqlite_file)))
  }
  
  
  # Get the ATLAS data- either from the server, or from an SQLite file
  source(paste0(path_to_atlas_data_analysis_repo,"get_ATLAS_data.R"))
  raw_location_data = get_ATLAS_data(data_requests = data_request, 
                                     retrieve_data_from_server = retrieve_data_from_server,
                                     save_data_to_sqlite_file = save_data_to_sqlite_file,
                                     full_paths_to_sqlite_files = fullpath_to_sqlite_file)
  
  # Assign day numbers to the data
  source(paste0(path_to_atlas_data_analysis_repo, "time_conversions.R"))
  # convert the time column to the POSIXct format- required for using AssignDayNumber.R
  raw_location_data$dateTime <- convert_to_POSIXct(raw_location_data$TIME)
  source(paste0(path_to_atlas_data_analysis_repo, "AssignDayNumber.R"))
  raw_location_data <- AssignDayNumber(data=raw_location_data, TimeColName = "dateTime")
  
  if (save_data_to_csv_file) {
    # Save the raw data as CSV
    write.csv(raw_location_data, paste0(path_to_csv_files, "BO.csv"), row.names = FALSE)
  }
  
  return(raw_location_data)
}

# Activate the shiny Visual Filter
# source(paste0(path_to_atlas_data_analysis_repo, "visual_filter_shiny_app.R"))