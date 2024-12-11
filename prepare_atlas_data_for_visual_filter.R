# Get ATLAS data and prepare it for using the visual filter

prepare_atlas_data_for_visual_filter <- function(data_requests, folder_paths_to_retrieve_sqlite_files=NULL, folder_path_to_save_sqlite=NULL)
{
  # Get the required paths from the config file config.R
  source(file.path(getwd(), "config.R"))
  
  # # Install the required R packages- in not yet installed
  # source(paste0(path_to_scripts,"install_required_R_packages.R"))
  
  # Get the ATLAS data- either from the server, or from an SQLite file
  source(paste0(path_to_atlas_data_analysis_repo,"get_ATLAS_data.R"))
  raw_location_data = get_ATLAS_data(data_requests, 
                                     folder_paths_to_retrieve_sqlite_files,
                                     folder_path_to_save_sqlite)
  
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