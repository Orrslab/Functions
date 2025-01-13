#' Prepare Raw ATLAS Data for Visual Filter
#'
#' This function retrieves ATLAS tracking data for a given animal and time range, either from an SQLite file or the ATLAS server, 
#' and processes it to be used in a visual filtering workflow.
#'
#' @param animal_name_code A string representing the animal's identifier (e.g., "BO" for Barn Owl).
#' @param tag_number An integer or string representing the ATLAS tag number (e.g. 972006000556).
#' @param start_time A POSIXct timestamp or string specifying the start time of the data request (e.g. "2021-07-04 17:00:00").
#' @param end_time A POSIXct timestamp or string specifying the end time of the data request.
#' @param raw_data_folder_path A string specifying the folder path where SQLite files are stored.
#' @param atlas_db_credentials 
#'
#' @return A dataframe containing raw ATLAS location data, including assigned day numbers.
#'
#' @details 
#' - Constructs the expected SQLite file path using `create_sqlite_filepath()`.
#' - Checks whether data already exists in an SQLite file or needs to be retrieved from the ATLAS server.
#' - Loads or retrieves ATLAS data using `get_ATLAS_data()`.
#' - Converts timestamps to POSIXct format using `convert_to_POSIXct()`.
#' - Assigns day numbers to the dataset using `AssignDayNumber()`.
#'
#' @note 
#' - Ensure that all required scripts (e.g., `create_sqlite_filepath.R`, `get_ATLAS_data.R`, `time_conversions.R`, `AssignDayNumber.R`) 
#'   are available in the specified paths before running this function.
#' - The function automatically adds `"_raw"` to the SQLite filename to differentiate it from processed files.
#'
#' @examples
#' \dontrun{
#' raw_data <- prepare_raw_atlas_data_for_visual_filter(
#'   animal_name_code = "BO_0556",
#'   tag_number = 972006000556,
#'   start_time = "2021-07-04 17:00:00",
#'   end_time = "2021-07-04 23:59:59",
#'   raw_data_folder_path = "C:/Users/manual_tagging_database/Raw_data/"
#' )
#' }
#'
#' @export

prepare_raw_atlas_data_for_visual_filter <- function(animal_name_code,
                                                     tag_number,
                                                     start_time,
                                                     end_time,
                                                     raw_data_folder_path,
                                                     atlas_db_credentials)
{
  
  data_request <- list(
    list(tag = tag_number, 
    start_time = start_time, 
    end_time = end_time)
  )
  
  # Generate the file names from the tag numbers and dates
  source(paste0(getwd(), "/create_sqlite_filepath.R"))
  fullpath_to_sqlite_file <- create_sqlite_filepath(animal_name_code = animal_name_code,
                                                    tag_numbers = tag_number,
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
  
  # Check if the sqlite file already exists
  if (file.exists(fullpath_to_sqlite_file)) {
    retrieve_data_from_server = FALSE
    print("Loading data from sqlite")
  } else {
    retrieve_data_from_server = TRUE
    save_data_to_sqlite_file = TRUE
    print("Retrieving data from the ATLAS server")
  }
  
  # Get the ATLAS data- either from the server, or from an SQLite file
  source(paste0(getwd(),"/get_ATLAS_data.R"))
  raw_location_data = get_ATLAS_data(data_requests = data_request, 
                                     retrieve_data_from_server = retrieve_data_from_server,
                                     atlas_db_credentials = atlas_db_credentials,
                                     save_data_to_sqlite_file = save_data_to_sqlite_file,
                                     full_paths_to_sqlite_files = fullpath_to_sqlite_file)
  
  # Assign day numbers to the data
  source(paste0(getwd(), "/time_conversions.R"))
  # convert the time column to the POSIXct format- required for using AssignDayNumber.R
  raw_location_data$dateTime <- convert_to_POSIXct(raw_location_data$TIME)
  source(paste0(getwd(), "/AssignDayNumber.R"))
  raw_location_data <- AssignDayNumber(data=raw_location_data,
                                       DayStartTime = day_start_time,
                                       DayEndTime = day_end_time,
                                       TimeColName = "dateTime")
  
  return(raw_location_data)
}