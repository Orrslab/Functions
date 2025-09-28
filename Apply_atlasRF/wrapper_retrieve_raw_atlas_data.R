
source(file.path(getwd(), "get_global_time_range_of_data_requests.R"))
source(file.path(getwd(), "create_filename_without_extension.R"))
source(file.path(getwd(), "get_ATLAS_data.R"))

#' @title Wrapper Function to Retrieve Raw ATLAS Data
#'
#' @description
#' This wrapper orchestrates the retrieval of raw ATLAS animal movement data.
#' It determines the global time range across all data requests, constructs a 
#' standardized filename, checks if an SQLite file already exists, and either 
#' loads data from the file or retrieves it from the ATLAS server.
#'
#' @param config A list containing configuration information with the following elements:
#' \describe{
#'   \item{\code{data_requests}}{A list of data request objects. Each object must contain:
#'     \code{animal_name_code}, \code{tag}, \code{start_time}, \code{end_time}.}
#'   \item{\code{animal_name_code}}{Character string, the animal code to be included in the filename.}
#'   \item{\code{paths}}{A list of paths, must include:
#'     \code{fodler_raw_atlas_data} (path to the directory where SQLite files are stored).}
#'   \item{\code{atlas_db_credentials}}{A list with ATLAS database connection details:
#'     \code{system_name}, \code{db_username}, \code{db_pass}, \code{db_host_ip},
#'     \code{db_port_number}, \code{db_name}.}
#' }
#'
#' @return A data.frame containing the raw ATLAS data, either loaded from an existing SQLite file
#' or retrieved from the ATLAS server.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Computes the earliest start time and latest end time across all requests using
#'   \code{get_global_time_range_of_data_requests()}.
#'   \item Creates a standardized filename (without extension) using
#'   \code{create_filename_without_extension()}.
#'   \item Constructs the full SQLite file path.
#'   \item Checks if the SQLite file already exists. If yes, data retrieval from server is skipped.
#'   \item Otherwise, data is retrieved from the ATLAS server and saved to SQLite.
#' }
#'
#' @export
#' 
wrapper_retrieve_raw_atlas_data <- function(config) {
  
  message("*** Retrieving raw ATLAS data. ***")
  
  # Get the global start and end times of all the data
  global_times_data <- get_global_time_range_of_data_requests(
    data_requests = config$data_requests,
    data_time_zone = config$atlas_time_and_coordinates_info$atlas_time_zone,
    data_time_format = config$atlas_time_and_coordinates_info$atlas_time_format)

  # Generate the file name without extension- to save the ATLAS data
  filename_without_extension <- create_filename_without_extension(
    animal_name_code = config$animal_name_code, 
    tag_numbers = sapply(config$data_requests, function(x) x$tag), 
    start_time = global_times_data$start_time, 
    end_time = global_times_data$end_time)
  
  filename_with_extension <- paste0(filename_without_extension, ".sqlite")
  
  fullpath_to_sqlite_file <- file.path(config$paths$fodler_raw_atlas_data, filename_with_extension)
  
  # Check if the file already exists
  if (file.exists(fullpath_to_sqlite_file)) {
    retrieve_data_from_atlas_server <- FALSE
  } else {
    retrieve_data_from_atlas_server <- TRUE
  }
  
  # Get the ATLAS database credentials from the config file
  atlas_db_credentials <- list(
    # System name
    system_name = config$atlas_db_credentials$system_name,         
    # username
    db_username = config$atlas_db_credentials$db_username,         
    # password
    db_pass = config$atlas_db_credentials$db_pass,                 
    # host IP address
    db_host_ip = config$atlas_db_credentials$db_host_ip,           
    # port number
    db_port_number = config$atlas_db_credentials$db_port_number,   
    # database name
    db_name = config$atlas_db_credentials$db_name                  
  )
  
  # Get the ATLAS data- either from the server, or from an SQLite file
  source(file.path(getwd(),"get_ATLAS_data.R"))
  raw_atlas_data = get_ATLAS_data(data_requests = config$data_requests, 
                                  atlas_db_credentials = atlas_db_credentials,
                                  retrieve_data_from_server = retrieve_data_from_atlas_server,
                                  save_data_to_sqlite_file = TRUE,
                                  full_paths_to_sqlite_files = fullpath_to_sqlite_file)
  
  return(raw_atlas_data)
  
}