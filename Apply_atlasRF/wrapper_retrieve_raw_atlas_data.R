
source(file.path(getwd(), "get_global_time_range_of_data_requests.R"))
source(file.path(getwd(), "create_filename_without_extension.R"))
source(file.path(getwd(), "get_ATLAS_data.R"))

wrapper_retrieve_raw_atlas_data <- function(config) {
  
  # Get the global start and end times of all the data
  global_times_data <- get_global_time_range_of_data_requests(config$data_requests)

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
  
  raw_localization_data <- raw_atlas_data$LOCALIZATIONS
  raw_detection_data <- raw_atlas_data$DETECTIONS
  
}