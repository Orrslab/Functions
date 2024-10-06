
source(file.path(getwd(), "Scripts", "config.R"))

#' Retrieve ATLAS data from either the server or local SQLite files.
#'
#' This function checks the `retrieve_data_from_server` flag from the config file,
#' to determine whether to download data directly from the TAU server or load it from 
#' multiple SQLite files. If downloading from the server, a VPN connection 
#' is required.
#'
#' @return A data frame containing the raw location data retrieved from the 
#'         ATLAS system, for all data specified in ATLAS_data_requests.R.
#'         
get_ATLAS_data <- function() {
  
  if (retrieve_data_from_server) {
    # Download ATLAS data directly from the server
    # This requires a VPN connection to the TAU server
    
    source(paste0(path_to_atlas_data_analysis_repo, "retrieve_and_store_atlas_data.R"))
    raw_location_data <- retrieve_and_store_atlas_data(data_requests)
    
  } else {
    
    source(paste0(path_to_atlas_data_analysis_repo, "create_list_of_sqlite_filepaths.R"))
    filepaths <- create_list_of_sqlite_filepaths(data_requests)
    
    source(paste0(path_to_atlas_data_analysis_repo, "load_atlas_data_from_multiple_sqlite_files.R"))
    raw_location_data <- load_atlas_data_from_multiple_sqlite_files(filepaths)
    
  }
  return(raw_location_data)
}



