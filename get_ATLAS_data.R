
source(file.path(getwd(), "config.R"))

#' Retrieve ATLAS data from either the server or local SQLite files.
#'
#' This function checks the `retrieve_data_from_server` flag from the config file,
#' to determine whether to download data directly from the TAU server or load it from 
#' multiple SQLite files. If downloading from the server, a VPN connection 
#' is required.
#' 
#' @param data_requests A list of lists (or a dataframe) where each element 
#'                      contains:
#'                      - tag: A character string representing the tag number.
#'                      - start_time: A character string representing the start time.
#'                      - end_time: A character string representing the end time.
#'                      
#' @param folder_paths_to_retrieve_sqlite_files A list of character strings representing the paths to the SQLite files.
#'
#' @param folder_path_to_save_sqlite The path in which the data should be saved. 
#'                                   The file name will be generated automatically from the tag numbers and dates.
#' 
#' @return A data frame containing the raw location data retrieved from the 
#'         ATLAS system, for all data specified in ATLAS_data_requests.R.
#'         
get_ATLAS_data <- function(data_requests, folder_paths_to_retrieve_sqlite_files=NULL, folder_path_to_save_sqlite=NULL) {
  
  if (retrieve_data_from_server) {
    # Download ATLAS data directly from the server
    # This requires a VPN connection to the TAU server
    
    source(paste0(path_to_atlas_data_analysis_repo, "retrieve_and_store_atlas_data.R"))
    raw_location_data <- retrieve_and_store_atlas_data(data_requests, folder_path_to_save_sqlite)
    
  } else {
    
    if (is.null(folder_paths_to_retrieve_sqlite_files)){
      
      warning("No folder to retrieve the SQLite data was provided. Exiting the script.")
      stop("Run aborted due to missing folder path.")
      
    } else {
      
      source(paste0(path_to_atlas_data_analysis_repo, "create_list_of_sqlite_filepaths.R"))
      fullpaths <- create_list_of_sqlite_filepaths(data_requests, folder_paths_to_retrieve_sqlite_files)
      
      source(paste0(path_to_atlas_data_analysis_repo, "load_atlas_data_from_multiple_sqlite_files.R"))
      raw_location_data <- load_atlas_data_from_multiple_sqlite_files(fullpaths)
      
    }
  }
  return(raw_location_data)
}



