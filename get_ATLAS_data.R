#' Retrieve ATLAS data from the server or local SQLite files
#'
#' This function retrieves ATLAS data either by downloading it from the TAU server 
#' (when `retrieve_data_from_server = TRUE`) or by loading it from local SQLite files 
#' (when `retrieve_data_from_server = FALSE`). When downloading data, a VPN connection 
#' to the TAU server is required. The function can optionally save the downloaded data 
#' to specified SQLite files.
#'
#' @param data_requests A list of lists (or a dataframe) where each element contains:
#'   - `tag`: A character string representing the tag number.
#'   - `start_time`: A character string representing the start time.
#'   - `end_time`: A character string representing the end time.
#' @param retrieve_data_from_server Logical. If `TRUE`, data will be downloaded 
#'   from the TAU server. If `FALSE`, data will be loaded from local SQLite files.
#' @param save_data_to_sqlite_file Logical. If `TRUE`, downloaded data will be saved 
#'   to SQLite files. Ignored if `retrieve_data_from_server = FALSE`.
#' @param full_paths_to_sqlite_files A character string or vector of character strings 
#'   representing the full paths to the SQLite files. Defaults to 
#'   `paste0(getwd(), "atlas_data.sqlite")`. If `NULL` and 
#'   `retrieve_data_from_server = FALSE`, the function will stop with an error.
#'
#' @return A dataframe containing the raw location data retrieved from the ATLAS system 
#'   for all data specified in `data_requests`.
#'
#' @details When `retrieve_data_from_server = TRUE`, this function sources the 
#'   `retrieve_and_store_atlas_data` script to download data from the server. If 
#'   `save_data_to_sqlite_file = TRUE`, the data will be saved to the specified SQLite files. 
#'   If `retrieve_data_from_server = FALSE`, it sources the 
#'   `load_atlas_data_from_multiple_sqlite_files` script to load data from the provided SQLite files.
#'
#' @note A VPN connection is required to access the TAU server when downloading data.
#'
#' @examples
#' # Example: Download data from the server and save it to SQLite files
#' data_requests <- list(
#'   list(tag = "972006000430", start_time = "2021-08-23 17:00:00", end_time = "2021-08-24 05:00:00")
#' )
#' folder_path_to_save_sqlite <- "path/to/save/sqlite/"
#' raw_data <- get_ATLAS_data(data_requests, retrieve_data_from_server = TRUE, 
#'                            save_data_to_sqlite_file = TRUE)
#'
#' # Example: Load data from existing SQLite files
#' full_paths_to_sqlite_files <- c("path/to/sqlite1.sqlite", "path/to/sqlite2.sqlite")
#' raw_data <- get_ATLAS_data(data_requests, retrieve_data_from_server = FALSE, 
#'                            full_paths_to_sqlite_files = full_paths_to_sqlite_files)
#'       
get_ATLAS_data <- function(data_requests, 
                           retrieve_data_from_server = TRUE,
                           save_data_to_sqlite_file = TRUE,
                           full_paths_to_sqlite_files = paste0(getwd(), "atlas_data.sqlite")) {
  
  if (retrieve_data_from_server) {
    # Download ATLAS data directly from the server
    # This requires a VPN connection to the TAU server
    
    source(paste0(path_to_atlas_data_analysis_repo, "retrieve_and_store_atlas_data.R"))
    raw_location_data <- retrieve_and_store_atlas_data(data_requests = data_requests, 
                                                       save_data_to_sqlite_file = save_data_to_sqlite_file,
                                                       full_paths_to_store_sqlite_files= full_paths_to_sqlite_files)
    
  } else {
    
    if (is.null(full_paths_to_sqlite_files)){
      
      warning("No folder to retrieve the SQLite data was provided. Exiting the script.")
      stop("Run aborted due to missing folder path.")
      
    } else {
      
      source(paste0(path_to_atlas_data_analysis_repo, "load_atlas_data_from_multiple_sqlite_files.R"))
      raw_location_data <- load_atlas_data_from_multiple_sqlite_files(full_paths_to_sqlite_files)
      
    }
  }
  return(raw_location_data)
}



