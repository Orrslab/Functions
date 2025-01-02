library("dplyr")

#' Retrieve and store ATLAS data based on specified requests
#'
#' This function connects to the ATLAS database, retrieves localization data 
#' based on the specified `data_requests`, and optionally saves the data to 
#' SQLite and/or CSV files. The retrieved data is consolidated into a single 
#' data frame and returned.
#'
#' @param data_requests A list of lists or a dataframe where each element 
#'   contains:
#'   - `tag`: A character string representing the tag number.
#'   - `start_time`: A character string representing the start time in a 
#'     standard date-time format.
#'   - `end_time`: A character string representing the end time in a 
#'     standard date-time format.
#'
#' @param save_data_to_sqlite_file Logical. If `TRUE`, saves the retrieved 
#'   localization data to SQLite files. Defaults to `TRUE`.
#'
#' @param full_paths_to_store_sqlite_files A character string specifying the 
#'   full path for the SQLite files to store the data. If not provided, defaults 
#'   to `paste0(getwd(), "atlas_data.sqlite")`.
#'
#' @param save_data_to_csv_file Logical. If `TRUE`, saves the retrieved 
#'   localization data to CSV files. Defaults to `FALSE`.
#'
#' @param fullpath_to_csv_files A character string specifying the full path for 
#'   the CSV files to store the data. If not provided, defaults to 
#'   `paste0(getwd(), "/atlas_data.csv")`.
#'
#' @return A data frame containing the combined localization data retrieved 
#'   from the ATLAS system for all specified requests.
#'
#' @details The function iterates over the `data_requests`, retrieves 
#'   localization data for each request, and appends the results to a list. 
#'   Optionally, the data can be saved to SQLite or CSV files. The retrieved 
#'   data is consolidated into a single data frame, which is returned.
#'
#' @examples
#' # Example data requests
#' data_requests <- list(
#'   list(tag = "972006000430", start_time = "2021-08-23 17:00:00", end_time = "2021-08-24 05:00:00"),
#'   list(tag = "972006000431", start_time = "2021-08-24 06:00:00", end_time = "2021-08-24 18:00:00")
#' )
#' 
#' # Retrieve data and save to SQLite files
#' raw_data <- retrieve_and_store_atlas_data(
#'   data_requests = data_requests,
#'   save_data_to_sqlite_file = TRUE,
#'   full_paths_to_store_sqlite_files = "path/to/save/atlas_data.sqlite"
#' )
#'
#' @seealso \code{\link{connect_to_atlas_db}} for database connection, 
#'   \code{\link{save_ATLAS_data_to_sqlite}} for saving data to SQLite, 
#'   and \code{\link{Data_from_ATLAS_server}} for retrieving data.
#'
retrieve_and_store_atlas_data <- function(data_requests, 
                                          save_data_to_sqlite_file = TRUE,
                                          full_paths_to_store_sqlite_files = paste0(getwd(), "atlas_data.sqlite"),
                                          save_data_to_csv_file = FALSE,
                                          fullpath_to_csv_files = paste0(getwd(), "/atlas_data.csv")) {
  
  all_data_frames <- list()
  
  # Load the ATLAS Harod database credentials
  source(file.path(getwd(), "config.R"))
  
  # Connect to the database
  source(paste0(path_to_atlas_data_analysis_repo, "connect_to_atlas_db.R"))
  db_conn <- connect_to_atlas_db(db_username = db_username_harod,         # username
                                 db_pass = db_pass_harod,                 # password
                                 db_host_ip = db_host_ip_harod,           # host ip address
                                 db_port_number = db_port_number_harod,   # port Number
                                 db_name = db_name_harod)
  
  for (request in data_requests) {
    
    tag_numbers <- request$tag
    start_time <- request$start_time
    end_time <- request$end_time
    
    source(paste0(path_to_atlas_data_analysis_repo, "Data_from_ATLAS_server.R"))
    AllData <- Data_from_ATLAS_server(start_time,
                                      end_time,
                                      tag_numbers,
                                      includeLoc = TRUE,
                                      includeDet=FALSE,
                                      db_conn)
    
    RawLoc0 <- AllData$LOC
    RawDet0 <- AllData$DET
    
    all_data_frames[[length(all_data_frames) + 1]] <- RawLoc0
    
    if (save_data_to_sqlite_file) {
      
      if (is.null(full_paths_to_store_sqlite_files)) {
        
        warning("No folder to save the SQLite data was provided. Exiting the script.")
        stop("Run aborted due to missing folder path.")
        
      } else {
        
        # Save the data as sqlite
        source(paste0(path_to_atlas_data_analysis_repo, "save_ATLAS_data_to_sqlite.R"))
        save_ATLAS_data_to_sqlite(localizations_data = RawLoc0,
                                  fullpath=full_paths_to_store_sqlite_files)
      }
    }
    
    if (save_data_to_csv_file) {
      write.csv(RawLoc0, fullpath_to_csv_files, row.names = FALSE)
    }
  }
  
  # Disconnect from the database
  dbDisconnect(db_conn)
  
  raw_location_data <- bind_rows(all_data_frames)
  
  # Return the last retrieved localizations table
  return(raw_location_data)
}