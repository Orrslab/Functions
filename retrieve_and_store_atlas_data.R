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
#' @param atlas_db_credentials Required credentials to connect to the ATLAS database.
#' 
#' @param save_data_to_sqlite_file Logical. If `TRUE`, saves the retrieved 
#'   localization data to SQLite files. Defaults to `TRUE`.
#'
#' @param full_paths_to_store_sqlite_files A character string specifying the 
#'   full path for the SQLite files to store the data, including a file name 
#'   (e.g., `paste0(getwd(), "/atlas_data.sqlite")`). Defaults to 
#'   `paste0(getwd(), "atlas_data.sqlite")`.
#'
#' @param save_data_to_csv_file Logical. If `TRUE`, saves the retrieved 
#'   localization data to CSV files. Defaults to `FALSE`.
#'
#' @return A data frame containing the combined localization data retrieved 
#'   from the ATLAS system for all specified requests. The data frame includes 
#'   localization data for all specified tags and time periods.
#'
#' @details The function iterates over the `data_requests`, retrieves 
#'   localization data for each request, and appends the results to a list. 
#'   Optionally, the data can be saved to SQLite or CSV files. The retrieved 
#'   data is consolidated into a single data frame, which is returned. Note that 
#'   the function will overwrite existing files at the specified locations.
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
#'   and \code{\link{data_from_atlas_server}} for retrieving data.
#'
retrieve_and_store_atlas_data <- function(data_requests, 
                                          atlas_db_credentials,
                                          save_data_to_sqlite_file = TRUE,
                                          full_paths_to_store_data_files = paste0(getwd(), "atlas_data.sqlite"),
                                          save_data_to_csv_file = FALSE) {
  
  library("dplyr")
  
  all_data_frames_loc <- list()
  all_data_frames_det <- list()
  
  # Connect to the database
  source(paste0(getwd(), "/connect_to_atlas_db.R"))
  db_conn <- connect_to_atlas_db(atlas_db_credentials)
  
  for (request in data_requests) {
    
    tag_numbers <- request$tag
    start_time <- request$start_time
    end_time <- request$end_time
    
    source(paste0(getwd(), "/data_from_atlas_server.R"))
    AllData <- data_from_atlas_server(start_time,
                                      end_time,
                                      tag_numbers,
                                      includeLoc = TRUE,
                                      includeDet = TRUE,
                                      db_conn)
    
    RawLoc0 <- AllData$LOCALIZATIONS
    RawDet0 <- AllData$DETECTIONS
    
    # Add the locations data segment to the locations data frame
    all_data_frames_loc[[length(all_data_frames_loc) + 1]] <- RawLoc0
    # Add the detections data segment to the detections data frame
    if (!is.null(RawDet0) && nrow(RawDet0) > 0) {
      all_data_frames_det[[length(all_data_frames_det) + 1]] <- RawDet0
    }
    
    if (save_data_to_sqlite_file) {
      
      if (is.null(full_paths_to_store_data_files)) {
        
        warning("No folder to save the SQLite data was provided. Exiting the script.")
        stop("Run aborted due to missing folder path.")
        
      } else {
        
        if (save_data_to_sqlite_file) {
          
          # Add .sqlite extension to the file names
          full_paths_to_store_sqlite_files <- paste0(full_paths_to_store_data_files, ".sqlite")
          
          # Save the data as sqlite
          source(paste0(getwd(), "/save_ATLAS_data_to_sqlite.R"))
          save_ATLAS_data_to_sqlite(localization_data = RawLoc0,
                                    detections_data = RawDet0,
                                    fullpath=full_paths_to_store_sqlite_files)
          
        }
        
        if (save_data_to_csv_file) {
          
          # Add .csv extension to the file names
          full_paths_to_store_csv_files <- paste0(full_paths_to_store_data_files, ".csv")
          
          # Save the localizations data as csv- only if it has rows
          if (nrow(RawLoc0) > 0) {
            write.csv(RawLoc0, paste0(full_paths_to_store_csv_files, "_Localizations.csv"), row.names = FALSE)
          }
          
          # Save the localizations data as csv- only if it has rows
          if (nrow(RawDet0) > 0) {
            write.csv(RawDet0, paste0(full_paths_to_store_csv_files, "_Detections.csv"), row.names = FALSE)
          }
          
        }
      }
    }
  }
  
  # Disconnect from the database
  dbDisconnect(db_conn)
  
  raw_location_data <- bind_rows(all_data_frames_loc)
  raw_detection_data <- if (length(all_data_frames_det) > 0) {
    bind_rows(all_data_frames_det)
  } else {
    NULL  # No detection data retrieved
  }
  
  # Return the last retrieved localizations and detections tables
  return(list(
    LOCALIZATIONS = raw_location_data,
    DETECTIONS = raw_detection_data
  ))
}