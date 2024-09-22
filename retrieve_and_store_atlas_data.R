
# Load the functions that connect to the ATLAS database
source(paste0(path_to_atlas_data_analysis_repo, "ATLAS_database_connection.R"))


#' Retrieve and store ATLAS data based on specified requests.
#'
#' This function connects to the ATLAS database, retrieves data for each 
#' request in the `data_requests`, and optionally saves the data to 
#' SQLite files. The function collects localization data and returns a 
#' combined data frame.
#'
#' @param data_requests A list of lists (or a dataframe) where each element 
#'                      contains:
#'                      - tag: A character string representing the tag number.
#'                      - start_time: A character string representing the start time.
#'                      - end_time: A character string representing the end time.
#'
#' @return A data frame containing the combined localization data retrieved 
#'         from the ATLAS system.
#'         
retrieve_and_store_atlas_data <- function(data_requests) {
  
  all_data_frames <- list()
  
  #connect to the database
  db_conn <- connect_to_atlas_db()
  
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
      
      source(paste0(path_to_atlas_data_analysis_repo, "save_ATLAS_data_to_sqlite.R"))
      
      save_ATLAS_data_to_sqlite(localizations_data = RawLoc0, 
                                tag_number = tag_numbers, 
                                start_time = start_time,
                                end_time = end_time)
    }
  }
  
  # Disconnect from the database
  disconnect_from_db(db_conn)
  
  raw_location_data <- bind_rows(all_data_frames)
  
  # Return the last retrieved localizations table
  return(raw_location_data)
}