#' Add Detections to a Single SQLite File
#'
#' This function retrieves detection data from the ATLAS server and appends it to 
#' the appropriate SQLite file based on species ID, tag number, and time range.
#'
#' @param species_id A character string representing the species ID. Example: "BO" for Barn Owl
#' @param tag_number A numeric value representing the tag number.
#' @param start_time A character string of the start time in the format \%Y-\%m-\%d \%H:\%M:\%S.
#' @param end_time A character string of the end time in the format \%Y-\%m-\%d \%H:\%M:\%S.
#' @param atlas_db_conn A database connection object to the ATLAS server.
#' @param path_to_sqlite_files A character string specifying the folder path where SQLite files are stored.
#'
#' @return None. The function saves the detections data into a corresponding SQLite file.
#'
#' @details 
#' The function:
#' - Retrieves detection data from the ATLAS server using `data_from_atlas_server()`
#' - Generates the appropriate SQLite file path using `create_sqlite_filepath()`
#' - Saves the detections data to the generated SQLite file using `save_ATLAS_data_to_sqlite()`
#' 
#' A VPN connection to the ATLAS server is required.
#'
#' @examples
#' \dontrun{
#' add_detections_to_single_file(
#'   species_id = "BS",
#'   tag_number = 972006001074,
#'   start_time = "2023-03-24 00:00:00",
#'   end_time = "2023-03-24 23:59:57",
#'   atlas_db_conn = my_atlas_server_db_connection,
#'   path_to_sqlite_files = "/path/to/sqlite/files"
#' )
#' }
#'
#' @export
add_detections_to_single_file <- function(species_id,
                                          tag_number,
                                          start_time,
                                          end_time,
                                          atlas_db_conn,
                                          path_to_sqlite_files) {
  
  source(paste0(getwd(), "/data_from_atlas_server.R"))
  source(paste0(getwd(), "/create_sqlite_filepath.R"))
  source(paste0(getwd(), "/save_ATLAS_data_to_sqlite.R"))
  
  # Retrieve the DETECTIONS from the ATLAS server (VPN connection is required)
  atlas_data <- data_from_atlas_server(Start_Time_Str = start_time,
                                       End_Time_Str = end_time,
                                       Tag_numbers = tag_number,
                                       includeDet = TRUE,
                                       includeLoc = FALSE, 
                                       dbc = atlas_db_conn)
  
  detections_data <- atlas_data$DETECTIONS
  
  # Get the file name and full path of the sqlite file to which the DETECTIONS data should be added
  path_to_sqlite_file <- create_sqlite_filepath(animal_name_code = species_id, 
                                                tag_numbers = tag_number, 
                                                start_time = start_time, 
                                                end_time = end_time, 
                                                folder_path_to_sqlite_files = path_to_sqlite_files)
  
  # Append "_annotated" before ".sqlite"
  path_to_sqlite_file <- sub("\\.sqlite$", "_annotated.sqlite", path_to_sqlite_file)
  
  # Save the DETECTIONS data into the sqlite file
  save_ATLAS_data_to_sqlite(detections_data = detections_data,
                            fullpath = path_to_sqlite_file)
  
}

