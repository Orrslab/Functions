
source(paste0(path_to_atlas_data_analysis_repo, "create_sqlite_filepath.R"))

#' Generate a list of SQLite file paths based on data requests
#'
#' This function creates a list of SQLite file paths by extracting tag numbers, 
#' start times, and end times from the provided data requests. Each file path 
#' is generated using the `create_sqlite_filepath` function.
#'
#' @param data_requests A list of lists or a dataframe where each element 
#'   contains:
#'   - `tag`: A character string representing the tag number.
#'   - `start_time`: A character string representing the start time in a 
#'     standard date-time format.
#'   - `end_time`: A character string representing the end time in a 
#'     standard date-time format.
#'
#' @param folder_path_to_sqlite_files A character string representing the path 
#'   to the folder where the SQLite files are stored.
#'
#' @return A list of character strings, each representing the full path to an 
#'   SQLite file corresponding to the data request.
#'
#' @details This function iterates through the `data_requests` object, extracts 
#'   the `tag`, `start_time`, and `end_time` for each request, and generates a 
#'   corresponding SQLite file path by calling the `create_sqlite_filepath` 
#'   function. The resulting file paths are stored in a list and returned.
#'
#' @examples
#' # Example data requests
#' data_requests <- list(
#'   list(tag = "972006000430", start_time = "2021-08-23 17:00:00", end_time = "2021-08-24 05:00:00"),
#'   list(tag = "972006000431", start_time = "2021-08-24 06:00:00", end_time = "2021-08-24 18:00:00")
#' )
#' 
#' # Folder containing SQLite files
#' folder_path_to_sqlite_files <- "path/to/sqlite/files"
#' 
#' # Generate the list of file paths
#' filepaths <- create_list_of_sqlite_filepaths(data_requests, folder_path_to_sqlite_files)
#'
#' @seealso \code{\link{create_sqlite_filepath}} for the logic used to generate individual file paths.
#'
create_list_of_sqlite_filepaths <- function(data_requests, folder_path_to_sqlite_files) {
  
  filepaths <- list()
  
  for (request in data_requests) {
    
    animal_name_code = request$animal_name_code
    tag_numbers = request$tag
    start_time = request$start_time
    end_time = request$end_time
    
    filepaths[[length(filepaths) + 1]] <- create_sqlite_filepath(animal_name_code,
                                                                 tag_numbers,
                                                                 start_time,
                                                                 end_time,
                                                                 folder_path_to_sqlite_files)
    
  }
  return(filepaths)
}