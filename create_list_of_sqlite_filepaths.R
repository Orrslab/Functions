source(file.path(getwd(), "ATLAS_run_scripts", "config.R"))
source(paste0(path_to_atlas_data_analysis_repo, "create_sqlite_filepath.R"))

#' Create a list of SQLite file paths based on data requests.
#'
#' This function generates a list of SQLite file paths by extracting tag 
#' numbers, start times, and end times from the provided data requests.
#'
#' @param data_requests A list of lists (or a dataframe) where each element 
#'                      contains:
#'                      - tag: A character string representing the tag number.
#'                      - start_time: A character string representing the start time.
#'                      - end_time: A character string representing the end time.
#'
#' @return A list of character strings representing the paths to the SQLite files.
#'
create_list_of_sqlite_filepaths <- function(data_requests) {
  
  filepaths <- list()
  
  for (request in data_requests) {
    
    tag_numbers = request$tag
    start_time = request$start_time
    end_time = request$end_time
    
    filepaths[[length(filepaths) + 1]] <- create_sqlite_filepath(tag_numbers,
                                                                 start_time,
                                                                 end_time)
    
  }
  return(filepaths)
}