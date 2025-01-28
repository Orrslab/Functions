# Function to load data from all SQLite files in a folder
get_full_paths_of_files_with_specified_extension<- function(folder_path, file_extension="sqlite") {
  # Check if the folder exists
  if (!dir.exists(folder_path)) {
    stop("The specified folder does not exist.")
  }
  
  # Construct the pattern to match files with the given extension
  pattern <- paste0("\\.", file_extension, "$")
  
  # List all matching files in the folder
  sqlite_full_paths <- list.files(path = folder_path, pattern = pattern, full.names = TRUE)
  
  # Check if any SQLite files were found
  if (length(sqlite_full_paths) == 0) {
    stop(paste("No", file_extension, "files found in the specified folder."))
  }
  
  return(sqlite_full_paths)
}
