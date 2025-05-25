# Convert RData files to .csv

# Define a function to load and save objects from each .RData file
save_all_rdata_to_csv <- function(input_folder, output_folder) {
  # Get a list of all .RData files in the input folder
  rdata_files <- list.files(input_folder, pattern = "\\.RData$", full.names = TRUE)
  
  # Loop over each .RData file
  for (rdata_path in rdata_files) {
    # Load all objects in the .RData file
    loaded_objects <- load(rdata_path)
    
    # Loop through each object in the loaded .RData file
    for (object_name in loaded_objects) {
      # Get the actual data (using get() to retrieve it by name)
      data <- get(object_name)
      
      # Check if the object is a data frame (only save data frames to CSV)
      if (is.data.frame(data)) {
        # Define the output CSV file path, using the RData file name and object name
        csv_filename <- paste0(tools::file_path_sans_ext(basename(rdata_path)), "_", object_name, ".csv")
        csv_path <- file.path(output_folder, csv_filename)
        
        # Write the data frame to a CSV file
        write.csv(data, csv_path, row.names = FALSE)
        
        cat("Saved", object_name, "from", basename(rdata_path), "to", csv_path, "\n")
      } else {
        cat(object_name, "in", basename(rdata_path), "is not a data frame. Skipping.\n")
      }
    }
  }
}

# Example usage:
input_folder <- "C:/Users/netat/Documents/Movement_Ecology/Confidence_Filter/human_tagging_database/Michal_Handel"  # path to your .RData files
output_folder <- "C:/Users/netat/Documents/Movement_Ecology/Confidence_Filter/human_tagging_database/Michal_Handel" # path where you want to save CSVs

# Run the function
save_all_rdata_to_csv(input_folder, output_folder)