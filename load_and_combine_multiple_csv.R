# Load necessary library
library(dplyr)

# Function to load and combine CSV files from a folder
load_and_combine_multiple_csv <- function(folder_path) {
  # Check if the folder exists
  if (!dir.exists(folder_path)) {
    stop("The specified folder does not exist.")
  }
  
  # List all CSV files in the folder
  csv_files <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)
  
  # Check if any CSV files were found
  if (length(csv_files) == 0) {
    stop("No CSV files found in the specified folder.")
  }
  
  # Read and combine all CSV files into a single dataframe
  combined_data <- csv_files %>%
    lapply(read.csv) %>%   # Read each CSV file
    bind_rows()            # Combine them into one dataframe
  
  return(combined_data)
}

# Example usage
# folder_path <- "path/to/your/csv/folder"
# data <- load_and_combine_multiple_csv(folder_path)

# View the first few rows
# head(data)

# Check the structure of the data
# str(data)