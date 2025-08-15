
library(dplyr)

#' Get metadata per species from metadata_per_tag.csv
#'
#' This function reads the `metadata_per_tag.csv` file from the specified
#' combined species data folder, processes the start and end times, 
#' aggregates metadata at the species level, and writes the results to
#' `metadata_per_species.csv`.
#'
#' @param combined_species_data_folder Character string.  
#'   Path to the folder containing the `metadata_per_tag.csv` file and where 
#'   the output `metadata_per_species.csv` will be saved.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Reads the `metadata_per_tag.csv` file.
#'   \item Converts `Start_time` and `End_time` columns to POSIXct in UTC.
#'   \item Groups data by `Species_ID` and calculates:
#'     \itemize{
#'       \item Number of unique tags (`Num_tags`)
#'       \item Earliest start time (`Start_time`)
#'       \item Latest end time (`End_time`)
#'       \item Total number of records (`Num_records`)
#'     }
#'   \item Saves the aggregated table as `metadata_per_species.csv`.
#' }
#'
#' @return
#' This function is called for its side effects.  
#' It saves `metadata_per_species.csv` to `combined_species_data_folder` 
#' and prints a message with the file path.  
#' No value is returned.
#'
#' @examples
#' \dontrun{
#' get_metadata_per_species("path/to/combined_species_data")
#' }
#'
#' @import dplyr
#' @export
get_metadata_per_species <- function(combined_species_data_folder) {
  
  # Open the metadata per tag file
  metadata_file_path <- file.path(combined_species_data_folder, "metadata_per_tag.csv")
  if (file.exists(metadata_file_path)) {
    metadata_per_tag <- read.csv(metadata_file_path)
    
    # Convert the Start and End times to POSIXct format
    metadata_per_tag <- metadata_per_tag %>%
      mutate(
        Start_time = as.POSIXct(Start_time, tz = "UTC"),
        End_time = as.POSIXct(End_time, tz = "UTC")
      )
    
    # Summarize metadata per species
    metadata_per_species <- metadata_per_tag %>%
      group_by(Species_ID) %>%
      summarise(
        Num_tags = n_distinct(TAG),
        Start_time = min(Start_time, na.rm = TRUE),
        End_time = max(End_time, na.rm = TRUE),
        Num_records = sum(Num_records, na.rm = TRUE)
      ) %>%
      ungroup()
    
    # Save the dataframe as metadata_per_species.csv
    output_file_path <- file.path(combined_species_data_folder, "metadata_per_species.csv")
    write.csv(metadata_per_species, output_file_path, row.names = FALSE)
    
    message("Metadata per species saved to: ", output_file_path)
    
  } else {
    stop(paste("Error: File", metadata_file_path, "does not exist."))
  }
  
}

