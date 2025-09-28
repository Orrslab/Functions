#' Generate or Update Metadata Summary per TAG for a Species
#'
#' This function generates a metadata summary for each `TAG` from a species'
#' localization dataset, including start and end times and the number of records.
#' It then appends or updates this metadata in a `metadata_per_tag.csv` file located
#' in the species data folder.
#'
#' @param localization_data Data frame containing localization records. Must include
#'   at least the `TAG` and `dateTime` columns.
#' @param combined_species_data_folder Character. Path to the species data folder
#'   where the metadata CSV file will be stored or updated.
#' @param species_id Character. Identifier of the species.
#' @param data_source Character. Source of the data (e.g., project name or repository).
#'
#' @return Returns the updated metadata dataframe invisibly.  
#'   As a side effect, writes (and optionally updates) a `metadata_per_tag.csv` file
#'   in the specified folder and prints a confirmation message.
#'
#' @details
#' The metadata per tag includes:
#' \itemize{
#'   \item \code{Species_id} — Species identifier.
#'   \item \code{TAG} — Tag identifier.
#'   \item \code{Start_time} — First timestamp for the tag.
#'   \item \code{End_time} — Last timestamp for the tag.
#'   \item \code{Num_records} — Number of records for the tag.
#'   \item \code{Data_source} — Source of the data.
#' }
#'
#' If a `metadata_per_tag.csv` file already exists, the function removes all rows
#' related to the same `Species_id` and appends the new metadata. Otherwise, it
#' creates a new file.
#'
#' @examples
#' \dontrun{
#' species_id <- "Lapwing"
#' data_source <- "ATLAS"
#' generate_metadata_per_tag_from_species_localization_data(localization_data,
#'                                                          "data/Lapwing",
#'                                                          species_id,
#'                                                          data_source)
#' }
#'
#' @import dplyr
#' @export 
generate_metadata_per_tag_from_species_localization_data <- function(localization_data,
                                                                     combined_species_data_folder,
                                                                     species_id,
                                                                     data_source) {
  
  # Create a dataframe with the metadata of the file
  metadata_per_tag <- localization_data %>%
    group_by(TAG) %>%
    arrange(dateTime) %>%
    summarise(
      Start_time = first(dateTime),         # The first time entry for each tag
      End_time = last(dateTime),            # The last time entry for each tag
      Num_records = n()                     # The count of rows for each TAG
    ) %>%
    ungroup()
  
  # Add the Species ID and data source
  metadata_per_tag <- metadata_per_tag %>%
    mutate(
      Species_id = species_id,    # Assuming species_id is a single value for all rows
      Data_source = data_source
    )
  
  # Re-order the column names
  metadata_per_tag <- metadata_per_tag %>%
    dplyr::select(Species_id, TAG, Start_time, End_time, Num_records, Data_source)
  
  # If the metadata file exists add the current metadata to the file and replace the relevant row if exists
  metadata_file_path <- file.path(combined_species_data_folder, "metadata_per_tag.csv")
  if (file.exists(metadata_file_path)) {
    existing_metadata_per_tag <- read.csv(metadata_file_path)
    
    # Ensure datetime columns have the same format as new_species_metadata (POSIXct)
    existing_metadata_per_tag <- existing_metadata_per_tag %>%
      mutate(
        Start_time = as.POSIXct(Start_time, tz = "UTC"),
        End_time = as.POSIXct(End_time, tz = "UTC")
      )
    
    # Remove existing records of the same Species_ID before adding new ones
    updated_metadata_per_tag <- existing_metadata_per_tag %>%
      filter(Species_id != species_id) %>%  # Keep all except the species being updated
      bind_rows(metadata_per_tag)  # Add the new data
    
  } else {
    # Create new metadata file and insert the current metadata
    updated_metadata_per_tag <- metadata_per_tag
  }
  
  # Save updated metadata
  write.csv(updated_metadata_per_tag, metadata_file_path, row.names = FALSE)
  message("Metadata per Tag file updated successfully!")
  
  return(updated_metadata_per_tag)
  
}

