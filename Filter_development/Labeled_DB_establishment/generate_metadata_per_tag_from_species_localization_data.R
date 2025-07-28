#' Generate and update metadata summary per TAG for a given species
#'
#' This function generates a metadata summary per `TAG` from a localization dataset, 
#' including start and end times and the number of records per tag. It then appends or updates 
#' this metadata in a `metadata_per_tag.csv` file located in the species data folder.
#'
#' The function assumes that species-level variables such as `species_id`, `reviewer_name`, 
#' `data_source`, and `filter_applied` are available in the global environment.
#'
#' @param localization_data A data frame containing localization records. Must include at least `TAG` and `dateTime` columns.
#' @param combined_species_data_folder A string specifying the path to the species data folder where the metadata CSV file will be stored.
#'
#' @return Writes (and optionally updates) a `metadata_per_tag.csv` file in the specified folder.
#' Also prints a message confirming that the metadata has been updated.
#'
#' @details The metadata per tag includes:
#' \itemize{
#'   \item \code{Species_ID} – The species identifier (from global variable \code{species_id}).
#'   \item \code{TAG} – The identifier for each tagged animal.
#'   \item \code{Start_time} – The first dateTime entry for each TAG.
#'   \item \code{End_time} – The last dateTime entry for each TAG.
#'   \item \code{Num_records} – The number of rows/records for each TAG.
#'   \item \code{Data_source}, \code{Reviewer}, \code{Filter_applied} – Additional metadata assumed to be defined in the global environment.
#' }
#'
#' If a `metadata_per_tag.csv` file already exists, the function removes all rows related to the same `Species_ID` and appends the new metadata.
#'
#' @examples
#' \dontrun{
#' species_id <- "Lapwing"
#' reviewer_name <- "NT"
#' data_source <- "ATLAS"
#' filter_applied <- "Visual Filter App"
#' generate_metadata_per_tag_from_species_localization_data(localization_data, "data/Lapwing")
#' }
#'
#' @export
#' 
generate_metadata_per_tag_from_species_localization_data <- function(localization_data,
                                                                      combined_species_data_folder) {
  
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
  
  # Add the Species ID, Reviewer and data source
  metadata_per_tag <- metadata_per_tag %>%
    mutate(
      Species_ID = species_id,    # Assuming species_id is a single value for all rows
      Reviewer = reviewer_name,         # Assuming Reviewer is a single value for all rows
      Data_source = data_source,
      Filter_applied = filter_applied
    )
  
  # Re-order the column names
  metadata_per_tag <- metadata_per_tag %>%
    dplyr::select(Species_ID, TAG, Start_time, End_time, Num_records, Data_source, Reviewer, Filter_applied)
  
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
      filter(Species_ID != species_id) %>%  # Keep all except the species being updated
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

