
library(dplyr)

#' Generate Metadata per Species
#'
#' This function aggregates metadata across tags to the species level, 
#' using input metadata from individual tags and the species labeling metadata.
#' It computes summary statistics (number of tags, total records, and time span) 
#' for each species, adds species names, and writes the result to a CSV file.
#'
#' @param metadata_per_tag Data frame.  
#'   Metadata aggregated at the tag level. Must contain columns:
#'   \code{Species_id}, \code{TAG}, \code{Start_time}, \code{End_time}, 
#'   and \code{Num_records}.
#' 
#' @param labeling_metadata Data frame.  
#'   Species labeling metadata. Must contain columns \code{Species_id} and 
#'   \code{Species_name}, which are merged into the output.
#' 
#' @param species_metadata_file_path Character string.  
#'   Path where the resulting CSV file \code{metadata_per_species.csv} 
#'   will be written.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Converts \code{Start_time} and \code{End_time} columns to POSIXct (UTC).
#'   \item Groups data by \code{Species_id} and computes:
#'     \itemize{
#'       \item \code{Num_tags} — Number of unique tags.
#'       \item \code{Start_time} — Earliest start time.
#'       \item \code{End_time} — Latest end time.
#'       \item \code{Num_records} — Total number of records.
#'     }
#'   \item Merges species names from \code{labeling_metadata}.
#'   \item Reorders columns to place \code{Species_name} directly after \code{Species_id}.
#'   \item Saves the resulting table as a CSV file.
#' }
#'
#' @return 
#' This function is called for its side effects.  
#' It saves the file specified by \code{species_metadata_file_path} and prints 
#' a message with the file path.  
#' No value is returned.
#'
#' @examples
#' \dontrun{
#' metadata_per_tag <- read.csv("path/to/metadata_per_tag.csv")
#' labeling_metadata <- read.csv("path/to/labeling_metadata.csv")
#' get_metadata_per_species(
#'   metadata_per_tag,
#'   labeling_metadata,
#'   "path/to/metadata_per_species.csv"
#' )
#' }
#'
#' @import dplyr
#' @export
get_metadata_per_species <- function(metadata_per_tag,
                                     labeling_metadata,
                                     species_metadata_file_path) {
  
    # Convert the Start and End times to POSIXct format
    metadata_per_tag <- metadata_per_tag %>%
      mutate(
        Start_time = as.POSIXct(Start_time, tz = "UTC"),
        End_time = as.POSIXct(End_time, tz = "UTC")
      )
    
    # Summarize metadata per species
    metadata_per_species <- metadata_per_tag %>%
      group_by(Species_id) %>%
      summarise(
        Num_tags = n_distinct(TAG),
        Start_time = min(Start_time, na.rm = TRUE),
        End_time = max(End_time, na.rm = TRUE),
        Num_records = sum(Num_records, na.rm = TRUE)
      ) %>%
      ungroup()
    
    # Add the species names to the species metadata
    metadata_per_species <- merge(
      metadata_per_species,
      labeling_metadata[, c("Species_id", "Species_name")],
      by = "Species_id",
      all.x = TRUE
    )
    
    # Reorder columns to put Species_name right after Species_id
    id_col <- which(names(metadata_per_species) == "Species_id")
    metadata_per_species <- metadata_per_species[, 
                                                 c(1:id_col, ncol(metadata_per_species), (id_col+1):(ncol(metadata_per_species)-1))]
    
    # Save the dataframe as metadata_per_species.csv
    write.csv(metadata_per_species, species_metadata_file_path, row.names = FALSE)
    
    message("Metadata per species saved to: ", species_metadata_file_path)
}

