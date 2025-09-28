
source(file.path(getwd(), "Filter_development/Labeled_DB_establishment/get_metadata_from_all_sqlite_files_in_folder.R"))


#' Generate Metadata for Tag Files of a Species
#'
#' This function retrieves metadata from all SQLite files of labeled data
#' for a given species and appends species-level and data source information.  
#' It produces a standardized metadata table summarizing the tag files.
#'
#' @param path_to_species Character. Path to the folder containing the SQLite
#'   files with labeled data for the species.
#' @param species_id Character. Identifier of the species being processed.
#' @param data_source Character. Source of the data (e.g., project name or repository).
#'
#' @return A tibble (or dataframe) with metadata for all tag files of the species,
#'   containing the following columns:
#'   \itemize{
#'     \item \code{Species_ID} — Species identifier.
#'     \item \code{TAG} — Tag identifier.
#'     \item \code{Start_time} — Earliest timestamp in the data file.
#'     \item \code{End_time} — Latest timestamp in the data file.
#'     \item \code{Num_records} — Number of records in the file.
#'     \item \code{Data_source} — Source of the data.
#'   }
#'
#' @details
#' Internally, this function calls 
#' \code{\link{get_metadata_from_all_sqlite_files_in_folder}} 
#' to extract metadata from all SQLite files in the species folder and then 
#' appends the species ID and data source to each record.
#'
#' @import dplyr
#' @export
generate_metadata_for_tag_files <- function(path_to_species,
                                            species_id,
                                            data_source,
                                            reviewer_name,
                                            filter_applied) {
  
  # Retreive the metadata from all the sqlite files of the labeled data sent from the reviewers ###
  files_metadata <- get_metadata_from_all_sqlite_files_in_folder(path_to_species)
  
  # Add the Species ID, Reviewer's name and data source
  files_metadata <- files_metadata %>%
    mutate(
      Species_id = species_id,    # Assuming species_id is a single value for all rows
      Data_source = data_source,
      Reviewer = reviewer_name,
      Filter_applied = filter_applied
    )
  
  # Re-order the column names
  files_metadata <- files_metadata %>%
    dplyr::select(Species_id, TAG, Start_time, End_time, Num_records, Data_source, Reviewer, Filter_applied)
  
  return(files_metadata)
}
