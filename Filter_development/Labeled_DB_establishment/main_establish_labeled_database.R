
library(readxl)

source(file.path(getwd(), "Filter_development/Labeled_DB_establishment/process_and_combine_species_data.R"))
source(file.path(getwd(), "Filter_development/Labeled_DB_establishment/get_metadata_per_species.R"))
source(file.path(getwd(), "Filter_development/Labeled_DB_establishment/combine_all_species_data.R"))
source(file.path(getwd(), "Filter_development/Labeled_DB_establishment/Data_visualization/create_maps_of_all_sqlite_files_in_DB.R"))

#' Main Pipeline: Establish the Labeled Data Database
#'
#' This function orchestrates the entire labeled data database establishment process
#' by running all major steps in sequence. It reads metadata, processes per-species data,
#' combines them, generates summary metadata, and creates visual maps.
#'
#' @param config A named list containing configuration parameters, including:
#'   \itemize{
#'     \item \code{paths$path_to_db} — Path to the main database directory.
#'     \item \code{paths$Labeled_data_metadata_path} — Path to the Excel metadata file.
#'   }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Reads the Excel metadata file specified in \code{config$paths$Labeled_data_metadata_path}.
#'   \item Creates a folder (\code{Combined_species_data}) for storing per-species combined data if it does not exist.
#'   \item Loops over each species in the metadata and calls \code{\link{process_and_combine_species_data}}.
#'   \item Calls \code{\link{get_metadata_per_species}} to generate aggregated metadata across species.
#'   \item Calls \code{\link{combine_all_species_data}} to merge all species data into a single SQLite database.
#'   \item Extracts unique species IDs and calls \code{\link{create_maps_of_all_sqlite_files_in_DB}} to create maps.
#' }
#'
#' @return This function does not return a value; it generates files and visualizations as side effects.
#'
#' @note
#' Requires that the helper scripts:
#' \itemize{
#'   \item \code{process_and_combine_species_data.R}
#'   \item \code{get_metadata_per_species.R}
#'   \item \code{combine_all_species_data.R}
#'   \item \code{create_maps_of_all_sqlite_files_in_DB.R}
#' }
#' are sourced before running.
#'
#' @examples
#' \dontrun{
#' config <- list(
#'   paths = list(
#'     path_to_db = "C:/path/to/db",
#'     Labeled_data_metadata_path = "C:/path/to/Labeled_data_metadata.xlsx"
#'   )
#' )
#' main_establish_labeled_database(config)
#' }
#'
#' @import readxl
#' @export
main_establish_labeled_database <- function(config) {
  
  # Read the Excel config table of the labeled data metadata
  labeled_data_metadata <- read_excel(config$paths$Labeled_data_metadata_path, sheet = "Sheet1")
  
  # Folder in which the combined data of each species should be saved
  combined_species_data_folder <- file.path(config$paths$path_to_db, "Combined_species_data")
  if (!dir.exists(combined_species_data_folder)) {
    dir.create(combined_species_data_folder, recursive = TRUE)
  }
  
  # Loop over all species
  for(i in 1:nrow(labeled_data_metadata)) {
    process_and_combine_species_data(
      species_id     = labeled_data_metadata$Species_id[i],
      reviewer_name  = labeled_data_metadata$Reviewer_name[i],
      data_source    = labeled_data_metadata$Data_source[i],
      filter_applied = labeled_data_metadata$Filter_applied[i],
      plot_resolution = labeled_data_metadata$Time_range_plot_resolution[i],
      path_to_db     = config$paths$path_to_db,
      combined_species_data_folder = combined_species_data_folder
    )
  }

  # Generate the metadata per species
  get_metadata_per_species(combined_species_data_folder)

  # Combine all the data from the species' data files to one file and save it
  combine_all_species_data(config$paths$path_to_db)
  
  # Create interactive maps of the data segments from the Visual Filter App
  species_id_column <- unique(labeled_data_metadata$Species_id)
  create_maps_of_all_sqlite_files_in_DB(config$paths$path_to_db, species_id_column)
  
}

