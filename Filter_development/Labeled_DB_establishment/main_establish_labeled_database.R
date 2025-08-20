
library(readxl)

source(file.path(getwd(), "Filter_development/Labeled_DB_establishment/process_and_combine_species_data.R"))
source(file.path(getwd(), "Filter_development/Labeled_DB_establishment/get_metadata_per_species.R"))
source(file.path(getwd(), "Filter_development/Labeled_DB_establishment/combine_all_species_data.R"))
source(file.path(getwd(), "Filter_development/Labeled_DB_establishment/Data_visualization/create_maps_of_all_sqlite_files_in_DB.R"))

#' Main Pipeline: Establish the Labeled Data Database
#'
#' This function orchestrates the entire labeled data database establishment process
#' by running all major steps in sequence. It reads labeling metadata, processes
#' per-species data, combines them, generates summary metadata (per tag and per species),
#' and creates visual maps.
#'
#' @param config A named list containing configuration parameters, including:
#'   \itemize{
#'     \item \code{paths$path_to_db} — Path to the main database directory.
#'     \item \code{paths$labeling_info_path} — Path to the Excel file with the labeling
#'           information of the segments of each species.
#'     \item \code{paths$combined_species_data_folder} — Path to the folder where
#'           combined data for each species will be saved.
#'     \item \code{paths$tags_metadata_file_path} — Path to the CSV file containing
#'           metadata per tag (must exist).
#'     \item \code{paths$species_metadata_file_path} — Path where the generated
#'           species-level metadata file will be saved.
#'   }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Reads the Excel labeling metadata file specified in \code{config$paths$labeling_info_path}.
#'   \item Creates the folder specified in \code{config$paths$combined_species_data_folder}
#'         if it does not exist.
#'   \item Loops over each species in the labeling metadata and calls
#'         \code{\link{process_and_combine_species_data}} to process and save its combined data.
#'   \item Reads the tag-level metadata CSV file specified in
#'         \code{config$paths$tags_metadata_file_path}; stops with an error if the file does not exist.
#'   \item Calls \code{\link{get_metadata_per_species}} to generate aggregated metadata per species
#'         (saved to \code{config$paths$species_metadata_file_path}).
#'   \item Calls \code{\link{combine_all_species_data}} to merge all species data into a single SQLite database.
#'   \item Extracts unique species IDs and calls \code{\link{create_maps_of_all_sqlite_files_in_DB}}
#'         to create interactive maps of the data segments.
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
#'     labeling_info_path = "C:/path/to/Labeled_data_metadata.xlsx",
#'     combined_species_data_folder = "C:/path/to/Combined_species_data",
#'     tags_metadata_file_path = "C:/path/to/tags_metadata.csv",
#'     species_metadata_file_path = "C:/path/to/Species_metadata.xlsx"
#'   )
#' )
#' main_establish_labeled_database(config)
#' }
#'
#' @import readxl
#' @export
main_establish_labeled_database <- function(config) {
  
  # Read the Excel config table of the labeled data metadata
  labeling_metadata <- read_excel(config$paths$labeling_info_path, sheet = "Sheet1")
  
  # Folder in which the combined data of each species should be saved
  if (!dir.exists(config$paths$combined_species_data_folder)) {
    dir.create(config$paths$combined_species_data_folder, recursive = TRUE)
  }
  
  # Loop over all species
  for(i in 1:nrow(labeling_metadata)) {
    print(labeling_metadata$Species_id[i])
    process_and_combine_species_data(
      species_id     = labeling_metadata$Species_id[i],
      reviewer_name  = labeling_metadata$Reviewer_name[i],
      data_source    = labeling_metadata$Data_source[i],
      filter_applied = labeling_metadata$Filter_applied[i],
      plot_resolution = labeling_metadata$Time_range_plot_resolution[i],
      path_to_db     = config$paths$path_to_db,
      combined_species_data_folder = config$paths$combined_species_data_folder
    )
  }
  
  # Upload metadata per tag
  if (file.exists(config$paths$tags_metadata_file_path)) {
    metadata_per_tag <- read.csv(config$paths$tags_metadata_file_path)
  } else {
    stop(paste("Error: The file does not exist at path:", 
               config$paths$tags_metadata_file_path))
  }

  # Generate the metadata per species
  get_metadata_per_species(metadata_per_tag = metadata_per_tag,
                           labeling_metadata = labeling_metadata,
                           species_metadata_file_path = config$paths$species_metadata_file_path)

  # Combine all the data from the species' data files to one file and save it
  combine_all_species_data(config$paths$path_to_db,
                           config$paths$combined_species_data_folder)
  
  # Create interactive maps of the data segments from the Visual Filter App
  species_id_column <- unique(labeling_metadata$Species_id)
  create_maps_of_all_sqlite_files_in_DB(config$paths$path_to_db, species_id_column)
  
}

