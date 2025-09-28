
library(RSQLite)
library(htmlwidgets)
library(webshot2)

source(file.path(getwd(), "create_filename_without_extension.R"))
source(file.path(getwd(), "Build_atlasRF/Labeled_DB_establishment/Data_visualization/plot_visual_filter_data.R"))
source(file.path(getwd(), "load_atlas_data_from_sqlite.R"))

#' Create Maps of All SQLite Files in the Database
#'
#' This function loops over a set of species IDs and generates interactive and static maps
#' from annotated SQLite files containing ATLAS localization data. For each tag in each species,
#' the function loads the data, creates a map visualization, and saves it in both HTML and PNG formats.
#'
#' @param path_to_db Character string. Path to the main database directory containing species subfolders.
#' @param species_id_column Character vector. Species IDs to process (typically from a metadata table column).
#'
#' @details
#' The function performs the following steps for each species:
#' \enumerate{
#'   \item Creates an output directory (`Data_maps`) inside \code{path_to_db} if it does not exist.
#'   \item Reads the species-specific \code{<species_id>_files_metadata.csv} file containing tag IDs, start times, and end times.
#'   \item For each tag, constructs the expected annotated SQLite filename.
#'   \item Loads the \code{LOCALIZATIONS} table from the SQLite file.
#'   \item Generates a map visualization using \code{\link{plot_visual_filter_data}}.
#'   \item Saves the map as both an HTML widget and a PNG image.
#' }
#'
#' The HTML maps are saved with \code{saveWidget} for interactive viewing in a browser.
#' The PNG maps are generated from the HTML files using the \pkg{webshot2} package.
#'
#' @return This function does not return a value; it generates files as a side effect.
#'
#' @note
#' The function requires the helper scripts:
#' \itemize{
#'   \item \code{create_filename_without_extension.R}
#'   \item \code{plot_visual_filter_data.R}
#'   \item \code{load_atlas_data_from_sqlite.R}
#' }
#' Ensure these scripts are sourced before running.
#'
#' @examples
#' \dontrun{
#' create_maps_of_all_sqlite_files_in_DB(
#'   path_to_db = "C:/path/to/db",
#'   species_id_column = c("SP1", "SP2")
#' )
#' }
#'
#' @import RSQLite
#' @import htmlwidgets
#' @import webshot2
#' @export
create_maps_of_all_sqlite_files_in_DB <- function(path_to_db, species_id_column) {
  
  # Create the output folder if it does not exist
  folder_to_save_the_maps <- file.path(path_to_db, "Data_maps")
  if (!dir.exists(folder_to_save_the_maps )) {
    dir.create(folder_to_save_the_maps , recursive = TRUE)
  }
  
  # Loop over the species and create the maps
  for (species_id in species_id_column) {
    
    message(paste("Generating maps of the", species_id, "data in all sqlite files in the species folder"))
    
    # Open the files metadata of the species sqlie files
    path_to_species_folder <- file.path(path_to_db, species_id)
    files_metadata <- read.csv(file.path(path_to_species_folder, paste0(species_id, "_files_metadata.csv")))
    
    # Get the tag number, start and end times of all files 
    tag_numbers <- files_metadata$TAG
    start_times <- files_metadata$Start_time
    end_times <- files_metadata$End_time
    
    print(species_id)
    
    for (i in seq_along(tag_numbers)) {
      
      tag_number <- tag_numbers[i]
      start_time <- start_times[i]
      end_time <- end_times[i]
      
      print(i)
      
      # Get the file name and full path of the sqlite file with the data for the map
      base_file_name <- create_filename_without_extension(
        animal_name_code = species_id, 
        tag_numbers = tag_number, 
        start_time = start_time, 
        end_time = end_time)
      
      # Append "_annotated" and sqlite extension, and create the path
      path_to_sqlite_file <- file.path(path_to_species_folder, paste0(base_file_name,"_annotated.sqlite"))
      
      print(path_to_sqlite_file)
      
      # Load LOCALIZATIONS data from the sqlite file
      atlas_data <- load_atlas_data_from_sqlite(path_to_sqlite_file)
      
      location_data <- atlas_data$LOCALIZATIONS
      
      # Plot the location data on a map and save the map
      map <- plot_visual_filter_data(data = location_data, 
                                     color_valid_points = "#5D3A9B",
                                     color_outliers = "#E66100",
                                     color_uncertain_points = "#FFB000")
      
      # Define the base file name to which the map should be saved
      map_filename <- base_file_name
      
      folder_to_save_the_maps_species <- file.path(folder_to_save_the_maps, species_id)
      # Create the saving folder if it does not exist
      if (!dir.exists(folder_to_save_the_maps_species)) {
        dir.create(folder_to_save_the_maps_species, recursive = TRUE)
      }
      
      # Save the map as HTML
      html_file <- file.path(folder_to_save_the_maps_species, paste0(map_filename, ".html"))
      saveWidget(map, file = html_file, selfcontained = FALSE)
      
      # save the map as PNG
      # Use webshot to convert HTML to PNG
      webshot2::webshot(html_file, 
                        file = file.path(folder_to_save_the_maps_species, paste0(map_filename, ".png")), 
                        vwidth = 2000, vheight = 1500, cliprect = "viewport")
      
    }
    
  }
  
}

