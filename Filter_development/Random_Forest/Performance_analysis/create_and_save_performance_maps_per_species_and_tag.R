
library(dplyr)
library(htmlwidgets)

source(file.path(getwd(), "Mapping_tools/interactive_map_single_atlas_dataset_color_by_vector.R"))

#' Create and Save Interactive Performance Maps per Species and TAG
#'
#' This function generates interactive maps for each unique combination of species and TAG
#' in the provided dataset. Points are colored based on a specified column (e.g., outlier
#' classification or confusion category), and each map is saved as an HTML file.
#'
#' @param data A data frame containing at least the columns \code{Species_id}, \code{TAG},
#'   and the column specified in \code{color_column}. Each row represents a single data point.
#' @param color_column Name of the column used to determine the color of the points on the map.
#'   Default is \code{"Outliers"}.
#' @param output_dir Directory where the HTML maps will be saved. If the directory does not
#'   exist, it will be created. Default is \code{"ATLAS_maps"}.
#'
#' @details
#' The function performs the following steps:
#' 1. Creates the output directory if it does not exist.
#' 2. Identifies all unique combinations of \code{Species_id} and \code{TAG}.
#' 3. For each combination, subsets the data and generates an interactive map using
#'    \code{interactive_map_single_atlas_dataset_color_by_vector()}.
#' 4. Skips combinations with fewer than two data points.
#' 5. Saves each map as an HTML file named \code{Species_<Species_id>_TAG_<TAG>.html}.
#' 6. Prints progress messages in the console.
#'
#' @return
#' The function does not return an R object. All maps are saved as self-contained HTML
#' files in \code{output_dir}.
#'
#' @examples
#' \dontrun{
#' create_and_save_performance_maps_per_species_and_tag(
#'   data = test_data,
#'   color_column = "confusion_category",
#'   output_dir = "Performance_maps"
#' )
#' }
#'
#' @import dplyr
#' @import htmlwidgets
#' @export
create_and_save_performance_maps_per_species_and_tag <- function(data, 
                                 color_column = "Outliers", 
                                 output_dir = "ATLAS_maps") {
  
  # Create the output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Get unique combinations of Species_id and TAG
  combos <- data %>%
    distinct(Species_id, TAG) %>%
    arrange(Species_id, TAG)
  
  for (i in seq_len(nrow(combos))) {
    species <- combos$Species_id[i]
    tag <- combos$TAG[i]
    
    cat("Processing: Species =", species, ", TAG =", tag, "\n")
    
    # Subset data for the current species and TAG
    sub_data <- data %>% filter(Species_id == species, TAG == tag)
    
    # Skip if not enough data points
    if (nrow(sub_data) < 2) next
    
    # Create map
    map <- interactive_map_single_atlas_dataset_color_by_vector(
      dd = sub_data,
                        color_column = color_column,
                        legend_title = color_column)
    
    # Define output file name
    file_name <- sprintf("%s/Species_%s_TAG_%s.html", output_dir, species, tag)
    
    # Save as HTML
    saveWidget(map, file = file_name, selfcontained = TRUE)
  }
  
  cat("All maps saved in:", normalizePath(output_dir), "\n")
}