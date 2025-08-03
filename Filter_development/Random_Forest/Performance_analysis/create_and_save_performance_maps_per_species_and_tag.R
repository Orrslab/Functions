
library(dplyr)
library(htmlwidgets)

source(file.path(getwd(), "Mapping_tools/interactive_map_single_atlas_dataset_color_by_vector.R"))

# Wrapper function to create and save maps per species and TAG
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