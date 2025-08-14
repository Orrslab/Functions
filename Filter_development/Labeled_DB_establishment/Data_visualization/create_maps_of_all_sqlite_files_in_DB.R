
# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data, not

library(RSQLite)
library(htmlwidgets)
library(webshot2)

source(paste0(getwd(), "/ATLAS_data_retrieval/config.R"))
source(paste0(getwd(), "/create_sqlite_filepath.R"))
source(paste0(getwd(), "/Filter_development/Visual_Filter_DB_establishment/Data_visualization/plot_visual_filter_data.R"))
source(paste0(getwd(), "/load_atlas_data_from_sqlite.R"))

###   USER'S INPUT BEGIN ###
species_id <- "BO"

# Specify the necessary paths
path_to_db <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Annotated_data_DB/Visual_Filter_DB"
path_to_species_metadata <- paste0(path_to_db, "/Species_metadata.xlsx")
folder_to_save_the_maps <- paste0(path_to_db, "/Data_maps")

# Get the ATLAS database credentials from the configuration file
harod_db_credentials <- list(
  system_name = system_name_harod,         # System name
  db_username = db_username_harod,         # username
  db_pass = db_pass_harod,                 # password
  db_host_ip = db_host_ip_harod,           # host IP address
  db_port_number = db_port_number_harod,   # port number
  db_name = db_name_harod                  # database name
)

### USER'S INPUT END

# Create the output folder if it does not exist
if (!dir.exists(folder_to_save_the_maps )) {
  dir.create(folder_to_save_the_maps , recursive = TRUE)
}

message(paste("Generating maps of the", species_id, "data in all sqlite files in the species folder"))

# Open the files metadata of the species sqlie files
path_to_species_folder <- paste0(path_to_db, "/", species_id)
files_metadata <- read.csv(paste0(path_to_species_folder, "/", species_id, "_files_metadata.csv"))

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
  path_to_sqlite_file <- create_sqlite_filepath(animal_name_code = species_id, 
                                                tag_numbers = tag_number, 
                                                start_time = start_time, 
                                                end_time = end_time, 
                                                folder_path_to_sqlite_files = path_to_species_folder)
  
  # Append "_annotated" before ".sqlite"
  path_to_sqlite_file <- sub("\\.sqlite$", "_annotated.sqlite", path_to_sqlite_file)
  
  print(path_to_sqlite_file)
  
  # Load LOCALIZATIONS data from the sqlite file
  atlas_data <- load_atlas_data_from_sqlite(path_to_sqlite_file)
    
  location_data <- atlas_data$LOCALIZATIONS
  
  # Plot the location data on a map and save the map
  map <- plot_visual_filter_data(data = location_data, 
                                 color_valid_points = "#5D3A9B",
                                 color_outliers = "#E66100",
                                 color_uncertain_points = "#FFB000")
  
  # Define the file name to which the map should be saved
  map_filename <- paste0(sub("_annotated\\.sqlite$", "", basename(path_to_sqlite_file)))
  
  folder_to_save_the_maps_species <- paste0(folder_to_save_the_maps, "/", species_id)
  # Create the saving folder if it does not exist
  if (!dir.exists(folder_to_save_the_maps_species)) {
    dir.create(folder_to_save_the_maps_species, recursive = TRUE)
  }
  
  # Save the map as HTML
  html_file <- paste0(folder_to_save_the_maps_species, "/", map_filename, ".html")
  saveWidget(map, file = html_file, selfcontained = FALSE)
  
  # save the map as PNG
  # Use webshot to convert HTML to PNG
  webshot2::webshot(html_file, 
                    file = paste0(folder_to_save_the_maps_species, "/", map_filename, ".png"), 
                    vwidth = 2000, vheight = 1500, cliprect = "viewport")

}
