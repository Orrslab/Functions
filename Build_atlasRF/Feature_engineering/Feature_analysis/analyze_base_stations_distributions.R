# Analyze the missed base stations, which were closer than the participating ones

# clean the data and set some preferences
rm(list=ls()) # clean history
options(digits = 14) # Makes sure long numbers are not abbreviated.
rm(list = setdiff(ls(), lsf.str())) # removes data

library(ggplot2)
library(data.table)

source(file.path(getwd(), "Build_atlasRF/Feature_engineering/load_data_with_features.R"))

# USER'S INPUT BEGIN

analysis_folder <- "C:/Users/netat/Documents/Movement_Ecology/Filter_development/Feature_Engineering/Features_analysis/Base_stations_distribution"

# tables_to_load <- c("LOCALIZATIONS", 
#                     "DETECTIONS", 
#                     "PARTICIPATING_BASE_STATIONS", 
#                     "MISSED_BASE_STATIONS", 
#                     "PROPERTIES")

tables_to_load <- c("LOCALIZATIONS")

# USER'S INPUT END

# Load the features data
features_data <- load_data_with_features(tables_to_load)

localization_data <- features_data$LOCALIZATIONS

# Make sure data is a data.table
localization_data <- as.data.table(localization_data)

##############################
### Comnfusion table- is in polygon versus Outliers

# Ensure both columns exist and are of the right type
localization_data <- localization_data[!is.na(is_loc_inside_bs_polygon) & !is.na(Outliers)]

# Create confusion-style table
confusion_table <- table(
  InsidePolygon = localization_data$is_loc_inside_bs_polygon,
  OutlierStatus = localization_data$Outliers
)

# Print the table
print(confusion_table)

###############################
### Histograms of the participating base stations' polygon area, per species.

# Remove rows with missing polygon area or species
plot_data <- localization_data[!is.na(bs_polygon_area) & !is.na(Species_id)]

# Loop over species and save histogram per species
unique_species <- unique(plot_data$Species_id)

for (species in unique_species) {
  species_data <- plot_data[Species_id == species]
  
  p <- ggplot(species_data, aes(x = bs_polygon_area)) +
    geom_histogram(binwidth = 1000000, fill = "skyblue", color = "black") +
    ggtitle(paste("Polygon Area Histogram - Species", species)) +
    xlab("Polygon Area (m²)") +
    ylab("Count") +
    theme_minimal()
  
  ggsave(
    filename = paste0(analysis_folder, "/bs_polygon_area_species_", species, ".png"),
    plot = p,
    width = 8,
    height = 5,
    bg = "white"
  )
}