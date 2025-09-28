# Label outliers in data of nesting barn owls by looking at the distance from the nest box

library(geosphere)

source(file.path(getwd(), "Build_atlasRF/Nesting_barn_owls/add_nesting_box_coordinates.R"))

label_outleirs_by_dist_from_nest_box <- function(localization_data, 
                                                 nesting_bo_metadata, 
                                                 threshold_dist_from_nest_m) {
  
  # Add the nesting box coordinates to each location point
  localization_data_with_box_coords <- add_nesting_box_coordinates(localization_data, nesting_bo_metadata)
  
  # Calculate the distance between each location and its' corresponding nesting box
  localization_data_with_box_coords$dist_from_nesting_box <- geosphere::distHaversine(
    matrix(c(localization_data_with_box_coords$lon, localization_data_with_box_coords$lat), ncol = 2),
    matrix(c(localization_data_with_box_coords$nesting_box_lon, localization_data_with_box_coords$nesting_box_lat), ncol = 2)
  )
  
  # Add column outliers, which is 1 if the distance to nest box is larger that the threshold, and 0 elsewhere
  localization_data_with_box_coords$Outliers <- ifelse(
    localization_data_with_box_coords$dist_from_nesting_box > threshold_dist_from_nest_m, 1, 0
  )
  
  # Remove nesting_box_lat, nesting_box_lon and dist_from_nesting_box columns
  localization_data_with_box_coords <- localization_data_with_box_coords[, !names(localization_data_with_box_coords) %in% c("nesting_box_lat", "nesting_box_lon", "dist_from_nesting_box", "Nesting_box_id", "box_count")]
  
  return(localization_data_with_box_coords)
  
}