library(sf)

calculate_lat_lon_coordinates <- function(localization_data, data_crs) {
  
  # Make sure that X and Y columns are in the data
  
  # Keep original X and Y before converting
  localization_data$X_original <- localization_data$X
  localization_data$Y_original <- localization_data$Y
  
  # Convert X and Y to an sf object in EPSG:2039
  localization_data_sf <- st_as_sf(localization_data, coords = c("X", "Y"), crs = data_crs)
  
  # Transform the geometry column to WGS84 (lat/lon)
  localization_data_sf <- st_transform(localization_data_sf, crs = 4326)
  
  # Extract transformed coordinates (lat/lon)
  localization_data_sf$lat <- st_coordinates(localization_data_sf)[, 2]  # Latitude
  localization_data_sf$lon <- st_coordinates(localization_data_sf)[, 1]  # Longitude
  
  # Convert back to an sf object using lat/lon as geometry
  localization_data_sf <- st_as_sf(localization_data_sf, coords = c("lon", "lat"), crs = 4326)
  
  # Add back the original X and Y columns
  st_geometry(localization_data_sf) <- "geometry"  # Ensure it remains an sf object
  localization_data_sf$X <- localization_data$X_original
  localization_data_sf$Y <- localization_data$Y_original
  
  # Delete the columns X_original and Y_original
  localization_data_sf$X_original <- NULL
  localization_data_sf$Y_original <- NULL
  
  return(localization_data_sf)
  
}