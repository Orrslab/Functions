library(raster)
library(sp)

# Function to add elevation from local DEM to your localizations data
calculate_elevation_per_location <- function(localizations_data, dem_file = "DEM_Harod.tif") {
  # Load the DEM raster
  DEM <- raster(dem_file)
  
  # Check CRS of DEM
  dem_crs <- crs(DEM)
  
  # Prepare points as SpatialPoints, assuming localizations_data has lon & lat columns
  points_sp <- SpatialPoints(localizations_data[, c("lon", "lat")], proj4string = CRS("+proj=longlat +datum=WGS84"))
  
  # If DEM CRS is different from WGS84, transform points to DEM CRS
  if (!compareCRS(dem_crs, CRS("+proj=longlat +datum=WGS84"))) {
    points_sp <- spTransform(points_sp, dem_crs)
  }
  
  # Extract elevation values for each point
  elevations <- raster::extract(DEM, points_sp)
  
  # Add elevation column to your dataframe
  localizations_data$DEM_elevation <- elevations
  
  return(localizations_data)
}