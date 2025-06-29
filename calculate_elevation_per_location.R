library(raster)
library(sp)

#' Add elevation data to localization points using a Digital Elevation Model (DEM)
#'
#' Extracts elevation values from a Digital Elevation Model (DEM) raster and adds them
#' to a localization data set, based on geographic coordinates (longitude and latitude).
#'
#' @param localizations_data A data frame containing localization points with columns named \code{lon} and \code{lat}
#' representing geographic coordinates in decimal degrees (WGS84).
#' @param dem_file A character string specifying the path to the DEM file (GeoTIFF or similar raster format). 
#' Default is \code{"DEM_Harod.tif"}. Such a file can be downloaded from the internet.
#' Here is how: https://www.youtube.com/watch?v=yYWdxExabHo&ab_channel=GeoDeltaLabs
#'
#' @return A data frame identical to \code{localizations_data}, with an additional column \code{DEM_elevation}
#' containing the elevation (in meters) extracted from the DEM.
#'
#' @details If the DEM uses a coordinate reference system (CRS) other than WGS84, the function will reproject
#' the points before extracting elevation values. The function uses the \pkg{raster} and \pkg{sp} packages.
#'
#' @examples
#' \dontrun{
#' locs <- data.frame(lon = c(35.2, 35.3), lat = c(32.5, 32.6))
#' locs_with_elev <- calculate_elevation_per_location(locs, dem_file = "DEM_Harod.tif")
#' }
#'
#' @importFrom raster raster extract crs
#' @importFrom sp SpatialPoints CRS spTransform
#' @export

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