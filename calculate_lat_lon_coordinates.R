library(sf)

#' @title Calculate Latitude and Longitude Coordinates
#'
#' @description
#' Converts planar X/Y coordinates from a specified CRS into geographic latitude
#' and longitude (WGS84, EPSG:4326). The function preserves the original X/Y
#' values, appends transformed latitude and longitude columns, and returns the
#' data as an `sf` object with geographic coordinates as geometry.
#'
#' @param localization_data A data.frame or tibble containing at least two columns:
#'   \code{X} and \code{Y}, representing spatial coordinates in the specified CRS.
#' @param data_crs An integer or \code{crs} object specifying the coordinate
#'   reference system (CRS) of the input data, e.g. \code{2039} for Israeli TM Grid.
#'
#' @return An `sf` object containing:
#' \itemize{
#'   \item Original data columns.
#'   \item \code{lat}, \code{lon} columns with geographic coordinates (EPSG:4326).
#'   \item Geometry set to geographic coordinates (EPSG:4326).
#'   \item Original X and Y preserved as \code{X} and \code{Y}.
#' }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Stores original X/Y coordinates.
#'   \item Converts X/Y into an `sf` object in the specified \code{data_crs}.
#'   \item Transforms geometry to EPSG:4326 (WGS84 lat/lon).
#'   \item Extracts and appends latitude and longitude as columns.
#'   \item Resets geometry to use lat/lon.
#'   \item Restores original X and Y columns.
#' }
#'
#' @examples
#' \dontrun{
#' library(sf)
#' df <- data.frame(
#'   X = c(219529.6, 220000.0),
#'   Y = c(732997.2, 733500.0),
#'   id = 1:2
#' )
#'
#' # EPSG:2039 is the Israeli TM Grid
#' result <- calculate_lat_lon_coordinates(df, data_crs = 2039)
#' print(result)
#' }
#'
#' @export
#' 
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