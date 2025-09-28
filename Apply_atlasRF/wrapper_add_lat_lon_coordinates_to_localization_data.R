
source(file.path(getwd(), "calculate_lat_lon_coordinates.R"))

#' @title Add Latitude and Longitude Coordinates
#'
#' @description
#' Wrapper around \code{calculate_lat_lon_coordinates()} that adds latitude and
#' longitude (WGS84) to localization data and returns a plain data.frame.
#'
#' @param localization_data A data.frame with columns \code{X} and \code{Y}
#'   representing coordinates in the specified CRS.
#' @param data_crs Integer or \code{crs} object specifying the CRS of the input
#'   coordinates (e.g., \code{2039} for Israeli TM Grid).
#'
#' @return A data.frame with added \code{lat} and \code{lon} columns.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   X = c(219529.6, 220000.0),
#'   Y = c(732997.2, 733500.0)
#' )
#' wrapper_add_lat_lon_coordinates_to_localization_data(df, 2039)
#' }
#'
#' @export
#' 
wrapper_add_lat_lon_coordinates_to_localization_data <- function(localization_data, data_crs) {
  
  message("*** Adding latitude and longitude coordinates to the raw localization data. ***")
  
  # Calculate 'lat' and 'lon' coordinates
  localization_data <- calculate_lat_lon_coordinates(localization_data, data_crs)
  
  # Convert raw_localization_data from sf object back to a dataframe
  localization_data <- as.data.frame(localization_data)
  
  return(localization_data)
  
}

