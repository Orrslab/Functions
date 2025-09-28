library(dplyr)

source(file.path(getwd(), "atlas_metrics.R"))
source(file.path(getwd(), "calculate_elevation_per_location.R"))

#' Calculate point-based movement, location, and error-based features
#'
#' This function computes a set of point-based features for each localization 
#' record to characterize movement dynamics, positional uncertainty, and 
#' topography. These features are intended to support the identification 
#' of biologically implausible movements and assist in feature engineering 
#' for filtering or modeling with the atlasRF framework.
#'
#' @param localization_data A data frame containing ATLAS localization records. 
#' Must include the following columns:
#' \itemize{
#'   \item `TAG` – the tag identifier
#'   \item `TIME` – timestamp of the localization
#'   \item `X`, `Y` – projected coordinates
#'   \item `VARX`, `VARY`, `COVXY` – error variance–covariance estimates
#'   \item `lat`, `lon` – geographic coordinates
#' }
#'
#' @return A data frame identical to the input \code{localization_data}, 
#' but with the following additional columns:
#' \itemize{
#'   \item `time_diff_sec` – time difference from previous fix [s]
#'   \item `dist_m` – distance from previous fix [m]
#'   \item `distance_triangle_ratio` – ratio of consecutive distances, 
#'         useful for detecting geometric outliers
#'   \item `Speed_m_s` – speed between consecutive points [m/s]
#'   \item `Acceleration_m_s_2` – acceleration [m/s²]
#'   \item `STD` – standard deviation of ATLAS localization error, 
#'         derived from variance–covariance terms
#'   \item `cos_turning_angle` – cosine of the turning angle at each point
#'   \item `turning_angle` – turning angle between consecutive movement segments [rad]
#'   \item `Elevation` – elevation above sea level from a Digital Elevation Model 
#'         (requires \code{DEM_Harod.tif} in the working directory)
#' }
#'
#' @details 
#' The function integrates several metrics:
#' \itemize{
#'   \item Movement features: distance, speed, acceleration, turning angles.
#'   \item Error features: standard deviation of ATLAS localization uncertainty.
#'   \item Spatial features: elevation extracted from a DEM.
#' }
#'
#' @import dplyr
#' @export
calculate_point_based_features <- function(localization_data) {
  
  message("Calculating point-based features.")
  
  # Verify that the data frame has the columns TAG and time_diff_sec
  if (!"TAG" %in% colnames(localization_data)) {
    stop("Error: 'TAG' column is missing from the dataframe.")
  }
  
  if (!all(c("VARX", "VARY", "COVXY") %in% colnames(localization_data))) {
    stop("Error: 'VARX', 'VARY' and 'COVXY' columns are missing from the dataframe.")
  }
  
  if (!all(c("lat", "lon") %in% colnames(localization_data))) {
    stop("Error: 'lat' and 'lon' columns are missing from the dataframe.")
  }
  
  # Calculate the time difference between consecutive points
  localization_data$time_diff_sec <- calculate_time_diff(localization_data$TIME)
  
  # Distance between consecutive points
  localization_data$dist_m <- calculate_distance(localization_data$X, 
                                                 localization_data$Y)
  
  # For each location point, calculate the ratio between the distance from the previous point and the distance between the previous and next point
  localization_data$distance_triangle_ratio <- calculate_triangle_distance_ratio(localization_data$X,
                                                                                 localization_data$Y)
  
  # Speed [m/s]
  localization_data$Speed_m_s <- calculate_speed(localization_data$dist_m, 
                                                 localization_data$time_diff_sec)
  
  # Acceleration [m/s^2]
  localization_data$Acceleration_m_s_2 <- calculate_acceleration(localization_data$Speed_m_s, 
                                                                 localization_data$time_diff_sec)
  # STD of the ATLAS localizations
  localization_data$STD <- calculate_std(localization_data$VARX,
                                         localization_data$VARY,
                                         localization_data$COVXY)
  
  # Cosine of the turning angle
  localization_data$cos_turning_angle <- calculate_cosine_turning_angle(localization_data$X,
                                                                        localization_data$Y)
  
  # Turning angle between each location and its' previous and next consecutive points
  localization_data$turning_angle <- calculate_directional_turning_angle(localization_data$X, 
                                                                         localization_data$Y)
  
  # Elevation above sea level from Digital Elevation Model (DEM)
  localization_data <- calculate_elevation_per_location(localization_data,
                                                        dem_file = "DEM_Harod.tif")
  
  return(localization_data)
}