library(dplyr)

source(file.path(getwd(), "atlas_metrics.R"))
source(file.path(getwd(), "calculate_elevation_per_location.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/load_and_format_base_stations_info.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/calculate_abs_avg_elevation_diff_between_location_and_participating_bs.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/calculate_detection_based_features.R"))
source(file.path(getwd(), "Filter_development/Feature_engineering/calculate_beacon_derived_features.R"))

#' Calculate point-based movement, location, and signal-based features for the outliers filtering algorithm
#'
#' Computes a set of point-based features for each localization point to characterize movement, 
#' location, and signal quality. This function supports the identification of biologically implausible 
#' movement patterns and helps in feature engineering for filtering or modeling.
#'
#' @param localization_data A data frame containing localization records. Must include columns `TAG`, `X`, `Y`, `Speed_m_s`, and `time_diff_sec`.
#' @param detection_data A data frame containing detection information for calculating signal-based features- the raw data retrieved from the ATLAS database.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{localization_data}{A data frame with the original localizations and newly added features such as distance, acceleration, turning angle, and elevation.}
#'   \item{participating_base_stations}{A data frame with the id numbers of the participating base stations per localization.}
#'   \item{missed_base_stations}{A data frame with the expected but missing base stations id numbers per localization.}
#' }
#'
#' @details The function calculates:
#' \itemize{
#'   \item \strong{Distance} between consecutive points.
#'   \item \strong{Distance triangle ratio} to identify outlier movements.
#'   \item \strong{Acceleration} based on speed and time difference.
#'   \item \strong{Cosine of turning angle} to measure path angularity.
#'   \item \strong{Elevation} using a digital elevation model (`DEM_Harod.tif`- you need to have a DEM file for your data's region, saved in your working directory).
#'   \item Signal-related features based on detection data (e.g., SNR).
#' }
#'
#' @import dplyr
#' @export

calculate_point_based_features <- function(localization_data, 
                                           detection_data, 
                                           base_stations_info_path,
                                           beacons_detection_ratio_per_hour, 
                                           base_stations_summary_per_beacon,
                                           low_beacon_detection_fraction) {
  
  # Verify that the data frame has the columns TAG and time_diff_sec
  if (!"TAG" %in% colnames(localization_data)) {
    stop("Error: 'TAG' column is missing from the dataframe.")
  }
  
  if (!all(c("VARX", "VARY", "COVXY") %in% colnames(localization_data))) {
    stop("Error: 'VARX', 'VARY' and 'COVXY' columns are missing from the dataframe.")
  }
  
  ### 1. Movement-Based Features (Time-Dependent)- to capture unrealistic movement patterns
  
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

  ### 2. Location-Based Features
  # Currently I calculate these features only within a time window
  
  ### 3. Signal-Based Features
  
  # Elevation above sea level from Digital Elevation Model (DEM)
  localization_data <- calculate_elevation_per_location(localization_data,
                                                        dem_file = "DEM_Harod.tif")
  
  # NBS- Number of participation Base Stations- already included in the raw data from the ATLAS database

  # SNR- Signal to Noise Ratio and other detection-based features
  
  # Load the base stations info
  base_stations_info <- load_and_format_base_stations_info(base_stations_info_path)
  
  # Calculate the detection-based features
  results <- calculate_detection_based_features(localization_data, 
                                                detection_data,
                                                base_stations_info)
  
  localization_data <- results$localization_data
  participating_base_stations <- results$participating_base_stations
  missed_base_stations <- results$missed_base_stations
  
  # Calculate the absolute value of the average difference between the location's elevation 
  # and the elevation of each participating base station
  localization_data <- calculate_abs_avg_elevation_diff_between_location_and_participating_bs(
    localization_data,
    participating_base_stations,
    base_stations_info
  )
  
  # Calculate beacons' features
  localization_data <- calculate_beacon_derived_features(localization_data, 
                                                         participating_base_stations, 
                                                         beacons_detection_ratio_per_hour,
                                                         base_stations_summary_per_beacon,
                                                         low_beacon_detection_fraction) 
  
  return(list(
    localization_data = localization_data,
    participating_base_stations = participating_base_stations,
    missed_base_stations = missed_base_stations
  ))
  
}