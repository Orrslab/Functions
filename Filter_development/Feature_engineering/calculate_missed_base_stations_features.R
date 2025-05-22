#' Calculate Missed Base Stations Features
#'
#' Identifies base stations that were active and closer than the participating base stations, but did not participate in a given localization, 
#' and computes features describing these "missed" base stations, such as their count and distance.
#'
#' @param localizations_data A `data.frame` or `data.table` of localization results, containing:
#' \itemize{
#'   \item `TAG` – animal or tag identifier
#'   \item `TIME` – timestamp in milliseconds
#'   \item `lat`, `lon` – coordinates of the localization
#'   \item `Min_distance_to_BS` – distance to the closest participating base station (required for filtering)
#' }
#' @param base_stations_info A `data.frame` or `data.table` describing base station metadata. Must include:
#' \itemize{
#'   \item `Radio_serial_number` – base station ID
#'   \item `bs_lat`, `bs_lon` – coordinates of each base station
#'   \item `Since`, `Until` – operational times (in seconds since epoch)
#' }
#'
#' @return A list with two elements:
#' \describe{
#'   \item{`localizations_data`}{The input localization data, enriched with the following new columns: 
#'     \itemize{
#'       \item `num_missed_bs` – number of missed base stations that were active and closer than the closest detected one
#'       \item `closest_missed_bs_distance` – distance to the nearest missed base station
#'       \item `mean_missed_bs_distance` – mean distance to missed base stations (if there is more than one missed base station)
#'     }
#'   }
#'   \item{`missed_base_stations`}{A `data.table` of missed base stations per localization, with:
#'     \itemize{
#'       \item `TAG`, `TIME` – identifiers of the localization
#'       \item `missed_bs_id` – ID of the missed base station
#'     }
#'   }
#' }
#'
#' @details
#' \itemize{
#'   \item A "missed" base station is defined as one that was active at the time of localization and
#'   was geographically closer than the closest base station that did participate.
#'   \item Assumes `TIME` is in milliseconds and converts it to seconds internally for comparison with `Since` and `Until` from `base stations info`.
#'   \item Uses the Haversine formula via `geosphere::distHaversine()` to compute geographic distances.
#' }
#'
#' @import data.table
#' @importFrom geosphere distHaversine
#'
#' @examples
#' \dontrun{
#' result <- calculate_missed_base_stations_features(localizations_data, base_stations_info)
#' updated_data <- result$localizations_data
#' missed_bs_table <- result$missed_base_stations
#' }
#'
#' @export

library(data.table)

calculate_missed_base_stations_features <- function(localizations_data, base_stations_info) {
  message("calculating features of missed base stations in each localization.")
  
  # Ensure the data.tables are sorted and properly typed
  loc_data <- as.data.table(localizations_data)
  bs_info <-  as.data.table(base_stations_info)
  
  # Convert loc_data$TIME from milliseconds to POSIXct
  loc_data[, TIME_sec := TIME / 1000]
  
  # Rename lat/lon columns in loc_data to avoid confusion with bs_lat/bs_lon
  setnames(loc_data, old = c("lat", "lon"), new = c("loc_lat", "loc_lon"))
  
  # Perform the non-equi join and keep only bs_lat and bs_lon from bs_info
  loc_with_bs <- loc_data[bs_info, 
                          on = .(TIME_sec >= Since, TIME_sec < Until), 
                          allow.cartesian = TRUE, nomatch = 0L]
  
  # FOR DEBUGGING PURPOSES
  # print(head(loc_with_bs[loc_with_bs$Radio_serial_number == 972006002, c("dateTime", "bs_lat", "bs_lon")], 100))
  # print(loc_with_bs[loc_with_bs$Radio_serial_number == 972006007, c("dateTime", "bs_lat", "bs_lon")])
  # print(head(loc_with_bs))
  
  # Compute the distance between each localization and each active base station
  loc_with_bs[, bs_distance := distHaversine(
    matrix(c(loc_lon, loc_lat), ncol = 2),
    matrix(c(bs_lon, bs_lat), ncol = 2)
  ), by = .(TAG, TIME)]
  
  # Filter missed base stations that are closer than the closest participating one
  missed_closer_bs <- loc_with_bs[bs_distance < Min_distance_to_BS] 
  # missed_closer_bs <- loc_with_bs[bs_distance < Max_distance_to_BS]  # Gives missed bs for almost all locations
  
  # Create a table of the missed base stations per localization
  missed_bs <- missed_closer_bs[, .(missed_bs_id = Radio_serial_number), by = .(TAG, TIME)]
  
  # Compute the missed base stations features
  missed_features <- missed_closer_bs[, .(
    num_missed_bs = .N,
    closest_missed_bs_distance = if (.N > 0) min(bs_distance) else NA_real_,
    mean_missed_bs_distance = if (.N > 1) mean(bs_distance) else NA_real_
  ), by = .(TAG, TIME)]
  
  # Merge features back to localizations_data
  localizations_data <- merge(localizations_data, missed_features, 
                              by = c("TAG", "TIME"), all.x = TRUE)
  
  # Replace NAs with 0 or NA_real_ as needed- in case num_missed_bs got NA value instead of zero
  localizations_data$num_missed_bs[is.na(localizations_data$num_missed_bs)] <- 0
  localizations_data$closest_missed_bs_distance[is.na(localizations_data$closest_missed_bs_distance) & 
                                                  localizations_data$num_missed_bs == 0] <- NA_real_
  localizations_data$mean_missed_bs_distance[is.na(localizations_data$mean_missed_bs_distance) & 
                                               localizations_data$num_missed_bs <= 1] <- NA_real_
  
  return(list(
    localizations_data = localizations_data,
    missed_base_stations = missed_bs
  ))

}