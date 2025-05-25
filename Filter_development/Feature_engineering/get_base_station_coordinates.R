#' Get Coordinates of a Base Station at a Specific Time
#'
#' Retrieves the longitude and latitude of a base station at a given UNIX timestamp, based on metadata 
#' that includes the station's location over time.
#'
#' @param base_station_number Character or numeric. The identifier of the base station (must match the 
#'   `Radio_serial_number` column in `base_stations_info`).
#'
#' @param location_time_unix Numeric. The time of interest as a UNIX timestamp (i.e., seconds since 
#'   "1970-01-01 00:00:00 UTC").
#'
#' @param base_stations_info A data.table with information on base stations over time. Must contain the columns:
#'   - `Radio_serial_number`: Unique base station ID
#'   - `Since`: Start time (as numeric UNIX timestamp)
#'   - `Until`: End time (as numeric UNIX timestamp or `Inf` for ongoing)
#'   - `bs_lat`: Latitude of the base station during the interval
#'   - `bs_lon`: Longitude of the base station during the interval
#'
#' @return A named list with the elements:
#'   - `bs_lon`: Longitude of the base station
#'   - `bs_lat`: Latitude of the base station  
#'   If no matching record is found, returns `NULL`.
#'
#' @details 
#' If the base station has multiple location records (e.g., due to relocation), this function returns
#' the coordinates that were valid at the specified time. It assumes that `Since` and `Until` columns
#' are already parsed as UNIX timestamps (numeric). The function returns the first matching row.
#'
#' @examples
#' \dontrun{
#' get_base_station_coordinates(972006010, 1625400000, base_stations_info)
#' }
#'
#' @export
get_base_station_coordinates <- function(base_station_number, location_time_unix, base_stations_info) {
  # # Parse time columns in bs_info (only once is enough if already parsed externally)
  # base_stations_info[, Since := as.numeric(as.POSIXct(Since, format = "%Y-%b-%d %H:%M", tz = "UTC"))]
  # base_stations_info[, Until := ifelse(Until == "Infinity", Inf,
  #                           as.numeric(as.POSIXct(Until, format = "%Y-%b-%d %H:%M", tz = "UTC")))]
  
  # Filter for the given base station and time
  bs_subset <- base_stations_info[Radio_serial_number == base_station_number & 
                                    Since <= location_time_unix & Until > location_time_unix]
  
  if (nrow(bs_subset) == 0) return(NULL)
  
  return(list(bs_lon = bs_subset$bs_lon[1], bs_lat = bs_subset$bs_lat[1]))
}