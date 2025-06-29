source(file.path(getwd(), "Filter_development/Feature_engineering/get_base_station_coordinates.R"))

#' Calculate Average Absolute Elevation Difference Between Localization and Participating Base Stations
#'
#' @param localizations_data A data.table with localization info. Must contain columns:
#'   - `TAG`: Tag ID
#'   - `TIME`: UNIX timestamp
#'   - `DEM_elevation`: Elevation at the tag location
#'
#' @param participating_base_stations A data.table with participating base stations per location point.
#'   Must contain columns:
#'   - `TAG`, `TIME`, `participating_bs_id`
#'
#' @param base_stations_info A data.table with metadata about base stations over time.
#'   Must contain: `Radio_serial_number`, `Since`, `Until`, `bs_lon`, `bs_lat`, `Elevation`
#'
#' @return The input `localizations_data` with an additional column:
#'   - `avg_abs_elevation_diff`: Average absolute elevation difference [m]
#'
#' @export
calculate_abs_avg_elevation_diff_between_location_and_participating_bs <- function(
    localizations_data,
    participating_base_stations,
    base_stations_info
) {
  # Ensure data.table
  localizations_data <- data.table::as.data.table(localizations_data)
  participating_base_stations <- data.table::as.data.table(participating_base_stations)
  base_stations_info <- data.table::as.data.table(base_stations_info)
  
  # Add row_id for merging back
  localizations_data[, row_id := .I]
  
  # Join DEM_elevation to participating_base_stations
  merged <- merge(
    participating_base_stations,
    localizations_data[, .(row_id, TAG, TIME, DEM_elevation)],
    by = c("TAG", "TIME"),
    all.x = TRUE
  )
  
  # Create POSIX time column for internal use — do not overwrite original TIME
  merged[, TIME_POSIX := as.POSIXct(TIME / 1000, origin = "1970-01-01", tz = "UTC")]
  
  # Add base station elevation using POSIX time
  merged[, bs_elevation := sapply(.I, function(i) {
    info <- get_base_station_coordinates(
      base_station_number = merged$participating_bs_id[i],
      location_time_unix = merged$TIME_POSIX[i],
      base_stations_info = base_stations_info
    )
    if (!is.null(info)) info$bs_elevation else NA_real_
  })]
  
  # Compute elevation difference
  merged[, elevation_diff := abs(DEM_elevation - bs_elevation)]
  
  # Compute average absolute elevation difference by row_id
  avg_diffs <- merged[, .(avg_abs_elevation_diff = mean(elevation_diff, na.rm = TRUE)), by = row_id]
  
  # Merge back using row_id
  result <- merge(localizations_data, avg_diffs, by = "row_id", all.x = TRUE)
  
  # Clean up
  result[, row_id := NULL]
  
  return(result)
}