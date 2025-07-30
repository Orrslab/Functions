library(data.table)

calculate_circular_variance_of_participating_base_stations <- function(localization_data, matched) {
  
  # Convert to data.table
  matched <- as.data.table(matched)
  localization_data <- as.data.table(localization_data)
  
  # Convert the time of matched to miliseconds to match the time units in localization_data.
  if (max(matched$TIME, na.rm = TRUE) < 1e12) {
    matched[, TIME := TIME * 1000]
  }
  
  # Add location ID to group matched detections by localization
  matched[, loc_id := .GRP, by = .(TAG, TIME)]
  localization_data[, loc_id := .GRP, by = .(TAG, TIME)]
  
  # Calculate direction vector components from localization to base station
  matched[, `:=`(
    dx = bs_lon - loc_lon,
    dy = bs_lat - loc_lat
  )]
  
  # Compute angle from the reference north direction (0,1)
  matched[, angle_rad := atan2(dx, dy)]  # angle in radians
  
  # Project onto unit circle
  matched[, `:=`(
    x = cos(angle_rad),
    y = sin(angle_rad)
  )]
  
  # Aggregate by loc_id to compute circular variance
  circular_stats <- matched[, .(
    mean_x = mean(x, na.rm = TRUE),
    mean_y = mean(y, na.rm = TRUE)
  ), by = loc_id]
  
  circular_stats[, R_bar := sqrt(mean_x^2 + mean_y^2)]
  circular_stats[, circ_variance := 1 - R_bar]
  
  # Merge circular variance back into localization_data
  localization_data <- merge(localization_data, circular_stats[, .(loc_id, circ_variance)], by = "loc_id", all.x = TRUE)
  
  # Remove helper column
  localization_data[, loc_id := NULL]
  
  # Return as data.frame
  return(as.data.frame(localization_data))
}