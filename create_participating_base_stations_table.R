library(data.table)

create_participating_base_stations_table <- function(localizations_data, matched_detections) {
  # Ensure both inputs are data.tables
  localizations_data <- as.data.table(localizations_data)
  matched_detections <- as.data.table(matched_detections)
  
  # Add roundTIME to localizations_data if missing
  if (!"roundTIME" %in% names(localizations_data)) {
    localizations_data[, roundTIME := round(TIME / 1000) * 1000]
  }
  
  # Group by TAG and roundTIME, and collapse BS numbers into a list or character string
  participating_base_stations <- matched_detections[
    , .(participating_bs = list(BS)), 
    by = .(TAG, roundTIME)
  ]
  
  # Merge with TIME from localizations_data (one TIME per TAG & roundTIME is guaranteed)
  time_lookup <- unique(localizations_data[, .(TAG, roundTIME, TIME)])
  
  participating_base_stations <- merge(
    participating_base_stations,
    time_lookup,
    by = c("TAG", "roundTIME"),
    all.x = TRUE
  )
  
  return(participating_base_stations)
}