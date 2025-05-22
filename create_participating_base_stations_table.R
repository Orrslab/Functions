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
  bs_grouped <- matched_detections[
    , .(participating_bs = list(BS)), 
    by = .(TAG, roundTIME)
  ]
  
  # Merge with TIME from localizations_data (one TIME per TAG & roundTIME is guaranteed)
  time_lookup <- unique(localizations_data[, .(TAG, roundTIME, TIME)])
  
  # Merge to add TIME
  bs_merged <- merge(
    bs_grouped,
    time_lookup,
    by = c("TAG", "roundTIME"),
    all.x = TRUE
  )
  
  # Unnest the list column so each BS gets its own row
  participating_base_stations <- bs_merged[
    , .(participating_bs_id = unlist(participating_bs)), 
    by = .(TAG, TIME)
  ]
  
  return(participating_base_stations)
}