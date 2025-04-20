
library(data.table)

match_detections_to_localizations <- function(localizations_data, detections_data) {
  loc <- copy(as.data.table(localizations_data))
  det <- copy(as.data.table(detections_data))
  
  # Round time
  loc[, roundTIME := round(TIME / 1000) * 1000]
  det[, roundTIME := round(TIME / 1000) * 1000]
  
  # Set keys for fast join
  setkeyv(loc, c("TAG", "roundTIME"))
  setkeyv(det, c("TAG", "roundTIME"))
  
  # Fast join - only matching pairs
  matched <- det[loc, nomatch = 0, allow.cartesian = TRUE]
  
  return(matched)
}