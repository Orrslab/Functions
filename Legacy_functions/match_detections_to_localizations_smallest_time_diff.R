# This function has never been used and therefore needs to be tested before the first use
match_detections_to_localizations_smallest_time_diff <- function(localizations, detections, time_window_ms = 1500) {
  loc <- copy(as.data.table(localizations))
  det <- copy(as.data.table(detections))
  
  setnames(loc, "TIME", "loc_TIME")  # Rename for clarity
  setkey(loc, TAG, loc_TIME)
  
  # For each detection, find the closest localization within the time window
  det[, `:=`(det_TIME = TIME)]  # Keep original
  setkey(det, TAG, det_TIME)
  
  # Perform a non-equi join within ±time_window_ms
  matched <- foverlaps(
    x = det[, .(TAG, det_TIME, det_TIME_start = det_TIME - time_window_ms, det_TIME_end = det_TIME + time_window_ms)],
    y = loc[, .(TAG, loc_TIME)],
    by.x = c("TAG", "det_TIME_start", "det_TIME_end"),
    by.y = c("TAG", "loc_TIME"),
    type = "any",
    nomatch = 0L
  )
  
  # For each detection, keep the closest localization
  matched[, time_diff := abs(det_TIME - loc_TIME)]
  setorder(matched, TAG, det_TIME, time_diff)
  closest <- matched[, .SD[1], by = .(TAG, det_TIME)]  # Pick closest
  
  # Assign roundTIME = loc_TIME of closest localization
  closest[, roundTIME := loc_TIME]
  
  return(closest)
}