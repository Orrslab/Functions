#' Add Base Station Coordinates to Matched Detections
#'
#' This function adds latitude and longitude coordinates to each detection in a dataset of matched detections,
#' based on the base station (BS) that detected it and the time of detection. The coordinates are taken from a
#' base station info table (`bs_info`), which may contain multiple coordinate entries per base station depending
#' on time intervals. The dataframe of `bs_info` is loaded from a csv file 
#' which is created from locations.txt file in the altas system folder.
#'
#' @param matched_detections A data.table containing detection records, with at least the columns:
#'   - `BS`: Base station identifier
#'   - `TIME`: Time of detection (POSIXct or numeric timestamp)
#'
#' @param bs_info A data.table containing metadata about base stations, with the columns:
#'   - `Radio_serial_number`: Unique identifier for the base station (matching `BS` in `matched_detections`)
#'   - `Since`: Start time (inclusive) for the validity of the coordinate entry
#'   - `Until`: End time (inclusive) for the validity of the coordinate entry
#'   - `bs_lat`: Latitude of the base station during the interval
#'   - `bs_lon`: Longitude of the base station during the interval
#'
#' @return A modified version of `matched_detections`, with two additional columns:
#'   - `bs_lat`: Latitude of the corresponding base station at the time of detection
#'   - `bs_lon`: Longitude of the corresponding base station at the time of detection
#'
#' @details
#' If a base station has moved over time, and `bs_info` includes multiple entries for it, this function ensures
#' that each detection is assigned coordinates that are valid at the specific time of detection. Detections
#' for which no matching base station information is found are left with `NA` in the coordinate fields.
#'
#' @examples
#' \dontrun{
#' result <- get_bs_coordinates_from_matched_detections(matched_detections, bs_info)
#' }
#'
#' @export

get_bs_coordinates_from_matched_detections <- function(matched_detections, bs_info) {
  # Add empty columns
  matched_detections[, `:=`(bs_lat = NA_real_, bs_lon = NA_real_)]
  
  # Loop over each unique BS
  for (bs in unique(matched_detections$BS)) {
    
    # Get the rows in which the base station number equals to bs
    matched_detections_bs <- matched_detections[BS == bs]
    
    # Get the rows from bs_info, that correspond to bs
    bs_segment <- bs_info[Radio_serial_number == bs]
    
    if (nrow(bs_segment) == 0) next  # Skip if no matching BS info
    
    # For each time interval in bs_segment, assign coordinates to the matching detections
    for (j in seq_len(nrow(bs_segment))) {
      since <- bs_segment$Since[j]
      until <- bs_segment$Until[j]
      basestation_lat <- bs_segment$bs_lat[j]
      basestation_lon <- bs_segment$bs_lon[j]
      
      # Find the matching rows in matched_detections
      rows_to_update <- matched_detections$BS == bs & matched_detections$TIME >= since & matched_detections$TIME <= until
      
      matched_detections[rows_to_update, `:=`(bs_lat = basestation_lat, 
                                              bs_lon = basestation_lon)]
    }
  }
  
  return(matched_detections)
}