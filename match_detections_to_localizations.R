library(data.table)

#' Match detections to localizations by TAG and rounded TIME
#'
#' This function matches each localization to its corresponding detections based on `TAG` and a rounded `TIME` value.
#' The time is rounded to the nearest second (in milliseconds) to allow robust matching.
#'
#' @param localizations_data A data frame or data.table containing the localization data, with columns `TAG` and `TIME` (Unix Timestamp in milliseconds).
#' @param detections_data A data frame or data.table containing the detection data, also with columns `TAG` and `TIME` (Unix Timestamp in milliseconds).
#'
#' @return A data.table containing the matched detections for each localization. Only pairs that match on both `TAG` and rounded `TIME` are included.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Converts input data to `data.table`
#'   \item Rounds the `TIME` columns in both datasets to the nearest second (multiples of 1000 ms)
#'   \item Performs a fast join on `TAG` and rounded `TIME` using data.table keys
#' }
#' This join allows multiple detections to match a single localization if necessary (`allow.cartesian = TRUE`).
#'
#' @examples
#' \dontrun{
#' matched_detections <- match_detections_to_localizations(localizations_data, detections_data)
#' }
#'
#' @import data.table
#' @export

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