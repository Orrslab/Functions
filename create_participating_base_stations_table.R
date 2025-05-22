#' Create Participating Base Stations Table
#'
#' Generates a table listing all base stations that participated in the calculation of each ATLAS location point,
#' based on `TAG` and rounded `TIME`.
#'
#' @param localizations_data A `data.frame` or `data.table` containing localization data with `TAG`, `TIME`, and optionally `roundTIME`.
#' @param matched_detections A `data.frame` or `data.table` of detection data with `TAG`, `roundTIME`, and `BS` (base station ID).
#'
#' @return A `data.table` with one row per participating base station per localization, containing the columns:
#' \describe{
#'   \item{TAG}{Animal's tag number}
#'   \item{TIME}{Exact timestamp of the localization (in milliseconds since epoch)}
#'   \item{participating_bs_id}{Base station ID that participated in the localization}
#' }
#'
#' @details
#' \itemize{
#'   \item If `roundTIME` is not present in `localizations_data`, it is calculated by rounding `TIME` to the nearest second (in milliseconds).
#'   \item Collapses base stations per `(TAG, roundTIME)` group from `matched_detections`.
#'   \item Merges with localization `TIME` values to return exact timestamps.
#'   \item Expands the list of base stations into individual rows.
#' }
#'
#' @import data.table
#'
#' @examples
#' \dontrun{
#' participating_bs <- create_participating_base_stations_table(localizations_data, matched_detections)
#' }
#'
#' @export

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