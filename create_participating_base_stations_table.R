library(data.table)

#' Create Participating Base Stations Table with Distances
#'
#' Generates a table listing all base stations that participated in the calculation of each ATLAS location point,
#' along with their distances to the estimated location, based on `TAG` and rounded `TIME`.
#'
#' @param localizations_data A `data.frame` or `data.table` containing localization data with columns `TAG`, `TIME` (in milliseconds since epoch),
#'        and optionally `roundTIME` (in seconds since epoch). If `roundTIME` is missing, it will be computed as `round(TIME / 1000)`.
#' @param matched_detections_with_dist A `data.frame` or `data.table` containing detection data with columns `TAG`, `roundTIME` (in seconds),
#'        `BS` (base station ID), and `dist` (distance in meters between the localization and the base station).
#'
#' @return A `data.table` with one row per participating base station per localization, containing the columns:
#' \describe{
#'   \item{TAG}{Animal's tag number}
#'   \item{TIME}{Exact timestamp of the localization (in milliseconds since epoch)}
#'   \item{participating_bs_id}{Base station ID that participated in the localization}
#'   \item{Distance_m}{Distance (in meters) between the base station and the localization}
#' }
#'
#' @details
#' \itemize{
#'   \item If `roundTIME` is not present in `localizations_data`, it is computed from `TIME`.
#'   \item Base stations are grouped per `(TAG, roundTIME)` from the detection data, and matched to localization times.
#'   \item The result expands the list of base stations and distances into individual rows for analysis.
#' }
#'
#' @import data.table
#'
#' @examples
#' \dontrun{
#' participating_bs <- create_participating_base_stations_table(localizations_data, matched_detections_with_dist)
#' }
#'
#' @export

create_participating_base_stations_table <- function(localizations_data, matched_detections_with_dist) {
  # Ensure both inputs are data.tables
  localizations_data <- as.data.table(localizations_data)
  matched_detections_with_dist <- as.data.table(matched_detections_with_dist)
  
  # Add roundTIME to localizations_data if missing
  if (!"roundTIME" %in% names(localizations_data)) {
    localizations_data[, roundTIME := round(TIME / 1000)]
  }
  
  # Merge with TIME from localizations_data (one TIME per TAG & roundTIME is guaranteed)
  time_lookup <- unique(localizations_data[, .(TAG, roundTIME, TIME)])
  
  # Group by TAG and roundTIME, and collect BS and dist together
  bs_grouped <- matched_detections_with_dist[
    , .(participating_bs = list(BS),
        distances = list(dist)), 
    by = .(TAG, roundTIME)
  ]
  
  # Ensure that the roundTIME column in localizations_data and matched_detections_with_dist have the same format
  bs_grouped[, roundTIME := as.numeric(roundTIME)]
  
  # Merge to add TIME
  bs_merged <- merge(
    bs_grouped,
    time_lookup,
    by = c("TAG", "roundTIME"),
    all.x = TRUE
  )
  
  # Unnest the lists: each row will contain one BS and its corresponding distance
  participating_base_stations <- bs_merged[
    , .(participating_bs_id = unlist(participating_bs),
        Distance_m = unlist(distances)), 
    by = .(TAG, TIME)
  ]
  
  return(participating_base_stations)
}