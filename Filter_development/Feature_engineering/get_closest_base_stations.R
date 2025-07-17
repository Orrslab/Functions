library(data.table)

#' Get Closest Participating Base Stations Per Localization
#'
#' For each unique (TAG, TIME) combination, selects the specified number of closest participating base stations
#' based on their distance from the localization.
#'
#' @param participating_base_stations A `data.frame` or `data.table` with columns: `TAG`, `TIME`, `participating_bs_id`, and `Distance_m`.
#' @param number_of_closest_bs An integer specifying how many of the closest base stations to select per localization.
#'
#' @return A `data.table` with columns: `TAG`, `TIME`, `BS`, containing the closest base stations.
#'
#' @import data.table
#' @examples
#' \dontrun{
#' closest_bs <- get_closest_base_stations(participating_base_stations, number_of_closest_bs = 4)
#' }
#' @export

get_closest_base_stations <- function(participating_base_stations, number_of_closest_bs) {
  
  dt <- as.data.table(participating_base_stations)
  
  # Check that required columns exist
  required_cols <- c("TAG", "TIME", "participating_bs_id", "Distance_m")
  if (!all(required_cols %in% colnames(dt))) {
    stop("Input data must contain the columns: TAG, TIME, participating_bs_id, Distance_m")
  }
  
  # # Check for any group that has fewer base stations than requested
  # insufficient_bs <- dt[, .N, by = .(TAG, TIME)][N < number_of_closest_bs]
  # if (nrow(insufficient_bs) > 0) {
  #   stop("Some (TAG, TIME) groups have fewer than 'number_of_closest_bs'. Reduce the value or preprocess the data.")
  # }

  # Select the closest base stations
  closest_bs <- dt[
    ,
    head(.SD[order(Distance_m)], number_of_closest_bs),
    by = .(TAG, TIME)
  ][
    , .(TAG, TIME, BS = participating_bs_id)
  ]
  
  return(closest_bs)
}