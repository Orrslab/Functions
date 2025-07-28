library(data.table)

source(file.path(getwd(), "Filter_development/Feature_engineering/calculate_features_in_time_window.R"))

#' Calculate Time Window-Based Features for each location point
#'
#' Computes features for each localization based on a surrounding time window of localizations from the same tag.
#' \code{calculate_features_in_time_window()}.
#'
#' @param localization_data A `data.frame` or `data.table` containing localization data, with columns:
#' \itemize{
#'   \item `TAG` – identifier of the animal/tag
#'   \item `TIME` – timestamp in milliseconds
#'   \item `X`, `Y` – coordinates of the localization (typically in projected units)
#' }
#' @param half_window_size_sec Numeric. Half the size of the time window (in seconds) to use around each localization.
#' The full time window size will be twice this value. Default is 20 seconds (i.e., a 40-second window).
#'
#' @return A `data.frame` containing the original data with additional columns for the computed features.
#' The specific features added depend on the implementation of the function \code{calculate_features_in_time_window()}.
#'
#' @details
#' For each localization, the function selects all localizations with the same `TAG` within a specified time window.
#' It then applies a custom feature extraction routine to that subset.
#'
#' \itemize{
#'   \item The time window is centered on each localization.
#'   \item Features are computed based on the surrounding localizations and their relationship to the focal point (`X`, `Y`).
#'   \item Progress is displayed with a text-based progress bar.
#' }
#'
#' @seealso \code{\link{calculate_features_in_time_window}} for the actual feature computation logic.
#'
#' @import data.table
#'
#' @examples
#' \dontrun{
#' enriched_data <- calculate_time_window_based_features(localization_data, half_window_size_sec = 30)
#' }
#'
#' @export
#' 
calculate_time_window_based_features <- function(localization_data, half_window_size_sec = 20) {
  
  # Convert to data.table
  loc_dt <- as.data.table(localization_data)
  
  # Ensure data is sorted (important for speed)
  setkey(loc_dt, TAG, TIME)
  
  # Convert the time window size from seconds to milliseconds
  half_window_millisec <- half_window_size_sec * 1000
  
  # Pre-allocate list for new columns (each element will be a named list)
  features_list <- vector("list", nrow(loc_dt))
  
  # Optional: Progress bar
  pb <- txtProgressBar(min = 0, max = nrow(loc_dt), style = 3)
  
  
  for (i in seq_len(nrow(loc_dt))) {
    tag_i <- loc_dt[i, TAG]
    time_i <- loc_dt[i, TIME]  # This is in milliseconds
    
    # Subset the data within the window for the same TAG
    window_data <- loc_dt[
      TAG == tag_i & TIME >= (time_i - half_window_millisec) & TIME <= (time_i + half_window_millisec)
    ]
    
    # Get the X and Y coordinated of the obsereved point
    x_i <- loc_dt[i, X]
    y_i <- loc_dt[i, Y]
    
    # Compute features for the current window
    features_list[[i]] <- calculate_features_in_time_window(window_data, 
                                                            x_i,
                                                            y_i)
    
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  # Combine the features into a data.table
  features_dt <- rbindlist(features_list, fill = TRUE)
  
  # Bind features directly onto original data
  loc_dt <- cbind(loc_dt, features_dt)
  
  return(as.data.frame(loc_dt))
}