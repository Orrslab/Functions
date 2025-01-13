
#' Baseline identification of Outliers in ATLAS Tracking Data
#'
#' This function identifies outliers in a given data frame based on predefined 
#' thresholds for speed, standard deviation (STD), and the number of base stations (NBS).
#' The function updates the "Outliers" column, marking rows as outliers (1) if 
#' any threshold condition is met.
#'
#' @param df A data frame containing tracking data with columns: \code{Speed_m_s}, \code{STD}, and \code{NBS}.
#' @param speed_threshold A numeric value representing the maximum allowed speed (in meters per second). 
#'        Rows exceeding this threshold are marked as outliers.
#' @param std_threshold A numeric value representing the maximum allowed standard deviation (STD).
#'        Rows exceeding this threshold are marked as outliers.
#' @param nbs_threshold A numeric value representing the minimum required number of base stations (NBS).
#'        Rows with fewer base stations than this threshold are marked as outliers.
#'
#' @return The input data frame with an updated "Outliers" column, where:
#'         \itemize{
#'         \item \code{0} indicates a non-outlier.
#'         \item \code{1} indicates an outlier based on the given thresholds.
#'         }
#'
#' @details
#' If the "Outliers" column does not already exist in the data frame, it is initialized with zeros.
#' The function then applies the threshold conditions, updating the "Outliers" column to 1 where 
#' any of the conditions are met.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(Speed_m_s = c(2.5, 15.0, 5.0),
#'                  STD = c(0.5, 3.2, 1.0),
#'                  NBS = c(5, 2, 7))
#' 
#' filtered_df <- baseline_filter(df, speed_threshold = 10, std_threshold = 2, nbs_threshold = 3)
#' print(filtered_df)
#' }
#'
baseline_filter <- function(df, speed_threshold, std_threshold, nbs_threshold) {
  
  # Ensure the 'Outliers' column exists
  if (!"Outliers" %in% colnames(df)) {
    df$Outliers <- 0  # Initialize Outliers column to 0 if it doesn't exist
  }
  
  # Apply threshold conditions for speed, STD, and number of base stations
  df$Outliers <- ifelse(
    df$Speed_m_s > speed_threshold | df$STD > std_threshold | df$NBS < nbs_threshold, 
    1,  # Set Outliers to 1 if any condition is met
    df$Outliers  # Otherwise, keep the current value (either 0 or previously set)
  )
  
  return(df)
}