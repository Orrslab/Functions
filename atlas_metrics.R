
#' Calculate Time Differences
#'
#' This function computes the time differences (in seconds) between consecutive rows in a time column.
#'
#' @param time_column A numeric vector of timestamps in milliseconds.
#'
#' @return A numeric vector of time differences in seconds. The first value will be \code{NA} as there is no previous timestamp to calculate a difference.
#'
#' @examples
#' time_column <- c(1000, 2000, 3500, 4000)
#' time_diff <- calculate_time_diff(time_column)
#'
#' @export
calculate_time_diff <- function(time_column) {
  # Convert to seconds and compute differences
  time_diff <- c(NA, diff(time_column) / 1000)
  return(time_diff)
}

#' Calculate Euclidean Distance
#'
#' This function computes the Euclidean distance between consecutive points in three-dimensional space.
#'
#' @param x A numeric vector representing the X-coordinates.
#' @param y A numeric vector representing the Y-coordinates.
#' @param z A numeric vector representing the Z-coordinates.
#'
#' @return A numeric vector of distances between consecutive points. The first value will be \code{NA} as there is no previous point to calculate a distance.
#'
#' @examples
#' x <- c(0, 1, 2, 3)
#' y <- c(0, 1, 1, 1)
#' z <- c(0, 0, 0, 0)
#' distance <- calculate_distance(x, y, z)
#'
#' @export
calculate_distance <- function(x, y) {
  # Compute Euclidean distance between consecutive points
  distance <- c(NA, sqrt(diff(x)^2 + diff(y)^2))
  return(distance)
}

#' Calculate Speed
#'
#' This function computes speed by dividing distances by time differences.
#'
#' @param distance A numeric vector of distances (e.g., from \code{\link{calculate_distance}}).
#' @param time_diff A numeric vector of time differences in seconds (e.g., from \code{\link{calculate_time_diff}}).
#'
#' @return A numeric vector of speeds. Values where \code{time_diff <= 0} will be \code{NA}.
#'
#' @examples
#' distance <- c(NA, 5, 10, 15)
#' time_diff <- c(NA, 1, 2, 3)
#' speed <- calculate_speed(distance, time_diff)
#'
#' @export
calculate_speed <- function(distance, time_diff) {
  # Avoid division by zero
  speed <- ifelse(time_diff > 0, distance / time_diff, NA)
  return(speed)
}

#' Calculate Standard Deviation
#'
#' This function calculates the standard deviation based on the variance of X, variance of Y, and the covariance of X and Y.
#'
#' @param var_x A numeric vector representing the variance of X.
#' @param var_y A numeric vector representing the variance of Y.
#' @param cov_xy A numeric vector representing the covariance between X and Y.
#'
#' @return A numeric vector of calculated standard deviations.
#'
#' @details
#' The formula used is: \code{sqrt(var_x + var_y + 2 * cov_xy)}. All inputs must be numeric; otherwise, the function throws an error.
#'
#' @examples
#' var_x <- c(0.1, 0.2, 0.3)
#' var_y <- c(0.1, 0.2, 0.3)
#' cov_xy <- c(0.05, 0.1, 0.15)
#' std <- calculate_std(var_x, var_y, cov_xy)
#'
#' @export
calculate_std <- function(var_x, var_y, cov_xy) {
  # Ensure the inputs are numeric
  if (!is.numeric(var_x) || !is.numeric(var_y) || !is.numeric(cov_xy)) {
    stop("All inputs must be numeric.")
  }
  
  # Calculate the standard deviation
  std <- sqrt(var_x + var_y + 2 * cov_xy)
  
  return(std)
}