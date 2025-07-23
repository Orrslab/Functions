
# Helper functions to calculate different metrices

calculate_min_of_column <- function(x) {
  min(x, na.rm = TRUE)
}

# Returns the maximum value, ignoring NAs
calculate_max_of_column <- function(x) {
  max(x, na.rm = TRUE)
}

# Calculate the mean of a column
calculate_mean_of_column <- function(x) mean(x, na.rm = TRUE)

# Calculate the median of a column
calculate_median_of_column <- function(x) median(x, na.rm = TRUE)

# calculate_range_of_column <- function(x) diff(range(x, na.rm = TRUE))

# Calculate the variance of a column
calculate_variance_of_column <- function(x) var(x, na.rm = TRUE)

# Calculate the standard deviation of a column
calculate_std_of_column <- function(x) {
  return(sd(x, na.rm = TRUE))
}

# calculate_cv_of_column <- function(x) {
#   m <- mean(x, na.rm = TRUE)
#   s <- sd(x, na.rm = TRUE)
#   if (m == 0) return(NA)
#   return(s / m)
# }


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

# Calculate the 2D Euclidean distance between two points: (X1, Y1) and (X2, Y2)
calculate_euclidean_distance <- function(X1, X2, Y1, Y2) {
  
  sqrt((X2 - X1)^2 + (Y2 - Y1)^2)
  
}

#' Calculate triangle distance ratio for movement analysis of location data
#'
#' Computes the ratio between the distance from the current location point to the previous point and 
#' the distance between the previous and next points. This metric helps identify movement irregularities,
#' such as sudden deviations or spikes in trajectories.
#'
#' @param X_column A numeric vector of the X coordinates (e.g., easting or longitude).
#' @param Y_column A numeric vector of the Y coordinates (e.g., northing or latitude).
#'
#' @return A numeric vector of the same length as the input, containing the triangle distance ratio for each location point.
#' Values at the start and end of the vector may be `NA` due to lack of the previous and next points in these cases.
#'
#' @examples
#' x <- c(1, 2, 4, 5)
#' y <- c(1, 1, 1, 1)
#' calculate_triangle_distance_ratio(x, y)
#'
#' @export
calculate_triangle_distance_ratio <- function(X_column, Y_column) {
  
  # Calculate the distance between each location point and the previous point
  dist_p1_p2 <- sqrt((X_column - dplyr::lag(X_column, 1))^2 + (Y_column - dplyr::lag(Y_column, 1))^2)
  
  # Calculate the distance between the previous and next points to each location point
  dist_p1_p3 <- sqrt((dplyr::lead(X_column, 1) - dplyr::lag(X_column, 1))^2 +
                       (dplyr::lead(Y_column, 1) - dplyr::lag(Y_column, 1))^2)
  
  # Calculate ratio between the distances
  triangle_distance_ratio <- dist_p1_p2 / dist_p1_p3
  
  return(triangle_distance_ratio)
  
  ## Function's version for data type data.table
  #
  # # Calculate the distance between each location point and the previous point
  # dist_p1_p2 <- sqrt((X_column - shift(X_column, 1))^2 + (Y_column - shift(Y_column, 1))^2)
  # # Calculate the distance between the previous and next points to each location point
  # dist_p1_p3 <- sqrt((shift(X_column, -1) - shift(X_column, 1))^2 + (shift(Y_column, -1) - shift(Y_column, 1))^2)
  # 
  # # Calculate ratio between the distances
  # triangle_distance_ratio <- dist_p1_p2 / dist_p1_p3
  # 
  # return(triangle_distance_ratio)
  
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

#' Calculate Acceleration from Speed and Time Difference
#'
#' Computes acceleration as the change in speed divided by the time difference between consecutive points.
#'
#' @param speed_column A numeric vector of speed values (in meters per second).
#' @param time_diff_column A numeric vector of time differences (in seconds) between consecutive observations.
#'
#' @return A numeric vector of acceleration values (in meters per second squared), 
#'         with `NA` for the first element and wherever time differences are zero or `NA`.
#'
#' @export
calculate_acceleration <- function(speed_column, time_diff_column) {
  speed_diff <- c(NA, diff(speed_column))
  # Avoid division by zero
  acceleration <- ifelse(time_diff_column > 0, speed_diff / time_diff_column, NA)
  return(acceleration)
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

#' Calculate Cosine of Turning Angles
#'
#' This function calculates the cosine of turning angles for a given sequence of (X, Y) coordinates. 
#' The turning angle is computed for each triplet of consecutive points, with the result being a vector 
#' of cosine values. For the first and last points, the result will be `NA` as turning angles cannot 
#' be computed at these positions.
#'
#' @param X_column A numeric vector representing the X coordinates of the trajectory.
#' @param Y_column A numeric vector representing the Y coordinates of the trajectory.
#'
#' @return A numeric vector of the same length as the input columns, containing the cosine of turning angles. 
#' The first and last elements will be `NA` since turning angles cannot be computed for these points.
#'
#' @examples
#' # Example usage
#' X <- c(1, 2, 3, 4)
#' Y <- c(1, 2, 1, 0)
#' calculate_cosine_turning_angle(X, Y)
#'
#' @export
calculate_cosine_turning_angle <- function(X_column, Y_column) {
  
  # Initialize a vector to store the cosine of turning angles
  cos_angles <- rep(NA, length(X_column))
  
  # Loop through the rows to calculate turning angles
  for (i in 2:(length(Y_column) - 1)) {
      
    # Vectors
    v1 <- c(X_column[i] - X_column[i - 1], Y_column[i] - Y_column[i - 1])  # Previous to current point
    v2 <- c(X_column[i + 1] - X_column[i], Y_column[i + 1] - Y_column[i])  # Current to next point
    
    # Calculate norms
    norm_v1 <- sqrt(sum(v1^2))
    norm_v2 <- sqrt(sum(v2^2))
    
    # Calculate dot product
    dot_product <- sum(v1 * v2)
    
    # Calculate cosine of the turning angle
    cos_angles[i] <- dot_product / (norm_v1 * norm_v2)
    
  }
  
  # Return the data frame with cosine values added as a new column
  return(cos_angles)
}

# Calculate the turning angle between each location point and its previous and next points.
# Results vary between 0 and 360 degrees.
calculate_directional_turning_angle <- function(X, Y) {
  # Compute the differences between consecutive points
  dx <- diff(X)
  dy <- diff(Y)
  
  # Calculate the angle in radians using atan2, then convert to degrees
  angle_rad <- atan2(dy, dx)
  angle_deg <- (angle_rad * 180 / pi) %% 360  # Ensure the angle is between 0 and 360
  
  # Return NA for the first row (since it has no previous point)
  return(c(NA, angle_deg))
}