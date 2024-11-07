
# Load the config file
source(file.path(getwd(), "config.R"))


#' Convert a Time String to a UTC Time Stamp in Milliseconds
#'
#' This function converts a time string in the format \code{"%Y-%m-%d %H:%M:%S"} into
#' a numeric UTC timestamp expressed in milliseconds since the Unix epoch 
#' (\code{1970-01-01 00:00:00 UTC}).
#'
#' @param timestring A character string representing the time in the format \code{"%Y-%m-%d %H:%M:%S"}.
#'
#' @return A numeric value representing the time in milliseconds since the Unix epoch.
#'
#' @examples
#' utc_time <- time_str_to_utc_object("2024-09-19 12:00:00")
#' 
time_str_to_utc_timestamp <- function(timestring)
{
  
  utc_timestamp <- as.numeric(as.POSIXct(timestring, atlas_time_format, tz=atlas_time_zone))*1000
  return(utc_timestamp)
  
}

#' Convert Milliseconds Timestamp to POSIXct
#'
#' This function converts a Unix timestamp in milliseconds to a `POSIXct` date-time object in UTC.
#'
#' @param timestamp_ms A numeric vector representing Unix timestamps in milliseconds (ms since 1970-01-01).
#'
#' @return A `POSIXct` vector in UTC, representing the converted date-time values.
#' @examples
#' # Example usage with a single timestamp
#' convert_to_POSIXct(1703376004071)
#'
#' # Example usage with a vector of timestamps
#' convert_to_POSIXct(c(1703376004071, 1703377004071))
#'
#' @export
convert_to_POSIXct <- function(timestamp_ms) {
  # Check if input is numeric
  if (!is.numeric(timestamp_ms)) {
    stop("The input must be a numeric vector representing Unix timestamps in milliseconds.")
  }
  
  # Convert milliseconds to seconds and then to POSIXct in UTC
  posix_time <- as.POSIXct(timestamp_ms / 1000, origin = "1970-01-01", tz = atlas_time_zone)
  
  # Return the converted POSIXct time
  return(posix_time)
}


#' Convert Unix Timestamp to a Human-readable Date in ATLAS Format
#'
#' This function converts Unix timestamps (in milliseconds) to a human-readable date format 
#' according to the ATLAS system specifications. 
#' It handles both single timestamp values and vectors of Unix Timestamps.
#'
#' @param unix_timestamp A numeric vector or single numeric value representing Unix timestamps in milliseconds.
#'
#' @return A character vector of formatted date and time strings in the ATLAS format. 
#' If the input is a single value, the return will be a single string.
#' 
unix_timestamp_to_human_date <- function(unix_timestamp) {
  # Convert to POSIXct object
  converted_atlas_time <- convert_to_POSIXct(unix_timestamp)
  # Format the POSIXct object to the desired format
  atlas_time_formatted <- format(converted_atlas_time, atlas_time_format)
  return(atlas_time_formatted)
}

