
# Load the config file
source(file.path(getwd(), "ATLAS_run_scripts", "config.R"))


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
  # Convert time from milliseconds to seconds
  timestamp_seconds <- unix_timestamp / 1000 
  # Convert to POSIXct object
  converted_atlas_time <- as.POSIXct(timestamp_seconds, origin = "1970-01-01", tz = atlas_time_zone)
  # Format the POSIXct object to the desired format
  atlas_time_formatted <- format(converted_atlas_time, atlas_time_format)
  return(atlas_time_formatted)
}

