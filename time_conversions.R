
# Load the config file
source(file.path(getwd(), "Scripts", "config.R"))


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
