
#' @title Get Global Time Range from Data Requests
#'
#' @description
#' Given a list of data requests, each containing `start_time` and `end_time`
#' elements, this function finds the earliest start time and the latest end time
#' across all requests. The times are returned as character strings.
#'
#' @param data_requests A list of lists, where each element should contain
#'   \code{start_time} and \code{end_time} fields in the format
#'   \code{"%Y-%m-%d %H:%M:%S"}.
#'
#' @return A named list with two character strings:
#' \itemize{
#'   \item \code{start_time} - the earliest start time across all requests.
#'   \item \code{end_time}   - the latest end time across all requests.
#' }
#'
#' @examples
#' data_requests <- list(
#'   list(animal_name_code = "GJ", 
#'        tag = 972006000761, 
#'        start_time = "2023-10-01 00:00:01", 
#'        end_time = "2023-10-31 23:59:00"),
#'   list(animal_name_code = "GJ", 
#'        tag = 972006000781, 
#'        start_time = "2025-10-01 00:00:01", 
#'        end_time = "2025-10-31 23:59:00")
#' )
#'
#' get_global_time_range_of_data_requests(data_requests)
#'
#' @export
#' 
get_global_time_range_of_data_requests <- function(data_requests, data_time_zone, data_time_format) {
  # Extract start and end times
  start_times <- sapply(data_requests, function(x) x$start_time)
  end_times   <- sapply(data_requests, function(x) x$end_time)
  
  # Convert to POSIXct
  start_times <- as.POSIXct(start_times, tz = data_time_zone, format = data_time_format)
  end_times   <- as.POSIXct(end_times, tz = data_time_zone, format = data_time_format)
  
  # Find min/max and return as strings
  list(
    start_time = format(min(start_times, na.rm = TRUE), data_time_format),
    end_time   = format(max(end_times, na.rm = TRUE), data_time_format)
  )
}