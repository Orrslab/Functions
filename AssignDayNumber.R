#' Assign Day Number to Data
#'
#' This function assigns a day number to each row in the input data frame, based on either Julian days or a sequential count from the first date in each group (defined by `GroupIdentifier`). 
#' The day count starts at the specified `DayStartTime` (in "HH:MM:SS" format, UTC), and a new column, `DAY`, is added to the input data frame to hold these values.
#'
#' @param data A `data.frame` containing a POSIXct time column, with timezone set to "UTC" and origin as "1970-01-01".
#' @param DayStartTime A character string in "HH:MM:SS" format specifying the start of each day in UTC. Defaults to "00:00:00".
#' @param TimeColName A character string specifying the name of the time column in the `data` data frame. Defaults to "dateTime".
#' @param GroupIdentifier A character string specifying the name of the column in `data` that identifies the grouping variable (e.g., tag or individual ID). Defaults to "TAG".
#' @param Julian A logical value indicating whether to use Julian days (if `TRUE`) or sequential days from the first date in each group (if `FALSE`). Defaults to `FALSE`.
#'
#' @return A `data.frame` with an additional column, `DAY`, containing the calculated day numbers.
#' 
#' @details
#' - The `DAY` column contains either Julian days or sequential days counted from the first date of each `GroupIdentifier`.
#' - If `Julian` is `TRUE`, `DAY` is based on Julian day numbering. If `FALSE`, it is calculated as sequential days from the first date in each `GroupIdentifier`.
#' - The start of each day is defined by `DayStartTime`, provided as a "HH:MM:SS" character in UTC.
#' - The function checks if the specified `TimeColName` column is in POSIXct format and throws an error if not.
#'
#' @examples
#' # Example usage
#' data <- data.frame(
#'   TAG = c("A", "A", "B", "B"),
#'   dateTime = as.POSIXct(c("2023-01-01 01:00:00", "2023-01-01 23:00:00", 
#'                           "2023-01-02 01:00:00", "2023-01-02 23:00:00"), tz = "UTC")
#' )
#' AssignDayNumber(data, DayStartTime="00:00:00", TimeColName="dateTime", GroupIdentifier="TAG", Julian=FALSE)
#'
#' @export

AssignDayNumber <- function(data,DayStartTime="00:00:00",TimeColName = "dateTime",GroupIdentifier = "TAG",Julian=FALSE)
{
  library(lubridate)
  # Ensure input data is a data frame
  data <- as.data.frame(data)
  
  # Identify the column number for the time variable specified by TimeColName
  dateTimeCol <- which(colnames(data)==TimeColName)
  # Store the original name of the time column for later restoration
  InitialColname <- colnames(data)[dateTimeCol]
  # Temporarily rename the time column to "dateTime" for consistent processing
  colnames(data)[dateTimeCol] <-  "dateTime"
  
  # Identify the column number for the grouping variable specified by GroupIdentifier
  GroupIdentifierCol <- which(colnames(data)==GroupIdentifier)
  # Store the original name of the group column for later restoration
  InitialGroupname <- colnames(data)[GroupIdentifierCol]
  # Temporarily rename the group column to "TAG" for consistent processing
  colnames(data)[GroupIdentifierCol] <-  "TAG"
  
  # Check if the time column is in POSIXct format; if not, raise an error
  if (!inherits(data[[dateTimeCol]], "POSIXct"))
  {er <- errorCondition(sprintf("AssignDayNumber: the specified variable (%s) in the provided data.frame must be in POSIXct format",TimeColName))
    stop(er)
  }
  
  # Assign day numbers based on Julian or sequential day calculation
  if (Julian) {
    # Convert DayStartTime to POSIXct in UTC format to create a reference timestamp for the start of the day
    timeshift <- as.POSIXct(paste("1970-01-01", DayStartTime), format=atlas_time_format, tz=atlas_time_zone)
    # Calculate Julian days (starting from 1970-01-01)
    data <- data %>%
      mutate(DAY = floor(as.numeric(difftime(dateTime, timeshift, units = "days"))) + 1)
      # mutate(DAY = as.numeric(yday(dateTime - timeshift)))
  } else {
    # Calculate sequential days from the first available date in each group (TAG)
    data <- data %>%
      group_by(TAG) %>%
      mutate(DAY = floor(as.numeric(difftime(dateTime, as.POSIXct(paste(min(as.Date(dateTime)), DayStartTime), format=atlas_time_format, tz=atlas_time_zone), units = "days")) + 1)) %>%
      ungroup()
  }
  
  # Convert the column names back to the original ones
  colnames(data)[dateTimeCol] <- InitialColname
  colnames(data)[GroupIdentifierCol] <- InitialGroupname
  
  return(data)
}


