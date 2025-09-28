library(data.table)

#' Load and format base stations metadata
#'
#' This function loads the base stations information from a CSV file, standardizes date formats,
#' handles "Infinity" entries, and renames columns for compatibility with localization data.
#'
#' @param base_stations_info_path A character string specifying the path to the base stations info CSV file.
#' The information about the base stations can be taken from the \...\atlas\runs\receiver\locations.txt configuration file of the atlas system.
#'
#' @return A `data.table` with the following columns:
#' \describe{
#'   \item{Radio_serial_number}{Serial number of the base stations}
#'   \item{bs_lat}{Latitude of the base stations}
#'   \item{bs_lon}{Longitude of the base stations}
#'   \item{Since}{Start time of station activity in certain coordinates, as Unix timestamp}
#'   \item{Until}{End time of station activity in certain coordinates, as Unix timestamp (with "Infinity" if these are the current coordinates of the base stations.)}
#' }
#'
#' @details
#' \itemize{
#'   \item Replaces "Infinity" in the `Until` column with a placeholder far date ("2100-Jan-01 00:00")
#'   \item Forces the system locale to `"C"` to ensure reliable month parsing (e.g., for September)
#'   \item Parses date strings into POSIXct, then converts them into Unix timestamp numeric values
#'   \item Renames latitude and longitude columns to `bs_lat` and `bs_lon` to prevent confusion with localization coordinates later.
#' }
#'
#' @examples
#' \dontrun{
#' bs_info <- load_and_format_base_stations_info("path/to/Base_stations_info.csv")
#' }
#'
#' @import data.table
#' @export

load_and_format_base_stations_info <- function(base_stations_info_path) {
  
  # Load the base stations info
  bs_info <- fread(base_stations_info_path)
  
  # Change "Infinity in the Since column to a far future data
  bs_info[, Until := ifelse(Until == "Infinity", "2100-Jan-01 00:00", Until)]
  
  # Ensure consistent locale for parsing- without it the code does not convert September dates correctly...
  Sys.setlocale("LC_TIME", "C")
  
  # Convert bs times to POSIXct (correct time format assumed)
  bs_info[, Since := as.POSIXct(Since, format = "%Y-%b-%d %H:%M", tz = "UTC")]
  bs_info[, Until := as.POSIXct(Until, format = "%Y-%b-%d %H:%M", tz = "UTC")]
  
  # Convert bs times to numeric (Unix timestamp)
  bs_info[, Since := as.numeric(Since)]
  bs_info[, Until := as.numeric(Until)]
  
  # Filter relevant columns and rename to distinguish them from the localizations' coordinates
  bs_info <- bs_info[, .(Radio_serial_number, bs_lat = Latitude, bs_lon = Longitude, Elevation, Since, Until)]
  
  return(bs_info)
}

