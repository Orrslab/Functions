
#' Retrieve Data from the ATLAS Server
#'
#' This function reads animal tracking data directly from the ATLAS database for a given time range and set of tag IDs.
#' It queries the database for **detections** and/or **localizations** and returns them as data frames.
#'
#' @param Start_Time_Str A character string representing the **start time** in UTC, formatted as \code{"YYYY-MM-DD HH:MM:SS"}.
#' @param End_Time_Str A character string representing the **end time** in UTC, formatted as \code{"YYYY-MM-DD HH:MM:SS"}.
#' @param FullTag A numeric vector of tag IDs, where each ID is an integer (e.g., \code{972006000223}).
#' @param includeDet Logical. If \code{TRUE} (default), detections data is retrieved.
#' @param includeLoc Logical. If \code{TRUE} (default), localizations data is retrieved.
#' @param dbc A **database connection object** (from \code{DBI::dbConnect()}), representing an active connection to the ATLAS database.
#'
#' @return A named **list** with two elements:
#' \itemize{
#'   \item{\code{DET}:} {A **data frame** containing detections data, with columns: \code{TAG}, \code{TIME}, \code{BS}, \code{RSSI}, \code{GAIN}, \code{SNR}, \code{SAMPLES_CLK}.}
#'   \item{\code{LOC}:} {A **data frame** containing localizations data, with columns: \code{TAG}, \code{TIME}, \code{X}, \code{Y}, \code{Z}, \code{VARX}, \code{VARY}, \code{COVXY}, \code{NBS}, \code{PENALTY}.}
#' }
#'
#' If no data is found for a tag, the corresponding data frame will be empty.
#'
#' @details
#' - This function **requires a VPN connection** to the **Tel Aviv University (TAU) network** or another relevant ATLAS database server.
#' - It **only supports the "Harod" system** at the moment.
#' - The function converts the start and end times into **ATLAS time format** (milliseconds since epoch).
#' - Queries are executed **individually for each tag** in `FullTag`, and the results are combined.
#'
#' @import DBI
#' @import RMySQL
#' @import RSQLite
#'
#' @examples
#' \dontrun{
#'
#' # Define tag numbers and time range
#' tag_numbers <- c(972006000837)
#' Start_Time_Str <- "2023-12-24 00:08:00" # Start time in UTC
#' End_Time_Str   <- "2023-12-24 00:09:00" # End time in UTC
#'
#' # Retrieve data from ATLAS server
#' AllData <- data_from_atlas_server(Start_Time_Str, End_Time_Str, tag_numbers, dbc = dbc)
#'
#' # View detections and localizations data
#' head(AllData$DET)
#' head(AllData$LOC)
#' }
#'
#' @export
data_from_atlas_server <- function(Start_Time_Str,End_Time_Str,FullTag,includeDet=TRUE,includeLoc=TRUE, dbc)
{
  
  # Load the required packages
  required_packages <- c("DBI", "RMySQL", "RSQLite")
  invisible(lapply(required_packages, library, character.only = TRUE))
  
  # Load the functions that convert time formats
  source(paste0(getwd(), "/time_conversions.R"))
  
  # Convert the start and end times to the ATLAS time format
  ATLAS_Start_Time <- time_str_to_utc_timestamp(Start_Time_Str)
  ATLAS_End_Time <- time_str_to_utc_timestamp(End_Time_Str)

  # Initialize lists for detections and localizations
  AllTagsDet <- list() 
  AllTagsLoc <- list()

  # If 'includeDet' is TRUE, retrieve the detections data from the database
  if(includeDet) {  
    for (i in 1:length(FullTag)) { 
      # build a  DETECTIONS query for the system, the results include the variables listed below
      query = paste('select TAG,TIME,BS,RSSI,GAIN,SNR,SAMPLES_CLK from DETECTIONS WHERE TAG=',FullTag[i],
                    'AND TIME >=', ATLAS_Start_Time, 'AND TIME <=', ATLAS_End_Time)
      All_Data <- dbGetQuery(dbc,query)
      AllTagsDet[[i]] <- All_Data
    }
  }
  
  # If 'includeLoc' is TRUE, retrieve the locations data from the database
  if(includeLoc) {
    for (i in 1:length(FullTag)) { 
      # build a  LOCALIZATIONS query for the system, the results include the variables listed below # NCONSTRAINTS
      query = paste('SELECT TAG,TIME,X,Y,Z,VARX,VARY,COVXY,NBS,PENALTY', 
                    'FROM LOCALIZATIONS WHERE TAG=', FullTag[i], 
                    'AND TIME >=', ATLAS_Start_Time, 
                    'AND TIME <=', ATLAS_End_Time)
      All_Data <- dbGetQuery(dbc,query)
      AllTagsLoc[[i]] <- All_Data
    }
  }
  
  # Combine the ATLAS data of different tags to a single data frame
  RawDet0 <- do.call(rbind.data.frame, AllTagsDet)
  RawLoc0 <- do.call(rbind.data.frame, AllTagsLoc)
  
  return(list("DET"=RawDet0,"LOC"=RawLoc0))
}