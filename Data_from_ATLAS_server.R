# Load the required packages
required_packages <- c("DBI", "RMySQL", "RSQLite")
sapply(required_packages, library, character.only = TRUE)

# Load the config file
source(file.path(getwd(), "Scripts", "config.R"))

# Load the functions that convert time formats
source(paste0(path_to_atlas_data_analysis_repo, "time_conversions.R"))


#' Retrieve Data from the ATLAS Server
#'
#' This function reads data directly from the ATLAS server, given a specific time range and vector of tags.
#' A VPN connection to the Tel Aviv University (TAU) servers or any other relevant server is required.
#' The function currently only supports the "Harod" system.
#'
#' @param Start_Time_Str A character string representing the start time in the format \code{'YYYY-MM-DD HH:MM:SS'} in UTC.
#' @param End_Time_Str A character string representing the end time in the format \code{'YYYY-MM-DD HH:MM:SS'} in UTC.
#' @param FullTag A vector of tags (as integers) in the format \code{972006000223}.
#' @param includeDet Logical. If \code{TRUE}, detections data is included in the return value. Default is \code{TRUE}.
#' @param includeLoc Logical. If \code{TRUE}, localizations data is included in the return value. Default is \code{TRUE}.
#'
#' @return A list of two data frames:
#' \item{DET}{A data frame containing detections, including columns: \code{TAG}, \code{TIME}, \code{BS}, \code{RSSI}, \code{GAIN}, \code{SNR}, \code{SAMPLES_CLK}.}
#' \item{LOC}{A data frame containing localizations, including columns: \code{TAG}, \code{TIME}, \code{X}, \code{Y}, \code{Z}, \code{VARX}, \code{VARY}, \code{COVXY}, \code{NBS}, \code{PENALTY}.}
#'
#' @details
#' The function converts the start and end time strings into ATLAS time (in milliseconds) 
#' and queries the ATLAS server for detections and/or localizations within the specified time range. 
#' The data for each tag in \code{FullTag} is fetched separately.

Data_from_ATLAS_server <- function(Start_Time_Str,End_Time_Str,FullTag,includeDet=TRUE,includeLoc=TRUE, dbc)
{
  
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

# # Example of using the function 'Data_from_ATLAS_server'
# 
# tag_numbers = c(972006000837)
# Start_Time_Str ='2023-12-24 00:08:00' # start time in UTC
# End_Time_Str   ='2023-12-24 00:09:00' # end time in UTC
# 
# AllData <- Data_from_ATLAS_server(Start_Time_Str,End_Time_Str,tag_numbers)