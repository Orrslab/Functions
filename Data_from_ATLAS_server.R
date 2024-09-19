# Load the required packages
required_packages <- c("DBI", "RMySQL", "RSQLite")
sapply(required_packages, library, character.only = TRUE)

# Load the config file
source(file.path(getwd(), "Scripts", "config.R"))


#' Read Data from the ATLAS Server
#'
#' This function reads data directly from the ATLAS server, given a specific time range and vector of tags.
#' A VPN connection to the Tel Aviv University (TAU) servers or any other relevant server is required.
#' The function currently only supports the "Harod" system.
#'
#' @param Start_Time_Str A character string representing the start time in the format \code{'YYYY-MM-DD HH:MM:SS'} in UTC.
#' @param End_Time_Str A character string representing the end time in the format \code{'YYYY-MM-DD HH:MM:SS'} in UTC.
#' @param FullTag A vector of tags (as integers) in the format \code{972006000223}.
#' @param SYS The name of the system. Currently, only \code{"Harod"} is implemented.
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

Data_from_ATLAS_server <- function(Start_Time_Str,End_Time_Str,FullTag, SYS=system_name_harod,includeDet=TRUE,includeLoc=TRUE)
{
  
  #connects to the Harod server
  if (SYS==system_name_harod){
    # Get the Harod database credentials from the configuration file
    db_username = db_username_harod
    db_pass = db_pass_harod
    db_host_ip = db_host_ip_harod
    db_port_number = db_port_number_harod
    db_name = db_name_harod
    
    # Connect to the database
    dbc <- dbConnect(RMySQL::MySQL(),
                     user = db_username,    # username 
                     password = db_pass,    # password
                     host = db_host_ip,     # host ip address
                     port=db_port_number,   # port Number
                     dbname=db_name)        # name of data base
  } else {
    stop("system not defined")
  }
  
  # --- Examine the tables contained in the database 
  # dbListTables(dbc)           
  
  # --- Examine the names of the columns in a table
  # dbListFields(dbc, 'DETECTIONS')
  # dbListFields(dbc, 'LOCALIZATIONS')
  
  # --- Set start & end time and convert to ATLAS time

  Start_Time_Str_Temp <- as.character.Date(Start_Time_Str) 
  ATLAS_Start_Time<-as.numeric(as.POSIXct(Start_Time_Str_Temp,
                                          "%Y-%m-%d %H:%M:%S", tz="UTC"))*1000
  End_Time_Str_Temp <- as.character.Date(End_Time_Str)
  ATLAS_End_Time<-as.numeric(as.POSIXct(End_Time_Str_Temp,
                                        "%Y-%m-%d %H:%M:%S", tz="UTC"))*1000 

  # Initialize lists for detections and localizations
  AllTagsDet <- list() 
  AllTagsLoc <- list()

  # Query for detections: check if includeDet is TRUE
  if(includeDet) {  
    for (i in 1:length(FullTag)) { 
      # build a  DETECTIONS query for the system, the results include the variables listed below
      query = paste('select TAG,TIME,BS,RSSI,GAIN,SNR,SAMPLES_CLK from DETECTIONS WHERE TAG=',FullTag[i],
                    'AND TIME >=', ATLAS_Start_Time, 'AND TIME <=', ATLAS_End_Time)
      All_Data <- dbGetQuery(dbc,query)
      AllTagsDet[[i]] <- All_Data
    }
  }
  
  # Query for localizations: check if includeLoc is TRUE
  if(includeLoc) {
    for (i in 1:length(FullTag)) { 
      # build a  LOCALIZATIONS query for the system, the results include the variables listed below # NCONSTRAINTS
      query = paste('select TAG,TIME,X,Y,Z,VARX,VARY,COVXY,NBS,PENALTY from LOCALIZATIONS WHERE TAG=',FullTag[i], 
                    'AND TIME >=', ATLAS_Start_Time, 'AND TIME <=', ATLAS_End_Time)
      All_Data <- dbGetQuery(dbc,query)
      AllTagsLoc[[i]] <- All_Data
    }
  }
  
  # Disconnect from the database and combine results
  dbDisconnect(dbc)
  RawDet0 <- do.call(rbind.data.frame, AllTagsDet)
  RawLoc0 <- do.call(rbind.data.frame, AllTagsLoc)
  A <- list("DET"=RawDet0,"LOC"=RawLoc0)
  
  return(A)
}