Data_from_ATLAS_server <- function(Start_Time_Str,End_Time_Str,FullTag)
{
  dbc <- dbConnect(RMySQL::MySQL(),
                   user = 'roatlasharod',            # username 
                   password = 'roatlasHarodOrr5678#',# password
                   host = '132.66.79.21',            # host ip address
                   port=5900,                        # port Number
                   dbname='harod')                   # name of data base
  
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

  AllTagsDet <- list() #make an empty list for detections
  
  for (i in 1:length(FullTag)) 
    {
    query = paste('select TAG,TIME,BS,SNR,SAMPLES_CLK from DETECTIONS WHERE TAG=',FullTag[i],
                  'AND TIME >=', ATLAS_Start_Time, 'AND TIME <=', ATLAS_End_Time)
    All_Data <- dbGetQuery(dbc,query)
    AllTagsDet[[i]] <- All_Data
    }
  
  
  AllTagsLoc <- list() #make an empty list for localizations
  
  for (i in 1:length(FullTag)) 
    {
    query = paste('select TAG,TIME,X,Y,Z,VARX,VARY,"NBS",COVXY,NCONSTRAINTS from LOCALIZATIONS WHERE TAG=',FullTag[i],
                  'AND TIME >=', ATLAS_Start_Time, 'AND TIME <=', ATLAS_End_Time)
    All_Data <- dbGetQuery(dbc,query)
    AllTagsLoc[[i]] <- All_Data
    }
  dbDisconnect(dbc)
  RawDet0 <- do.call(rbind.data.frame, AllTagsDet)
  RawLoc0 <- do.call(rbind.data.frame, AllTagsLoc)
  A <- list(RawDet0,RawLoc0)
  return(A)
}