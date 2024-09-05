

# Download ATLAS data directly from the server
# This requires a VPN connection to the TAU server

source("C:/Users/netat/Documents/Movement_Ecology/R_Projects/Functions/Data_from_ATLAS_server.R")

#Tags <- read.csv("Atlas_Tag_Usage_17022021.csv")
#Tags <- Tags[which(Tags$Deployed=="Y"),]  
# Tags <- Tags[which(Tags$Working. =="Y"),] 
# Start_Time_Str ='2024-09-04 00:10:00' # start time in UTC
# End_Time_Str   ='2024-09-04 00:11:00' # end time in UTC
#FullTag <- c(972006000615,972006000558,972006000226)
#FullTag <- Tags$Tag.Number[-which(Tags$Tag.Number %in% c(972006000003,972006000004,972006000006))]
#Sys.time()
# tag_numbers = c(972006000837)
AllData <- Data_from_ATLAS_server(Start_Time_Str,End_Time_Str,tag_numbers)
#Sys.time()
RawLoc0 <- AllData$LOC
RawDet0 <- AllData$DET #21:33 - 21:39
#Sys.Time()
