# a genral template for running ATLAS codes

#---------- cleaning and setting preferences ----------------------------- 
{
rm(list=ls()) # clean history 
options(digits = 14) # Makes sure long numbers are not abbreviated.
# Sys.setenv(TZ = 'UTC')
}
# --------- Setting paths  ------------
{
general_path <- "C:/Users/97254/Google Drive/POST/ATLAS/" # my dell computer directory
# general_path <- "C:/Users/eitam.arnon/OneDrive - GTIIT/POST/ATLAS/" # my GT computer
setwd       (paste0(general_path,"Harod"))
path2func <- paste0(general_path,"functions/")
path2data <- paste0(general_path,"data/")
}
# --------- Sourcing libraries and functions -------------- 
{
source(paste0(path2func,"ConnectLib.R"))
ConnectLib(path2func)
}
# --------- Another sourcing option -------------
{
pcks <- list("mapview","dplyr","lubridate","dbscan","dbscan","sp","leaflet",
             "htmltools","RSQLite","RSQLite","ggpubr","ggpubr","ggmap","ggmap",
             "ggplot2","toolsForAtlas","atlastools") # a list of external packages to source
sapply(pcks, require, char = TRUE)        #sourcing these packages
files.sources = list.files(path2func)     # a list of local functions to source
files.sources <- files.sources[grep(".R",files.sources)]
sapply(paste0(path2func,files.sources), source) #sourcing these functions
}
# --------- Load raw data & from a local SQLite file created using an ATLAS query --------------------------
{
file_name<-"loc_lapwing4spec-Aug2020.sqlite"
dbname=paste(path2data,file_name,sep="") # full path name
RawLoc0<-loadFromSQLite(dbname) # load data atlastools function
RawLoc0$TIME<-as.double(RawLoc0$TIME) # bypass the default 'integer64' class that is the default when querying RawLoc0 ATLAS data
RawLoc0$TAG<-as.character(RawLoc0$TAG) # Change from meaningless "factor" class to character (text format) 
# select only relevant columns:
redundant_columns<-c("TX", "Z","PENALTY","GRADNRM","VARZ","COVXZ","COVYZ","NBS","DIM")
RawLoc0<-RawLoc0[,-which(colnames(RawLoc0) %in% redundant_columns)]
}
# --------- Downloading data directly from server (requires VPN to TAU) -----------------
{
Start_Time_Str ='2020-10-10 12:00:00' # define start time
End_Time_Str ='2020-10-14 12:00:00' # Need to change to current date
FullTag <- c(972006000223, 972006000219)
AllData <- Data_from_ATLAS_server(Start_Time_Str,End_Time_Str,FullTag)
RawLoc0 <- AllData[[2]]
RawDet0 <- AllData[[1]]
}
# --------- Save data to SQLite file ------------------------------------
{
# saveIntoSQLite(dbname,RawLoc1) # (toolsForAtlas fuction didn't work for me)
file_name<-"loc_Owl223-11Oct2020.sqlite"
dbname=paste0(path2data,file_name) # full path name
conn <- dbConnect(RSQLite::SQLite(), dbname)
dbWriteTable(conn, "LOC", RawLoc0,overwrite=T)
dbWriteTable(conn, "DET", RawDet0,overwrite=F, append=T)
#  checking and reading!
dbListTables(conn)
dbListFields(conn, 'LOC')
RawLoc0 <- dbGetQuery(conn, "SELECT * FROM LOC")
RawDet0<- dbGetQuery(conn, "SELECT * FROM DET")
dbDisconnect(conn)
rm(conn)
}
# --------- Basic adding of the data --------------------------

# Order all data according to TAG and Then Time 
RawDet1<-RawDet0[order(RawDet0$TAG,RawDet0$TIME),] #make sure data is sorted chronologically (per tag)
RawLoc1<-RawLoc0[order(RawLoc0$TAG,RawLoc0$TIME),] #make sure data is sorted chronologically (per tag)

# Create a new columns with geographic, wgs84 coordinates
RawLoc1 <-convertSpatial.ITM2WGS84(RawLoc1, xyColNames=c("X","Y"))
RawLoc1 <- as.data.frame(RawLoc1)

# Create a new column with human readablr time
RawDet1$DetecTime<-as.POSIXct((RawDet1$TIME)/1000, tz="UTC", origin="1970-01-01")
# RawLoc1$LocTime<-as.POSIXct((RawLoc1$TIME)/1000, tz="UTC", origin="1970-01-01")

# Create a new columns: datetime distance, speed, angle, STD,
# angle is the angle between sections and not the turning angle
RawLoc1<-addLocAttribute(RawLoc1, locAttributs=c("distanceSpeed", "locQuality","angle")) # function to add attributes for each pair of conseucutive points. 
RawLoc1$angl <- 180-abs(RawLoc1$angl)

# Create a new columns with Sun and Moon angle above the horizen
Sun_pos <-  getSunlightPosition(date=RawLoc1$dateTime,lat = RawLoc1$LAT[1], lon = RawLoc1$LON[1])
Moon_pos <- getMoonPosition    (date=RawLoc1$dateTime,lat = RawLoc1$LAT[1], lon = RawLoc1$LON[1]) 
RawLoc1$Sun_angle <- Sun_pos$altitude/pi*180
RawLoc1$Moon_angle <- Moon_pos$altitude/pi*180

# Create a new columns with day numbering
RawLoc1 <- AssignDayNumber(RawLoc1,DayStartTime="12:00:00",TimeColName="dateTime")

# Specify tag by 2 or three meaningful digits
RawLoc1$TAG<-gsub("9720060000", '', RawLoc1$TAG) #9720010000 for Hula system
RawLoc1$TAG<-gsub("972006000", '', RawLoc1$TAG)
RawLoc1$TAG<-gsub("97200600", '', RawLoc1$TAG)

# deleting redundant columns
redundant_columns<-c("traceNorm","angl","NBS","Z") # columns we won't use in this example.
RawLoc1<-RawLoc1[,-which(colnames(RawLoc1) %in% redundant_columns)]

rm(AllData,Moon_pos,RawDet0,RawLoc0,Sun_pos)
rm(Start_Time_Str,End_Time_Str,redundant_columns,times,FullTag) # remove objects we no longer neeed
table(RawLoc1$DAY,RawLoc1$TAG)

# --------- Basic filtering -----------------------------------------
TAGTIMES <- Tagtime("tagtimes.csv") # reads a csv file with the relevant times to each tag
# add frequency to Tagtime!
TAGTIMES <- Tagtime(RawLoc1)

# can filter any attribute :  tag, STD,NCONSTRAINTS, time: ilters=c(" Sun_angle<5 ","between( X,2.39e5, 2.50e5)")
# here filters sun elevation, position:
# FiltLoc0 <- atl_filter_covariates(data=RawLoc1, filters=c(" Sun_angle<5 ")) (from pratik, atlastools)
FiltLoc0 <- RawLoc1[which(RawLoc1$Sun_angle<5),]
# ----------------- Plotting histograms & summary statistics: --------

x11()
dev.set(which = dev.prev())
dev.off()
table(FiltLoc0$DAY,FiltLoc0$TAG)
hist(FiltLoc0$stdVarXY) # due to very few exremely high values, we can't see the distribution of most values.
par(mfrow=c(1,2))
hist(FiltLoc0$stdVarXY[which(FiltLoc0$DAY%in% c(1,2,3) & FiltLoc0$TAG%in% c("223"))],xlim =c(0,100), breaks=10000, main= "Stdev Distribution") # Now the distribution is more informative.
hist(FiltLoc0$spd,  xlim=c(0,50), breaks=10000,main= "Speed Distribution") # Note that the speed units are (meters/seconds)
summary(FiltLoc0$stdVarXY)
summary(FiltLoc0$spd)
quantile(FiltLoc0$stdVarXY, c(0.5,0.6,0.7,0.8,0.9,0.95,0.99), na.rm=TRUE)
quantile(FiltLoc0$spd,c(0.5,0.6,0.7,0.8,0.9,0.95,0.99), na.rm=TRUE)

# -----------------  Basic plotting -------------------------------------
unique(FiltLoc0$TAG) # choose a radom TAG.

TAG_ex<-223
A <- FiltLoc0[which(FiltLoc0$TAG==TAG_ex),]
plot(FiltLoc0$X[which(FiltLoc0$TAG==TAG_ex)],FiltLoc0$Y[which(FiltLoc0$TAG==TAG_ex)])
limits <- identify(A$X,A$Y, labels =paste0( round(A$X[1],0),",", round(A$Y[1],0)) , plot=TRUE)
round(A$X[limits])
round(A$Y[limits])
xlims <- c(2.39e5, 2.50e5)
ylims <- c(7.14e5, 7.28e5)
plot(FiltLoc0$X[which(FiltLoc0$TAG==TAG_ex)],FiltLoc0$Y[which(FiltLoc0$TAG==TAG_ex)],xlim=xlims,ylim=ylims,asp=1)

plotdays(FiltLoc0,TAG_ex,xlims,ylims)
atl_mapleaf(FiltLoc0[which(FiltLoc0$DAY==2&FiltLoc0$TAG==TAG_ex),])
atl_mapgg(FiltLoc0[which(FiltLoc0$DAY==2&FiltLoc0$TAG==TAG_ex),])

# -----------------------  FILTERING           --------------------
    AA <- as.data.frame(FiltLoc0 %>%  filter(TAG==TAG_ex))
    # A <- as.data.frame(A %>%  filter(DAY==DAY_ex))
#tracking amount of lost data
    data_track <- as.numeric(nrow(AA))

#  coordinate box
    coords_box <- c(2.39e5, 2.50e5, 7.14e5, 7.28e5)
    A<-AA[which((AA$X>coords_box[1])&(AA$X<coords_box[2])&(AA$Y>coords_box[3])&(AA$Y<coords_box[4])),]
    plot(AA$X[which(AA$TAG==TAG_ex)],AA$Y[which(AA$TAG==TAG_ex)])
    points(A$X[which(A$TAG==TAG_ex)],A$Y[which(A$TAG==TAG_ex)],col="red")
    data_track <- rbind(data_track,nrow(A))

# STD filtering
    stdev<-20 # stdev 
    B<-A[which(A$stdVarXY<stdev),]
    points(B$X[which(B$TAG==TAG_ex)],B$Y[which(B$TAG==TAG_ex)],col="green")
    lines (B$X[which(B$TAG==TAG_ex)],B$Y[which(B$TAG==TAG_ex)],col="green")
    data_track <- rbind(data_track,nrow(B))

#velocity filtering
    spdThreshold<-20 # speed 
    C <- B
    C <- velocity_filter (C,spdThreshold, x = "X", y = "Y", time = "TIME", steps=10)
    C <- distance_filter (C,distThreshold=500, x = "X", y = "Y", steps=5)
    C <- addLocAttribute(C, locAttributs=c("speed")) # function to add attributes for each pair of conseucutive points. 
    points(C$X[which(C$TAG==TAG_ex)],C$Y[which(C$TAG==TAG_ex)],col="black")
    lines (C$X[which(C$TAG==TAG_ex)],C$Y[which(C$TAG==TAG_ex)],col="black")
    data_track <- rbind(data_track,nrow(C))
    atl_mapleaf(C[which(C$DAY==2&C$TAG==TAG_ex),])
# plotting filtered data
  TAG_ex <- 223
  DAY_ex <- 2
  E <- A[which(A$DAY==DAY_ex&A$TAG==TAG_ex),]
  E <- B[which(B$DAY==DAY_ex&B$TAG==TAG_ex),]
  E <- C[which(C$DAY==DAY_ex&C$TAG==TAG_ex),]
  E <- D[which(D$DAY==DAY_ex&D$TAG==TAG_ex),]
  atl_mapleaf(D)
  plot(E$X,E$Y)
  lines(E$X,E$Y)
  points(E$X,E$Y,col="red",pch=3)
  lines(E$X,E$Y,col="green")
  
  D <- addLocAttribute(D, locAttributs=c("speed")) # function to add attributes for each pair of conseucutive points. 
  plot(D$spd)

# Advanced velocity Filter ----------
  optionsArg=c("v_filter_max_v"=spdThreshold,
             "min_time_dif_for_stats"=5,
             "v_filter_time_dif"=12)
  # v_filter_max_v; #maximum allowed velocity
  # min_time_dif_for_stats; # number of consecutive valid points to acquire reliability
  # v_filter_time_dif; # number of allowed missing time-steps before reliability is lost
  D<-filterByVelocity(B,
                       dataLegend=c("X","Y","TIME"),
                       options=optionsArg)

# bind different tags together: --------------------------------
  FiltLoc1 <- NULL
  FiltLoc1 <- rbind(FiltLoc1, C)
  rm(A,B,AA,C,D)
  
# -----------Track Segmentation: Separate and summarize stops vs movement-------------

  ADP <- wrap_ADP(FiltLoc1,freq=8)

# plotting stops over tacks on a map. 
  TAG_ex <- 223
  dispDAY <- c(2,3)
  atl_mapleaf_withstops(list(FiltLoc1,ADP),Tags=TAG_ex,Days=dispDAY)

  hist(stops$position_qlt, breaks=20, main="Stops Position Quality Distribution")
  summary(stops$position_qlt)
  write.csv(ADP, "data/bats_stops.csv")