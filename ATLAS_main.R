# a general template for running ATLAS codes
# Includes examples for most functions in the repository

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
path2data <- paste0(general_path,"Harod/data/")
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


file_name <-"loc_TAG212_Jan122021_2D_l.sqlite"

dbname=paste(path2data,file_name,sep="") # full path name

conn <- dbConnect(RSQLite::SQLite(), dbname)
dbListTables(conn)
dbListFields(conn, 'DETECTIONS')
dbListFields(conn, 'LOCALIZATIONS')

RawLoc0 <- dbGetQuery(conn, "SELECT * FROM LOCALIZATIONS")
RawDet0<- dbGetQuery(conn, "SELECT * FROM DETECTIONS")
table(RawLoc0$TAG)
table(RawDet0$TAG)
dbDisconnect(conn)
rm(conn)

RawLoc0$TIME<-as.double(RawLoc0$TIME) # bypass the default 'integer64' class that is the default when querying RawLoc0 ATLAS data
RawLoc0$TAG<-as.character(RawLoc0$TAG) # Change from meaningless "factor" class to character (text format) 
# select only relevant columns:
redundant_columns<-c("TX", "Z","GRADNRM","VARZ","COVXZ","COVYZ","DIM")
RawLoc0<-RawLoc0[,-which(colnames(RawLoc0) %in% redundant_columns)]
}
# --------- Downloading data directly from server (requires VPN to TAU)-----------------
{
  Tags <- read.csv("Atlas_Tag_Usage_17022021.csv")
  Tags <- Tags[which(Tags$Deployed=="Y"),]  
  # Tags <- Tags[which(Tags$Working. =="Y"),] 
Start_Time_Str ='2021-03-27 00:00:01' # define start time
End_Time_Str   ='2021-03-29 24:00:00' # Need to change to current date
FullTag <- c(972006000201,972006000161)
FullTag <- Tags$Tag.Number[-which(Tags$Tag.Number %in% c(972006000003,972006000004,972006000006))]
AllData <- Data_from_ATLAS_server(Start_Time_Str,End_Time_Str,FullTag)
RawLoc0 <- AllData$LOC
RawDet0 <- AllData$DET
}

# --------- Save data to SQLite file ------------------------------------
{
# saveIntoSQLite(dbname,RawLoc1) # (toolsForAtlas fuction didn't work for me)
file_name<-"car_strategies.sqlite"
dbname=paste0(path2data,file_name) # full path name
conn <- dbConnect(RSQLite::SQLite(), dbname)
dbWriteTable(conn, "LOC", a_list,overwrite=T)
dbWriteTable(conn, "DET", RawDet0,overwrite=F, append=T)
}
# back to sqlite format readable by kamadata
exportForKamadata(loc.df = RawLoc0,sqliteName ="data4kamada.sqlite",atlas.system = "harod")
# ---------------------reading and checking saved data ----------
{
dbListTables(conn)
dbListFields(conn, 'LOC')
RawLoc0 <- dbGetQuery(conn, "SELECT * FROM LOC")
RawDet0<- dbGetQuery(conn, "SELECT * FROM DET")
dbDisconnect(conn)
rm(conn)
}
# --------- Basic adding of the data --------------------------


#adding antenna data to locations ( requires corresponding detection - same tags and times)
RawLoc1 <- RawLoc0
RawLoc1 <- identifyDetections(Loc=RawLoc0,Det=RawDet0,unloclalized =F)
# Order all data according to TAG and Then Time 
RawDet1<-RawDet0[order(RawDet0$TIME),] #make sure data is sorted chronologically (per tag)
RawLoc1<-RawLoc1[order(RawLoc1$TAG,RawLoc1$TIME),] #make sure data is sorted chronologically (per tag)

# Create a new columns with geographic, wgs84 coordinates
RawLoc1 <-convertSpatial.ITM2WGS84(RawLoc1, xyColNames=c("X","Y"))
RawLoc1 <- as.data.frame(RawLoc1)

# Create a new column with human readable time
RawDet1$dateTime<-as.POSIXct((RawDet1$TIME)/1000, tz="UTC", origin="1970-01-01")
RawLoc1$dateTime<-as.POSIXct((RawLoc1$TIME)/1000, tz="UTC", origin="1970-01-01")

# Create a new columns: datetime distance, speed, angle, STD,
# angle is the angle between sections and not the turning angle
RawLoc1<-addLocAttribute(RawLoc1, locAttributs=c("distanceSpeed", "locQuality","angle")) # function to add attributes for each pair of conseucutive points. 
RawLoc1$angl <- 180-abs(RawLoc1$angl)


 # Create a new columns with Sun and Moon angle above the horizon (accurate to each location and time)
Sun_pos <-  getSunlightPosition(data=data.frame(date=RawLoc1$dateTime,lat=RawLoc1$LAT,lon=RawLoc1$LON))
Moon_pos <- getMoonPosition    (data=data.frame(date=RawLoc1$dateTime,lat=RawLoc1$LAT,lon=RawLoc1$LON)) 
RawLoc1$Sun_angle <- Sun_pos$altitude/pi*180 #inserted int to data.frame, converted to degrees
RawLoc1$Moon_angle <- Moon_pos$altitude/pi*180
  
# Create a new columns with day numbering
RawLoc1 <- AssignDayNumber(RawLoc1,DayStartTime="00:00:00",TimeColName="dateTime")

# adding day / time progress to each varialbe
RawLoc1$dayprogress <- DayNight_Progress(RawLoc1$dateTime,RawLoc1$LAT,RawLoc1$LON)

# Specify tag by 2 or 3 meaningful digits
# RawLoc1$TAG<-gsub("9720060000", '', RawLoc1$TAG) #9720010000 for Hula system
RawLoc1$TAG<-gsub("972006000", '', RawLoc1$TAG)
# RawLoc1$TAG<-gsub("97200600", '', RawLoc1$TAG)

RawDet1$BS<-gsub("9720060", '', RawDet1$BS)
RawDet1$TAG<-gsub("972006000", '', RawDet1$TAG)

# deleting redundant columns
redundant_columns<-c("traceNorm","angl","NBS","Z") # columns we won't use in this example.
RawLoc1<-RawLoc1[,-which(colnames(RawLoc1) %in% redundant_columns)]
# or
RawLoc1 <- RawLoc1[,c("X","Y","TIME","TAG","NBS","PENALTY","allBS","dateTime","dT","spd","stdVarXY")]
# deletind redundant variables
rm(AllData,Moon_pos,RawDet0,RawLoc0,Sun_pos)
rm(Start_Time_Str,End_Time_Str,redundant_columns,times,FullTag) # remove objects we no longer neeed
table(RawLoc1$DAY,RawLoc1$TAG)

# --------- Basic filtering -----------------------------------------
# add frequency to Tagtime!
TAGTIMES <- Tagtime(RawLoc1)
TAGTIMES$freq <- 8

# can filter any attribute :  tag, STD,NCONSTRAINTS, time: ilters=c(" Sun_angle<5 ","between( X,2.39e5, 2.50e5)")
# here filters sun elevation, position:
# FiltLoc0 <- atl_filter_covariates(data=RawLoc1, filters=c(" Sun_angle<5 ")) (from pratik, atlastools)
FiltLoc0 <- RawLoc1[which(RawLoc1$Sun_angle>-20),]

# ----------------- Plotting histograms & summary statistics: --------

x11()                       # creating new plot window
dev.set(which = dev.prev()) # toggeling active plot windows
dev.off()                   # closing active plot window
table(FiltLoc0$DAY,FiltLoc0$TAG)
par(mfrow=c(1,2))

hist(FiltLoc0$stdVarXY[which(FiltLoc0$DAY%in% c(1,2,3) & FiltLoc0$TAG%in% c("223"))],xlim =c(0,100), breaks=10000, main= "Stdev Distribution") # Now the distribution is more informative.
hist(FiltLoc0$spd,  xlim=c(0,50), breaks=10000,main= "Speed Distribution") # Note that the speed units are (meters/seconds)
summary(FiltLoc0$stdVarXY)
summary(FiltLoc0$spd)
quantile(FiltLoc0$stdVarXY, c(0.5,0.6,0.7,0.8,0.9,0.95,0.99), na.rm=TRUE)
quantile(FiltLoc0$spd,c(0.5,0.6,0.7,0.8,0.9,0.95,0.99), na.rm=TRUE)

# -----------------  Basic plotting -------------------------------------
unique(FiltLoc0$TAG) # list uniqhe tags in order to choose a TAG.

TAG_ex<-201
A <- FiltLoc0[which(FiltLoc0$TAG==TAG_ex),]

plot(FiltLoc0$X[which(FiltLoc0$TAG==TAG_ex)],FiltLoc0$Y[which(FiltLoc0$TAG==TAG_ex)])
limits <- locator(2,type="o")  # allow graphically choosing the x,y values on a plot

xlims <- round(limits$x)
ylims <- round(limits$y)
plot(FiltLoc0$X[which(FiltLoc0$TAG==TAG_ex)],FiltLoc0$Y[which(FiltLoc0$TAG==TAG_ex)],xlim=xlims,ylim=ylims,asp=1)
plotsqure(xlims,ylims)
symbols(FiltLoc0$X[1],FiltLoc0$Y[1],circles=c(100),fg="red",add=TRUE,inches = FALSE)

plotdays(A,TAG_ex)                               # for TAG_ex plot each day separately on 
atl_mapleaf(FiltLoc0[which(FiltLoc0$DAY==1&FiltLoc0$TAG==TAG_ex),]) #leaflet view (points and lines with some data to each point)
atl_mapgg(FiltLoc0[which(FiltLoc0$TAG==TAG_ex),])   # ggmap plot

# -----------------------  FILTERING           --------------------
    AA <- as.data.frame(FiltLoc0 %>%  filter(TAG==TAG_ex))   
    # A <- as.data.frame(A %>%  filter(DAY==DAY_ex))
#tracking amount of lost data
    data_track <- as.numeric(nrow(AA))

#  coordinate box
    coords_box <- c(xlims[1], xlims[2],ylims[1], ylims[2])
    A<-AA[which((AA$X>coords_box[1])&(AA$X<coords_box[2])&(AA$Y>coords_box[3])&(AA$Y<coords_box[4])),]
    plot(AA$X[which(AA$TAG==TAG_ex)],AA$Y[which(AA$TAG==TAG_ex)])
    points(A$X[which(A$TAG==TAG_ex)],A$Y[which(A$TAG==TAG_ex)],col="red")
    data_track <- rbind(data_track,nrow(A))

# STD filtering
    stdev<-20 # stdev 
    B<-C[which(C$stdVarXY<stdev),]
    points(B$X[which(B$TAG==TAG_ex)],B$Y[which(B$TAG==TAG_ex)],col="green")
    lines (B$X[which(B$TAG==TAG_ex)],B$Y[which(B$TAG==TAG_ex)],col="green")
    data_track <- rbind(data_track,nrow(B))

#velocity filtering
    spdThreshold<-20 # speed 
    C <- B
    C <- velocity_filter (C,spdThreshold, x = "X", y = "Y", time = "TIME", steps=10)
    C <- distance_filter (C,distThreshold=500, x = "X", y = "Y", steps=5)
    C <- addLocAttribute(C, locAttributs=c("speed")) # function to add attributes for each pair of conseucutive points. 
    points(C$X[which(C$TAG==TAG_ex)],C$Y[which(C$TAG==TAG_ex)],col="pink")
    lines (C$X[which(C$TAG==TAG_ex)],C$Y[which(C$TAG==TAG_ex)],col="pink")
    data_track <- rbind(data_track,nrow(C))
    
    atl_mapleaf(C[which(C$DAY==2&C$TAG==TAG_ex),])
    
# manual filter ------------------
    rm(list=setdiff(ls(), c("A","FiltLoc0",lsf.str())))
    E <- visual_filter(A[which(A$DAY %in% c(1:30)),])    
    lE1 <- E[[1]] # E$filtered
    E2 <- E[[2]]
    str(E1)
    str(E2)
    table(E2$segment,E2$DAY)

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
    D <- addLocAttribute(D, locAttributs=c("speed")) # function to add attributes for each pair of conseucutive points. 
    plot(D$spd)
    atl_mapleaf(E1)

# timeBurst and smoothing ---------- (Michal)
    E <- timeGapBurst(D,secTol=17,minBurstFix=39,sampRate=8)
    E1 <- AvgSmooth(E,Weight = c(0.1,0.2,0.4,0.2,0.1))

# new Stop identification ------------ (Shlomo)
    ind_rts<-data.frame("smp_rte"=c(8000,4000), # sample rate
                        "obs_min"=c(4,8 ), # minimum of within range localizations
                        "p_lim"=c(5,10), #Tolerance of n localizations outside the buffer
                        "adp_rng"=rep(35,2)) # adaptive Fixed point buffer radius
    F <- stop_move_analysis(E1,stopparams=ind_rts,DayColName=c("DAY"))
    PlotSegStop(F,E1,3)
    
    
# bind different tags together: --------------------------------
  FiltLoc1 <- NULL
  FiltLoc1 <- rbind(FiltLoc1, B)
  rm(A,B,AA,C,D)
  
# -----------Track Segmentation: Separate and summarize stops vs movement-------------
  ind_rts<-data.frame("smp_rte"=8000,"obs_min"=8,"p_lim"=8,"adp_rng"=4)
  ADP <- wrap_ADP(FiltLoc1,parameters=ind_rts)
  # ADP <- wrap_ADP(FiltLoc1,freq=8)
  cADP <- mergeCloseAdp(ADP,adp_rng=20,smp_rte=8000,time_gap=5*60*1000)


# plotting stops over tacks on a map. 
  TAG_ex <- 223
  dispDAY <- c(2,3)
  atl_mapleaf_withstops(list(FiltLoc1,ADP),Tags=TAG_ex,Days=dispDAY) # leaflet track data and calculated stops (with duration data)

  hist(stops$position_qlt, breaks=20, main="Stops Position Quality Distribution")
  summary(stops$position_qlt)
  write.csv(ADP, "data/bats_stops.csv")
  
# ---------- Revisiting positions along the track from recurse --------------------------------
  # from: Revisitation analysis uncovers spatio-temporal patterns in animal movement data, Chloe Bracis 2018

  install.packages("recurse")
  library(recurse)
  A <- FiltLoc0[which((FiltLoc0$TAG==TAG_ex)&(FiltLoc0$DAY %in% c(2,3,4,5,6))&(FiltLoc0$Sun_angle< 5)),]
  data4recurse <- data.frame(x=A$X,y=A$Y,t=A$dateTime,id=A$TAG)
  revisits = getRecursions(data4recurse, radius = 200)
  plot(revisits, data4recurse, legendPos = c(2.415e5, 7.35e5))
  drawCircle(2.415e5, 7.235e5, 100)
  
# --------- ATLAS Detection Analysis ----------------
  
  source(paste0(path2func,"BS_Beacon_Detection.R"))
  
  Start_Time_Str     <- c('2021-04-17 00:00:00')
  End_Time_Str       <- c('2021-05-19 00:01:00')
  sample_timesPerDay <- 24
  sample_lengthMinute<- 1
  FullTag <- c(972006000003,972006000004,972006000006)
  RawDet1 <- Get_ATLAS_Det_DATA (Start_Time_Str,End_Time_Str,sample_timesPerDay,sample_lengthMinute,FullTag )
  plotCountDet(RawDet1,xAxisRes='1 hours')
  plotBSperformance(RawDet1,BS2plot=c("01", "13") ,xAxisRes='1 hours')
