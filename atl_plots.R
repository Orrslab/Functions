#Basic leaflet plot for data with coordinates "X" and "Y" in itm coordinates
# requires an ATLAS data.frame is "X" and "Y" in itm coordinates
# does not return any variable
# red points, pink lines, presents data when cursor is on a point: number of detection, time, spd,std,TAG
atl_mapleaf <- function(dd)
{
  if( all(c("X","Y") %in% colnames(data))) 
  {Er <- simpleError("data must contain X and Y columns")
  stop(Er)}
  if( nrow(data)==0) 
  {Er <- simpleError("you must provide at least a single data point")
  stop(Er)}
  itm<-"+init=epsg:2039 +proj=tmerc +lat_0=31.73439361111111 +lon_0=35.20451694444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-48,55,52,0,0,0,0 +units=m +no_defs"
  wgs84<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  coordinates(dd)<-~X+Y
  proj4string(dd)<-CRS(itm)
  llpd <- spTransform(dd, wgs84)
# llpd2 <- llpd
  ll<-leaflet() %>% 
    addProviderTiles('Esri.WorldImagery') %>% # Esri.WorldGrayCanvas #CartoDB.Positron # comment this line to get a grey empty background!
      addCircles(data=llpd, weight = 5, fillOpacity = 1,color = "red",
                            popup = ~htmlEscape(paste0("time=",as.character(round(llpd$dateTime)),
                                                       ",NBS=",as.character((llpd$NBS)),
                                                       ",spd=",as.character(round(llpd$spd)),
                                                       ",pen=",as.character(round(llpd$PENALTY)),
                                                       ",std=",as.character(round(llpd$stdVarXY)),
                                                       ",TAG=",llpd$TAG  
                                                       ))) %>%
      addPolylines(data=llpd@coords, weight = 1, opacity = 1,col="pink")
  ll
}
#leaflet plot for movement data with specified stops
# requires two data.frames in a single list dd=list(FiltLoc1,ADP):
        # FiltLoc1 is an ATLAS data.frame "X" and "Y" in itm coordinates
        # ADP      is a data.frame with stops positions "medX" and "medY"
        # if Tags and Days are specified, plots only specific tags and days 
# does not return any variable
# red points, pink lines, presents data when cursor is on a point: number of detection, time, spd,std,TAG
# presents data when cursor is on a movement point: time,std,TAG
# presents data when cursor is on a stop point: duration in minutes
atl_mapleaf_withstops <- function(dd,Tags=NULL,Days=NULL)
{
  if( all(c("X","Y") %in% colnames(dd))) 
  {Er <- simpleError("data must contain X and Y columns")
  stop(Er)}
  if( nrow(dd)==0) 
  {Er <- simpleError("you must provide at least a single data point")
  stop(Er)}
  Loc1 <- dd[[1]]
  if(is.null(Days))
  {Days <- unique(Loc1$DAY)}
  if(is.null(Tags))
  {Tags <- unique(Loc1$TAG)}
  itm<-"+init=epsg:2039 +proj=tmerc +lat_0=31.73439361111111 +lon_0=35.20451694444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-48,55,52,0,0,0,0 +units=m +no_defs"
  wgs84<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    Loc1 <- dd[[1]]
    Loc1 <- Loc1[which((Loc1$TAG %in% Tags)&(Loc1$DAY %in% Days)),]
    coordinates(Loc1)<-~X+Y
    proj4string(Loc1)<-CRS(itm)
    llpd1 <- spTransform(Loc1, wgs84)
    
    Loc2 <- dd[[2]]
    Loc2 <- Loc2[which((Loc2$TAG %in% Tags)&(Loc2$DAY %in% Days)),]
    coordinates(Loc2)<-~medX+medY
    proj4string(Loc2)<-CRS(itm)
    llpd2 <- spTransform(Loc2, wgs84)
    # llpd2 <- llpd
    ll<-leaflet() %>% 
      addProviderTiles('Esri.WorldImagery') %>%
      addCircles(data=llpd1, weight = 5, fillOpacity = 1,color = "blue",
                             popup = ~htmlEscape(paste0(",std=",as.character(round(llpd1$stdVarXY)),
                                                        ",time=",as.character(round(llpd1$dateTime)),
                                                        ",TAG=",as.character((llpd1$TAG))))) %>%
      
      addPolylines(data=llpd1@coords, color = "blue",weight = 1, opacity = 1)  %>%
      addCircleMarkers(data=llpd2, radius=3,color = "red",fillColor = "Red",
                                   popup = ~htmlEscape(paste0("Duration = ",as.character(llpd2$duration_minutes)," mins")))
  
  ll
}

Leaf_TrackByDays <- function(Data,Tag,Color="red",calcDAY=F) {
  itm<-"+init=epsg:2039 "
  wgs84<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  
  TagChoosen <-subset(Data, TAG == Tag)
  if (calcDAY)
  {TagChoosen <- AssignDayNumber(TagChoosen)}
  
  DAYLists <- as.factor(unique(TagChoosen$DAY))
  
  m <- leaflet() %>% addProviderTiles('Esri.WorldImagery')
  DaysList <- list()
  for (i in DAYLists) {
    DayLo <- subset(TagChoosen, DAY == i)
    coordinates(DayLo)<-~X+Y
    proj4string(DayLo)<-CRS(itm)
    llpd1 <- spTransform(DayLo, wgs84)
    m <-  addPolylines(m, data=llpd1@coords, weight = 2, opacity = 2,group = i, color = Color)
  }
  
  daylistst <- as.factor(as.data.frame(DaysList))
  
  m <- addLayersControl(m,
                        baseGroups = DAYLists,
                        options = layersControlOptions(collapsed = FALSE)
  )
  return(m)
}

# plot a function using ggmap
# requires an ATLAS data.frame with "LAT" and "LON" in wgs84 geographic coordinates
# does not return any variable
atl_mapgg <- function(dd,lon_name="LON",lat_name="LAT")
{
  colnames(dd)[which(colnames(dd)==lon_name)] <- "LON"
  colnames(dd)[which(colnames(dd)==lat_name)] <- "LAT"
  Mapbox1 <- make_bbox(lon=dd$LON,lat=dd$LAT, f=0.1) # defines the borders of the box
  #Harod <- c(left = 35.375, bottom = 32.515, right = 35.6, top = 32.6) # defines the borders of the box
  #SatImagBox1<- get_map(location=Mapbox1, maptype = "roadmap", source="osm");ggmap(SatImagBox1)
  #SatImagBox1<- get_map(location=Mapbox1, "satellite", source="osm", zoom = 10);ggmap(SatImagBox1)
  SatImagBox1<- get_map(location=Mapbox1, maptype="satellite", source="osm",zoom=12);ggmap(SatImagBox1)
  #make plot for each variable of interest - colors by tag, including points and lines
  mapTry1 <- ggmap(SatImagBox1) +  theme(plot.margin = margin(0,0,0,0, "mm")) +  labs(x="longitude", y="latitude")
  mapTry1 + geom_point(  data=dd,  alpha = 0.5, aes(x=LON, y=LAT, col=DAY)) +
    geom_path (data=dd,aes(x=LON, y=LAT, col=DAY,group=TAG)) +
    # geom_point(  data=track_f,  alpha = 0.5, aes(x=LON, y=LAT, col=DAY+1)) +
    # geom_path (data=track_f,aes(x=LON, y=LAT, col=DAY+1,group=TAG)) +
    # geom_point(  data=track_ff,  alpha = 0.5, aes(x=LON, y=LAT, col=DAY+2)) +
    # geom_path (data=track_ff,aes(x=LON, y=LAT, col=DAY+2,group=TAG)) +
    facet_grid(TAG~.)
  #
  mapTry1 <- ggmap(SatImagBox1) +  theme(plot.margin = margin(0,0,0,0, "mm")) +  labs(x="longitude", y="latitude")
  mapTry1 + geom_point(  data=dd,  alpha = 0.5, aes(x=LON, y=LAT, col=DAY)) +
    geom_path (data=dd,aes(x=LON, y=LAT, col=DAY,group=TAG)) +
    facet_grid(TAG~.)
}

# plot days in a sequence
plotdays <- function(data,TAG_ex,xlims=0,ylims=0)
{
  if (xlims[1]==0)
  {
    xlims[1]=max(data$X[which(data$TAG==TAG_ex)])
    xlims[2]=min(data$X[which(data$TAG==TAG_ex)])
  }
  if (ylims[1]==0)
  {
    ylims[1]=max(data$Y[which(data$TAG==TAG_ex)])
    ylims[2]=min(data$Y[which(data$TAG==TAG_ex)])
  }
  DAY_list <- unique(data$DAY[which(data$TAG==TAG_ex)])
  
  for (i in DAY_list)
  {
    raw_tracks<-data[which(data$TAG==TAG_ex & data$DAY==i),] # selecting DAY number 3 of randomly selected tags. Naturally you can control which tag and DAY you want to examine too.
    plot(raw_tracks$X,raw_tracks$Y,asp=1,col="black",pch = 3,main=sprintf("Tag=%3.0f, DAY = %i",TAG_ex,i),xlim=xlims,ylim=ylims)
    lines(raw_tracks$X,raw_tracks$Y,col="black")
    # points(filtered_tracks1$X,filtered_tracks1$Y,col="red",pch = 4)
    
    readline(sprintf("DAY = %i, show next?",i))
  }
}
#  given two diagonal points of a squre, plots a squre in color a_col
plotsqure <- function(x,y,a_col="red",override=FALSE)
{
  if (override)
  {
    plot(c(x[1],x[1],x[2],x[2],x[1]),
         c(y[1],y[2],y[2],y[1],y[1]),col=a_col)
    lines(c(x[1],x[1],x[2],x[2],x[1]),
          c(y[1],y[2],y[2],y[1],y[1]),col=a_col)
  }
  else
  {
    lines(c(x[1],x[1],x[2],x[2],x[1]),
          c(y[1],y[2],y[2],y[1],y[1]),col=a_col)
  }
  
}