atl_mapleaf <- function(dd)
{
  itm<-"+init=epsg:2039 +proj=tmerc +lat_0=31.73439361111111 +lon_0=35.20451694444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-48,55,52,0,0,0,0 +units=m +no_defs"
  wgs84<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  coordinates(dd)<-~X+Y
  proj4string(dd)<-CRS(itm)
  llpd <- spTransform(dd, wgs84)
# llpd2 <- llpd
  ll<-leaflet() %>% addTiles() %>%
      addProviderTiles('Esri.WorldImagery') %>%
      addCircles(data=llpd, weight = 5, fillOpacity = 1,color = "red",popup = ~htmlEscape(paste0("det=",as.character((llpd$NCONSTRAINTS)),
                                                                                                   ",std=",as.character(round(llpd$stdVarXY)),
                                                                                                   ",spd=",as.character(round(llpd$spd)),
                                                                                                   ",time=",as.character(round(llpd$dateTime))))) %>%
      addPolylines(data=llpd@coords, weight = 1, opacity = 1)
  ll
}


atl_mapleaf_withstops <- function(dd,Tags=NULL,Days=NULL)
  {
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
    ll<-leaflet() %>% addTiles() %>%
      addProviderTiles('Esri.WorldImagery') %>%
      addCircles(data=llpd1, weight = 5, fillOpacity = 1,color = "blue",popup = ~htmlEscape(paste0(",std=",as.character(round(llpd1$stdVarXY)),
                                                                                                   ",time=",as.character(round(llpd1$dateTime)),
                                                                                                   ",TAG=",as.character((llpd1$TAG))))) %>%
      
      addPolylines(data=llpd1@coords, color = "blue",weight = 1, opacity = 1)  %>%
      addCircleMarkers(data=llpd2, radius=3,color = "red",fillColor = "Red",popup = ~htmlEscape(paste0("Duration = ",as.character(llpd2$duration_minutes)," mins")))
  
  ll
  }

atl_mapgg <- function(dd)
  {
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