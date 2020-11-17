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