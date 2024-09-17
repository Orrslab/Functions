
  atl_mapleaf <- function(dd,MapProvider='Esri.WorldImagery') # 'OpenStreetMap.BZH'
{
    
    #' @description
        #' plot an interactive map of the ATLAS location points, a popup of information about each point, 
        #' and trajectories between consequent points- from a single data set, for example a single track
    #' 
    #' @param dd A data frame that contains X (longitude) and Y (latitude) columns, 
    #' representing spatial coordinates of the location in itm coordinates: Israeli Transverse Mercator (EPSG:2039)
    #' @param MapProvider A string defining the map tile provider for the background map. 
    #' The default is 'Esri.WorldImagery'. 
    #' An alternative option ('OpenStreetMap.BZH') is commented out but can be used by changing the parameter.
    #' 
    #' @return An interactive leaflet map with the plotted data points, info popups, and paths
    #' 
    #' @import leaflet
    #' @import sp
    #' @importFrom RColorBrewer brewer.pal
  
    # A list of variables to check or add to the data frame if they don't exist
    varlist =c("PENALTY","spd","angl","stdVarXY")
    
    # For each variable in 'varlist', check if it exists in the data frame 'dd'. 
    # If not, create a column with NA values.
    for (varname in varlist) {
      if (!(varname %in% names(dd)))
        dd[,varname] <- NA
    } 
    
    # Check if the required columns "X" and "Y" (coordinates) are present in 'dd'.
    # If either is missing, stop the function and return an error.
    if(! all(c("X","Y") %in% colnames(dd))) {
      Er <- simpleError("data must contain X and Y columns")
      stop(Er)
    }
    
    # If the data frame is empty (no rows), stop the function and return an error.
    if( nrow(dd)==0) {
      Er <- simpleError("you must provide at least a single data point")
      stop(Er)
    }
    
    # Define the coordinate reference system (CRS) for Israeli Transverse Mercator (EPSG:2039):
    # +proj=tmerc +lat_0=31.73439361111111 +lon_0=35.20451694444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-48,55,52,0,0,0,0 +units=m +no_defs"
    # This is used to convert coordinates into the proper format for mapping.
    itm<-"+init=epsg:2039" 
    # Define the WGS84 CRS, commonly used for global coordinates (latitude and longitude).
    wgs84<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    
    # Convert the 'dd' data frame into a spatial object by assigning the coordinates.
    coordinates(dd) <- ~X+Y
    
    # Set the projection of 'dd' to the Israeli Transverse Mercator system (EPSG:2039).
    proj4string(dd)<-CRS(itm)
    
    # Transform the coordinates from EPSG:2039 to WGS84 so they can be properly displayed on the map.
    llpd <- spTransform(dd, wgs84) # "Latitude/Longitude Projection Data
    
    # Initialize a leaflet map object
    ll<-leaflet() %>% 
      # Add a tile layer from the specified map provider (default is 'Esri.WorldImagery')
      addProviderTiles(MapProvider) %>% 
      # uncomment this following line to get a grey empty background:
      # Esri.WorldGrayCanvas #CartoDB.Positron
      
      # Add red circles to the map at the transformed coordinates.
      # The popup shows various information about each point, including time, speed, angle, etc.
      addCircles(data=llpd, weight = 5, fillOpacity = 1,color = "red",
                 popup = ~htmlEscape(paste0("time=",as.character((llpd$dateTime)),
                                            ",NBS=",as.character((llpd$NBS)),
                                            ", spd=",as.character(round(llpd$spd)),
                                            ", angl=",as.character(round(llpd$angl)),
                                            ", pen=",as.character(round(llpd$PENALTY)),
                                            ", std=",as.character(round(llpd$stdVarXY)),
                                            ", TIME=",as.character((llpd$TIME)),
                                            ", ant=",llpd$allBS,
                                            ",TAG=",llpd$TAG))) %>%
      # Add pink polylines (lines connecting the points) to represent movement paths or routes.
      addPolylines(data=llpd@coords, weight = 1, opacity = 1,col="pink")
    
    # Display the map
    ll
}

  
atl_mapleaf2 <- function(dd1,dd2,MapProvider='Esri.WorldImagery',legendLables=c("1","2")) 
{
  
  #' @description
  #' plot an interactive map of the ATLAS location points, a popup of information about each point, 
  #' and trajectories between consequent points- from a single data set, for example a single track
  #' 
  #' @param dd1,dd2 The data sets that should be plotted.
  #' @param MapProvider The map tiles provider (default is 'Esri.WorldImagery').
  #' @param legendLables Labels of 'dd1' and 'dd2' for the map's legend.
  #' 
  #' @return An interactive map that displays two different tracks (datasets) with distinct colors and popups.
  #' 
  #' @import leaflet
  #' @import sp
  #' @importFrom RColorBrewer brewer.pal
  
  # Check if the required columns exist in both 'dd1' and 'dd2'. 
  # If a column is missing, add it with NA values.
  varlist =c("PENALTY","spd","distance","moveAngle","stdVarXY","val1","val2","ellipsDir","DistMed5","Z")
  for (varname in varlist){
    if (!(varname %in% names(dd1)))
        dd1[,varname] <- NA
    if (!(varname %in% names(dd2)))
      dd2[,varname] <- NA
  } 
  
  # Define the coordinate reference systems (CRS) for the data
  itm<-"+init=epsg:2039 "
  
  # Define the coordinate reference systems (CRS) for the map
  wgs84<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  # Convert both 'dd1' and 'dd2' from the local CRS (epsg:2039) to WGS84 (latitude/longitude).
  coordinates(dd1)<-~X+Y
  proj4string(dd1)<-CRS(itm)
  llpd1 <- spTransform(dd1, wgs84)
  
  coordinates(dd2)<-~X+Y
  proj4string(dd2)<-CRS(itm)
  llpd2 <- spTransform(dd2, wgs84)
  
  # Define a color palette for the map
  require("RColorBrewer")
  # display.brewer.all()
  # display.brewer.pal(n = 4, name = 'RdYlBu')
  # col=brewer.pal(n = 4, name = 'RdYlBu')
  col=brewer.pal(n = 6, name = 'Dark2')
  
  # Create the interactive map
  ll<-leaflet() %>%
    # Add the base map tiles from the specified provider
    addProviderTiles(MapProvider ) %>% # 'CartoDB.Positron' 'OpenStreetMap.Mapnik' 'Stadia.AlidadeSmooth','CartoDB.Positron'
    
    # Adds circles for the locations from data set 'dd1', with a popup showing detailed information
    addCircles(data=llpd1, weight = 5, fillOpacity = 1,color = col[4],group=legendLables[1],
               popup = ~htmlEscape(paste0("1:time=",as.character((llpd1$dateTime)),
                                          ", TIME=",as.character((llpd1$TIME)),
                                          ", Z=",as.character((llpd1$Z)),
                                          ", NBS=",as.character((llpd1$NBS)),
                                          ", NCON=",as.character((llpd1$NCONSTRAINTS)),
                                          ", allBS=",llpd1$allBS,
                                          ", pen=",as.character(round(llpd1$PENALTY)),
                                          ", spd=",as.character(round(llpd1$spd)),
                                          ", dist=",as.character(round(llpd1$distance)),
                                          ", moveAngle=",as.character(round(llpd1$moveAngle)),
                                          ", std=",as.character(round(llpd1$stdVarXY)),
                                          ", val1=",as.character(round(llpd1$val1)),
                                          ",val2=",as.character(round(llpd1$val2)),
                                          ",ellipsDir=",as.character(round(llpd1$ellipsDir)),
                                          ",DistMed5=",as.character(round(llpd1$DistMed5)),
                                          ", TAG=",llpd1$TAG))) %>%
    
    # Add the polylines for the 'dd1' dataset
    addPolylines(data=llpd1@coords, weight = 1, opacity = 1,col=col[4],group=legendLables[1]) %>% 
    
    # Adds circles for the locations from data set 'dd2', with a popup showing detailed information
    addCircles(data=llpd2, weight = 5, fillOpacity = 1,color = col[3],group=legendLables[2],
               popup = ~htmlEscape(paste0("2:time=",as.character((llpd2$dateTime)),
                                          ", TIME=",as.character((llpd2$TIME)),
                                          ", Z=",as.character((llpd2$Z)),
                                          ", NBS=",as.character((llpd2$NBS)),
                                          ", NCON=",as.character((llpd2$NCONSTRAINTS)),
                                          ", allBS=",llpd2$allBS,
                                          ", pen=",as.character(round(llpd2$PENALTY)),
                                          ", spd=",as.character(round(llpd2$spd)),
                                          ",dist=",as.character(round(llpd2$distance)),
                                          ",moveAngle=",as.character(round(llpd2$moveAngle)),
                                          ", std=",as.character(round(llpd2$stdVarXY)),
                                          ", val1=",as.character(round(llpd2$val1)),
                                          ",val2=",as.character(round(llpd2$val2)),
                                          ",ellipsDir=",as.character(round(llpd2$ellipsDir)),
                                          ", DistMed5=",as.character(round(llpd2$DistMed5)),
                                          ", TAG=",llpd2$TAG))) %>%
    
    # Add the polylines for the 'dd2' dataset
    addPolylines(data=llpd2@coords, weight = 1, opacity = 1,col=col[3],group=legendLables[2]) %>% 
    
    # Add a scale bar to the map
    addScaleBar(position = c("bottomleft"), 
                options = scaleBarOptions(imperial=FALSE,maxWidth=200)) %>% 
    
    # Adds a layers' control to switch between different datasets
    addLayersControl(
      overlayGroups = legendLables,
      options = layersControlOptions(collapsed = FALSE, autoZIndex=TRUE)) 
  
  # Display the map
  ll
}


atl_mapleaf_withstops <- function(dd,Tags=NULL,Days=NULL)
{
  
  #' @title Plot Movement Data with Stops on a Leaflet Map
  #'
  #' @param dd A list containing two data frames:
  #'   \describe{
  #'     \item{FiltLoc1}{A data frame with ATLAS movement data, containing "X" and "Y" coordinates in the 'itm' coordinate system.}
  #'     \item{ADP}{A data frame with stop positions, containing "medX" and "medY" coordinates for stop locations.}
  #'   }
  #' @param Tags Optional. A vector of tags (IDs) to filter and plot specific individuals or objects. If NULL, all tags will be plotted.
  #' @param Days Optional. A vector of days to filter the data. If NULL, all days will be plotted.
  #' 
  #' @return A leaflet map displaying:
  #'   \itemize{
  #'     \item Movement data in blue (lines and points).
  #'     \item Stops in red (circle markers).
  #'   }
  #'   Information shown on hover (popup) includes:
  #'   \itemize{
  #'     \item For movement points: time, standard deviation, and tag ID.
  #'     \item For stop points: stop duration in minutes.
  #'   }
  #' 
  #' @details This function generates a leaflet map to visualize movement data from the ATLAS system, combined with stop locations. 
  #'          The stops are displayed as red points, while the movement data is shown with blue lines and points. 
  #'          Users can filter by specific tags and days, with interactive pop-ups showing detailed information such as detection counts, speeds, and stop durations.
  #'
  #' @import leaflet
  #' @import sp
  
  # Check data validity
  if( all(c("X","Y") %in% colnames(dd))) 
  {Er <- simpleError("data must contain X and Y columns")
  stop(Er)}
  if( nrow(dd)==0) 
  {Er <- simpleError("you must provide at least a single data point")
  stop(Er)}
  
  # Filter the data by Days and Tags
  Loc1 <- dd[[1]]
  if(is.null(Days))
  {Days <- unique(Loc1$DAY)}
  if(is.null(Tags))
  {Tags <- unique(Loc1$TAG)}
  
  # Transform coordinates of both datasets (movement: llpd1 and stops: llpd2)
  itm<-"+init=epsg:2039"
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
  
  # Create the interactive map
  ll<-leaflet() %>% 
    
    # Add the base map
    addProviderTiles('Esri.WorldImagery') %>%
    
    # Add blue circles for the Movement data
    addCircles(data=llpd1, weight = 5, fillOpacity = 1,color = "blue",
               popup = ~htmlEscape(paste0(",std=",as.character(round(llpd1$stdVarXY)),
                                          ",time=",as.character((llpd1$dateTime)),
                                          ",TAG=",as.character((llpd1$TAG))))) %>%
    
    # Add blue polylines connecting the coordinates of the Movement data
    addPolylines(data=llpd1@coords, color = "blue",weight = 1, opacity = 1)  %>%
    
    # Add red circle markers for the Stops data, with popups showing the duration of each stop in minutes.
    addCircleMarkers(data=llpd2, radius=3,color = "red",fillColor = "Red",
                     popup = ~htmlEscape(paste0("Duration = ",as.character(llpd2$duration_minutes)," mins")))
  
  # Display the map
  ll
}

Leaf_TrackByDays <- function(Data,Tag,Color="red",calcDAY=F) {
  
  #' @title Visualize Movement Tracks by Day on a Leaflet Map
  #'
  #' @param Data A data frame containing movement data with at least the following columns:
  #'   \describe{
  #'     \item{TAG}{Identifier for the tracked object or individual.}
  #'     \item{DAY}{Date or day of the observation.}
  #'     \item{X}{X-coordinate of the movement.}
  #'     \item{Y}{Y-coordinate of the movement.}
  #'   }
  #' @param Tag A specific tag (ID) to filter and plot the movement tracks.
  #' @param Color Optional. Color to use for the lines representing the tracks. Defaults to "red".
  #' @param calcDAY Optional. Logical indicating whether to calculate day numbers. Defaults to FALSE.
  #'
  #' @return A Leaflet map object with movement tracks for each day. Tracks are colored according to the specified `Color`.
  #' 
  #' @details This function plots movement data for a specified tag over different days. 
  #'          Each day's track is displayed as a polyline on the map.
  #'          The map includes a layer control to toggle visibility of tracks by day.
  #'          
  #' @import leaflet
  #' @import sp
  
  # Set Coordinate Reference Systems (CRS) of the data and map
  itm<-"+init=epsg:2039 "
  wgs84<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  
  # Extract the data of the desired 'Tag'
  TagChoosen <-subset(Data, TAG == Tag)
  
  # An optional Day Number calculation
  if (calcDAY)
  {TagChoosen <- AssignDayNumber(TagChoosen)} # TODO source the function 'AssignDayNumber'
  
  # Store the unique days found in 'TagChoosen'
  DAYLists <- as.factor(unique(TagChoosen$DAY))
  
  # Create a Leaflet map object with the base map tiles
  m <- leaflet() %>% 
    addProviderTiles('Esri.WorldImagery')
  
  # Add the track of each day to the map
  DaysList <- list()
  for (i in DAYLists) {
    # Get the data of day 'i'
    DayLo <- subset(TagChoosen, DAY == i)
    # Transform the coordinates of the data to the mapping coordinates
    coordinates(DayLo)<-~X+Y
    proj4string(DayLo)<-CRS(itm)
    llpd1 <- spTransform(DayLo, wgs84)
    
    # Add the data of day 'i' to the map object 'm'
    m <-  addPolylines(m, data=llpd1@coords, weight = 2, opacity = 2,group = i, color = Color)
  }
  
  # Group 'DaysList' by days- might be a redundant line becaused it is not used elsewhere in this function.
  daylistst <- as.factor(as.data.frame(DaysList))
  
  # Add a layer control to the map to allow users to toggle the visibility of tracks for each day.
  m <- addLayersControl(m,
                        baseGroups = DAYLists,
                        options = layersControlOptions(collapsed = FALSE)
  )
  
  # Return the map object
  return(m)
}


atl_mapleafGPS1 <- function(gpsTrack,dd1,MapProvider='Esri.WorldImagery')
{
  
  #' @description 
      #' This function creates an interactive map using the leaflet package. It plots two datasets:
      #' - `dd1`: The ATLAS-derived data (requires columns "X" and "Y" in EPSG:2039 coordinates).
      #' - `gpsTrack`: The GPS data (requires columns "X" and "Y" in EPSG:2039 coordinates).
      #' The map displays circles and lines for the ATLAS data and circles for the GPS data.
  #'
  #' @param gpsTrack A data frame containing GPS data with columns "X" and "Y" in EPSG:2039 coordinates.
  #' @param dd1 A data frame containing original data with columns "X" and "Y" in EPSG:2039 coordinates.
  #' @param MapProvider A character string specifying the map provider. Defaults to 'Esri.WorldImagery'.
  #'
  #' @return A leaflet map object.
  #'
  #' @details
  #' - The function uses the EPSG:2039 projection for the input data and transforms it to WGS84 coordinates for plotting.
  #' - The map includes:
  #'   - Circles and lines for the original data (in the color specified by the `Color` parameter).
  #'   - Circles for the GPS data (in red color).
  #' - Popups display detailed information when hovering over the data points.
  #' - Includes a scale bar and layer control to toggle between the original and GPS data layers.
  #'
  #' @import leaflet
  #' @import sp
  #' @importFrom RColorBrewer brewer.pal
  
  # Check data validity
  varlist =c("PENALTY","stdVarXY")
  for (varname in varlist)
  {
    if (!(varname %in% names(dd1)))
      dd1[,varname] <- NA
  } 
  
  # Set up the coordinates reference systems for the ATLAS and GPS data and convert them from 'epsg:2039' to 'wgs84'
  itm<-"+init=epsg:2039 "
  wgs84<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  coordinates(dd1)<-~X+Y
  proj4string(dd1)<-CRS(itm)
  llpd1 <- spTransform(dd1, wgs84)
  
  coordinates(gpsTrack)<-~X+Y
  proj4string(gpsTrack)<-CRS(itm)
  llpd2 <- spTransform(gpsTrack, wgs84)
  
  # Set up the color palette for the map
  require("RColorBrewer")
  # display.brewer.all()
  # display.brewer.pal(n = 4, name = 'RdYlBu')
  # col=brewer.pal(n = 4, name = 'RdYlBu')
  col=brewer.pal(n = 6, name = 'Dark2')
  
  # Create a Leaflet map object
  ll<-leaflet() %>% 
    
    # Add the base map
    addProviderTiles(MapProvider) %>% # 'CartoDB.Positron' 'OpenStreetMap.Mapnik' 'Stadia.AlidadeSmooth','CartoDB.Positron'
    
    # Add circles for the ATLAS data, with popups showing detailed information
    addCircles(data=llpd1, weight = 5, fillOpacity = 1,color = col[4],group="original",
               popup = ~htmlEscape(paste0("1:time=",as.character((llpd1$dateTime)),
                                          ", TIME=",as.character((llpd1$TIME)),
                                          ", NBS=",as.character((llpd1$NBS)),
                                          ", NCON=",as.character((llpd1$NCONSTRAINTS)),
                                          ", pen=",as.character(round(llpd1$PENALTY)),
                                          ", std=",as.character(round(llpd1$stdVarXY)),
                                          ", Error=",llpd1$Error,
                                          ", TAG=",llpd1$TAG))) %>%
    
    # Add polylines for the ATLAS data
    addPolylines(data=llpd1@coords, weight = 1, opacity = 1,col=col[4],group="original") %>% 
    
    # Add circles for the GPS data, with popups showing TAG information
    addCircles(data=llpd2, weight = 5, fillOpacity = 1,color = col[1],group="GPS",
               popup = ~htmlEscape(paste0(", TAG=",llpd1$TAG))) %>%
    
    # Add a scale bar to the bottom left of the map
    addScaleBar(position = c("bottomleft"), 
                options = scaleBarOptions(imperial=FALSE,maxWidth=200)) %>%
    
    # Add a control to switch between the "ATLAS" and "GPS" layers.
    addLayersControl(
      overlayGroups = c("original","GPS"),
      options = layersControlOptions(collapsed = FALSE, autoZIndex=TRUE)) 
  
  # Display the map
  ll
}


atl_mapleaf4 <- function(dd1,dd2,dd3,dd4,MapProvider='Esri.WorldImagery',legendLables=c("1","2","3","4"))
{
  
  #' @title Visualize and compare movement data from four ATLAS datasets on an interactive map using leaflet.
  #'
  #' @param dd1 A data frame containing movement data with "X" and "Y" coordinates in ITM (EPSG:2039) projection.
  #' @param dd2 A data frame containing movement data with "X" and "Y" coordinates in ITM (EPSG:2039) projection.
  #' @param dd3 A data frame containing movement data with "X" and "Y" coordinates in ITM (EPSG:2039) projection.
  #' @param dd4 A data frame containing movement data with "X" and "Y" coordinates in ITM (EPSG:2039) projection.
  #' @param MapProvider A string specifying the base map provider. Default is 'Esri.WorldImagery'.
  #' @param legendLables A character vector of labels for the different datasets (dd1, dd2, dd3, dd4).
  #'
  #' @return A leaflet map object with overlaid circles and polylines representing the movement data.
  #' 
  #' @import leaflet
  #' @import sp
  #' @import RColorBrewer
  
  # Check the data validity
  varlist =c("PENALTY","stdVarXY")
  for (varname in varlist)
  {
    if (!(varname %in% names(dd1)))
      dd1[,varname] <- NA
    if (!(varname %in% names(dd2)))
      dd2[,varname] <- NA
    if (!(varname %in% names(dd3)))
      dd3[,varname] <- NA
    if (!(varname %in% names(dd4)))
      dd4[,varname] <- NA
  } 
  
  # Set up the coordinates reference systems for the each dataset and convert them from 'epsg:2039' to 'wgs84'
  
  itm<-"+init=epsg:2039 "
  wgs84<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  coordinates(dd1)<-~X+Y
  proj4string(dd1)<-CRS(itm)
  llpd1 <- spTransform(dd1, wgs84)
  
  coordinates(dd2)<-~X+Y
  proj4string(dd2)<-CRS(itm)
  llpd2 <- spTransform(dd2, wgs84)
  
  coordinates(dd3)<-~X+Y
  proj4string(dd3)<-CRS(itm)
  llpd3 <- spTransform(dd3, wgs84)
  
  coordinates(dd4)<-~X+Y
  proj4string(dd4)<-CRS(itm)
  llpd4 <- spTransform(dd4, wgs84)
  
  # Set up the color palette for the map
  require("RColorBrewer")
  # display.brewer.all()
  # display.brewer.pal(n = 4, name = 'RdYlBu')
  # col=brewer.pal(n = 4, name = 'RdYlBu')
  col=brewer.pal(n = 6, name = 'Dark2')
  
  # Create the leaflet map
  ll<-leaflet() %>% 
    
    # Add the base map
    addProviderTiles(MapProvider) %>% # 'Esri.WorldImagery' 'OpenStreetMap.Mapnik' 'Stadia.AlidadeSmooth','CartoDB.Positron'
    
    # Add circles for the 'dd1' data, with popups showing detailed information
    addCircles(data=llpd1, weight = 5, fillOpacity = 1,color = col[4],group=legendLables[1],
               popup = ~htmlEscape(paste0("1:time=",as.character((llpd1$dateTime)),
                                          ", TIME=",as.character((llpd1$TIME)),
                                          ", NBS=",as.character((llpd1$NBS)),
                                          ", NCON=",as.character((llpd1$NCONSTRAINTS)),
                                          ", pen=",as.character(round(llpd1$PENALTY)),
                                          ", std=",as.character(round(llpd1$stdVarXY)),
                                          ", allBS=",llpd1$allBS,
                                          ", TAG=",llpd1$TAG))) %>%
    
    # Add polylines for the 'dd1' dataset
    addPolylines(data=llpd1@coords, weight = 1, opacity = 1,col=col[4],group=legendLables[1]) %>% 
    
    # Add circles for the 'dd2' data, with popups showing detailed information
    addCircles(data=llpd2, weight = 5, fillOpacity = 1,color = col[3],group=legendLables[2],
               popup = ~htmlEscape(paste0("2:time=",as.character((llpd2$dateTime)),
                                          ", TIME=",as.character((llpd2$TIME)),
                                          ", NBS=",as.character((llpd2$NBS)),
                                          ", NCON=",as.character((llpd2$NCONSTRAINTS)),
                                          ", pen=",as.character(round(llpd2$PENALTY)),
                                          ", std=",as.character(round(llpd2$stdVarXY)),
                                          ", allBS=",llpd2$allBS,
                                          ", TAG=",llpd2$TAG))) %>%
    
    # Add polylines for the 'dd2' dataset
    addPolylines(data=llpd2@coords, weight = 1, opacity = 1,col=col[3],group=legendLables[2]) %>% 
    
    # Add circles for the 'dd3' data, with popups showing detailed information
    addCircles(data=llpd3, weight = 5, fillOpacity = 1,color = col[1],group=legendLables[3],
               popup = ~htmlEscape(paste0("3:time=",as.character((llpd3$dateTime)),
                                          ", TIME=",as.character((llpd3$TIME)),
                                          ", NBS=",as.character((llpd3$NBS)),
                                          ", NCON=",as.character((llpd3$NCONSTRAINTS)),
                                          ", pen=",as.character(round(llpd3$PENALTY)),
                                          ", std=",as.character(round(llpd3$stdVarXY)),
                                          ", allBS=",llpd3$allBS,
                                          ", TAG=",llpd3$TAG))) %>%
    
    # Add polylines for the 'dd3' dataset
    addPolylines(data=llpd3@coords, weight = 1, opacity = 1,col=col[1],group=legendLables[3]) %>% 
    
    # Add circles for the 'dd4' data, with popups showing detailed information
    addCircles(data=llpd4, weight = 5, fillOpacity = 1,color = col[6],group=legendLables[4],
               popup = ~htmlEscape(paste0("3:time=",as.character((llpd4$dateTime)),
                                          ", TIME=",as.character((llpd4$TIME)),
                                          ", NBS=",as.character((llpd4$NBS)),
                                          ", NCON=",as.character((llpd4$NCONSTRAINTS)),
                                          ", pen=",as.character(round(llpd4$PENALTY)),
                                          ", std=",as.character(round(llpd4$stdVarXY)),
                                          ", allBS=",llpd4$allBS,
                                          ", TAG=",llpd4$TAG))) %>%
    
    # Add polylines for the 'dd4' dataset
    addPolylines(data=llpd4@coords, weight = 1, opacity = 1,col=col[6],group=legendLables[4]) %>% 
    
    # Add a scale bar to the bottom left of the map
    addScaleBar(position = c("bottomleft"), 
                options = scaleBarOptions(imperial=FALSE,maxWidth=200)) %>% 
    
    # Add a control to switch between the "ATLAS" and "GPS" layers.
    addLayersControl(
      overlayGroups = legendLables,
      options = layersControlOptions(collapsed = FALSE, autoZIndex=TRUE)) 
  
  # Display the map
  ll
}


atl_mapleaf5GPS <- function(gpsTrack,dd1,dd2,dd3,dd4,MapProvider='Esri.WorldImagery',legendLables=c("GPS","1","2","3","4"))
{
  
  #' @title Plot GPS Track and Additional four ALTAS datasets on an Interactive Leaflet Map
  #'
  #' @description
    #' This function plots a GPS track along with four additional datasets on an interactive Leaflet map. 
    #' Each dataset is projected from the Israeli Transverse Mercator (EPSG:2039) projection to WGS84, 
    #' and circles and polylines are drawn on the map. The function allows for custom map providers 
    #' and color schemes and displays relevant information in popups.
    #' 
  #'
  #' @param gpsTrack A SpatialPointsDataFrame containing the GPS track data with columns `X`, `Y` for coordinates.
  #' @param dd1 A SpatialPointsDataFrame for the first ATLAS dataset. Must contain `X`, `Y` coordinates.
  #' @param dd2 A SpatialPointsDataFrame for the second ATLAS dataset. Must contain `X`, `Y` coordinates.
  #' @param dd3 A SpatialPointsDataFrame for the third ATLAS dataset. Must contain `X`, `Y` coordinates.
  #' @param dd4 A SpatialPointsDataFrame for the fourth ATLAS dataset. Must contain `X`, `Y` coordinates.
  #' @param MapProvider A character string representing the map tile provider for the background. 
  #'   Default is `'Esri.WorldImagery'`. Other options include `'OpenStreetMap.Mapnik'`, `'Stadia.AlidadeSmooth'`, etc.
  #' @param legendLables A character vector of labels for the map layers, used in the legend. 
  #'   The default is `c("GPS", "1", "2", "3", "4")`.
  #'
  #' @return An interactive Leaflet map with five layers: the GPS track and the four ATLAS datasets (`dd1`, `dd2`, `dd3`, `dd4`), 
  #'   including circles and polylines, with popups displaying metadata for each point.
  #'
  #' @import leaflet sp RColorBrewer
  #' @importFrom htmltools htmlEscape
  #' @importFrom sp spTransform proj4string CRS coordinates
  
  # Handle missing variables
  varlist =c("PENALTY","stdVarXY")
  for (varname in varlist)
  {
    if (!(varname %in% names(dd1)))
      dd1[,varname] <- NA
    if (!(varname %in% names(dd2)))
      dd2[,varname] <- NA
    if (!(varname %in% names(dd2)))
      dd3[,varname] <- NA
    if (!(varname %in% names(dd2)))
      dd4[,varname] <- NA
  } 
  
  # Coordinate transformation
  itm<-"+init=epsg:2039 "
  wgs84<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  coordinates(gpsTrack)<-~X+Y
  proj4string(gpsTrack)<-CRS(itm)
  llpdgps <- spTransform(gpsTrack, wgs84)
  
  coordinates(dd1)<-~X+Y
  proj4string(dd1)<-CRS(itm)
  llpd1 <- spTransform(dd1, wgs84)
  
  coordinates(dd2)<-~X+Y
  proj4string(dd2)<-CRS(itm)
  llpd2 <- spTransform(dd2, wgs84)
  
  coordinates(dd3)<-~X+Y
  proj4string(dd3)<-CRS(itm)
  llpd3 <- spTransform(dd3, wgs84)
  
  coordinates(dd4)<-~X+Y
  proj4string(dd4)<-CRS(itm)
  llpd4 <- spTransform(dd4, wgs84)
  
  # Set up a color palette for the map
  require("RColorBrewer")
  # display.brewer.all()
  # display.brewer.pal(n = 4, name = 'RdYlBu')
  # col=brewer.pal(n = 4, name = 'RdYlBu')
  col=brewer.pal(n = 6, name = 'Dark2')
  
  # Create a leaflet map object
  ll<-leaflet() %>% 
    
    # Add the base map
    addProviderTiles(MapProvider) %>% # 'Esri.WorldImagery' 'OpenStreetMap.Mapnik' 'Stadia.AlidadeSmooth','CartoDB.Positron'
    
    # Add the GPS track: circles, popups with metadata, and polylines
    addCircles(data=llpdgps, weight = 1, fillOpacity = 1,color = "black",group=legendLables[1],
               popup = ~htmlEscape(paste0("1:time=",as.character((llpdgps$dateTime))))) %>%
    addPolylines(data=llpdgps@coords, weight = 1, opacity = 1,col="black",group=legendLables[1]) %>% 
    
    # Add the first ATLAS dataset ('dd1'): circles, popups with metadata, and polylines
    addCircles(data=llpd1, weight = 5, fillOpacity = 1,color = col[4],group=legendLables[2],
               popup = ~htmlEscape(paste0("1:time=",as.character((llpd1$dateTime)),
                                          ", TIME=",as.character((llpd1$TIME)),
                                          ", NBS=",as.character((llpd1$NBS)),
                                          ", NCON=",as.character((llpd1$NCONSTRAINTS)),
                                          ", pen=",as.character(round(llpd1$PENALTY)),
                                          ", std=",as.character(round(llpd1$stdVarXY)),
                                          ", allBS=",llpd1$allBS,
                                          ", Error=",llpd1$Error,
                                          ", TAG=",llpd1$TAG))) %>%
    
    addPolylines(data=llpd1@coords, weight = 1, opacity = 1,col=col[4],group=legendLables[2]) %>% 
    
    # Add the second ATLAS dataset ('dd2'): circles, popups with metadata, and polylines
    addCircles(data=llpd2, weight = 5, fillOpacity = 1,color = col[3],group=legendLables[3],
               popup = ~htmlEscape(paste0("2:time=",as.character((llpd2$dateTime)),
                                          ", TIME=",as.character((llpd2$TIME)),
                                          ", NBS=",as.character((llpd2$NBS)),
                                          ", NCON=",as.character((llpd2$NCONSTRAINTS)),
                                          ", pen=",as.character(round(llpd2$PENALTY)),
                                          ", std=",as.character(round(llpd2$stdVarXY)),
                                          ", allBS=",llpd2$allBS,
                                          ", Error=",llpd2$Error,
                                          ", TAG=",llpd2$TAG))) %>%
    
    addPolylines(data=llpd2@coords, weight = 1, opacity = 1,col=col[3],group=legendLables[3]) %>% 
    
    # Add the third ATLAS dataset ('dd3'): circles, popups with metadata, and polylines
    addCircles(data=llpd3, weight = 5, fillOpacity = 1,color = col[1],group=legendLables[4],
               popup = ~htmlEscape(paste0("3:time=",as.character((llpd3$dateTime)),
                                          ", TIME=",as.character((llpd3$TIME)),
                                          ", NBS=",as.character((llpd3$NBS)),
                                          ", NCON=",as.character((llpd3$NCONSTRAINTS)),
                                          ", pen=",as.character(round(llpd3$PENALTY)),
                                          ", std=",as.character(round(llpd3$stdVarXY)),
                                          ", allBS=",llpd3$allBS,
                                          ", Error=",llpd3$Error,
                                          ", TAG=",llpd3$TAG))) %>%
    
    addPolylines(data=llpd3@coords, weight = 1, opacity = 1,col=col[1],group=legendLables[4]) %>% 
    
    # Add the fourth ATLAS dataset ('dd4'): circles, popups with metadata, and polylines
    addCircles(data=llpd4, weight = 1, fillOpacity = 1,color = col[6],group=legendLables[5],
               popup = ~htmlEscape(paste0("3:time=",as.character((llpd4$dateTime)),
                                          ", TIME=",as.character((llpd4$TIME)),
                                          ", NBS=",as.character((llpd4$NBS)),
                                          ", NCON=",as.character((llpd4$NCONSTRAINTS)),
                                          ", pen=",as.character(round(llpd4$PENALTY)),
                                          ", std=",as.character(round(llpd4$stdVarXY)),
                                          ", allBS=",llpd4$allBS,
                                          ", Error=",llpd4$Error,
                                          ", TAG=",llpd4$TAG))) %>%
    
    addPolylines(data=llpd4@coords, weight = 1, opacity = 1,col=col[6],group=legendLables[5]) %>% 
    
    # Add a scale bar to the bottom left of the map
    addScaleBar(position = c("bottomleft"), 
                options = scaleBarOptions(imperial=FALSE,maxWidth=200)) %>% 
    
    # Add a layer control so users can toggle the visibility of different groups (GPS, 1, 2, 3, 4).
    addLayersControl(
      overlayGroups = legendLables,
      options = layersControlOptions(collapsed = FALSE, autoZIndex=TRUE)) 
  
  # Display the map
  ll
}


atl_mapgg <- function(dd,lon_name="LON",lat_name="LAT")
{
  
  #' @title Plot GPS Data on a Satellite Map with ggmap
  #'
  #' This function plots GPS data on a satellite map using ggmap. The map is centered based on the 
  #' bounding box of the dataset, and the GPS points and paths are plotted with coloring by day and grouping by tag. 
  #' The function uses OSM (OpenStreetMap) as the source for map tiles and allows for customization 
  #' of longitude and latitude column names.
  #'
  #' @param dd A data frame containing the GPS data. It should have columns for longitude, latitude, day, and tag.
  #' @param lon_name A character string representing the name of the longitude column in the dataset. Default is `"LON"`.
  #' @param lat_name A character string representing the name of the latitude column in the dataset. Default is `"LAT"`.
  #'
  #' @return A ggmap object with GPS points and paths plotted on a satellite map, faceted by tag.
  #'
  #' @import ggmap ggplot2
  #' @importFrom ggmap get_map make_bbox ggmap
  
  # Rename the columns to standard names "LON" and "LAT"
  colnames(dd)[which(colnames(dd)==lon_name)] <- "LON"
  colnames(dd)[which(colnames(dd)==lat_name)] <- "LAT"
  
  # Define the borders of the bounding box for the map
  Mapbox1 <- make_bbox(lon=dd$LON,lat=dd$LAT, f=0.1)
  #Harod <- c(left = 35.375, bottom = 32.515, right = 35.6, top = 32.6) # defines the borders of the box
  
  # Get satellite map for the bounding box from OpenStreetMap
  #SatImagBox1<- get_map(location=Mapbox1, maptype = "roadmap", source="osm");ggmap(SatImagBox1)
  #SatImagBox1<- get_map(location=Mapbox1, "satellite", source="osm", zoom = 10);ggmap(SatImagBox1)
  SatImagBox1<- get_map(location=Mapbox1, maptype="satellite", source="osm",zoom=12);ggmap(SatImagBox1)
  
  # Create a map for each variable of interest - colors by tag, including points and lines:
  
  # Initialize the ggmap object with the satellite map background
  mapTry1 <- ggmap(SatImagBox1) +  
    theme(plot.margin = margin(0,0,0,0, "mm")) +  
    labs(x="longitude", y="latitude")
  
  # Plot points and paths for each tag, colored by day
  mapTry1 <- ggmap(SatImagBox1) +  
    theme(plot.margin = margin(0,0,0,0, "mm")) +  
    labs(x="longitude", y="latitude")
  
  mapTry1 <- mapTry1 + 
    geom_point(  data=dd,  alpha = 0.5, aes(x=LON, y=LAT, col=DAY)) +
    geom_path (data=dd,aes(x=LON, y=LAT, col=DAY,group=TAG)) +
    facet_grid(TAG~.)
  
  # Display the map
  return(mapTry1)
}


plotdays <- function(data,TAG_ex,xlims=0,ylims=0)
{
  
  #' @title Plot Sequential Days for a Given Tag
  #'
  #' This function plots the movement data (X, Y coordinates) of a specific tag across different days. 
  #' For each day, it displays the track of movements in a sequential manner and pauses after each plot 
  #' for the user to review the current day's movement before continuing to the next day.
  #'
  #' @param data A data frame containing the movement data. It should include at least the following columns: 
  #' `X` (X coordinates), `Y` (Y coordinates), `TAG` (identifier for the subject being tracked), and `DAY` (day identifier).
  #' @param TAG_ex A single value specifying the number of the tag of interest. The function will plot the data corresponding to this tag.
  #' @param xlims A vector of length 2 specifying the x-axis limits for the plots. Default is 0, which will auto-calculate the limits based on the data.
  #' @param ylims A vector of length 2 specifying the y-axis limits for the plots. Default is 0, which will auto-calculate the limits based on the data.
  #'
  #' @details
  #' The function will iterate through all the unique days (`DAY`) associated with the specified tag (`TAG_ex`). 
  #' For each day, it will plot the X and Y coordinates and connect them with lines to represent movement. 
  #' After each day's plot, the function will pause and prompt the user to continue to the next day.
  #' 
  #' If `xlims` or `ylims` are not provided, the function will automatically calculate the axis limits based on 
  #' the maximum and minimum X and Y values for the specified tag.
  #'
  #' @return None. The function is used for plotting and interaction, with no return value.
  
  # If no x-axis limits are provided, automatically calculate them based on the specified tag's data
  if (xlims[1]==0) {
    xlims[1]=max(data$X[which(data$TAG==TAG_ex)])
    xlims[2]=min(data$X[which(data$TAG==TAG_ex)])
  }
  
  # If no y-axis limits are provided, automatically calculate them based on the specified tag's data
  if (ylims[1]==0) {
    ylims[1]=max(data$Y[which(data$TAG==TAG_ex)])
    ylims[2]=min(data$Y[which(data$TAG==TAG_ex)])
  }
  
  # Get the list of unique days for the specified tag
  DAY_list <- unique(data$DAY[which(data$TAG==TAG_ex)])
  
  # Loop through each day in the list
  for (i in DAY_list)
  {
    # Subset the data for the current tag and day
    raw_tracks<-data[which(data$TAG==TAG_ex & data$DAY==i),] # selecting DAY number 3 of randomly selected tags. Naturally you can control which tag and DAY you want to examine too.
    
    # Plot the X and Y coordinates for the current day
    plot(raw_tracks$X,raw_tracks$Y,asp=1,col="black",pch = 3,
         main=sprintf("Tag=%3.0f, DAY = %i",TAG_ex,i),
         xlim=xlims,ylim=ylims)
    
    # Connect the points with lines to show the movement
    lines(raw_tracks$X,raw_tracks$Y,col="black")
    
    # points(filtered_tracks1$X,filtered_tracks1$Y,col="red",pch = 4)
    
    # Prompt the user to proceed to the next day (interactive pause)
    readline(sprintf("DAY = %i, show next?",i))
  }
}


plotsqure <- function(x,y,a_col="red",override=FALSE)
{
  
  #' @title Plot a Square Given Two Diagonal Points
  #'
  #' This function plots a square using two diagonal points. The square can either be added 
  #' to an existing plot or drawn in a new plot depending on the 'override' parameter. 
  #' You can also specify the color of the square.
  #'
  #' @param x A numeric vector of length 2 representing the x-coordinates of the diagonal points.
  #' @param y A numeric vector of length 2 representing the y-coordinates of the diagonal points.
  #' @param a_col A string specifying the color of the square. Default is "red".
  #' @param override A logical value indicating whether to override the existing plot. 
  #'        If TRUE, a new plot is created; if FALSE, the square is added to an existing plot. 
  #'        Default is FALSE.
  #'
  #' @details
  #' The function takes two diagonal points, defined by the x and y coordinates, and draws a square.
  #' If `override = TRUE`, the function will create a new plot with the square. If `override = FALSE`,
  #' it adds the square to an existing plot without creating a new one.
  #'
  #' @return No return value. The function creates or modifies a plot by drawing a square.
  
  # If 'override' is TRUE, create a new plot with the square
  if (override){
    
    plot(c(x[1],x[1],x[2],x[2],x[1]),                # X coordinates for the square
         c(y[1],y[2],y[2],y[1],y[1]),col=a_col)      # Y coordinates for the square
    
    # Draw the lines to form the square
    lines(c(x[1],x[1],x[2],x[2],x[1]),
          c(y[1],y[2],y[2],y[1],y[1]),col=a_col)
  
  # If 'override' is FALSE, only add the square to the existing plot (without creating a new plot)
  } else {
    
    
    lines(c(x[1],x[1],x[2],x[2],x[1]),   # X coordinates for the square
          c(y[1],y[2],y[2],y[1],y[1]),   # Y coordinates for the square 
          col=a_col)                     # Color of the square
  }
  
}