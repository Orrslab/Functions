library(dplyr)

source(paste0(path_to_atlas_data_analysis_repo, "time_conversions.R"))


#' Create a map of a single ATLAS dataset
#'
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
#'
atl_mapleaf <- function(dd,MapProvider='Esri.WorldImagery') # 'OpenStreetMap.BZH'
{
  
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


#' Create a map of two different ATLAS datasets
#' 
#' @description
#' plot an interactive map of the ATLAS location points, a popup of information about each point, 
#' and trajectories between consequent points- from a single data set, for example a single track
#' 
#' @param dd1,dd2 The data sets that should be plotted.
#' @param MapProvider The map tiles provider (default is 'Esri.WorldImagery').
#' @param legendLabels Labels of 'dd1' and 'dd2' for the map's legend. Example: c("1", "2")
#' 
#' @return An interactive map that displays two different tracks (datasets) with distinct colors and popups.
#' 
#' @import leaflet
#' @import sp
#' @importFrom RColorBrewer brewer.pal  
#'
atl_mapleaf2 <- function(dd1,dd2,MapProvider='Esri.WorldImagery',legendLabels=c("1", "2")) 
{
  
  # Check if the required columns exist in both 'dd1' and 'dd2'. 
  # If a column is missing, add it with NA values.
  varlist =c("PENALTY","spd","distance","moveAngle","stdVarXY","val1","val2","ellipsDir","DistMed5","Z")
  for (varname in varlist){
    if (!(varname %in% names(dd1)))
        dd1[,varname] <- NA
    if (!(varname %in% names(dd2)))
      dd2[,varname] <- NA
  }
  
  # Define CRS for the datasets
  itm <- 2039  # EPSG code for your local CRS
  wgs84 <- 4326  # EPSG code for WGS84
  
  # Convert data frames to sf objects
  dd1_sf <- st_as_sf(dd1, coords = c("X", "Y"), crs = itm)
  dd2_sf <- st_as_sf(dd2, coords = c("X", "Y"), crs = itm)
  
  # Transform to WGS84
  llpd1_sf <- st_transform(dd1_sf, crs = wgs84)
  llpd2_sf <- st_transform(dd2_sf, crs = wgs84)
  
  # Group points by TAG, and then create LINESTRING for polylines
  # Grouping by tag is done to avoid connecting the geometries of different animals
  llpd1_lines <- llpd1_sf %>%
    group_by(TAG) %>%
    summarize(do_union = FALSE) %>%  # Prevent union of geometries
    st_cast("LINESTRING")
  
  llpd2_lines <- llpd2_sf %>%
    group_by(TAG) %>%
    summarize(do_union = FALSE) %>%
    st_cast("LINESTRING")
  
  # Convert Unix timestamp to a UTC humandate in ATLAS format
  llpd1_sf$dateTimeFormatted <- unix_timestamp_to_human_date(llpd1_sf$TIME)
  llpd2_sf$dateTimeFormatted <- unix_timestamp_to_human_date(llpd2_sf$TIME)
  
  # Define color palette
  col <- brewer.pal(n = 6, name = 'Dark2')
  
  ll <- leaflet() %>%
    
    # Add the base map
    addProviderTiles(MapProvider) %>%
    
    # Add circles at the locations of the first dataset 'dd1'
    addCircles(data = llpd1_sf, weight = 1, fillOpacity = 1, color = col[4], group = legendLabels[1],
               popup = ~htmlEscape(paste0("1:time=", as.character(llpd1_sf$dateTimeFormatted),
                                          ", TIME=", as.character(llpd1_sf$TIME),
                                          ", Z=", as.character(llpd1_sf$Z),
                                          ", NBS=", as.character(llpd1_sf$NBS),
                                          ", NCON=", as.character(llpd1_sf$NCONSTRAINTS),
                                          ", allBS=", llpd1_sf$allBS,
                                          ", pen=", as.character(round(llpd1_sf$PENALTY)),
                                          ", spd=", as.character(round(llpd1_sf$spd)),
                                          ", dist=", as.character(round(llpd1_sf$distance)),
                                          ", moveAngle=", as.character(round(llpd1_sf$moveAngle)),
                                          ", std=", as.character(round(llpd1_sf$stdVarXY)),
                                          ", val1=", as.character(round(llpd1_sf$val1)),
                                          ", val2=", as.character(round(llpd1_sf$val2)),
                                          ", ellipsDir=", as.character(round(llpd1_sf$ellipsDir)),
                                          ", DistMed5=", as.character(round(llpd1_sf$DistMed5)),
                                          ", TAG=", llpd1_sf$TAG))) %>%

    # Add lines that connect the point locations included in 'dd1'
    addPolylines(data = llpd1_lines, weight = 1, opacity = 1, color = col[4], group = legendLabels[1]) %>%
    
    # Add circles at the locations of the first dataset 'dd2'
    addCircles(data = llpd2_sf, weight = 1, fillOpacity = 1, color = col[3], group = legendLabels[2],
               popup = ~htmlEscape(paste0("2:time=", as.character(llpd2_sf$dateTimeFormatted),
                                          ", TIME=", as.character(llpd2_sf$TIME),
                                          ", Z=", as.character(llpd2_sf$Z),
                                          ", NBS=", as.character(llpd2_sf$NBS),
                                          ", NCON=", as.character(llpd2_sf$NCONSTRAINTS),
                                          ", allBS=", llpd2_sf$allBS,
                                          ", pen=", as.character(round(llpd2_sf$PENALTY)),
                                          ", spd=", as.character(round(llpd2_sf$spd)),
                                          ", dist=", as.character(round(llpd2_sf$distance)),
                                          ", moveAngle=", as.character(round(llpd2_sf$moveAngle)),
                                          ", std=", as.character(round(llpd2_sf$stdVarXY)),
                                          ", val1=", as.character(round(llpd2_sf$val1)),
                                          ", val2=", as.character(round(llpd2_sf$val2)),
                                          ", ellipsDir=", as.character(round(llpd2_sf$ellipsDir)),
                                          ", DistMed5=", as.character(round(llpd2_sf$DistMed5)),
                                          ", TAG=", llpd2_sf$TAG))) %>%
    
    # Add lines that connect the point locations included in 'dd2'
    addPolylines(data = llpd2_lines, weight = 1, opacity = 1, color = col[3], group = legendLabels[2]) %>%
    
    # Add a scale bar to the map
    addScaleBar(position = c("bottomleft"), options = scaleBarOptions(imperial = FALSE, maxWidth = 200)) %>%
    
    # Add a layer control to the map to allow users to toggle the visibility of the different tracks.
    addLayersControl(
      overlayGroups = legendLabels,
      options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE))

  return(ll)
  # print(ll) # To display the map without returning it.
}
