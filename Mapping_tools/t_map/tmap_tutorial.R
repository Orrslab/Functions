################################################################################
############################# Intro to tmap ####################################
################################################################################

# define required paths
path_to_working_directory <- "C:/Users/netat/Documents/Movement_Ecology/R_Projects/Functions/Mapping_tools/t_map"
path_to_save_the_t_map <- path_to_working_directory

{
# set working directory
setwd(path_to_working_directory)

# packages
library(osmdata) # query vector data 
library(OpenStreetMap) # query raster data
library(tmap) # plot map
library(tmaptools) # tmap extension
library(rnaturalearth) # download maps of other countries
library(sf) # manipulate borders
library(raster) # convert "OpenStreet" object to raster
library(rnaturalearthhires) # For the country boarders
#library(dplyr) # manipulate sf objects
# source("disputed_boundaries.R") # get disputed boundaries shape files
source("Mapping_Tutorial_FUNCTIONS.R")
# source("../Mapping_tools/IMS_function.R")
}
############################ Base Map View Mode ################################
{
  # specify latitude and longitude coordinates
  lat1 <- 34
  lat2 <- 36
  lon1 <- 29.5
  lon2 <- 34
  
  # set to plot mode
  tmap_mode("view")
  
  # interactive map
  tm_basemap("Esri.WorldImagery") + # base map
    tm_view(bbox = c(lon2, lat2, lon1, lat1))
}
############################ Base Map Plot Mode ################################
{
  # basemaps
  nm <- c("osm","bing","osm-german","esri",
          "esri-topo","esri-physical","esri-shaded","esri-imagery",
          "esri-terrain","esri-natgeo","nps","apple-iphoto")
  
  # plot setup
  par(mfrow = c(3,4), mar = c(.1,.1,.1,.1))
  
  for(i in 1:length(nm)){
    cat("Uploading", nm[i], "\n")
    # query raster data
    map_tiles <- openmap(upperLeft = c(lon1, lat2),
                         lowerRight = c(lon2, lat1), # lat/long
                         type = nm[i], # map type
                         minNumTiles = 5L, # minimum number of tiles
                         mergeTiles = TRUE) # merge tiles
    
    # plot
    plot(map_tiles)
  }
}
############################## OSM Shape Files #################################
{
  # look up tags at https://www.openstreetmap.org/#map=8/31.438/35.074
  
  # map tags are key-value pairs used to describe geographic features and their properties in OSM. 
  # These tags define what each map element (like a point, line, or polygon) represents and provide detailed metadata.
  # For example:
  # A tag with key = "boundary" and value = "protected_area" identifies features like national parks, nature reserves, or other protected areas.
  
  # search tags
  available_features()
  
  # search associated tags
  available_tags("boundary")
  
  # get bounding box, for example Ein Gedi bounding box
  bb <- getbb("Ein Gedi, Israel")
  
  # query data
  query <- opq(bb) |> 
    add_osm_feature(key = 'boundary', value = 'protected_area') 
  
  # obtaining feature object
  ein_gedi_data <- osmdata_sf(query)
  
  # extract boundary geom
  ein_gedi_boundary <- ein_gedi_data$osm_polygons
}
################################################################################
############################### Map of Israel ##################################
################################################################################
{
  # country borders
  world <- ne_countries(scale = "large", returnclass = "sf") # set scale to "large" high detailed borders
  
  # plot the world
  plot(st_geometry(world),
       col = "lightblue",
       border = "black",
       main = "We Are the World")
  
  # extract desired countries borders
  map_data <- world[world$name %in% c("Israel","Palestine","Syria","Lebanon","Egypt","Jordan"),]
  
  # check for validity
  if(!all(st_is_valid(map_data))){ # if not all country borders are valid
    
    map_data <- st_make_valid(map_data)
  }
  
  # set label placement
  suppressWarnings(centroid_locations <- st_coordinates(st_centroid(ein_gedi_boundary)))
  
  label_text <- st_as_sf(data.frame(
    lon = centroid_locations[1,"X"], 
    lat = centroid_locations[1,"Y"],
    text = "Study Area"
  ), coords = c("lon", "lat"), crs = 4326) 
  
  # split Israel/Palestine polygons to intersecting and non-intersecting lines
  
  # extract polygon geometry
  israel <- st_geometry(map_data[map_data$name == "Israel",])
  palestine <- st_geometry(map_data[map_data$name == "Palestine",])
  
  # intersection
  
  # # convert to 'LINESTRING'
  
  #israel_line <- israel |> 
  #  st_cast("POLYGON") |> 
  #  st_cast("LINESTRING")
  
  #palestine_line <- palestine |> 
  #  st_cast("POLYGON") |> 
  #  st_cast("LINESTRING")
  
  #intersection_israel_palestine <- st_intersection(israel_line,palestine_line)
  
  # non-intersection
  non_intersection_israel_palestine <- st_sym_difference(israel,palestine)
  
  # import shape file as sf object
  mid_east <- st_read("layers/borders_WGS1984.shp")
  
  # extract Dead Sea and Sea of Galilee
  dead_sea_Galilee <- mid_east[mid_east$NAME_ %in% c("Dead Sea","Sea of Galilee"),]
  
  # combine polygons by NAME_ column
  Dead_Galilee_Sea <- do.call('rbind',by(dead_sea_Galilee, INDICES = dead_sea_Galilee$NAME_, FUN = function(data){
    # return sf object
    st_sf(NAME = unique(data$NAME_), geometry = st_union(data$geometry))
  }))
  
  # Golan
  # see: https://hub.arcgis.com/datasets/a0d53b693e454c38bbb88127650ecfab
  #disputed_boundaries <- disputedBoundaries()
  disputed_boundaries <- ne_download(scale = 10,
                                     category = "cultural",
                                     type = "admin_0_boundary_lines_disputed_areas") # see https://cran.r-project.org/web/packages/rnaturalearth/vignettes/rnaturalearth.html
  
  Golan <- disputed_boundaries[disputed_boundaries$NAME %in% "Syrian claim",]
}
############################## Plot Map of Israel ##############################
{
  # set mode
  tmap_mode("plot")
  
  # plot
  (Israel_map <- tm_shape(map_data, bbox = c(xmin = 34,
                                             ymin = 29.5,
                                             xmax = 36,
                                             ymax = 34)) + # set desired boundary box
      # fill in polygons
      tm_polygons(
        col = "antiquewhite1", # color country polygons
        border.col = "antiquewhite1", # color country borders
        border.lwd = 20 # Change this value to adjust the border size
      ) + 
      
      # add surrounding countries borders
      tm_shape(map_data[!map_data$name %in% c("Israel","Palestine"),]) +
      tm_borders(lwd = 2, col = "grey40") +
      
      # add non-intersecting borders
      tm_shape(non_intersection_israel_palestine) +
      tm_borders(lwd = 2, col = "grey40") +
      
      # add palestine borders
      tm_shape(palestine) +
      tm_borders(lwd = 2, lty = "dashed", col = "grey40") +
      
      # add dead/galilee sea
      tm_shape(Dead_Galilee_Sea) +
      tm_polygons(col = "blue",
                  border.col = "antiquewhite1",
                  alpha = 0.3,
                  border.alpha = 0.5) +

      # disputed boundary: Golan
      tm_shape(Golan) +
      tm_lines(lty = "dashed",lwd = 2, col = "grey40") +
      
      # add ein gedi border
      tm_shape(ein_gedi_boundary) + # vector shape
      tm_polygons(col = "darkgreen",
                  border.col = "darkgreen",
                  border.lwd = 5,
                  alpha = 1,
                  lwd = 3,
                  lty = 1) +
      
      # add compass
      tm_compass(type = "4star",
                 position = c("left","top"), # add compass
                 size = 2.2,
                 text.size = 1,
                 color.dark = "black",
                 color.light = "white")  +
      
      # add label
      tm_shape(label_text) +
      tm_text(text = "text",
              size = 0.75, 
              col = "black",
              xmod = -0.05, # adjust positioning 
              ymod = 0.7, # adjust positioning
              bg.color = "white", # border color
              bg.alpha = 0.5, # border alpha
              just = "center") + # text positioning inside border
      
      # layout
      tm_layout(bg.color = "aliceblue"))
}
################################################################################
{
  # set directory of map file
  fullpath_to_save_map <- paste0(path_to_save_the_t_map, "/Israel_map.png")
  
  # save tmap
  tmap_save(Israel_map,
            filename = fullpath_to_save_map,
            width = 4.5,
            height = 8.75,
            dpi = 1000)
}
################################################################################
############################### GPS Data #######################################
################################################################################
{
  # set seed
  set.seed(8051)
  
  # get pink warbler data
  (bird_tracks <- ein_gedi_pinkWarbler())
}
################################ Raster Data ###################################
{
  # specify latitude and longitude coordinates
  lat1 <- 31.3815
  lat2 <- 31.5019
  lon1 <- 35.3305252
  lon2 <- 35.4252
  
  # query raster data
  map_tiles <- openmap(c(lat2, lon1), c(lat1, lon2), # lat/long
                       type = "bing", # map type
                       minNumTiles = 50L, # minimum number of tiles
                       mergeTiles = TRUE) # merge tiles
  
  # check plot
  
  plot(map_tiles)
  
  # convert to wgs84
  
  open_wgs84 <- openproj(map_tiles, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84")
  
  # convert to raster 
  
  ras_wgs84 <- raster::raster(open_wgs84)
}
################################# Plotting #####################################
{
  # set to plot mode
  tmap_mode("plot")
  
  (map <- tm_shape(ras_wgs84) + # basemap raster
    # set rgb scale for raster
    tm_rgb() + 
      
    # add ein gedi boundary
    tm_shape(ein_gedi_boundary) + 
    tm_polygons(col = "lightgreen",
                border.col = "darkgreen",
                border.lwd = 5,
                alpha = 0.3,
                lwd = 3,
                lty = 3) +
      
    # add gps tracks
    tm_shape(bird_tracks) +
    tm_lines(col = "IND",
             title.col = "Bird ID", # change name of legend
             lwd = 4) +
      
    # add scale bar
    tm_scale_bar(position = c("left","bottom"), # add scale
                 color.light = "white",
                 color.dark = "black",
                 bg.color = NA,
                 bg.alpha = 0.5,
                 text.size = 1.75,
                 lwd = 1.25,
                 breaks = seq(0,2,1)) +
      
    # add legend
    tm_legend(legend.bg.color = "white",
              legend.bg.alpha = 0.75,
              legend.title.fontface = "bold",
              legend.text.fontface = "plain",
              legend.frame = TRUE,
              legend.outside = FALSE,
              legend.title.size = 1.85,
              legend.text.size = 1.2) + 
      
    # add image of ein gedi sandgrouse
    # If you don't see the bird's picture on the map, try to adjust the height
    tm_logo(file = paste0(path_to_working_directory, "/pink_warbler.png"),
            position = c(0.54,0.7),
            margin = 1,
            height = 12) +
      
    # customize layout
    tm_layout(frame = FALSE))

}
################################ Export Map ####################################
{
  
  # save tmap
  tmap_save(map,
            filename = paste0(path_to_save_the_t_map, "/pink_warbler_map.png"),
            dpi = 1000)
}
############################# Multiple Plots ####################################
{
  tm_shape(ras_wgs84) + # Basemap raster
    tm_rgb() + 
    
    # add ein gedi boundary
    tm_shape(ein_gedi_boundary) + 
    tm_polygons(col = "lightgreen",
                border.col = "darkgreen",
                border.lwd = 5,
                alpha = 0.3,
                lwd = 3,
                lty = "solid") + 
    
    # add GPS tracks
    tm_shape(bird_tracks) +
    tm_lines(col = "IND",
             title.col = "Bird ID", # Change name of legend
             lwd = 4,
             palette = 'Set1') + 
    
    # facet by SEX 
    tm_facets(by = "SEX", ncol = 2, free.coords = FALSE) +  # Increase size of facet titles
    
    # add graticules
    tm_graticules(x = NA,
                  y = NA,
                  n.x = 2,
                  n.y = 3,
                  lines = FALSE,
                  labels.inside.frame = FALSE) +
    
    tm_legend(legend.show = FALSE) +
    
    # layout
    tm_layout(panel.labels = c("Female","Male"),
              panel.label.fontface = "bold",
              panel.label.size = 1.25,
              frame = FALSE,
              frame.lwd = NA,
              panel.label.bg.color = NA)
}

################################################################################
################################################################################