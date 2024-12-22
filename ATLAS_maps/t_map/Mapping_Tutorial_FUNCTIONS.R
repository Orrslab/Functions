################################################################################
######################## MAPPING TUTORIAL FUNCTIONS ############################
################################################################################

# Function to calculate new latitude and longitude
calculate_new_coordinates <- function(lat1, lon1, distance, bearing) {
  # Convert degrees to radians
  lat1 <- lat1 * pi / 180
  lon1 <- lon1 * pi / 180
  bearing <- bearing * pi / 180  # Convert bearing to radians
  
  # Radius of Earth in kilometers
  R <- 6378137
  
  # Calculate new latitude
  lat2 <- asin(sin(lat1) * cos(distance / R) + 
                 cos(lat1) * sin(distance / R) * cos(bearing))
  
  # Calculate new longitude
  lon2 <- lon1 + atan2(sin(bearing) * sin(distance / R) * cos(lat1),
                       cos(distance / R) - sin(lat1) * sin(lat2))
  
  # Convert back to degrees
  lat2 <- lat2 * 180 / pi
  lon2 <- lon2 * 180 / pi
  
  return(c(lon2, lat2))
}

# Get track locations
GET_locations <- function(lat,lon,step_min = 20,step_max=40,nloc, crs = 4326){
  
  # Store path
  path_storage <- matrix(nrow = nloc,ncol = 2)
  # Calculate path
  for(i in 1:nloc){
    
    # Direction and Distance of next step
    bearing <- sample(360,1)
    distance <- sample(step_min:step_max,1)
    
    # Calculate next location
    next_location <- calculate_new_coordinates(lat,lon,distance,bearing)
    
    # Reset latitude and longitude 
    lon <- next_location[1]
    lat <- next_location[2]
    
    # Store next location
    path_storage[i,] <- next_location
  }
  
  # Convert matrix to geometries
  geom <- st_sfc(st_linestring(path_storage), crs = 4326)
  
  return(geom)
}

# ein gedi simulated pink warbler tracks
ein_gedi_pinkWarbler <- function(){
  
  lat <- c(31.447001,31.4629159,31.4029159,31.4529159,31.4029159)
  lon <- c(35.373516,35.346484,35.356484,35.396484,35.3996484)
  n = length(lat)
  
  simulate_track_data <- do.call('rbind', lapply(1:n, FUN = function(i){
    
    
    data_features <- data.frame(IND = as.character(sample(1000:2000,1)),
                                SEX = sample(c("F", "M"), 1))
    # set step_min & step_max
    switch (data_features$SEX,
            "F" = {
              step_min = 100
              step_max = 200
            },
            "M" = {
              step_min = 300
              step_max = 350
            })
    
    # simulate tracks
    geometry = GET_locations(lat[i], lon[i], nloc = 100, step_min = step_min,step_max = step_max)
    
    # sf object
    sf_object <- st_sf(data_features, geometry)
    
    # return
    return(sf_object)
  }))
  
  return(simulate_track_data)
}

