# require(data.table)
require(dplyr)
# velocity filter removes a location according to velocity
# details:
# it removes a location whenever both the velocity from the previous point to it (v_in) and the velocity from it to the next point (v_out) are greater than  spdThreshold
# it repeats the filtering with different step size:
# step=0 means that the velocity is calculated between nearest neighbors (in time)
# step=1 means that the velocity is calculated between second nearest neighbors
# the input variable "steps" determines up to which neighbor to check velocity (default=1)
# thus it can filter locations in case that a set of points up to size "steps" was drifted away
# input variable "Data" is a data.frame with locations saved with column names x,y, and time
# returns the data.frame without the filtered locations

velocity_filter <- function (Data,spdThreshold=15, x = "X", y = "Y", time = "TIME", steps=1, printfilt=T) 
{
  if (!all(c('TAG','X','Y','TIME') %in% names(Data)))
  {stop("velocity_filter needs the variables X,Y,TAG, and TIME (in miliseconds) to run ")}
  listoffilteredData <- list()
  for (tag in unique(Data$TAG))
  {  
  tagData <- Data %>% dplyr::filter(TAG==tag)
  for(stp in 1:steps){
    tempVar <- tagData %>% arrange(TIME) %>% 
                        mutate(dist=sqrt((X-lag(X,stp))^2+(Y-lag(Y,stp))^2),
                               dT=(TIME-lag(TIME,stp))/1000,
                               spd=dist/dT)
    KEEP <- (tempVar$spd<spdThreshold)|(lead(tempVar$spd,stp)<spdThreshold)
    KEEP[is.na(KEEP)] <- TRUE  
    tagData<-tagData[which(KEEP),]
    if(printfilt)
      print(sprintf("tag %s step %i removed %i locations",tag,stp,sum(!KEEP)))
  }
  listoffilteredData[[tag]] <- tagData
  }
  Data <- do.call(rbind.data.frame,listoffilteredData)
  return(Data)  
}


# distance filter removes a location  according to distance
# details:
# it removes a location whenever both the distance from the previous point to it and the distance from it to the next point are greater than distThreshold
# it repeats the filtering with different step size:
# step=0 means that the distance is calculated between nearest neighbors (in time)
# step=1 means that the distance is calculated between second nearest neighbors
# the input variable "steps" determines up to which neighbor to check distance (default=1)
# thus it can filter locations in case that a set of points up to size "steps" was drifted away
# input variable "Data" is a data.frame with locations saved with column names x,y, and time
# returns the data.frame without the filtered locations
distance_filter <- function (Data,distThreshold=15*8, x = "X", y = "Y", steps=1,printfilt=T) 
{
  if (!all(c('TAG','X','Y','TIME') %in% names(Data)))
  {stop("distance_filter needs the variables X,Y,TAG, and TIME (in miliseconds) to run ")}
  listoffilteredData <- list()
  for (tag in unique(Data$TAG))
  {  
    tagData <- Data %>% dplyr::filter(TAG==tag)
    for(stp in 1:steps){
    tempVar <- tagData %>% arrange(TIME) %>% 
               mutate(dist=sqrt((X-lag(X,stp))^2+(Y-lag(Y,stp))^2))
    KEEP <- (tempVar$dist<distThreshold)|(lead(tempVar$dist,stp)<distThreshold)
    KEEP[is.na(KEEP)] <- TRUE  
    tagData<-tagData[which(KEEP),]
    if (printfilt)
      print(sprintf("tag %s step %i removed %i locations",tag,stp,sum(!KEEP)))
    }
    listoffilteredData[[tag]] <- tagData
  }
  Data <- do.call(rbind.data.frame,listoffilteredData)
  return(Data)  
}


