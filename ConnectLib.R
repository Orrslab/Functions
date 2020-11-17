ConnectLib <- function (path2func="/functions/") {
# Sourcing packages and local functions
# Installing packages is commented (sometimes needed before first use)
# if no input is given, looks for a "functions" directory in current directory   
#  ----------------------------- 

# install.packages("mapview")
# install.packages("rgdal")
# install.packages("sf")

library(dplyr) # summary and re-arrangement of data
library(lubridate) # For date & time  manipulations
# install.packages("dbscan")
library(dbscan) # clustering algorithm
# install.packages("sp")
library(sp) # library to work with spatial objects (for mapping)
# install.packages("leaflet")
library(leaflet) # function to visualise tracks on a map
library(htmltools) # to add "pop-ups" to leaflet maps
# install.packages("RSQLite")
library(RSQLite) # package needed to the ATLAS package for loading sqlite files
# install.packages("ggpubr")
library(ggpubr)# package needed to the ATLAS package (for plotting)
# install.packages("ggmap")
# install.packages("ggplot2")
library(ggmap)
library(ggplot2)
library(mapview)
# install.packages("suncalc")
library(suncalc)

# ------- packages from Git_hub or local source: 

# install.packages(paste0(general_path,"/Workshop/toolsForAtlas_1.0.tar.gz"), repos = NULL, type = "source")
library(toolsForAtlas) # the ATLAS package (Sivan Margalit)
# install.packages("devtools")  # for atlastiools by pratikunter
# devtools::install_github("pratikunterwegs/atlastools") # for atlastiools by pratikunter
library(atlastools)
  

# source(paste0(path2func,"visualMaps.R"))    # my functions for maps
source(paste0(path2func,"Movement_Metrics_Functions.R"))   # my enhancements for ADP
# source(paste0(path2func,"SplitNights.R")) # helper functions for time-segmentation and data filtering
source(paste0(path2func,"advancedADP.R"))   # my enhancements for ADP
# source(paste0(path2func,"points_to_line.R"))
  
source(paste0(path2func,"Data_from_ATLAS_server.R"))    # my functions 
source(paste0(path2func,"AssignDayNumber.R"))     # my functions
source(paste0(path2func,"matl_simple_dist.R"))    # my functions 
source(paste0(path2func,"matl_get_speed.R"))      # my functions 
source(paste0(path2func,"atl_mapleaf.R"))         # my functions
source(paste0(path2func,"Tagtime.R"))             # my functions 
source(paste0(path2func,"velocity_filter.R"))     # my functions   
source(paste0(path2func,"distance_filter.R"))     # my functions
}