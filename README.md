# Functions
a set of function for working with movement data

This repository is an updating library of functions for working with movement data

Here we only specify the general descriptipon of function, its dependency on other functions and, sometimes, its source

Users are encourged to download the functions, save them in their "functions" directory, use, debug and change them freely
Please share your improvments and new functions

## List of functions:
ATALS_main                                                          - R script, wrapping most of the other functions, usefull for starting a project and for examples!

	ConnectLib(path2func="\functions")                                  - a functions that imports nedded libraries and functions (used in ATLAS_main) does not retrun a value

	A <- Data_from_ATLAS_server(Start_Time_Str,End_Time_Str,FullTag)    - a function that import data from the ATLAS server (requires a VPN connection to server), returns a list, of two data frames

	A<- AssignDayNumber(data,DayStartTime="12:00:00",TimeColName="LocTime",Julian=FALSE) - a function that assigns a day number to each location ( a day is defined as starting at DayStartTime, returns the same data with additional column (DAY)

	A <-Tagtime("tagtimes.csv")       - a function to create a data.frame of tags, each with is relevant time limits (if given a CSV file it reads it, if given a location data.frame it finds it content), returns a data frame

	plotdays(data,TAG_ex,xlims,ylims) - ploting function,  plot each day on a simple plot and stops (no background)

	atl_mapleaf(data)                 - ploting function,  plot the entire data on a leaflet map, each point is assigned some data

	atl_mapgg(data)                   - ploting function,  plot the entire data on a ggmap map, uses geographic coordinates!

	A <- velocity_filter (data,spdThreshold, x = "X", y = "Y", time = "TIME", steps=20) - filter according to velocity. Discards a set of drifted points up to length "steps" when  this drifted section required speed over some spdThreshold to connect , returns a data.frame that does not include the discarded points 

	A <- distance_filter (data,distThreshold=500, x = "X", y = "Y", steps=2)            - filter according to distance. Discards a set of thrown-away points up to length "steps", when this drifted section required was overdistThreshold from its surrounding. Returns a  data.frame that does not include the discarded points. 

	A <-filterByVelocity (data,dataLegend=c("X","Y","TIME"), options=optionsArg)        - filter according to velocity. Identefies a reliable point and throws any point which is not connected (with reasonable speed to this reliabl point. might be relativly slow and inefficient. Returns a data.frame that does not include the discarded points
