# Functions
a set of function for working with movement data

This repository is an updating library of functions for working with movement data

Here we only specify the general descriptipon of function, its dependency on other functions and, sometimes, its source

Users are encourged to download the functions, save them in their "functions" directory, use, debug and change them freely
Please share your improvments and new functions

## List of functions:
	ATALS_main                                                          - R script, wrapping most of the other functions, usefull for starting a project and for examples!

	ConnectLib(path2func="\functions")                                  - a functions that imports needed libraries and functions (used in ATLAS_main) does not retrun a value
	A <- Data_from_ATLAS_server(Start_Time_Str,End_Time_Str,FullTag)    - a function that import data from the ATLAS server (requires a VPN connection to server), returns a list, of two data frames
	A <- identifyDetections(Loc=RawLoc0,Det=RawDet0,unloclalized =F) - a function that assignes the antennas number used to calculate each localizaion, when specifing unloclalized =F, also return a list of detections not used for localization
	A<- AssignDayNumber(data,DayStartTime="12:00:00",TimeColName="LocTime",Julian=FALSE) - a function that assigns a day number to each location ( a day is defined as starting at DayStartTime, returns the same data with additional column (DAY)
	A <-Tagtime("tagtimes.csv")       - a function to create a data.frame of tags, each with is relevant time limits. accepts an ATLAS location data.frame , returns a data frame
	A <-humanTime2Unix(DATE,TIME,ATLAS=FALSE)       - he function accepts date in d/m/Y format and time in H:M:S format in, UTC and returns a numeric format (time from 1/1/1970), if ATLAS=TRUE the time is returned in milliseconds, else it is returned in seconds

	file: velocity filter includes 4 functions: (All currently works for a single tag)
		A <- velocity_filter (data,spdThreshold, x = "X", y = "Y", time = "TIME", steps=20) - filter according to velocity. Discards a set of drifted points up to length "steps" when  this drifted section required speed over some spdThreshold to connect , returns a data.frame that does not include the discarded points 
		A <- distance_filter (data,distThreshold=500, x = "X", y = "Y", steps=2)            - filter according to distance. Discards a set of thrown-away points up to length "steps", when this drifted section required was overdistThreshold from its surrounding. Returns a  data.frame that does not include the discarded points. 
		A <- matl_simple_dist <- function (data, x = "x", y = "y",step=1) 		    - accepts the data.farme and the names of the itm coordintes and returns a vector of distances between locations seperated by "step" steps
		A <- matl_get_speed <- function (data, x = "x", y = "y", time = "time", type = "in", step=1) - accepts the data.farme and the names of the "ITM" coordintes and returns a vector of velocities between locations seperated by "step" steps
		
	A <-filterByVelocity (data,dataLegend=c("X","Y","TIME"), options=optionsArg)        - a toolsForAtlas function ! not available with ORRSlab github "function" repository. It filters according to velocity. Identefies a reliable point and throws any point which is not connected (with reasonable speed to this reliabl point. might be relativly slow and inefficient. Returns a data.frame that does not include the discarded points

	file: atl_plots include 3 functions
		plotsqure(x,y,a_col="red",override=FALSE) - given two diagonal points of a squre, plots a squre
		plotdays(data,TAG_ex,xlims,ylims) - ploting function,  plot each day on a simple plot and stops (no background)
		atl_mapleaf(data)                 - ploting function,  plot the entire data on a leaflet map, each point is assigned some data
		atl_mapleaf_withstops(list(FiltLoc1,bat_adp),Tags=TAG_ex,Days=dispDAY) - ploting function,  plot two data.frames, the first is a usuall track the second a stop data.frame each point is assigned some data, 
		atl_mapgg(data)                   - ploting function,  plot the entire data on a ggmap map, uses geographic coordinates!
		
	A <- visual_filter <- function(data,printoptions=TRUE,DefalutN2filter=FALSE)  - plots data on a simple plot and allows you to graphicaly discard or collect points, return a dat.frame with data after the filter. If points were collected it return a list, with two data.frames A$filterd and $collected.
	A <- wrap_ADP(FiltLoc1,freq=8)             -wraps the function AdpFixedPoint that calculates stops and their parameters (duration mean position etc) it includes loop over days and tags and post-processing it returns a data.frame with all parameters

