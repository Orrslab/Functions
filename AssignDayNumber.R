# A function to assign day number
# Every day begins on DayStartTime (a HH:MM:SS character, in UTC, just like the ATLAS times)
# The input variable data is a data.frame containing a POSIXct time columns (tz="UTC",origin="1970-01-01") 
# The data frame time columnmust be named TimeColName
# The day are counted from either as Julian days (Julian=TRUE) or from the first day in the array (default, Julian=FALSE) 
# Currently the days are counted from the earliest time in the set without taking into account different Tags
# Returns the same data.frame with an additional column ("DAY")

AssignDayNumber <- function(data,DayStartTime="00:00:00",TimeColName = "TIME",Julian=FALSE)
{
  datatimes <- as.data.frame(data)
  datatimes <-datatimes[,TimeColName]
  timeshift <- as.numeric(as.POSIXct(paste("1970-01-01",DayStartTime),tz="UTC",origin="1970-01-01"))
  shifteddays <- as.POSIXct(as.numeric(datatimes)-timeshift, tz="UTC", origin="1970-01-01")
   if (Julian)
    {
    data$DAY <- yday(shifteddays) 
    }
  else
    {
    data$DAY <- yday(shifteddays)-min(yday(shifteddays))+1
    }
  return(data)
}