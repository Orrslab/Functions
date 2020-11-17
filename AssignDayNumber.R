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
    data$DAY <- yday(shifteddays)-yday(shifteddays[1])+1
    }
  return(data)
}