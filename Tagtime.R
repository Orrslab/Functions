Tagtime <- function(AA)
  {
  if(typeof(AA)== "character")
  {
    times<-read.csv(AA) # separate time frames per tag 
    # the "code" is written seperatly here since we normally write the abridged tag names, but you can merge it in the original file as well.
    times$TAG<-paste(times$code,times$TAG,sep="") # attach code to TAG names to fit the way the tags are written in the raw data
    times$TAG<-as.character(times$TAG) # Change from meaningless "factor" class to character (text format) 
    times<-times[,-1]# erase code column (not crucial)
    # convert times to UNIX time (ATLAS format)
    times$capture_time<-paste(times$date_capture,times$start_hour)
    times$capture_unix<-as.double(as.numeric(as.POSIXct(times$capture_time, "%d/%m/%Y %H:%M:%S", tz="UTC"))*1000)
    times$off_time<-paste(times$last_date_detection,times$end_hour)
    times$off_unix<-as.double(as.numeric(as.POSIXct(times$off_time, "%d/%m/%Y %H:%M:%S", tz="UTC"))*1000)
  }
  else
  {
  TagList <- unique(AA$TAG)
  capture_unix <- numeric()
  off_unix <- numeric()
  for (tg in TagList)
    {
    capture_unix <- c(capture_unix,min(AA$TIME[which(AA$TAG==tg)]))
    off_unix <- c(max(AA$TIME[which(AA$TAG==tg)]))
    }
  times <- data.frame("TAG"=TagList,"capture_unix"=capture_unix,"off_unix"=off_unix)
  }

  return(times)
  }