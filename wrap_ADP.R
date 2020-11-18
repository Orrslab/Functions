wrap_ADP <- function(data,freq=8)
{
  # indices to match tag frequency:
  ind_rts<-data.frame("smp_rte"=c(8000,4000,2000,1000),"obs_min"=c(8,15,30,60),"p_lim"=c(8,15,30,60),"adp_rng"=rep(30,4))
  
  ADP<-NULL
  TagList <- unique(data$TAG)
  freq <- freq*1000
  for (tg in TagList){
    # what is this tag;s sampling frequency?
    # freq<-TagsFreq$Frequency[which(TagsFreq$TAG==tg)]*1000
    # Get AdpFixedPoint criterias from the table according to sampling rate:
    smp_rte <- ind_rts$smp_rte[which(ind_rts$smp_rte==freq)]
    adp_rng <-  ind_rts$adp_rng[which(ind_rts$smp_rte==freq)]
    obs_min <- ind_rts$obs_min[which(ind_rts$smp_rte==freq)]
    p_lim   <- ind_rts$p_lim  [which(ind_rts$smp_rte==freq)] 
    time_gap<-3600*1000
    
    # created a list of DAYs tracked per tag for looping:
    nDAYs<-unique(data$DAY[which(data$TAG==tg)])
    
    # Start loop per tag-DAY. Notice that data MUST be sorted chrnologically first.
    for (nn in nDAYs)
    {
      # nn <- nDAYs[1]
      DAY_dat<-as.data.frame(data%>%
                               filter((TAG==tg) & (DAY==nn))%>%
                               arrange(TIME))
      
      
      if(nrow(DAY_dat)!=0) # only run if DAY data is not empty:
      {
        
        AFPList <- AdpFixedPoint (time.vec = DAY_dat$TIME,
                                  x=DAY_dat$X,
                                  y=DAY_dat$Y,                             
                                  adp_rng=adp_rng,
                                  smp_rte=smp_rte,
                                  obs_min=obs_min,
                                  p_lim=p_lim,
                                  time_gap=time_gap) 
        
        #remove dummy points created by AdpFixedPoint
        kdx <- which(AFPList$duration!="1")
        AFPList <- AFPList[kdx,]
        
        
        if (nrow(AFPList)>1){
          AFPList$DAY_number<-nn
          AFPList$TAG<-tg
          AFPList$week_num<-(nn%/%7)+1
          ADP<-rbind(ADP, AFPList)
        }
      }
    }   
  }
  
  # Add more intuitive format of time, dates and stop-duration columns (i.e. not UNIX times)
  indx<-which(is.infinite(ADP$start))
  ADP$start[indx]<-ADP$end[indx]-1000*60*10
  time_start<-as.POSIXct((ADP$start)/1000, tz="UTC", origin="1970-01-01")
  time_end<-as.POSIXct((ADP$end)/1000, tz="UTC", origin="1970-01-01")
  ADP$date<-as.Date(time_start)
  ADP$time_start<- format(as.POSIXct(strptime(time_start,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M:%S")
  ADP$time_end<- format(as.POSIXct(strptime(time_end,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M:%S")
  ADP$duration_minutes<-round(ADP$duration/(60*1000))
  return(ADP)
}