## amt Paper: https://onlinelibrary.wiley.com/doi/full/10.1002/ece3.4823
## amt: https://cran.r-project.org/web/packages/amt/vignettes/p2_hr.html

# This function divide the track into bursts based of continuous detection (only time-wise )
# it is based on time gaps between two point and requires the following variables:
    # data: a data.frame cantainint the varibles: "X","Y" (coordinates in any format),"dateTime" (POSIXct fromat)
    # secTol # TOLARANCE SEC FOR NEXT BURST
    # minBurstFix # Min points in burst
    # sampRate : the numinal sampling rate of the tag
#The output is a data.frame with the following variables:
    #"X", "Y", "dateTime", (equal to the input data), "TIME" (numeric value of POSIXct),"timeGapBurst" (a unique burst integer ID ), "pointInBurst" (number of points in the specific burst)
# Packages required: amt, lubridate

timeGapBurstNew <- function(data,secTol,minBurstFix,sampRate=8){
  
  require(amt)
  dataBurst_1 <- data %>%
    make_track(.x=X, .y=Y, .t=dateTime) %>%
    track_resample(rate = seconds(sampRate), tolerance = seconds(secTol)) %>%
    steps_by_burst() %>%
    group_by(burst_) %>%
    ungroup() %>%
    select(x1_,y1_,t1_,burst_) %>%
    rename(X = x1_,Y = y1_,dateTime = t1_, timeGapBurst = burst_) %>%
    group_by(timeGapBurst) %>% # remove burst with pointInBurst lower than "minBurstFix"
    add_count()%>%
    filter(n > minBurstFix) %>%
    rename(pointInBurst=n) %>% 
    mutate(TIME = as.numeric(as.POSIXct(dateTime,"%Y-%m-%d %H:%M:%S", tz="UTC"))*1000 ) # Atlas time 
    colOrder <- c("X", "Y", "TIME","dateTime","timeGapBurst", "pointInBurst") #Change the order of columns
    dataBurst_1 <- dataBurst_1[, colOrder]
 
   return(dataBurst_1)
}