velocity_filter <- function (data,spdThreshold=15, x = "X", y = "Y", time = "TIME", type = "in", steps=1) 
{
for(i in 1:steps){
  spd <- matl_get_speed(data,x=x,y=y,time=time,type = "in",step = i)*1000
  KEEP <- (spd<spdThreshold)|(shift(spd,-i)<spdThreshold)
  KEEP[is.na(KEEP)] <- TRUE  
  data<-data[which(KEEP),]
  print(sprintf("step %i removed %i locations",i,sum(!KEEP)))
}
return(data)  
}

distance_filter <- function (data,distThreshold=15*8, x = "X", y = "Y", time = "TIME", type = "in", steps=1) 
{
  for(i in 1:steps){
    dst <- matl_simple_dist(data,x=x,y=y,step = i)
    KEEP <- (dst<distThreshold)|(shift(dst,i)<distThreshold)
    KEEP[is.na(KEEP)] <- TRUE  
    data<-data[which(KEEP),]
    print(sprintf("step %i removed %i locations",i,sum(!KEEP)))
  }
  return(data)  
}

matl_simple_dist <- function (data, x = "x", y = "y",step=1) 
{
  assertthat::assert_that(is.data.frame(data), is.character(x), 
                          is.character(y), msg = "simpleDist: some data assumptions are not met")
  if (nrow(data) > 1) {
    x1 <- data[[x]][seq_len(nrow(data) - step)]
    x2 <- data[[x]][(1+step):nrow(data)]
    y1 <- data[[y]][seq_len(nrow(data) - step)]
    y2 <- data[[y]][(1+step):nrow(data)]
    dist <- c(sqrt((x1 - x2)^2 + (y1 - y2)^2))
  }
  else {
    dist <- NA_real_
  }
  return(dist)
}

matl_get_speed <- function (data, x = "x", y = "y", time = "time", type = "in", step=1) 
{
  # atlastools::atl_check_data(data, names_expected = c(x, y, time))
  data.table::setorderv(data, time)
  distance <- matl_simple_dist(data, x, y,step)
  # distance <- distance[(step+1):length(distance)]
  dtime <- data[[time]][(step+1):nrow(data)]-data[[time]][1:(nrow(data)-step)]
  # time <- c(NA, diff(data[[time]]))
  speed <- distance/dtime
  if (type == "in") {
    speed <- c(rep(NA,step),speed)
  }
  else if (type == "out") {
    speed <-c(speed,rep(NA,step))
  }
  return(speed)
}
