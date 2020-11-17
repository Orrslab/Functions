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