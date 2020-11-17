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