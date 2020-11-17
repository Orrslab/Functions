plotdays <- function(data,TAG_ex,xlims=0,ylims=0)
{
  if (xlims[1]==0)
  {
  xlims[1]=max(data$X[which(data$TAG==TAG_ex)])
  xlims[2]=min(data$X[which(data$TAG==TAG_ex)])
  }
  if (ylims[1]==0)
  {
    ylims[1]=max(data$Y[which(data$TAG==TAG_ex)])
    ylims[2]=min(data$Y[which(data$TAG==TAG_ex)])
  }
  DAY_list <- unique(data$DAY[which(data$TAG==TAG_ex)])
  
  for (i in DAY_list)
  {
    raw_tracks<-data[which(data$TAG==TAG_ex & data$DAY==i),] # selecting DAY number 3 of randomly selected tags. Naturally you can control which tag and DAY you want to examine too.
    plot(raw_tracks$X,raw_tracks$Y,asp=1,col="black",pch = 3,main=sprintf("Tag=%3.0f, DAY = %i",TAG_ex,i),xlim=xlims,ylim=ylims)
    lines(raw_tracks$X,raw_tracks$Y,col="black")
    # points(filtered_tracks1$X,filtered_tracks1$Y,col="red",pch = 4)
    
    readline(sprintf("DAY = %i, show next?",i))
  }
}