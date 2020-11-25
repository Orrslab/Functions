# accepts a movement data of one tag plots every day separately and allows on-plot filtering and choosing
# input variables:
    # data - a positions data (coordinates are in columns labeled "X" and "Y")
    # printoptions - sholud the function print the options at each step (default=TRUE,
    # DefalutN2filter - see n option (throw  specific points, default=FALSE)
# at every iteration the function waits for user command:
      # c = show next day,
      # i = zoom-in (according to two points on diagonal), 
      # o = zoom-out(back to a scope containin the entire day)
      # s = throw two points square (according to two points on diagonal), 
      # n = throw  specific points (these points should be pointed specifically on the map)
      #     if DefalutN2filter is FALSE, you will be requred to specify the number of point to filter-out
      #     you can stop choosing by clicking stop on the map
      #     if DefalutN2filter was choosen as another value, this is the number of points to filter-out
      # l = collect/mark a line between two points (point two points on the track, all points in b
      #     between will be colored red, then you can either collect them or not)
      # p = collect points in a squre,
      #     specify a square by designating two diagonal points, all points will be colored, yopu can either collect them or not!
      # b = break loop return the data collected and filter so far.
      # t = set a default number of filtered to points(n),
      # d = toggle option not presentation")
# output
      # the function return a data.frame with all the points left after filtration.
      # if specific points were collected, the function returns a list of two data.frames, 
          # the first contains the points left after filtration
          # the second is the collected points (each point has another variable "segment" that specifies its sample)

visual_filter <- function(data,printoptions=TRUE,DefalutN2filter=FALSE)
{

  DAY_list <- unique(data$DAY)
  TAG_list <- as.numeric(unique(A$TAG))
  filtData <- NULL
  Collected_points <- NULL
  xlims <- NULL
  ylims <- NULL
  segment_index <- 1
  for (TAG_ex in TAG_list)
  {
  for (day in DAY_list)
  {
    daydata<-data[which( data$DAY==day),]
    Tag_ex <- 
    xlims[1] <- max(daydata$X,na.rm =TRUE) # the plot limits are defined for each day
    xlims[2] <- min(daydata$X,na.rm =TRUE)
    ylims[1] <- max(daydata$Y,na.rm =TRUE)
    ylims[2] <- min(daydata$Y,na.rm =TRUE)
   
    userinput <- "i"
    Xlims <- xlims
    Ylims <- ylims
    while (userinput!="c")
    {
      plot(daydata$X,daydata$Y,asp=1,col="black",pch = 3,main=sprintf("Tag=%3.0f, DAY = %i",TAG_ex,day),xlim=Xlims,ylim=Ylims)
      lines(daydata$X,daydata$Y,col="black")
      if(printoptions)
      {
      writeLines("options:  c = show next day,
          i = two points zoom-in, 
          o = zoom-out,
          s = throw two points square, 
          n = throw  specific points,
          l = collect/mark a line between two points 
          p = collect points in a squre,
          b = break loop return the data collected and filter so far,
          t = set a default number of filtered to points(n),
          d = toggle option not presentation")
      }
      userinput <- readline(sprintf("DAY = %i, what to do? ",day)) # the system askes the user what to do
         
           if (userinput=="i") # zoom-in
             {
             limits <- locator(2,type="o")  # allow graphically choosing the x,y values on a plot
             Xlims <- round(limits$x)
             Ylims <- round(limits$y)
             }
            else if (userinput=="o") #zoom-out
            {
            Xlims <- xlims
            Ylims <- ylims
            }
            else if (userinput=="s") #filter-out square
            {
            limits <- locator(2,type="o")  # allow graphically choosing the x,y values on a plot
            daydata<-daydata[which(!(daydata$X>min(limits$x,na.rm =TRUE)&(daydata$X<max(limits$x,na.rm =TRUE))&
                                     daydata$Y>min(limits$y,na.rm =TRUE)&(daydata$Y<max(limits$y,na.rm =TRUE)))),]  
            }
            else if (userinput=="n") # filter-out n points
            {
              if(DefalutN2filter)
                {points2throw <- identify(daydata$X,daydata$Y,n=DefalutN2filter,labels="x",plot=TRUE)}
              else
                {
                points2throw <- readline("how many points to throw ?") 
                points2throw <- identify(daydata$X,daydata$Y,n=as.numeric(points2throw),labels="x",plot=TRUE)
                }
            daydata <- daydata[-points2throw,]
            }
            else if (userinput=="l") # collect points between to points
            {
              points2collect   <- identify(daydata$X,daydata$Y,n=2,labels="o",plot=TRUE,offset=0)
              few_Collected_points <- daydata[seq(min(points2collect,na.rm =TRUE),max(points2collect,na.rm =TRUE)),]
              points(few_Collected_points$X,few_Collected_points$Y,col="red")
              collect <- readline(sprintf("marked %i points? do you want to save them as segment %i?  (n/y)",nrow(few_Collected_points),segment_index))
              if (collect=="y")
                {
                few_Collected_points$segment <- rep(segment_index,nrow(few_Collected_points))
                segment_index <- segment_index+1
                Collected_points <- rbind(Collected_points,few_Collected_points)
                }
              
            }
            else if (userinput=="p") # collect points in a squre
            {
              limits <- locator(2,type="o")  # allow graphically choosing the x,y values on a plot
              few_Collected_points<-daydata[which((daydata$X>min(limits$x)&(daydata$X<max(limits$x))&
                                         daydata$Y>min(limits$y)&(daydata$Y<max(limits$y)))),] 
              points(few_Collected_points$X,few_Collected_points$Y,col="red")
              collect <- readline(sprintf("marked %i points? do you want to save them as segment %i?  (n/y)",nrow(few_Collected_points),segment_index))
              if (collect=="y")
              {
                few_Collected_points$segment <- rep(segment_index,nrow(few_Collected_points))
                segment_index <- segment_index+1
                Collected_points <- rbind(Collected_points,few_Collected_points)
              }
            }
            else if (userinput=="b") # break the while loop and close the function
            {break}
            else if (userinput=="d") #toggle option not presentation 
            {
              if(printoptions) {printoptions <- FALSE}
              else {printoptions <- TRUE}
            }
            else if (userinput=="t") #toggle option not presentation 
            {
              DefalutN2filter <- as.numeric(readline("enter default number for samples to filter (0 = no default) "))
            }  
    }
    if(userinput=="b")
    {break}
    filtData <- rbind(filtData, daydata)
    
  }
  }
  if (is.null(Collected_points))
  {return(filtData)}
  else
  {return(list(filterd=filtData,collected=Collected_points))}
}