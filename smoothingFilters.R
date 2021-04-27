# Smoothing filters include two smoothing tools (outlayerSmooth, movMean)
# and a functions that applies this two tools on each burst in a dataframe (AvgSmooth)

# 1- outlayerSmooth
# the functions calculate the mean location between points 1-3 in case that the 
# distance between point 1-2 is bigger than the distance between points 1-3.

# 2- movMean
# For each burst the function 

# 3- AvgSmooth 
# The input is a data.frame with X, Y and burst columns
# This function apply each one of the previous functions (outlayersmoot,movMean) on a data frame with "X", "Y" and "timeGapBurst" (a unique burst integer ID )

outlayerSmooth <- function(dataBurst)
{  track <- dataBurst
for (locIdx in (nrow(track)-2)) {
  dist1_2 = sqrt((track$X[locIdx] - track$X[locIdx+1])^2 + (track$Y[locIdx] - track$Y[locIdx+1])^2)
  dist1_3 = sqrt((track$X[locIdx] - track$X[locIdx+2])^2 + (track$Y[locIdx] - track$Y[locIdx+2])^2)
  
  if (dist1_2 > dist1_3) {
    track$X[locIdx+1] = mean(c(track$X[locIdx],track$X[locIdx+2]))
    track$Y[locIdx+1] = mean(c(track$Y[locIdx],track$Y[locIdx+2]))
  }
}
return(track)}

movMean <- function(dataBurst,Weight = c(0.25,0.5,0.25),replace=T) 
{
  X <- as.numeric(stats::filter(dataBurst$X, Weight,method = "convolution",sides = 2))
  Y <- as.numeric(stats::filter(dataBurst$Y, Weight,method = "convolution",sides = 2))
  X[which(is.na(X))] <- dataBurst$X[which(is.na(X))]
  Y[which(is.na(Y))] <- dataBurst$Y[which(is.na(Y))]
  if(replace){
    dataBurst$X <- X
    dataBurst$Y <- Y
    return(dataBurst)}
  else
    return(data.frame(X=X,Y=Y))
}

AvgSmooth <- function(dataBurst,Weight = c(0.25,0.5,0.25))
{ dataBurst <- dataBurst %>% 
  group_by(timeGapBurst) %>%
    group_modify(~ outlayerSmooth(.)) %>% 
    group_modify(~ movMean(.,Weight ))
  return(dataBurst)
}