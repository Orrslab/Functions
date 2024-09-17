#http://adv-r.had.co.nz/Rcpp.html

print(' Attention: the file Track_cpp.R runs an essential sourceCpp commnad,
        if your current directory (getwd()) does not contain a functions/TrackConf.cpp file
        you can source it beforehand and inhibit the command form the file')

### Load the required packages to run this script
require('Rcpp')
library(dplyr)
sourceCpp('TrackConf.cpp')

# knitr:::input_dir()

# Evaluate the confidence level of each localization in 'Data', using the functions in TrackConf.cpp
TrackConfidenceLevelcpp <- function(Data,conectedVel=20,conectedDist=NA,stdlim=80,minNBSforConf2=7,
                                    minNBSforConf1=4,Nconf1forConf2=5)
{
  # the function is a wrapper to a cpp code (TrackConfidenceVec) saved in file "TrackConf.cpp"
  # that calculates the confidence of any point in a track without discarding it
  # it runs a loop on all data points per tag and gives higher confidence mark (in the range [0,1,2] )
  # to points which have large NBS or are close to confidant points
 
  # Arguments:
  #
  #   'Data': the dataset, which contains the localizations for which the confidence level should be evaluated
  #
  #   'minNBSforConf1', 'minNBSforConf2': the minimal allowed numbers of Base Stations participated in 
  #  	   the evaluation of the localization of the observed data point, used for the assessment of confidence level 1 and 2, respectively
  #
  #   'Nconf1forConf2': the minimal number of connected points with confidence level "1", which is used as a threshold to assess
  #		   if the observed localization point should obtain a confidence level of "2".
  #
  #   'conectedDist': the threshold distance [m] used to determine if two analyzed localisations are 'connected'(*).
  #
  #   'conectedVel': the speed [m/s] used for calculation of the threshold distance, 
  #	  	 which defines if two localization points are "connected"(*)-
  #
  #   'stdlim': the maximal allowed value of 'stdVARXY' value.
  #
  #   (*) A localization point is considered "connected" if its' distance from another confident point,
  #  		distance= 'conectedVel' * 'minimalTimeDifference', is smaller than the threshold 'conectedDist'.
  #   	The variable 'minimalTimeDifference' is the minimal time-difference between sampling points within the dataset. 
  
  # Check that all the data necessary for the confidence evaluation is available
  # If not all the mentioned columns below are included in the data set, stop the function from running and output a warning
  if (!all(c('TAG','X','Y','NBS','TIME') %in% names(Data)))
  {stop("TrackConfidenceLevel needs the variables TAG,X,Y,NBS, and TIME (in miliseconds) to run ")}
  
  # Calculate the larger eigenvalue of the covariate matrix of the X and Y coordinates of the location
  if (!('val2' %in% names(Data))) 
  {
    if (!all(c('VARX','VARY','COVXY') %in% names(Data))) {
      stop("TrackConfidenceLevel needs either val2 or VARX,VARY,COVXY to run")
    }
    
    # TODO: change val2 to stdVarXY
    print("TrackConfidenceLevel calculates stdVarXY as greater eigenvalue")
    Data <- Data %>% mutate(val2= sqrt(((VARX+VARY)+sqrt(VARX^2+VARY^2-2*VARX*VARY+4*COVXY^2))/2)) # greater eigenvalue
  }
  
  # Rearrange the data rows 'Data' from the beginning-time to the end-time (ascending order)
  Data <- Data %>%  arrange(TIME)
  
  # If the column 'Conf' does not exist in 'Data', add it and assign a default value of -1 to all the rows
  if(!('Conf' %in% names(Data))) {
    Data$Conf=-1
  }
  
  # Create a new column called 'aBS', and set its' values to be the same as those in the existing 'NBS' column
  Data$aBS <- Data$NBS
  
  # Create an empty list for the filtered data by TAG.
  # Each element in this list will include all the data and the confidence filter of the ATLAS data from each tag.
  listoffilteredData <- list()
  
  # Extract the unique values from the 'TAG' column of 'Data'
  tags <- unique(Data$TAG)
  
  # Run on 'tags'
  for (tagInd in 1:length(unique(Data$TAG)))
  {
    # Filter data for the current tag (find all the data which have the current tag number)
    tagData <- Data %>% dplyr::filter(TAG==tags[tagInd])
    
    # Calculate the unique time differences between consecutive time stamps in the 'tagData' data frame 
    # and converts these differences from milliseconds to seconds
    timediffs=unique(tagData$TIME[1:min(nrow(tagData),1e4)]-lag(tagData$TIME[1:min(nrow(tagData),1e4)]))/1000
    
    # Find the smallest unique time difference that is greater than 0.5 seconds
    minimalTimeDiff <- min(round(timediffs)[which(round(timediffs)>0.5)])
    
    # Calculate the threshold distance, which will later be used to determine if two localizations are "connected"
    if (is.na(conectedDist)) {
      conectedDist <- minimalTimeDiff*conectedVel
    }
    
    # Apply the function 'TrackConfidenceVec' from the file 'TrackConf.cpp'
    tagData$Conf <-TrackConfidenceVec(as.matrix(tagData %>% dplyr::select(X,Y,aBS,stdVarXY)),
                                      minNBSforConf2,minNBSforConf1,Nconf1forConf2,conectedDist,stdlim)
    
    # Store the filtered data in the previously created 'listoffilteredData'
    listoffilteredData[[tagInd]] <- tagData
  }
  
  # Combine the list of data frames 'listoffilteredData' into a single data frame called 'Data'
  Data <- do.call(rbind.data.frame,listoffilteredData)

  # Remove the column 'aBS' from 'Data' - WHY DID THEY CREATE THIS COLUMN AND NOT USED THE COLUMN 'NBS' IN 'TrackConfidenceVec'?
  Data <- Data %>% dplyr::select(-aBS)
  
  # Return 'Data'
  return(Data)
}


# A <- RawLoc0 %>% filter(TAG>0)
# B <- TrackConfidanceLevel(A,20,80)
# C <- TrackConfidenceLevelcpp(A,20,80)
# # A <- RawLoc0 %>% filter((TIME>1651800692452-20000)&(TIME<1651800692452+20000))%>% filter(TAG>0)
# # 
# # 
# atl_mapleaf4(B %>% filter(Conf>-1),
#              B %>% filter(Conf>1),
#              C%>% filter(Conf>-1),
#              C %>% filter(Conf>1))

# 
# 
# 
# cppFunction('NumericVector pdistC(double x, NumericVector ys) {
#   int n = ys.size();
#   NumericVector out(n);
# 
#   for(int i = 0; i < n; ++i) {
#     out[i] = sqrt(pow(ys[i] - x, 2.0));
#   }
#   return out;
# }')
# 
# TrackConfidanceLevel <- function(Data,conectedVel=20,stdlim=80)
# {
#   if (length(unique(Data$TAG))>1)
#   {stop("TrackConfidanceLevel can work only for a single tag data")}
#   Data <- Data %>% 
#     arrange(TIME) %>% 
#     mutate(stdVarXY=sqrt((VARX+VARY)/2+sqrt((VARX+VARY)^2/4+VARX*VARY-COVXY^2)))
#   if(!('Conf' %in% names(Data)))
#     Data$Conf=-1
#   Data$aBS <- Data$NBS
#   timediffs=unique(Data$TIME[1:min(nrow(Data),1e4)]-lag(Data$TIME[1:min(nrow(Data),1e4)]))/1000
#   minimalTimeDiff <- min(round(timediffs)[which(round(timediffs)>0.5)])
#   conectedDist <- minimalTimeDiff*conectedVel
#   IndC1 <- NA
#   IndC2 <- NA
#   for (Ind in 1:nrow(Data))
#   {
#     # if(Data$TIME[Ind]==1649870541423)
#     # {print(Ind)}
#     Conf=pointConfidanceLevel(Data,Ind,IndC1,IndC2,conectedDist,stdlim)
#     if (Conf>Data$Conf[Ind])
#     {Data$Conf[Ind]=Conf}
#     if (Conf>0) 
#     {IndC1=Ind}
#     if((Conf>1))
#     {IndC2=Ind}
#   }
#   
#   IndC1 <- NA
#   IndC2 <- NA
#   # Data$ConfBack=Data$Conf
#   for (Ind in nrow(Data):1)
#   {
#     # if(Data$TIME[Ind]==1649870541423)
#     # {print(Ind)}
#     Conf=pointConfidanceLevel(Data,Ind,IndC1,IndC2,conectedDist=160,stdlim=80)
#     if (Conf>Data$Conf[Ind])
#     {Data$Conf[Ind]=Conf}
#     if (Conf>0) 
#     {IndC1=Ind}
#     if((Conf>1))
#     {IndC2=Ind}
#   }
#   
#   
#   
#   return(Data)
#   
# }
# pointConfidanceLevel <- function(Data,Ind,IndC1,IndC2,conectedDist=160,stdlim=80)
# {
#   Conf=0
#   if (Data$aBS[Ind]>3)
#   {if (IdxDist(Data,Ind,IndC1)<conectedDist|IdxDist(Data,Ind,IndC2)<conectedDist)
#   {Conf <- 2} else if (Data$stdVarXY[Ind]<stdlim)
#   {Conf <- 1} 
#   } else if (Data$aBS[Ind]==3)
#   {if(IdxDist(Data,Ind,IndC2)<conectedDist)
#   {Conf <- 2} else if (IdxDist(Data,Ind,IndC1)<conectedDist)
#   {Conf <- 1}}
#   return(Conf) 
# }
# 
# x(i, j)
# as.matrix(RawLoc1 %>% select(X,Y,NBS,stdVarXY))
# for (int i = 0; i < nrow; i++) {
#   double total = 0;
#   for (int j = 0; j < ncol; j++) {
#     total += x(i, j);
#   }
# 
# cppFunction('double pointConfidanceLevel(NumericMatrix Data,int Ind, int IndC1, int IndC2,double conectedDist=160, double stdlim=80 ) {
#   double Conf=0;
#   if (Data(Ind,3)>3)  {
#       if (IdxDist(Data,Ind,IndC1)<conectedDist|IdxDist(Data,Ind,IndC2)<conectedDist) {
#           Conf = 2;
#           } 
#       else if (Data(Ind,4)<stdlim)   {
#           Conf = 1;
#           } 
#       } 
#   else if (Data(Ind,3)==3) {
#       if(IdxDist(Data,Ind,IndC2)<conectedDist) {
#           Conf = 2;
#           } 
#       else if (IdxDist(Data,Ind,IndC1)<conectedDist)   {
#           Conf = 1;
#           }
#       }
#   return(Conf); 
# 
# }')
# 
# # IdxDist <- function(Data,Ind1,Ind2)
# # {if (is.na(Ind2)) return(1e6)
# #   return(sqrt((Data$X[Ind1]-Data$X[Ind2])^2+(Data$Y[Ind1]-Data$Y[Ind2])^2))}
# 
# cppFunction('double IdxDist(NumericMatrix Data, int Ind1, int Ind2=-1) {
#   if (Ind2<0) {
#     return (1e6);
#     }
#   else   {
#     return(pow(pow(Data(Ind1,1)-Data(Ind2,1),2)+pow(Data(Ind1,2)-Data(Ind2,2),2),0.5));
#     }
# 
# }')
# 
# 
# cppFunction('int ccc(NumericMatrix Data) {
#   return(Data.nrow());
# 
# }')
# # double total = 0;
# # for(int i = 0; i < n; ++i) {
# #   total += x[i];
# # }
# # return total;
