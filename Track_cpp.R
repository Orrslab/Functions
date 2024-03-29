#http://adv-r.had.co.nz/Rcpp.html
#install.packages('Rcpp')
require('Rcpp')
# knitr:::input_dir()
print(' the file Track_cpp.R runs an essential sourceCpp commnad,
        if your current directory (getwd()) does not contain a functions/TrackConf.cpp file
        you can source it beforehand and inhibit the command form the file')
sourceCpp('functions/TrackConf.cpp')

# IdxDist(as.matrix(RawLoc1 %>% select(X,Y)),0,1)
# pointConfidanceLevel(as.matrix(RawLoc1 %>% dplyr::select(X,Y,NBS,stdVarXY)),0,0,0)
# TrackConfidanceVec(as.matrix(A %>% dplyr::select(X,Y,NBS,stdVarXY)),160,80)

TrackConfidanceLevelcpp <- function(Data,conectedVel=20,conectedDist=NA,stdlim=80,minNBSforConf2=7,
                                    minNBSforConf1=4,Nconf1forConf2=5)
{
  # the function is a wrapper to a cpp code (TrackConfidanceVec) saved in file "TrackConf.cpp"
  # that calculates the confidence of any point in a track without discarding it
  # it runs a loop on all data points per tag and gives higher confidence mark (in the range [0,1,2] )
  # to points which have large NBS or are close to confidant points
 
  
  
  if (!all(c('TAG','X','Y','NBS','TIME') %in% names(Data)))
  {stop("TrackConfidanceLevel needs the variables TAG,X,Y,NBS, and TIME (in miliseconds) to run ")}
  
  if (!('val2' %in% names(Data)))
  {
    if (!all(c('VARX','VARY','COVXY') %in% names(Data)))
    {stop("TrackConfidanceLevel needs either val2 or VARX,VARY,COVXY to run")}
    print("TrackConfidanceLevel calculates stdVarXY as greater eigenvalue")
    Data <- Data %>% mutate(val2= sqrt(((VARX+VARY)+sqrt(VARX^2+VARY^2-2*VARX*VARY+4*COVXY^2))/2)) # greater eigenvalue
  }
  
  Data <- Data %>%  arrange(TIME)
  
  if(!('Conf' %in% names(Data)))
    {Data$Conf=-1}
  Data$aBS <- Data$NBS
  
  listoffilteredData <- list()
  tags <- unique(Data$TAG)
  for (tagInd in 1:length(unique(Data$TAG)))
  {
  tagData <- Data %>% dplyr::filter(TAG==tags[tagInd])
  timediffs=unique(tagData$TIME[1:min(nrow(tagData),1e4)]-lag(tagData$TIME[1:min(nrow(tagData),1e4)]))/1000
  numinalTimeDiff <- min(round(timediffs)[which(round(timediffs)>0.5)])
  if (is.na(conectedDist))
      {conectedDist <- numinalTimeDiff*conectedVel}
  tagData$Conf <-TrackConfidanceVec(as.matrix(tagData %>% dplyr::select(X,Y,aBS,stdVarXY)),
                                    minNBSforConf2,minNBSforConf1,Nconf1forConf2,conectedDist,stdlim)
  listoffilteredData[[tagInd]] <- tagData
  }
  Data <- do.call(rbind.data.frame,listoffilteredData)

  Data <- Data %>% dplyr::select(-aBS)
  return(Data)
}
# A <- RawLoc0 %>% filter(TAG>0)
# B <- TrackConfidanceLevel(A,20,80)
# C <- TrackConfidanceLevelcpp(A,20,80)
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
#   numinalTimeDiff <- min(round(timediffs)[which(round(timediffs)>0.5)])
#   conectedDist <- numinalTimeDiff*conectedVel
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
