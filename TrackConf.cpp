
//sum.cpp
#include <Rcpp.h>
using namespace Rcpp;

// // [[Rcpp::export]]


double IdxDist(NumericMatrix Data, int Ind1, int Ind2) {
  
  // Calculate the Euclidean distance between the point with 'Ind1' and the point 'Ind2' in the matrix 'Data'
  if (Ind2<0) {
    return (1e6);
    }
  else   {
    return(pow(pow(Data(Ind1,0)-Data(Ind2,0),2)+pow(Data(Ind1,1)-Data(Ind2,1),2),0.5));
    }

}

// // [[Rcpp::export]]


double pointConfidanceLevel(NumericMatrix Data,int Ind, int IndC1,int IndC2,int minNBSforConf2,int minNBSforConf1,
                            int Nconf1forConf2,int CountC1,double conectedDist=160,double stdlim=80) {
  // Calculate the confidence filter of a single location point
  
  // Arguments:
  //
  //  'Data': the entire dataset, which includes all the location points
  //
  //  'ind': the index of the location point for which the confidence level should be evaluated
  //
  //  'IndC1' and 'IndC2': indices of the last points in the 'Data', which were obtained confidence levels of "1" and "2", respectively
  //
  //  'minNBSforConf2' and 'minNBSforConf1': the minimal allowed numbers of Base Stations, 
  //      that participated in the localizations of points with level of confidence "1" and "2", respectively.
  //
  //  'Nconf1forConf2': the minimal number of connected points with confidence level "1", 
  //      which is used as a threshold to assess if the observed localization point should obtain a confidence level of "2"
  //
  //  'CountC1': number of connected points with a confidence level of "1"
  //
  //  'conectedDist':  threshold distance used to determine if two analyzed localizations are 'connected'
  //
  //  'stdlim': the maximal allowed value of 'stdVARXY' value
  
  // Initialize 'Conf'
  int Conf=0;
  
  // If more than 'minNBSforConf2' Base Stations participated in the localization of the current point, assign to it confidence_level=2
  if (Data(Ind,2)>=minNBSforConf2) {
    Conf =2;
    }
  
  // Else, if more than 'minNBSforConf1' Base Stations participated in the localization of the point, further assess its confidence level
  else if (Data(Ind,2)>=minNBSforConf1) {
    
    // If: 
    // 1. The Euclidean distance between the current point, and the previous point in the dataset, 
    // which was assiged with a confidence level of "1", is smaller than 'conectedDist'
    // 2. The number of "connected" points with confidence level of "1" is equal or larger than the threshold 'Nconf1forConf2',
    //
    // Set the confidence level to "2"  
    if ((IdxDist(Data,Ind,IndC1)<conectedDist)&(CountC1>=Nconf1forConf2)) {
      Conf = 2;
    }
    
    // If the Euclidean distance between the current point, and the previous point in the dataset, 
    // which was assigned with a confidence level of "2", is smaller than 'conectedDist', 
    // set the point's confidence level to "2"
    if (IdxDist(Data,Ind,IndC2)<conectedDist) {
      Conf = 2;
      }
    
    // If the STD of the current point is smaller than the threshold 'stdlim', 
    // and the variable 'Conf' is smaller than 2, 
    //set the confidence level to the point to "1"
    if ((Data(Ind,3)<stdlim)&(Conf<2)) { 
      //stdVarXY <stdlim
      Conf = 1;
      } 
    
    } 
  
  // Another 'else': if more than three base stations participated in the localization of the current point
  else if (Data(Ind,2)==3) { //NBS==3
    
    // If the Euclidean distance between the current point, and the previous point in the data set, 
    // which was assigned with a confidence level of "2", is smaller than 'conectedDist', 
    // set the point's confidence level to "2"
    if(IdxDist(Data,Ind,IndC2)<conectedDist) {
      Conf = 2;
      } 
    
    // Else: if the Euclidean distance between the current point, 
    // and the previous point in the dataset, which was assiged with a confidence level of "1", 
    // is smaller than 'conectedDist', 
    // Set the confidence level to "1" 
    else if (IdxDist(Data,Ind,IndC1)<conectedDist)   {
      Conf = 1;
      }
    
    }
  
  // Rprintf("index=%i,Conf=%i\n",Ind,Conf);
  
  // Return the confidence level of the location point
  return(Conf); 
  
}

// [[Rcpp::export]]

NumericVector TrackConfidanceVec(NumericMatrix Data,int minNBSforConf2,int minNBSforConf1,int Nconf1forConf2,
                                   double conectedDist=20,double stdlim=80) {
  // evaluate the confidence level of each localisation point in the data set
  //
  // Arguments:
  //
  //  'Data': the dataset, which contains the localizations for which the confidence level should be evaluated
  //
  //  'minNBSforConf1' and 'minNBSforConf2': the minimal allowed numbers of Base Stations participated in
  //    the evaluation of the localization of the observed data point, 
  //    used for the assessment of confidence level 1 and 2, respectively
  //
  //  'Nconf1forConf2': the minimal number of connected points with confidence level "1", 
  //    which is used as a threshold to assess if the observed localization point should obtain a confidence level of "2".
  //
  //  'conectedDist': the threshold distance used to determine if two analyzed localizations are 'connected'(*).
  //
  //  'stdlim': the maximal allowed value of 'stdVARXY' value
  //
  //  'minimalTimeDifference': the minimal time-difference between sampling points within the dataset
  //
  //  (*) A localization point is considered 'connected' if its' distance from another confident point,
  //    distance= 'conectedVel' * 'minimalTimeDifference', is smaller than the threshold 'conectedDist'.
  
  // Define and initialize variables
  int IndC1 = -1;
  int IndC2 = -1;
  int nRows= Data.nrow();
  int Conf;
  int CountC1=0;
  // Initialize a vector with the confidence level of each location point
  // Each element in a NumericVector type is initialized to zero by definition
  NumericVector ConfVec(nRows);
  
  // Run from zero to 'nrows': left to right
  for (int Ind=0;Ind<nRows;Ind++) {
    
    // Apply the 'pointConfidanceLevel' function to evaluate the confidence level
    Conf=pointConfidanceLevel(Data,Ind,IndC1,IndC2,minNBSforConf2,minNBSforConf1,Nconf1forConf2,CountC1,conectedDist,stdlim);
    
    // If the confidence level of the current point is "1" or "2", update it in 'confVec'
    if (Conf>ConfVec(Ind)) {
      ConfVec(Ind)=Conf;
      }
    
    // If the confidence level of the current point is "1", set IndC1=Ind, and add 1 to 'CountC1'
    if (Conf==1) {
      IndC1=Ind;
      CountC1=CountC1+1;
      }
    
    // If the confidence level of the current point is "2", set IndC1=Ind, and set CountC1=0
    if (Conf==2) {
      IndC2=Ind;
      CountC1=0;
      }
    
    }
  
  // Reset 'IndC1' and 'IndC2'
  IndC1 = -1;
  IndC2 = -1;
  
  // Run from 'nrows' to zero: right to left in order to compare the confidence of each point to the points in front of it
  for (int Ind=nRows-1;Ind>=0;Ind--) {
    
    // Apply the 'pointConfidanceLevel' function to evaluate the confidence level
    Conf=pointConfidanceLevel(Data,Ind,IndC1,IndC2,minNBSforConf2,minNBSforConf1,Nconf1forConf2,CountC1, conectedDist,stdlim);
    
    // If the confidence level of the current point is "1" or "2", update it in 'confVec'
    if (Conf>ConfVec(Ind)) {
      ConfVec(Ind)=Conf;
      }
    
    // If the confidence level of the current point is "1", set IndC1=Ind, and add 1 to 'CountC1'
    if (Conf==1) {
      IndC1=Ind;
      CountC1=CountC1+1;
      }
    
    // If the confidence level of the current point is "2", set IndC1=Ind, and set CountC1=0
    if (Conf==2) {
      IndC2=Ind;
      CountC1=0;
      }
    }
  
  // Return the confidence vector of all the points in 'Data'
  return(ConfVec);
          
}
