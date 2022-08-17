
//sum.cpp
#include <Rcpp.h>
using namespace Rcpp;

// // [[Rcpp::export]]

double IdxDist(NumericMatrix Data, int Ind1, int Ind2) {
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
  // Rprintf("inside pointConfidanceLevel");
  // Rprintf(" NBS=%0.0f,countC1=%i,minNBSforConf2=%i,minNBSforConf1=%i\n",Data(Ind,2),CountC1,minNBSforConf2,minNBSforConf1);
  int Conf=0;
  if (Data(Ind,2)>=minNBSforConf2) 
      {Conf =2;}
  else if (Data(Ind,2)>=minNBSforConf1)
      {
       // Rprintf("0.NBS=%0.0f, \n",Data(Ind,2));
      if ((IdxDist(Data,Ind,IndC1)<conectedDist)&(CountC1>=Nconf1forConf2)) {
          Conf = 2;
         // Rprintf("1. index=%i,IndC1=%i,IndC2=%i,Conf=%i,NBS=%0.0f,std=%0.0f,d1=%3.0f,d2=%3.0f \n",Ind,IndC1,IndC2,Conf,Data(Ind,2),Data(Ind,3),IdxDist(Data,Ind,IndC1),IdxDist(Data,Ind,IndC2));
          } 
      if (IdxDist(Data,Ind,IndC2)<conectedDist) {
        Conf = 2;
         // Rprintf("2. index=%i,IndC1=%i,IndC2=%i,Conf=%i,NBS=%0.0f,std=%0.0f,d1=%3.0f,d2=%3.0f \n",Ind,IndC1,IndC2,Conf,Data(Ind,2),Data(Ind,3),IdxDist(Data,Ind,IndC1),IdxDist(Data,Ind,IndC2));
      } 
      if ((Data(Ind,3)<stdlim)&(Conf<2))   { //stdVarXY <stdlim
          Conf = 1;
         // Rprintf("3. index=%i,IndC1=%i,IndC2=%i,Conf=%i,NBS=%0.0f,std=%3.0f,d1=%3.0f,d2=%3.0f \n",Ind,IndC1,IndC2,Conf,Data(Ind,2),Data(Ind,3),IdxDist(Data,Ind,IndC1),IdxDist(Data,Ind,IndC2));
          } 
      } 
  else if (Data(Ind,2)==3) { //NBS==3
      if(IdxDist(Data,Ind,IndC2)<conectedDist) {
          Conf = 2;
        // Rprintf("4. index=%i,IndC1=%i,IndC2=%i,Conf=%i,NBS=%0.0f,std=%3.0f,d1=%3.0f,d2=%3.0f \n",Ind,IndC1,IndC2,Conf,Data(Ind,2),Data(Ind,3),IdxDist(Data,Ind,IndC1),IdxDist(Data,Ind,IndC2));
          } 
      else if (IdxDist(Data,Ind,IndC1)<conectedDist)   {
          Conf = 1;
        // Rprintf("5. index=%i,IndC1=%i,IndC2=%i,Conf=%i,NBS=%0.0f,std=%3.0f,d1=%3.0f,d2=%3.0f \n",Ind,IndC1,IndC2,Conf,Data(Ind,2),Data(Ind,3),IdxDist(Data,Ind,IndC1),IdxDist(Data,Ind,IndC2));
          }
      }
  
  // Rprintf("index=%i,Conf=%i\n",Ind,Conf);
    return(Conf); 
  

}

// [[Rcpp::export]]

NumericVector TrackConfidanceVec(NumericMatrix Data,int minNBSforConf2,int minNBSforConf1,int Nconf1forConf2,
                                   double conectedDist=20,double stdlim=80) {
      int IndC1 = -1;
      int IndC2 = -1;
      int nRows= Data.nrow();
      int Conf;
      int CountC1=0;
      NumericVector ConfVec(nRows);
      for (int Ind=0;Ind<nRows;Ind++)      {
        Conf=pointConfidanceLevel(Data,Ind,IndC1,IndC2,minNBSforConf2,minNBSforConf1,Nconf1forConf2,CountC1,
                                    conectedDist,stdlim);
       // Rprintf("out index=%i,Conf=%i\n",Ind,Conf);
        if (Conf>ConfVec(Ind)){
            ConfVec(Ind)=Conf;
            }
        if (Conf==1) 
        {IndC1=Ind;
          CountC1=CountC1+1;}
        if (Conf==2)
        {IndC2=Ind;
          CountC1=0;}

      }
      
      IndC1 = -1;
        IndC2 = -1;
        for (int Ind=nRows-1;Ind>=0;Ind--)      {
          Conf=pointConfidanceLevel(Data,Ind,IndC1,IndC2,minNBSforConf2,minNBSforConf1,Nconf1forConf2,CountC1,
                                    conectedDist,stdlim);
          if (Conf>ConfVec(Ind)){
            ConfVec(Ind)=Conf;
          }
          if (Conf==1) 
          {IndC1=Ind;
            CountC1=CountC1+1;}
          if (Conf==2)
          {IndC2=Ind;
            CountC1=0;}
        }
        
        
        return(ConfVec);
          
}