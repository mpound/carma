
#include <math.h>
#include <fstream>

#include "carma/util/Trace.h"
#include "carma/tipper/TipperDataSet.h"

using namespace carma::util;
using namespace carma::tipper;

Reading::Reading(){}


TipperDataSet::TipperDataSet():numReadings(0), reading(0){}

// This adds a full reading (all measured parameters)    
int TipperDataSet::addReading(Reading& r)
{
   if (numReadings == 0) {
      seq = r.seq;
      mjd = r.mjd;
   }  
   if (seq != r.seq)
   {
     CPTRACE( Trace::TRACE5, "     Sequence number of tip("<<r.seq<<
            ") doesn't match seq for new reading("<<r.seq<<")");
      return 0;
   }        

   r.airMass = 1/cos(r.zenAngle*3.14159/180); // airmass = secant(zenAngle);      
   const double absZero = 273.15;               // 0 C in Kelvin
   r.Tc = r.refTemp + absZero;
   r.Th = r.hotTemp + absZero;
   r.G  = r.hotMinusRef/(r.Th - r.Tc);
   r.Ta = r.ambTemp + absZero;
     
   Reading* rtmp = new Reading[numReadings+1];
   for(int i=0; i<numReadings;i++)rtmp[i] = reading[i];
   rtmp[numReadings] = r;
   if (reading)delete [] reading;
   reading = rtmp;
   numReadings++;
   return numReadings;
} 

int     TipperDataSet::getSeq(){                   return seq;}
double  TipperDataSet::getMjd(){                   return mjd;}
int     TipperDataSet::getNumReadings(){           return numReadings;}
Reading TipperDataSet::getReading(int index){  return reading[index]; }  
TipperDataSet::~TipperDataSet(){ if (reading)delete [] reading; }


// This adds only items required for a tip reduction (tau calc)
int TipperDataSet::addReduction(int seqin,
    double zenAngle, double _mjd,
    double aveSigMinusRef, double rmsSigMinusRef, double hotMinusRef,
    double refTemp, double ambTemp, double hotTemp)
{
  Reading in;

  in.seq            = seqin;
  in.mjd            = _mjd;
  in.zenAngle       = zenAngle;
  in.aveSigMinusRef = aveSigMinusRef;
  in.rmsSigMinusRef = rmsSigMinusRef;
  in.hotMinusRef    = hotMinusRef;
  in.refTemp        = refTemp;
  in.ambTemp        = ambTemp;
  in.hotTemp        = hotTemp;

  return addReading(in);
}

