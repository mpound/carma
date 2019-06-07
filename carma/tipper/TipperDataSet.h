#ifndef _TIPPERDATASET_H
#define _TIPPERDATASET_H

namespace carma
{
  namespace tipper
  {

    // All parameters of a single reading (line) of a tip
    class Reading{
      public:
	Reading(); 
	int    seq;   
	int    index;
	double zenAngle;
	double mjd;
	double aveSigMinusRef;
	double rmsSigMinusRef;
	double sigMinusRef;
	double hotMinusRef;
	double refTotalPower;
	double refTemp;
	double hotTemp;
	double ambTemp;
	double chassisTemp;
	double mixerCurrent;
	double triplerCurrent;
	double gunnCurrent;
	double batteryVoltage;
	double supplyCurrent;
	// Following are not in raw data; calculated on load 
	double airMass;  
	double Ta;
	double Tc;
	double Th;
	double G;
    };   

    // A full tip consisting of multiple readings
    class TipperDataSet
    {
      public:
	TipperDataSet();
	~TipperDataSet();
	int  addReading(Reading& r);
	int  addReduction(int seq, 
	    double zenAngle, double mjd, 
	    double aveSigMinusRef, double rmsSigMinusRef, double hotMinusRef, 
	    double refTemp, double ambTemp, double hotTemp);
	int     getSeq();
	double  getMjd();
	int     getNumReadings();
	Reading getReading(int index);
      private:
	int      seq;
	double   mjd;
	int      numReadings;
	Reading* reading;
    };

  } // namespace tipper
} // namespace carma

#endif // _TIPPERDATASET_H
