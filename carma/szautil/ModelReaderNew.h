// $Id: ModelReaderNew.h,v 1.1 2010/12/13 21:06:31 eml Exp $

#ifndef SZA_MATLAB_MODELREADERNEW_H
#define SZA_MATLAB_MODELREADERNEW_H

/**
 * @file ModelReaderNew.h
 * 
 * Tagged: Thu Sep  8 18:35:46 PDT 2005
 * 
 * @version: $Revision: 1.1 $, $Date: 2010/12/13 21:06:31 $
 * 
 * @author Erik Leitch
 */
#include <string>
#include <vector>

#include "carma/szautil/Angle.h"
#include "carma/szautil/Flux.h"
#include "carma/szautil/Frequency.h"
#include "carma/szautil/QuadraticInterpolatorNormal.h"
#include "carma/szautil/SolidAngle.h"
#include "carma/szautil/TimeVal.h"
#include "carma/szautil/Temperature.h"

class InputStream;

namespace sza {
  namespace util {

    class ModelReaderNew {
    public:

      // Enumerate interpolation error codes

      enum {
	ERR_NONE         = 0x0,
	ERR_OUTSIDE_MJD  = 0x1,
	ERR_OUTSIDE_FREQ = 0x2,
	ERR_NO_DATA      = 0x4
      };

      static const double arcSecPerRad_;

      /**
       * Constructor.
       */
      ModelReaderNew();
      ModelReaderNew(std::string dir, std::string fileName, std::vector<Frequency> freqs);
      void initialize(std::vector<Frequency> freqs);
      
      /**
       * Destructor.
       */
      virtual ~ModelReaderNew();

      void readFile(std::string dir, std::string fileName);

      /**
       * Find the starting index in the array of frequencies corresponding
       * to this frequency
       */
      void findMjdIndices(double mjd, unsigned& iStart, unsigned& iStop);
      void findFreqIndices(Frequency& freq, unsigned& iStart, unsigned& iStop);

      Temperature brightnessTemperature(TimeVal& mjd, Frequency& freq, 
					unsigned int& errCode);

      SolidAngle solidAngle(TimeVal& mjd, unsigned int& errCode);

      Angle eDiam(TimeVal& mjd, unsigned int& errCode);

      Angle pDiam(TimeVal& mjd, unsigned int& errCode);

      Flux flux(TimeVal& mjd, Frequency& freq, unsigned int& errCode);

      virtual void readRecord(InputStream* stream);
      virtual void readItem(InputStream* stream);

    public:

      std::vector<Frequency> freqs_;
      unsigned nFreq_;

      std::vector<double> mjd_;
      std::vector<double> eDiam_;
      std::vector<double> pDiam_;
      std::vector<std::vector<double> > t_;

      std::vector<sza::util::QuadraticInterpolatorNormal> tInterp_;
      sza::util::QuadraticInterpolatorNormal eDiamInterp_;
      sza::util::QuadraticInterpolatorNormal pDiamInterp_;

      sza::util::QuadraticInterpolatorNormal valInterp_;

    public:

      void fillInterpolationContainers(TimeVal& time, Frequency& freq);
      void fillInterpolationContainers(TimeVal& time);

    }; // End class ModelReaderNew

  } // End namespace matlab
} // End namespace sza



#endif // End #ifndef SZA_MATLAB_MODELREADERNEW_H
