// $Id: ModelReader.h,v 1.1 2010/12/13 21:06:31 eml Exp $

#ifndef SZA_MATLAB_MODELREADER_H
#define SZA_MATLAB_MODELREADER_H

/**
 * @file ModelReader.h
 * 
 * Tagged: Thu Sep  8 18:35:46 PDT 2005
 * 
 * @version: $Revision: 1.1 $, $Date: 2010/12/13 21:06:31 $
 * 
 * @author Erik Leitch
 */
#include <string>
#include<vector>

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

    class ModelReader {
    public:

      // Enumerate interpolation error codes

      enum {
	ERR_NONE         = 0x0,
	ERR_OUTSIDE_MJD  = 0x1,
	ERR_OUTSIDE_FREQ = 0x2,
      };

      static const double arcSecPerRad_;

      /**
       * Constructor.
       */
      ModelReader();
      ModelReader(std::string dir, std::string fileName);

      /**
       * Destructor.
       */
      virtual ~ModelReader();

      void readFile(std::string dir, std::string fileName);

      Temperature brightnessTemperature(TimeVal& mjd, Frequency& freq, 
					unsigned int& errCode);

      SolidAngle solidAngle(TimeVal& mjd, unsigned int& errCode);

      Angle eDiam(TimeVal& mjd, unsigned int& errCode);

      Angle pDiam(TimeVal& mjd, unsigned int& errCode);

      Flux flux(TimeVal& mjd, Frequency& freq, unsigned int& errCode);

      void readRecord(InputStream* stream);
      void readItem(InputStream* stream);

    public:

      std::vector<double> mjd_;
      std::vector<double> t26_;
      std::vector<double> t31_;
      std::vector<double> t36_;
      std::vector<double> eDiam_;
      std::vector<double> pDiam_;

      sza::util::QuadraticInterpolatorNormal t26Interp_;
      sza::util::QuadraticInterpolatorNormal t31Interp_;
      sza::util::QuadraticInterpolatorNormal t36Interp_;
      sza::util::QuadraticInterpolatorNormal eDiamInterp_;
      sza::util::QuadraticInterpolatorNormal pDiamInterp_;

    public:
      void fillInterpContainers(double mjd);

    }; // End class ModelReader

  } // End namespace matlab
} // End namespace sza



#endif // End #ifndef SZA_MATLAB_MODELREADER_H
