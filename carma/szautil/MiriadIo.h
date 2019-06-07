// $Id: MiriadIo.h,v 1.1 2010/12/13 21:06:30 eml Exp $

#ifndef SZA_UTIL_MIRIADIO_H
#define SZA_UTIL_MIRIADIO_H

/**
 * @file MiriadIo.h
 * 
 * Tagged: Mon Oct  3 15:31:19 PDT 2005
 * 
 * @version: $Revision: 1.1 $, $Date: 2010/12/13 21:06:30 $
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/Angle.h"
#include "carma/szautil/Pressure.h"
#include "carma/szautil/Speed.h"
#include "carma/szautil/Temperature.h"
#include "carma/szautil/VisIo.h"

namespace sza {
  namespace util {

    class MiriadIo : public VisIo {
    public:

      /**
       * Constructor.
       */
      MiriadIo();

      /**
       * Destructor.
       */
      virtual ~MiriadIo();

      // Versions of the base-class routines which handle Miriad files

      void openFile(std::string name, std::string openMode);
      void closeFile();
      void writeFile(double* data, double* date, double* uvw, double* rms);
      void writeFakeFile(double* data, double* date, double* uvw, double* rms);

      // Method to write a single frame of data, including uv
      // variables written by CARMA
      
      // Write antenna pointing information

      void writeAntennaPointing();
      
      // Write weather parameters

      void writeWeatherParameters();
      void writeTimeParameters(unsigned iFrame);
      void writeCarmaFormatData(unsigned iFrame);

      void resetVisStats();
      void reportVisStats();

      void setVersion(std::string version);

    private:
      
      std::string version_;

      // A UV handle used by Miriad writing routines

      int uvh_;

      unsigned nCarma_;

      unsigned iFirstGoodChannel_;
      unsigned nGoodChannel_;

      void writeFixedParameters();
      void writeVisibilityData(double* data, double* date, double* uvw, 
			       double* rms);

      void writeVisibilityData(unsigned iFrame);
      void writeWidebandVisibilityData(unsigned iFrame, unsigned iBaseline);
      void writeSpectralVisibilityData(unsigned iFrame, unsigned iBaseline);

      void writeFakeVisibilityData(double* data, double* date, double* uvw, 
				   double* rms);

      void writeSourceParameters();
      void writeSiteParameters();
      void writeAntennaParameters();
      void writeArrayParameters();
      void writeSysTemps(double* rmsInJy, unsigned iFrame);

      bool haveVisWideData();
      bool haveVisSpecData();

      unsigned nVis_;
      unsigned goodData_;
      unsigned badData_;
      unsigned badFlagData_;
      unsigned badWtData_;

    }; // End class MiriadIo

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_MIRIADIO_H
