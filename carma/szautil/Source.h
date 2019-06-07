#ifndef SZA_UTIL_SOURCE_H
#define SZA_UTIL_SOURCE_H

/**
 * @file Source.h
 * 
 * Tagged: Thu Nov 13 16:53:53 UTC 2003
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/Axis.h"
#include "carma/szautil/Angle.h"
#include "carma/szautil/DecAngle.h"
#include "carma/szautil/HourAngle.h"
#include "carma/szautil/QuadraticInterpolatorNormal.h"
#include "carma/szautil/QuadraticInterpolatorPositiveAngle.h"
#include "carma/szautil/QuadraticInterpolatorSignedAngle.h"

// Needed for SRC_LEN

#include "carma/szaarrayutils/szaregs.h"
#include "carma/szaarrayutils/source.h"

namespace sza {
  namespace util {
    
    /**
     * The following class is used to record details about the
     * current source trajectory.
     */
    class Source {
      
    public:
      
      /**
       * Constructor function
       *
       * @throws  Exception
       */
      Source();
      
      /**
       * Destructor
       */
      ~Source();
      
      /**
       * Reset this object's data
       */
      void reset();
      
      /**
       * Set the name of this source
       */
      void setName(char* name);
      
      /**
       * Set axis values for this source
       */
      void setAxis(sza::util::Axis::Type axisType, Angle az, Angle el, 
		   Angle pa);

      /**
       * Return the name of this source	  
       */
      char* getName();
      
      /**
       * Extend the track of this source
       *
       * @throws Exception
       */
      void extend(double mjd, HourAngle ra, DecAngle dec, double dist);
      
      /**
       * Return the Az of this source
       */
      Angle getAz();
      
      /**
       * Return the El of this source
       */
      Angle getEl();

      /**
       * Return the interpolated ra of this source.
       */
      HourAngle getRa(double tt);
      
      /**
       * Return the interpolated dec of this source.
       */
      DecAngle getDec(double tt);
      
      /**
       * Return the interpolated distance of this source.
       */
      double getDist(double tt);
      
      /**
       * Return the gradient for the RA.
       */
      HourAngle getGradRa(double tt);
      
      /**
       * Return the gradient for the DEC.
       */
      DecAngle getGradDec(double tt);
      
      /**
       * Set the type of source
       */
      inline void setType(sza::array::SourceType type) {
	type_ = type;
      }

      /**
       * Get the type of this source
       */
      inline sza::array::SourceType getType() {
	return type_;
      }

      /**
       * True if this source is a J2000 source
       */
      bool isJ2000();

      /**
       * True if this source is an ephemeris source
       */
      bool isEphem();

      /**
       * True if this source is an RaDec source
       */
      bool isRaDec();

      /**
       * True if this source is an Az/El source
       */
      bool isAzEl();

      /**
       * True if we can calculate source parameters for this timestamp
       */
      bool canBracket(double mjd);

    private:
      
      /**
       * The type of this source.
       */
      sza::array::SourceType type_;

      /**
       * The name of the source
       */
      char name_[SRC_LEN];    
      
      /**
       * The interpolated apparent Right Ascension
       */
      QuadraticInterpolator* ra_;          
      
      /**
       * The interpolated apparent Declination
       */
      QuadraticInterpolator* dec_;         
      
      /**
       * The interpolated geocentric distance
       */
      QuadraticInterpolator* dist_;        
      
      /**
       * Fixed source positions
       */
      Angle az_;
      Angle el_;
      Angle pa_;

    }; // End class Source
    
  }; // End namespace util
}; // End namespace sza

#endif // End #ifndef 
