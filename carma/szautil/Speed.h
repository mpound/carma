#ifndef SZA_UTIL_SPEED_H
#define SZA_UTIL_SPEED_H

/**
 * @file Speed.h
 * 
 * Tagged: Wed Dec  1 23:39:12 PST 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/ConformableQuantity.h"

namespace sza {
  namespace util {
    
    class Speed : public ConformableQuantity {
    public:
      
      class CentimetersPerSec {};
      class KilometersPerSec {};
      class MetersPerSec {};

      /**
       * Constructor.
       */
      Speed();
      Speed(const CentimetersPerSec& units, double cmPerSec);
      Speed(const MetersPerSec& units, double mPerSec);
      
      /**
       * Destructor.
       */
      virtual ~Speed();
      
      /**
       * Set a speed
       */
      void setCentimetersPerSec(double cmPerSec);
      void setMetersPerSec(double mPerSec);
      void setMilesPerHour(double mph);

      /**
       * Get a speed
       */
      double centimetersPerSec();
      double metersPerSec();
      double mph();

      void initialize();

      static const unsigned secPerHour_;
      static const double metersPerMile_;

    private:

      double cmPerSec_;

    }; // End class Speed
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_SPEED_H
