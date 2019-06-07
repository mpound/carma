#ifndef SZA_UTIL_CONSTANTS_H
#define SZA_UTIL_CONSTANTS_H

/**
 * @file Constants.h
 * 
 * Tagged: Tue Aug 10 13:17:59 PDT 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/Length.h"
#include "carma/szautil/Speed.h"
#include "carma/szautil/Temperature.h"

namespace sza {
  namespace util {
    
    class Constants {
    public:
      
      static Temperature Tcmb_;
      static const double hPlanckCgs_;
      static const double kBoltzCgs_;
      static const double JyPerCgs_;
      static const double sigmaTCgs_;
      static const double electronMassCgs_;
      static const double protonMassCgs_;
	
      static Speed lightSpeed_;
      static Length au_;
      static Length defaultEarthRadius_;

      /**
       * Constructor.
       */
      Constants();
      
      /**
       * Destructor.
       */
      virtual ~Constants();
      
      virtual double cgs() {
	return cgs_;
      }

      virtual double si() {
	return si_;
      }

    protected:

      void setGgs(double cgs) {
	cgs = cgs_;
      }

      void setSi(double si) {
	si = si_;
      }
      
    private:

      double cgs_;
      double si_;

    }; // End class Constants
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_CONSTANTS_H
