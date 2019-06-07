#ifndef REFRACTION_H
#define REFRACTION_H

/**
 * @file Refraction.h
 * 
 * Tagged: Thu Nov 13 16:53:50 UTC 2003
 * 
 * @author Erik Leitch
 */
#include "carma/antenna/sza/antenna/control/PointingCorrections.h"

// ICS include needed for rtomas

#include "carma/szaarrayutils/szaconst.h"

namespace sza {
  namespace antenna {
    namespace control {
      
      /**
       * Class to encapsulate refraction corrections
       *
       * The standard approximation for atmospheric refraction is:
       *
       *  vac = obs - (A.cot(obs) + B.cot(obs)^3)
       *
       * where vac is the elevation of the source as would be seen if
       * there were no atmosphere, and obs is the elevation after
       * refraction.  Updates of the A and B terms are periodically
       * received from the weather-station and recorded in an object
       * of the following type.
       */
      class Refraction {
	
      public:
	
	/**
	 * Enumerate valid refraction models
	 */
	enum Mode {
	  OPTICAL,
	  RADIO
	};
	
	/**
	 * Constructor
	 */
	Refraction();
	
	/**
	 * Reset internal data members to something sensible
	 */
	void reset();
	
	/**
	 * Method to query if this refraction correction is usable
	 */
	inline bool isUsable() {
	  return usable_;
	};
	
	/**
	 * Method to set the usable status of this correction
	 */
	inline void setUsable(bool usable) {
	  usable_ = usable;
	};
	
	/**
	 * Set the A coefficient of the above equation
	 */
	inline void setA(double a) {
	  a_ = a;
	}
	
	/**
	 * Return the A coefficient of the above equation
	 */
	inline double getA() {
	  return a_;
	}
	
	/**
	 * Set the B coefficient of the above equation
	 */
	inline void setB(double b) {
	  b_ = b;
	}
	
	/**
	 * Return the B coefficient of the above equation
	 */
	inline double getB() {
	  return b_;
	}
	
	/**
	 * Apply the refraction correction to the pointing corrections
	 */
	double apply(PointingCorrections* f);
	
	/**
	 * Method to pack our refraction coefficients in a format
	 * suitable for writing to the register database
	 */
	void pack(signed* s_elements);
	
      private:
	
	/**
	 * True once the A and B terms have been set 	  
	 */
	bool usable_; 
	
	/**
	 * The A term of the above equation (radians) 
	 */
	double a_;  
	
	/**
	 * The B term of the above equation (radians)   
	 */
	double b_;  
	
      }; // End class Refraction
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 
