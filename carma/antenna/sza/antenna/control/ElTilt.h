#ifndef ELTILT_H
#define ELTILT_H

/**
 * @file ElTilt.h
 * 
 * Tagged: Thu Nov 13 16:53:37 UTC 2003
 * 
 * @author Erik Leitch
 */
#include "carma/antenna/sza/antenna/control/AxisTilt.h"
#include "carma/antenna/sza/antenna/control/PointingCorrections.h"

namespace sza {
  namespace antenna {
    namespace control {
      
      
      /**
       * Define a class for handling Elevation tilts.
       */
      class ElTilt : public AxisTilt {
	
      public:
	
	/**
	 * Constructor trivially calls reset(), below.
	 */
	ElTilt();
	
	/**
	 * Reset internal data members.
	 */
	void reset();
	
	/**
	 * Install the tilt.
	 */
	void setTilt(double tilt);
	
	/**
	 * Correct the misalignment of the elevation axis.
	 *
	 * @param  f  PointingCorrections *  The az/el pointing to be corrected.
	 */
	void apply(PointingCorrections* f);
	
	/**
	 * Pack a tilt for archival in the register database.
	 */
	void packTilt(signed* s_elements);
	
      private:
	
	/**
	 * The tilt of the elevation axis perpendicular to the azimuth
	 * ring, measured clockwise around the direction of the
	 * azimuth std::vector (radians)
	 */
	double tilt_;      
	
	/**
	 * sin(tilt) 
	 */
	double sin_tilt_;  
	
	/**
	 * cos(tilt) 
	 */
	double cos_tilt_;  
	
      }; // End class ElTilt
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 
