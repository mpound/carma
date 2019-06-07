#ifndef COLLIMATION_H
#define COLLIMATION_H

/**
 * @file Collimation.h
 * 
 * Tagged: Thu Nov 13 16:53:35 UTC 2003
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/Angle.h"

#include "carma/antenna/sza/antenna/control/PointingCorrections.h"
#include "carma/antenna/sza/antenna/control/SkyOffset.h"

namespace sza {
  namespace antenna {
    namespace control {
      
      
      /**
       * Objects of the following type contain the collimation
       * components of the pointing model.
       */
      class Collimation : public SkyOffset {
	
      public:
	
	/**
	 * Constructor trivially calls reset() method, below.
	 */
	Collimation();
	
	/**
	 * Update the tilt associated with this collimation correction.
	 */
	void setXOffset(sza::util::Angle x);
	
	/**
	 * Update the azimuth associated with this collimation
	 * correction.
	 */
	void setYOffset(sza::util::Angle y);

	/**
	 * Increment the tilt associated with this collimation correction.
	 */
	void incrXOffset(sza::util::Angle x);
	
	/**
	 * Increment the azimuth associated with this collimation
	 * correction.
	 */
	void incrYOffset(sza::util::Angle y);

	bool isUsable();
	void setUsable(bool usable);
	void reset();

      private:
	
	bool usable_;

      }; // End class Collimation
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 
