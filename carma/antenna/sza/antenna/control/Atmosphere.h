#ifndef ATMOSPHERE_H
#define ATMOSPHERE_H

/**
 * @file Atmosphere.h
 * 
 * Tagged: Thu Nov 13 16:53:32 UTC 2003
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/PointingMode.h"
#include "carma/antenna/sza/antenna/control/Refraction.h"

namespace sza {
  namespace antenna {
    namespace control {
      
      
      /**
       * Class to encapsulate any atmospheric (refraction) corrections
       * to the antenna pointing model
       */
      class Atmosphere {
	
      public:
	
	/**
	 * Constructor
	 */
	Atmosphere();
	
	/**
	 * Return a pointer to the requested refraction container
	 *
	 * @throws Exception
	 */
	sza::antenna::control::Refraction* 
	  Refraction(sza::util::PointingMode::Type mode);
	
	/**
	 * Return true if the passed argument is the current
	 * refraction model
	 */
	bool isCurrent(sza::antenna::control::Refraction* r);
	
	/**
	 * Set the requested mode (optical | radio) to be the current
	 * refraction model
	 */
	void setCurrentRefraction(sza::util::PointingMode::Type mode);
	
	/**
	 * Get the current refraction
	 */
	sza::antenna::control::Refraction* currentRefraction();
	
	/**
	 * Apply the refraction correction to the current pointing
	 * corrections
	 */
	double applyRefraction(PointingCorrections* f);
	
	/**
	 * Restore the data members this object is managing to an
	 * intialization state
	 */
	void reset();
	
      private:
	
	/**
	 * The Radio refraction model
	 */
	sza::antenna::control::Refraction radio_;
	
	/**
	 * The Optical refraction model
	 */
	sza::antenna::control::Refraction optical_;
	
	/**
	 * This will point to the refraction model currently in use
	 */
	sza::antenna::control::Refraction* currentRefraction_;
	
      }; // End class Atmosphere
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 
