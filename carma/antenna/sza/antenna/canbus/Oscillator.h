#ifndef OSCILLATOR_H
#define OSCILLATOR_H

#include <string>

#include "carma/szautil/Oscillator.h"
#include "carma/antenna/sza/antenna/canbus/CanDevice.h"

/**
 * @file Oscillator.h
 * 
 * Started: Mon Nov 24 17:18:55 UTC 2003
 * 
 * @author Erik Leitch
 */
namespace sza {
  namespace antenna {
    namespace canbus {
      
      class Oscillator : public CanDevice {
      public:
	
	/**
	 * Constructor.
	 */
	Oscillator(sza::antenna::control::SzaShare* share, 
		   std::string boardName,
		   carma::canbus::apiType api,
		   carma::canbus::nodeType node, 
		   carma::canbus::CanOutput& io);
	
	/**
	 * Destructor.
	 */
	virtual ~Oscillator();
	
      }; // End class Oscillator
      
    }; // End namespace canbus
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 


