#ifndef PMACTARGET_H
#define PMACTARGET_H

/**
 * @file PmacTarget.h
 * 
 * Tagged: Thu Nov 13 16:53:46 UTC 2003
 * 
 * @author Erik Leitch
 */

#include "carma/szautil/PmacMode.h"

#include "carma/szautil/Axis.h"
#include "carma/antenna/sza/antenna/control/PmacAxis.h"

namespace sza {
  namespace antenna {
    namespace control {
      
      /**
       * Tracker will access private members of this class.
       */
      class Tracker;
      
      class PmacTarget {
	
      public:
	
	/**
	 * Return a pointer to the appropriate axis
	 *
	 * @throws Exception
	 */
	sza::antenna::control::PmacAxis* 
	  PmacAxis(sza::util::Axis::Type axis);
	
	/**
	 * Return the current mode
	 */
	sza::util::PmacMode::Mode getMode();
	
	/**
	 * Set the current mode
	 */
	void setMode(sza::util::PmacMode::Mode mode);
	
	/**
	 * Pack the current mode for archival in the register
	 * database.
	 */
	void packMode(unsigned* u_elements);
	
	/**
	 * Pack the target encoder count for archival in the register
	 * database.
	 */
	void packCounts(signed* s_elements);
	
	/**
	 * Pack the target encoder rates for archival in the register
	 * database.
	 */
	void packRates(signed* s_elements);
	
      private:
	
	friend class Tracker;
	
	sza::util::PmacMode::Mode mode_;   // The pmac mode 
	
	// The encoder positions and rates of the azimuth drive

	sza::antenna::control::PmacAxis az_; 
	
	// The encoder positions and rates of the elevation drive

	sza::antenna::control::PmacAxis el_; 
	
	// The encoder positions and rates of the PA drive

	sza::antenna::control::PmacAxis pa_; 
	
      }; // End class PmacTarget
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 
