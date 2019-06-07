#ifndef PMACMODE_H
#define PMACMODE_H

/**
 * @file PmacMode.h
 * 
 * Tagged: Thu Nov 13 16:53:45 UTC 2003
 * 
 * @author Erik Leitch
 */

namespace sza {
  namespace util {
    
    /**
     * Class to define valid Pmac command modes.
     */
    class PmacMode {

    public:
      /**
       * Enumerate the pmac control modes
       */
      enum Mode {
	
	// Follow 1-second tracking targets
	
	TRACK,
	
	// Slew the telescope to a given position then stop
	
	SLEW,
	
	// Bring the telesope to a stop ASAP
	
	HALT,
	
	// Start a new track when a pulse is received from the
	// time-code reader.
	
	SYNC,
	
	// Tell the pmac to reboot itself
	
	REBOOT,

	// New default is always to track

	DEFAULT = TRACK
      };
      
    }; // End class PmacMode
    
  }; // End namespace util
}; // End namespace sza

#endif // End #ifndef 
