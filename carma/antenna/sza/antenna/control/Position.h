#ifndef POSITION_H
#define POSITION_H

/**
 * @file Position.h
 * 
 * Tagged: Thu Nov 13 16:53:49 UTC 2003
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/Axis.h"
#include "carma/antenna/sza/antenna/control/MountOffset.h"

namespace sza {
  namespace antenna {
    namespace control {
      
      /**
       * Incomplete type specification for Pointing lets us declare
       * these as friends below without defining them.
       */
      class Pointing;
      class Tracker;
      class TrackerBoard;
      class TvOffset;
      
      /**
       * The following class is used to record the horizon
       * coordinates of a source.
       */
      class Position {
	
      public:
	
	/**
	 * Constructor.
	 */
	Position();
	
	/**
	 * Reset internal data members.
	 */
	void reset();
	
	/**
	 * Set an axis position.
	 */
	void set(sza::util::Axis::Type axis, double val);
	
	/**
	 * Set an axis position
	 */
	void set(double az, double el, double pa);
	
	/**
	 * Increment the requested position with mount offsets
	 */
	void increment(MountOffset* offset);
	
	/**
	 * Get an axis position.
	 */
	double get(sza::util::Axis::Type axis);
	
	/**
	 * Pack this position for archival in the register database.
	 */
	void pack(signed* s_elements);
	
      private:
	
	/**
	 * Declare Pointing as a friend so we can manipulate Position
	 * members in pack methods
	 */
	friend class Tracker;
	friend class Pointing;
	friend class TrackerBoard;
	friend class TvOffset;
	
	/**
	 * The azimuth of the source
	 */
	double az_;      
	
	/**
	 * The elevation of the source
	 */
	double el_;      
	
	/**
	 * The parallactic angle of the source
	 */
	
	double pa_;      
	
      }; // End class Position
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 
