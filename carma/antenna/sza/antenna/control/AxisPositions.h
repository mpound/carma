#ifndef AXISPOSITIONS_H
#define AXISPOSITIONS_H

/**
 * @file AxisPositions.h
 * 
 * Tagged: Thu Nov 13 16:53:33 UTC 2003
 * 
 * @author Erik Leitch
 */
#include "carma/antenna/sza/antenna/control/AxisPos.h"

namespace sza {
  namespace antenna {
    namespace control {
      
      
      /**
       * Incomplete type specification for classes lets us declare
       * them as friends below without defining them.
       */
      class Tracker;
      class TrackerBoard;
      class PmacBoard;
      
      /**
       * The AxisPositions class is used by the Tracker class to
       * record the positions of the telescope axes, as reported by
       * the pmac.
       */
      class AxisPositions {
	
      public:
	
	/**
	 * Constructor
	 */
	AxisPositions();
	
	/**
	 * Return a pointer to a particular AxisPos descriptor
	 *
	 * @throws Exception
	 */
	sza::antenna::control::AxisPos* 
	  AxisPos(sza::util::Axis::Type type);
	
	/**
	 * Pack relevant data for archival in the register database
	 */
	void pack(signed* s_elements);
	
      private:
	
	/**
	 * Friends of AxisPositions.
	 */
	friend class Tracker;
	friend class TrackerBoard;
	friend class PmacBoard;
	
	/**
	 * The position of the azimuth axis 
	 */
	sza::antenna::control::AxisPos az_; 
	
	/**
	 * The position of the elevation axis 
	 */
	sza::antenna::control::AxisPos el_; 
	
	/**
	 * The position of the pa axis 
	 */
	sza::antenna::control::AxisPos pa_; 
	
      }; // End class AxisPositions
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 
