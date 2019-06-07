#ifndef AXISPOS_H
#define AXISPOS_H

/**
 * @file AxisPos.h
 * 
 * Tagged: Thu Nov 13 16:53:32 UTC 2003
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/Axis.h"

namespace sza {
  namespace antenna {
    namespace control {
      
      
      /**
       * Incomplete type specifications lets us declare these as
       * friends without defining them.
       */
      class AxisPositions;
      class PmacBoard;
      class Tracker;
      class TrackerBoard;
      
      /**
       * A class to encapsulate the topocentric and encoder position
       * of a telescope axis
       */
      class AxisPos {
	
      public:
	
	/**
	 * Constructor
	 */
	AxisPos(sza::util::Axis::Type type);
	
	/**
	 * Reset internal members
	 *
	 * @throws Exception
	 */
	void reset();
	
	double getTopo() {
	  return topo_;
	}

      private:
	
	/**
	 * Friends of this class -- why don't I just make the data
	 * members public?
	 */
	friend class AxisPositions;
	friend class PmacBoard;
	friend class Tracker;
	friend class TrackerBoard;
	
	/**
	 * The axis this container represents.
	 */
	sza::util::Axis axis_; 
	
	/**
	 * The topocentric position of the axis in
	 * radians.
	 */
	double topo_;     
	
	/**
	 * The encoder count of the axis.
	 */
	signed count_;    
	
      }; // End class AxisPos
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 
