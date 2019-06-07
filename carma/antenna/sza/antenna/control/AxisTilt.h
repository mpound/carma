#ifndef AXISTILT_H
#define AXISTILT_H

/**
 * @file AxisTilt.h
 * 
 * Tagged: Thu Nov 13 16:53:33 UTC 2003
 * 
 * @author Erik Leitch
 */
#include "carma/antenna/sza/antenna/control/PointingCorrections.h"

namespace sza {
  namespace antenna {
    namespace control {
      
      
      /**
       * A base class for managing tilts.  We don't make this a pure
       * virtual class, since I want to be able to pass a handle to
       * either az or el tilts which inherit from this class as a
       * pointer to a single type, without having to explictly define
       * methods in one that have no relevance (for instance,
       * setHaTilt() for an elevation tilt).
       */
      class AxisTilt {
	
      public:
	
	/**
	 * Virtual destructor ensures that the correct destructor will
	 * be called for classes that inherit from AxisTilt.
	 */
	virtual ~AxisTilt() {};
	
	//------------------------------------------------------------
	// Methods which should be overwritten by inheritors
	
	/**
	 * Reset private members of this class
	 */
	virtual void reset() {};
	
	/**
	 * Set the tilt in HA
	 */
	virtual void setHaTilt(double ha_tilt) {};
	
	/**
	 * Set the latitude tilt
	 */
	virtual void setLatTilt(double lat_tilt) {};
	
	/**
	 * Generic method to set a tilt for classes which only manage
	 * one tilt
	 */
	virtual void setTilt(double tilt) {};
	
	/**
	 * Apply the tilts managed by this class to the pointing corrections
	 */
	virtual void apply(PointingCorrections* f) {};
	
	/**
	 * Pack an HA tilt for archival in the register database.
	 */
	virtual void packHaTilt(signed* s_elements) {};
	
	/**
	 * Pack a latitude tilt for archival in the register database.
	 */
	virtual void packLatTilt(signed* s_elements) {};
	
	/**
	 * Pack a tilt for archival in the register database.
	 */
	virtual void packTilt(signed* s_elements) {};
	
      }; // End class AxisTilt
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 
