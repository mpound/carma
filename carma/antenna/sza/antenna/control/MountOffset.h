#ifndef MOUNTOFFSET_H
#define MOUNTOFFSET_H

/**
 * @file MountOffset.h
 * 
 * Tagged: Thu Nov 13 16:53:41 UTC 2003
 * 
 * @author Erik Leitch
 */
#include "carma/antenna/sza/antenna/control/OffsetBase.h"

namespace sza {
  namespace antenna {
    namespace control {
      
      
      /**
       * A class for managing mount offsets (az/el).
       */
      class MountOffset : public OffsetBase {
	
      public:
	
	/**
	 * Constructor.
	 */
	MountOffset();
	
	/**
	 * Set the offsets
	 */
	void set(sza::util::OffsetMsg msg);
	
	/**
	 * Return the azimuth offset
	 */
	double getAz();
	
	/**
	 * Return the elevation offset
	 */
	double getEl();
	
	/**
	 * Increment the offsets, in radians
	 */
	void increment(double daz, double del);
	
	/**
	 * Initialize the offsets
	 */
	void reset();
	
	/**
	 * Method to pack the mount offsets for archival in the
	 * register database.
	 */
	void pack(signed* s_elements);
	
      private:
	
	/**
	 * The azimuth offset.
	 */
	double az_;
	
	/**
	 * The elevation offset.
	 */
	double el_;
	
	/**
	 * The PA offset.
	 */
	double pa_;
	
      }; // End class MountOffset
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 

