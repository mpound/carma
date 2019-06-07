#ifndef EQUATOFFSET_H
#define EQUATOFFSET_H

/**
 * @file EquatOffset.h
 * 
 * Tagged: Thu Nov 13 16:53:38 UTC 2003
 * 
 * @author Erik Leitch
 */
#include "carma/antenna/sza/antenna/control/OffsetBase.h"

namespace sza {
  namespace antenna {
    namespace control {
      
      
      /**
       * Incomplete type specification of class Tracker lets us
       * declare it as a friend below, without defining it.
       */
      class Tracker;
      
      /**
       * A class to manage equatorial (RA/DEC) tracking offsets.
       */
      class EquatOffset : public OffsetBase {
	
      public:
	
	/**
	 * Constructor.
	 */
	EquatOffset();
	
	/**
	 * Reset internal data members of this object.
	 */
	void reset();
	
	/**
	 * Install new offsets received from the control program
	 */
	void set(sza::util::OffsetMsg msg);
	
	/**
	 * Apply the offsets to the pointing correction.
	 */
	void apply(PointingCorrections* f);
	
	/**
	 * Pack equatorial offsets for archival in the register
	 * database.
	 */
	void pack(signed* s_elements);
	
      private:
	
	/**
	 * Tracker will access internal members directly.
	 */
	friend class Tracker;
	
	/**
	 * The RA offset.
	 */
	double ra_;
	
	/**
	 * The DEC offset.
	 */
	double dec_;
	
      }; // End class EquatOffset
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 
