#ifndef OFFSETBASE_H
#define OFFSETBASE_H

/**
 * @file OffsetBase.h
 * 
 * Tagged: Thu Nov 13 16:53:42 UTC 2003
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/OffsetMsg.h"
#include "carma/antenna/sza/antenna/control/PointingCorrections.h"

// Share includes for constants mastor, pi, twopi

#include "carma/szaarrayutils/szaconst.h"

namespace sza {
  namespace antenna {
    namespace control {

      class PointingCorrections;

      /**
       * Define a class to encapsulate a variety of offsets
       */
      class OffsetBase {
	
      public:

	/**
	 * Constructor.
	 */
	OffsetBase();
	
	/**
	 * Pure virtual destructor prevents instantiation of
	 * OffsetBase.
	 */
	virtual ~OffsetBase() = 0;
	
	// Define methods which should be over-written by classes
	// which inherit from Offset
	//
	// These will be stubbed out so they only need to be defined
	// by inheritors where relevant
	
	/**
	 * Apply these offsets to the pointing model.
	 */
	virtual void apply(PointingCorrections* f);
	
	/**
	 * Install new offsets received from the control program.
	 */
	virtual void set(sza::util::OffsetMsg);
	
	/**
	 * Install new offsets received from the control program, with
	 * sequence number.
	 */
	virtual void set(sza::util::OffsetMsg, unsigned seq);
	
	/**
	 * Set an angle.
	 */
	virtual void setAngle(double angle);
	
	// Some utility functions we want all inheritors of this class
	// to have.
	
	/**
	 * Round an angle into the range -pi..pi.
	 */
	double wrapPi(double angle);
	
	/**
	 * Round an angle into the range 0-2.pi.
	 */
	double wrap2pi(double angle);
      };
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif
