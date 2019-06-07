#ifndef POINTING_H
#define POINTING_H

/**
 * @file Pointing.h
 * 
 * Tagged: Thu Nov 13 16:53:46 UTC 2003
 * 
 * @author Erik Leitch
 */

// Needed for SRC_LEN

#include "carma/szaarrayutils/szaregs.h"

#include "carma/szautil/Axis.h"
#include "carma/szautil/PmacMode.h"
#include "carma/szautil/RegDate.h"

#include "carma/antenna/sza/antenna/control/AxisPositions.h"
#include "carma/antenna/sza/antenna/control/Encoder.h"
#include "carma/antenna/sza/antenna/control/PmacAxis.h"
#include "carma/antenna/sza/antenna/control/Position.h"
#include "carma/antenna/sza/antenna/control/SzaShare.h"
#include "carma/antenna/sza/antenna/control/TrackerMsg.h"

namespace sza {
  namespace antenna {
    namespace control {
      
      
      class Tracker;
      
      /**
       * A class used by Tracker to handle pointing.
       */
      class Pointing {
	
      public:

	/**
	 * An enumerator for the Position members below
	 */
	enum PositionType {
	  MOUNT_ANGLES,
	  MOUNT_RATES,
	  TOPOCENTRIC,
	  GEOCENTRIC
	};
	
	/**
	 * Constructor
	 */
	Pointing();
	
	/**
	 * Reset method
	 */
	void reset();
	
	// Methods to set up pointing in preparation for various moves
	
	/**
	 * Prepare for a halt of the antenna
	 */
	void setupForHalt(SzaShare* share);
	
	/**
	 * Prepare for a reboot (of the pmac).
	 */
	void setupForReboot(SzaShare* share);
	
	/**
	 * Prepare for a slew.
	 */
	void setupForSlew(SzaShare* share, TrackerMsg* msg);
	
	/**
	 * Prepare for a track command
	 */
	void setupForTrack();

	// Member modification methods
	
	/**
	 * Set the current time.
	 */
	void setTime(double utc);
	
	/**
	 * Set the current time.
	 */
	void setTime(int mjd, int sec);
	
	/**
	 * Set the current source name.
	 */
	void setName(char* name);
	unsigned char* getName();

	/**
	 * Install the set of axes to drive.
	 */
	void setAxes(sza::util::Axis::Type axes);
	
	/**
	 * Set the RA of the source.
	 */
	void setRa(double ra);
	
	/**
	 * Set the Dec of the source.
	 */
	void setDec(double dec);
	
	/**
	 * Set the source distance.
	 */
	void setDist(double dist);
	
	/**
	 * Install the refraction correction.
	 */
	void setRefraction(double refraction);
	
	// Member access methods
	
	/**
	 * Get the mask of axes to be driven.
	 */
	sza::util::Axis::Type getAxes();
	
	/**
	 * Return true if the passed axis is included in the set of
	 * axes to control.
	 */
	bool includesAxis(sza::util::Axis::Type axis);
	
	/**
	 * Return the current UTC
	 */
	double getUtc();
	sza::util::RegDate getDate();
	
	/**
	 * Return the refraction correction.
	 */
	double getRefraction();
	
	/**
	 * Convert a mount angle to encoder counts
	 */
	void convertMountToEncoder(Encoder* encoder,
				   PmacAxis* axis,
				   int current,
				   bool ignoreWrapLogic=true);
	
	/**
	 * Return a pointer to the requested pointing angle container
	 *
	 * @throws Exception
	 */
	sza::antenna::control::Position* Position(PositionType type);
	
	/**
	 * Install the angles to which the axes will be driven.
	 */
	void setAngles(double az, double el, double pa);
	
	/**
	 * Install the rates with which the axes will be driven.
	 */
	void setRates(double az, double el, double pa);
	
	/**
	 * Compute the current geocentric position.
	 */
	void computeGeocentricPosition(double lst, PointingCorrections* f);
	
	/**
	 * Pack the UTC for archival into the register database.
	 */
	void packUtc(unsigned* u_elements);
	
	/**
	 * Pack the source name for archival in the register database.
	 *
	 * @throws Exception
	 */
	void packSourceName(unsigned* u_elements, int nel);
	
	/**
	 * Pack geocentric equatorial coordinates.
	 */
	void packEquatGeoc(signed* s_elements);
	
	/**
	 * Pack geocentric horizon coordinates.
	 */
	void packHorizGeoc(signed* s_elements);
	
	/**
	 * Pack topocentric horizon coordinates.
	 */
	void packHorizTopo(signed* s_elements);
	
	/**
	 * Pack mount horizon coordinates.
	 */
	void packHorizMount(signed* s_elements);
	
	bool isFixed();
	bool isHalt();

	void setCurrentPosition(AxisPositions& axes);
	AxisPositions currentPosition_;
	sza::util::PmacMode::Mode mode_;

      private:
	
	friend class Tracker;
	
	bool currentPositionIsSet_;

	/**
	 * Round an angle into the range -pi..pi.
	 */
	double wrapPi(double angle);
	
	/**
	 * Round an angle into the range 0..2pi
	 */
	double wrap2pi(double angle);
	
	/**
	 * The name of the source
	 */
	char name_[SRC_LEN];  
	
	/**
	 * The MJD in days and seconds
	 */
	int mjd_, sec_;       
	
	/**
	 * The geocentric right ascension and declination
	 */
	double ra_,dec_;      
	
	/**
	 * The distance to the source (or 0 if irrelevant)
	 */
	double dist_;         
	
	/**
	 * The applied refraction correction (radians).
	 */
	double refraction_;   
	
	/**
	 * The geocentric azimuth,elevation,parallactic angle.
	 */
	sza::antenna::control::Position geocentric_;   
	
	/**
	 * The topocentric azimuth,elevation,parallactic angle
	 */
	sza::antenna::control::Position topocentric_;  
	
	/**
	 * The telescope azimuth,elevation,parallactic angle
	 */
	sza::antenna::control::Position mountAngles_; 
	
	/**
	 * The telescope az,el,pa move rates 
	 */
	sza::antenna::control::Position mountRates_;  
	
	sza::util::Axis::Type axes_;    // The set of axes to drive,
					// expressed as a bitwise
					// union of Axis enumerators.

	bool isFixed_; // Is this a fixed source?
	bool isHalt_;  // Is this a request to halt the telescope?

      }; // End class Pointing
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 
