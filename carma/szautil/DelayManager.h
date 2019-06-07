#ifndef SZA_UTIL_DELAYMANAGER_H
#define SZA_UTIL_DELAYMANAGER_H

/**
 * @file DelayManager.h
 * 
 * Tagged: Sat Jul 17 04:07:14 UTC 2004
 * 
 * @author 
 */
#include <vector>

#include "carma/szautil/Angle.h"
#include "carma/szautil/AntNum.h"
#include "carma/szautil/Coordinates.h"
#include "carma/szautil/DelayAntennaLocation.h"
#include "carma/szautil/DelayLocation.h"
#include "carma/szautil/Mutex.h"

namespace sza {
  namespace util {
    
    class RegMapDataFrameManager;
    class MonitorPoint;
    class MonitorPointManager;

    class DelayManager : public Mutex {
    public:
      
      /**
       * Constructor.
       */
      DelayManager(RegMapDataFrameManager* frame=0);
      
      /**
       * Destructor.
       */
      virtual ~DelayManager();
      
      /**
       * Set the location of a global reference point.
       */
      void setSite(Angle longitude, Angle latitude, double altitude);

      /**
       * Set an antenna location
       */
      void setAntennaLocation(AntNum::Id antId, 
			      double up, double east, double north);

      /**
       * Set a delay reference location
       */
      void setDelayRefLocation(double up, double east, double north);

      /**
       * Pick an antenna as the delay reference
       */
      void setRefAntenna(AntNum::Id antId);

      /**
       * Methods for getting a handle to an antenna
       */
      DelayAntennaLocation* getAntenna(AntNum::Id antId);

      /**
       * Method for accessing the delay reference location
       */
      DelayAntennaLocation* getDelayRef();

      /**
       * A function called whenever an outside caller changes antenna
       * parameters
       */
      void antParamsChanged(DelayAntennaLocation* loc);

      /**
       * A function called whenever an outside caller changes a
       * delay location
       */
      void locationChanged(DelayLocation* loc);

      /**
       * A function called whenever an outside caller changes a delay
       */
      void delaysChanged(DelayLocation* loc);

      /**
       * Mark antenna parameters as discontinuous
       */
      void markAsDiscontinuous(AntNum::Id antSet, bool disc);

      /**
       * Extend the ephemeris of the UT1-UTC correction.
       *
       * Note that the time coordinate in this method should be UTC,
       * expressed as an MJD.
       */
      void extendUt1Utc(double mjdUtc, double ut1Utc);

      /**
       * Extend the ephemeris of the equation of the equinoxes
       *
       * Note that the time coordinate in this method should be TT
       * expressed as an MJD.
       */
      void extendEqnEqx(double mjdTt, double eqnEqx);

      void updateSiteMonitor(Angle longitude, Angle latitude, double altitude);
      void updateRefAntMonitor(char* ant);
      void updateRefLocationMonitor();

      DelayAntennaLocation* getArbitraryRefLocation() {
	return &delayRefLocation_;
      }

    private:

      MonitorPointManager* monitor_;

      MonitorPoint* monLon_;
      MonitorPoint* monLat_;
      MonitorPoint* monAlt_;
      MonitorPoint* monRefAnt_;
      MonitorPoint* monRefLoc_;

      // An arbitrary delay reference location

      DelayAntennaLocation delayRefLocation_;

      // A vector of antenna locations

      std::vector<DelayAntennaLocation*> antennaLocations_;

      // A pointer to the current reference location

      DelayAntennaLocation* refLoc_;

      /**
       * Update delays for all antennas
       */
      void  updateDelays();

    }; // End class DelayManager
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_DELAYMANAGER_H
