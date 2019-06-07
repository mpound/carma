#ifndef SZA_UTIL_DELAYLOCATION_H
#define SZA_UTIL_DELAYLOCATION_H

/**
 * @file DelayLocation.h
 * 
 * Tagged: Thu Aug  5 06:51:59 PDT 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/Debug.h"
#include "carma/szautil/Delay.h"
#include "carma/szautil/Location.h"
#include "carma/szautil/Source.h"

namespace sza {
  namespace util {
    
    class DelayAntennaLocation;
    class DelayManager;
    class MonitorPoint;
    class MonitorPointManager;
    
    class DelayLocation : public Location {
    public:
      
      /**
       * Constructor.
       */
      DelayLocation();
      
      /**
       * Destructor.
       */
      virtual ~DelayLocation();
      
      /**
       * Set the fixed delay associated with this location
       */
      void setFixedDelay(Delay delay);
      
      /**
       * Set the adjustable delay associated with this location
       */
      void setAdjustableDelay(Delay delay);
      
      /**
       * Get the adjustable delay associated with this location
       */
      inline Delay adjustableDelay() {
	return referredAdjustableDelay_;
      }
      
      /**
       * Get the fixed delay associated with this location
       */
      inline Delay fixedDelay() {
	return referredFixedDelay_;
      }
      
      /**
       * Get a reference to our location
       */
      inline Location& location() {
	return (Location&) *this;
      }
      
      /**
       * A function called whenever an outside caller changes a location
       */
      virtual void locationChanged(Location* loc);
      
      // Toggle using various delays
      
      void useAdjustableDelay(bool use);
      void useFixedDelay(bool use);
      void useGeometricDelay(bool use);
      void useIonosphericDelay(bool use);
      void useTroposphericDelay(bool use);
      void useThermalDelay(bool use);
      
      /**
       * Get the total delay for an Ha Dec source position
       */
      Delay totalDelay(double mjd, sza::util::Source* src,
		       DelayAntennaLocation* refDLoc,
		       bool doMotionCorrection);

      /**.......................................................................
       * Get the total delay for an Az El source position
       */
      Delay totalDelay(Angle az, Angle el,
		       DelayAntennaLocation* refDLoc);
      
      /**.......................................................................
       * Get geometric delay for an Ha Dec source position
       */
      Delay geometricDelay(DelayAntennaLocation* refDLoc,
			   bool doMotionCorrection);
      
      /**.......................................................................
       * Get geometric delay for an Az El source position
       */
      Delay geometricDelay(Angle az, Angle el,
			   DelayAntennaLocation* refDLoc);
      
      /**.......................................................................
       * Get tropospheric delay for an Ha Dec source position
       */
      Delay troposphericDelay(DelayAntennaLocation* refDLoc);
      
      /**
       * Return the discontinuity flag
       */
      inline bool discontinuity() {
	return discontinuity_;
      }

      /**
       * Mark this location's params are discontinuous
       */
      void markAsDiscontinuous(bool disc) {
	discontinuity_ = disc;
      }
      
    protected:
      
      // DelayManager is a friend of this class
      
      friend class DelayManager;
      
      DelayManager* delayListener_;

      DelayManager* locationListener_;
      
    public:
      // The physical location associated with this object
      
      Location location_;
      
    protected:
      // A manager for monitor points

      MonitorPointManager* monitor_;

      MonitorPoint* monAdjustableDelay_;
      MonitorPoint* monUseAdjustableDelay_;
      MonitorPoint* monFixedDelay_;
      MonitorPoint* monUseFixedDelay_;
      MonitorPoint* monGeometricDelay_;
      MonitorPoint* monUseGeometricDelay_;
      MonitorPoint* monTroposphericDelay_;
      MonitorPoint* monUseTroposphericDelay_;
      MonitorPoint* monLocation_;

      // Update monitor points managed by this object in the register
      // database

      virtual void updateMonitors();

    protected:
      
      // True if this location's parameters have changed

      bool discontinuity_;

      // The fixed delay relative to some fiducial point
      
      Delay fiducialFixedDelay_;

      // A user-adjustable delay relative to some fixed point
      
      Delay fiducialAdjustableDelay_;
      
      // The fixed delay relative to an arbitrary reference point
      
      Delay referredFixedDelay_;
      
      // A user-adjustable delay relative to an arbitrary reference point
      
      Delay referredAdjustableDelay_;
      
      // The last calculated geometric delay
      
      Delay lastGeometricDelay_;

      // The last calculated tropospheric delay
      
      Delay lastTroposphericDelay_;

      // Toggle using various delays
      
      bool useAdjustableDelay_;
      bool useFixedDelay_;
      bool useGeometricDelay_;
      bool useIonosphericDelay_;
      bool useTroposphericDelay_;
      bool useThermalDelay_;
      
      /**
       * DelayManager is a friend because we want to be able to
       * register to be called when this object's parameters are
       * updated
       */
      void registerDelayCallback(DelayManager* delayListener);
      
      /**
       * Register with this object to be notified when its location
       * changes
       */
      void registerLocationCallback(DelayManager* locationListener);

      /**
       * Refer the fixed delay to a different reference point
       */
      void referFixedDelayTo(Delay delay);
      
      /**
       * Refer the adjustable delay to a different reference point
       */
      void referAdjustableDelayTo(Delay delay);
      
      /**
       * Get the fixed delay associated with this location
       */
      inline Delay fiducialFixedDelay() {
	return fiducialFixedDelay_;
      }
      
      /**
       * Get the adjustable delay associated with this location
       */
      inline Delay fiducialAdjustableDelay() {
	return fiducialAdjustableDelay_;
      }
      
      // Update values in the register database

      void updateUseAdjustableDelay();
      void updateUseFixedDelay();
      void updateUseGeometricDelay();
      void updateUseTroposphericDelay();

      void updateAdjustableDelay();
      void updateFixedDelay();
      void updateGeometricDelay();
      void updateTroposphericDelay();

      void updateLocation();
      
    }; // End class DelayLocation
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_DELAYLOCATION_H
