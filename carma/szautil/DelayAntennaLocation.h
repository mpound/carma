#ifndef SZA_UTIL_DELAYANTENNALOCATION_H
#define SZA_UTIL_DELAYANTENNALOCATION_H

/**
 * @file DelayAntennaLocation.h
 * 
 * Tagged: Thu Aug  5 09:39:55 PDT 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/AntNum.h"
#include "carma/szautil/Frequency.h"
#include "carma/szautil/Rx.h"
#include "carma/szautil/DelayLocation.h"

namespace sza {
  namespace util {
    
    class MonitorPoint;
    class MonitorPointManager;
    class RegMapDataFrameManager;

    class DelayAntennaLocation : public DelayLocation {
    public:
      
      /**
       * Constructor.
       */
      DelayAntennaLocation(RegMapDataFrameManager* frame=0);
      
      /**
       * Destructor.
       */
      virtual ~DelayAntennaLocation();

      /**
       * Set an id for this antenna
       */
      void setAntId(AntNum::Id antId);

      /**
       * Select a receiver for this antenna
       */
      void selectRx(Rx::Id rxId);

      /**
       * Set an axis misalignment
       */
      void setAxisMisalignment(Length misalignment);
      void useAxisMisalignment(bool use);

      /**
       * Return the LO frequency of this antenna
       */
      inline Frequency LOFrequency() {
	return LOFrequency_;
      }

      /**
       * Return the Sky frequency of this antenna
       */
      inline Frequency skyFrequency() {
	return skyFrequency_;
      }

      /**
       * Return the frequency at which we will lobe rotate for this antenna
       */
      inline Frequency lobeRotatorFrequency(bool useOffset) {

	if(useOffset)
	  return skyFrequency_ - offsetFrequency_;
	else
	  return skyFrequency_;

      }

      /**
       * Return true if we have frequency information for this antenna
       */
      inline double hasFrequency() {
	return hasFrequency_;
      }

      /**
       * Return the axis misalignment
       */
      inline Length axisMisalignment() {
	return axisMisalignment_;
      }

      /**
       * Set the fixed delay associated with this location
       */
      void setDefaultFixedDelay(Delay delay, Rx::Id rxId);
      
      /**
       * Set the adjustable delay associated with this location
       */
      void setDefaultAdjustableDelay(Delay delay, Rx::Id rxId);

      MonitorPoint* monAxisMisalignment_;
      MonitorPoint* monUseAxisMisalignment_;

      // Update quantities needed for delay calculation

      void updateTransientDelayQuantities(double mjd, sza::util::Source* src);

      // Get the AZ/EL coordinates corresponding to this HA/DEC
      // location
      
      void updateAzEl(HourAngle ha, DecAngle dec);

      // Get the AZ/EL coordinates corresponding to this HA/DEC
      // location
      
      void updateAxisMisalignmentCorrection(HourAngle ha, DecAngle dec);

      double X(bool ec=true);
      double Y(bool ec=true);
      double Z(bool ec=true);

    private:

      // NIA correction to the geocentric (X, Y, Z) relative to the fiducial
      
      Vector<double> dGeocentricXyz_; 
      
      // NIA correction to the topocentric (X, Y, Z) relative to the fiducial
      
      Vector<double> dTopocentricXyz_;
      
      // This array will store default fixed delays for each frequency

      Delay default30GHzFixedDelay_;
      Delay default90GHzFixedDelay_;
      Delay default230GHzFixedDelay_;

      // A pointer to the element of the above array that corresponds
      // to the current frequency

      Delay* defaultFixedDelay_;

      // This array will store default adjustable delays for each
      // frequency

      Delay default30GHzAdjustableDelay_;
      Delay default90GHzAdjustableDelay_;
      Delay default230GHzAdjustableDelay_;

      // A pointer to the element of the above array that corresponds
      // to the current frequency

      Delay* defaultAdjustableDelay_;

      AntNum antNum_;
      Frequency LOFrequency_;
      Frequency skyFrequency_;
      Frequency offsetFrequency_;

      bool hasFrequency_;
      MonitorPoint* monHasFrequency_;
      MonitorPoint* monLOFrequency_;
      MonitorPoint* monSkyFrequency_;

      Length axisMisalignment_;
      bool useAxisMisalignment_;

      friend class DelayManager;

      void registerAntParamCallback(DelayManager* manager);

      void updateHasFrequency(bool hasFrequency);
      void updateLOFrequency(Frequency freq);
      void updateSkyFrequency(Frequency freq);
      void updateDefaultDelays(Rx::Id);

      void updateAxisMisalignment();
      void updateUseAxisMisalignment();

      void updateMonitors();

    }; // End class DelayAntennaLocation
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_DELAYANTENNALOCATION_H
