// $Id: ShadowingCalculator.h,v 1.5 2012/07/25 16:40:11 eml Exp $

#ifndef CARMA_PIPELINE_SHADOWINGCALCULATOR_H
#define CARMA_PIPELINE_SHADOWINGCALCULATOR_H

/**
 * @file ShadowingCalculator.h
 * 
 * Tagged: Fri Oct  7 13:14:58 PDT 2011
 * 
 * @version: $Revision: 1.5 $, $Date: 2012/07/25 16:40:11 $
 * 
 * @author Erik Leitch
 */
#include "carma/monitor/ControlSubsystem.h"
#include "carma/monitor/MonitorSystem.h"

#include "carma/services/Global.h"

#include "carma/szautil/CarmaConfig.h"
#include "carma/szautil/Declination.h"
#include "carma/szautil/HourAngle.h"
#include "carma/szautil/Percent.h"

namespace carma {
  namespace pipeline {

    class ShadowingCalculator {
    public:

      // A struct for encapsulating a type of shadowing percentage

      struct Shadowing {
	sza::util::Percent percent_;
	bool percentOfDiameter_;
      };

      /**
       * Constructor.
       */
      ShadowingCalculator( const carma::monitor::MonitorSystem * const ms);

      /**
       * Destructor.
       */
      virtual ~ShadowingCalculator();

      // Update shadowing calculations from the monitor system

      void update();

      // Return a vector of boolean flags indicating if antennas are
      // shadowed by antennas in any subarray

      std::vector<bool> getSweptVolumeShadowing();

      // Return a vector of boolean flags indicating if antennas are
      // shadowed by antennas in the same subarray

      std::vector<bool> getInternalShadowing();

      // Set what constitutes shadowing, as a percentage of the
      // diameter of the shadowed dish.

      void setInternalShadowingDiameterPercentage(sza::util::Percent percent);
      void setSweptVolumeShadowingDiameterPercentage(sza::util::Percent percent);

      // Set what constitutes shadowing, as a percentage of the
      // area of the shadowed dish.

      void setInternalShadowingAreaPercentage(sza::util::Percent percent);
      void setSweptVolumeShadowingAreaPercentage(sza::util::Percent percent);

      // Override az/el positions for all telescopes with the passed az/el

      void setAzEl(sza::util::Angle az, sza::util::Angle el);
      void setHaDec(sza::util::HourAngle ha, sza::util::Declination declination);

      std::vector<sza::util::CarmaConfig::PadLocation>& getPadLocations();

    private:

      void initialize();
      void cacheMonitorSystemPointers();

    public:

      void updateConfigurationInformation();
      void updateShadowFlags();

      void defaultToSweptVolumeForAllAntennas(bool swept);

    public:

      unsigned nAnt_;
      const carma::monitor::MonitorSystem * const ms_;

      Shadowing sweptVolumeShadowing_;
      Shadowing internalShadowing_;

      std::vector<bool>              sweptVolumeShadowingFlags_;
      std::vector<bool>              internalShadowingFlags_;
      std::vector<unsigned>          subarrayNos_;
      std::vector<sza::util::Angle>  azimuths_;
      std::vector<sza::util::Angle>  elevations_;
      std::vector<sza::util::Angle>  latitudes_;
      std::vector<sza::util::Angle>  longitudes_;
      std::vector<sza::util::Length> altitudes_;
      std::vector<bool>              tracking_;

      std::vector<carma::monitor::AntennaCommon*>                 antennaCommonPtrs_;
      std::vector<carma::monitor::ControlSubsystemBase::Antenna*> controlAntennaPtrs_;

      std::vector<sza::util::CarmaConfig::PadLocation> padLocations_;

      bool sweptOverride_;

    }; // End class ShadowingCalculator

  } // End namespace pipeline
} // End namespace carma



#endif // End #ifndef CARMA_PIPELINE_SHADOWINGCALCULATOR_H
