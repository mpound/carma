#include "carma/monitor/AntennaCommon.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/monitor/ControlSubsystemExt.h"

#include "carma/pipeline/ShadowingCalculator.h"

#include "carma/szautil/Geoid.h"

using namespace std;

using namespace carma::monitor;
using namespace carma::pipeline;
using namespace carma::services;
using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
ShadowingCalculator::ShadowingCalculator( const MonitorSystem * const ms ) :
    ms_( ms )
{
  if(ms_ == 0) {
    throw CARMA_EXCEPTION(carma::util::UserException, "Monitor system pointer is NULL");
  }

  initialize();
  cacheMonitorSystemPointers();
}

/**.......................................................................
 * Cache pointers to monitor points we will need to access
 */
void ShadowingCalculator::cacheMonitorSystemPointers( ) 
{
  for(unsigned iAnt=0; iAnt < Global::maxAntennas(); iAnt++) {
    antennaCommonPtrs_[iAnt]  = &(ms_->antennaCommon(iAnt));
    controlAntennaPtrs_[iAnt] = &(ms_->control().antenna(iAnt));
  }
}

/**.......................................................................
 * Destructor.
 */
ShadowingCalculator::~ShadowingCalculator() {}

/**.......................................................................
 * Initialize internals to sensible defaults
 */
void ShadowingCalculator::initialize()
{
  nAnt_ = Global::maxAntennas();

  // Assume shadowing percentage refers to diameter

  sweptVolumeShadowing_.percentOfDiameter_ = true;
  internalShadowing_.percentOfDiameter_    = true;

  // Default to ANY overlap --> shadowed

  sweptVolumeShadowing_.percent_.setPercentMax1(0.0);
  internalShadowing_.percent_.setPercentMax1(0.0);

  // Resize arrays

  sweptVolumeShadowingFlags_.resize(nAnt_);
  internalShadowingFlags_.resize(nAnt_);
  subarrayNos_.resize(nAnt_);
  azimuths_.resize(nAnt_);
  elevations_.resize(nAnt_);
  latitudes_.resize(nAnt_);
  longitudes_.resize(nAnt_);
  altitudes_.resize(nAnt_);

  tracking_.resize(nAnt_);

  antennaCommonPtrs_.resize(nAnt_);
  controlAntennaPtrs_.resize(nAnt_);

  padLocations_.resize(nAnt_);

  // Default to NOT overriding the proper calculations!

  sweptOverride_ = false;

  // Initialize antenna information that doesn't change

  for(unsigned iAnt=0; iAnt < Global::nOvroAntennas(); iAnt++) {
    padLocations_[iAnt].ant_ = CarmaConfig::getAntennaInfo(CarmaConfig::OVRO);
  }

  for(unsigned iAnt=Global::nOvroAntennas(); iAnt < Global::nOvroAntennas()+Global::nBimaAntennas(); iAnt++) {
    padLocations_[iAnt].ant_ = CarmaConfig::getAntennaInfo(CarmaConfig::BIMA);
  }

  for(unsigned iAnt=Global::nOvroAntennas()+Global::nBimaAntennas(); iAnt < nAnt_; iAnt++) {
    padLocations_[iAnt].ant_ = CarmaConfig::getAntennaInfo(CarmaConfig::SZA);
  }
}

void ShadowingCalculator::setInternalShadowingDiameterPercentage(sza::util::Percent percent)
{
  internalShadowing_.percent_              = percent;
  internalShadowing_.percentOfDiameter_    = true;
}

void ShadowingCalculator::setSweptVolumeShadowingDiameterPercentage(sza::util::Percent percent)
{
  sweptVolumeShadowing_.percent_           = percent;
  sweptVolumeShadowing_.percentOfDiameter_ = true;
}

void ShadowingCalculator::setInternalShadowingAreaPercentage(sza::util::Percent percent)
{
  internalShadowing_.percent_              = percent;
  internalShadowing_.percentOfDiameter_    = false;
}

void ShadowingCalculator::setSweptVolumeShadowingAreaPercentage(sza::util::Percent percent)
{
  sweptVolumeShadowing_.percent_           = percent;
  sweptVolumeShadowing_.percentOfDiameter_ = false;
}

/**.......................................................................
 * Update quantities from the monitor system
 */
void ShadowingCalculator::update()
{
  updateConfigurationInformation();
  updateShadowFlags();
}

/**.......................................................................
 * Update information from the monitor system pertaining to the
 * configuration of the array and current pointing of the antennas
 */
void ShadowingCalculator::updateConfigurationInformation()
{
  for(unsigned iAnt=0; iAnt < nAnt_; iAnt++) {
    CarmaConfig::PadLocation&      pad       = padLocations_[iAnt];
    ControlSubsystemBase::Antenna* ant       = controlAntennaPtrs_[iAnt];
    AntennaCommon*                 antCommon = antennaCommonPtrs_[iAnt];

    // Get the pad location for this antenna
    
    pad.east_.setMeters(ant->totalENU().east().getValue());
    pad.north_.setMeters(ant->totalENU().north().getValue());
    pad.up_.setMeters(ant->totalENU().up().getValue());

    pad.padNumber_      = ant->padNumber().getValue();
    pad.ant_.antNumber_ = ant->carmaAntennaNumber().getValue();

    // Store the subarray number for this antenna

    subarrayNos_[iAnt] = ant->subarrayNumber().getValue();

    // Get the Az and El for this antenna

    azimuths_[iAnt].setDegrees(antCommon->drive().track().actualAzimuth().getValue());
    elevations_[iAnt].setDegrees(antCommon->drive().track().actualElevation().getValue());

    // Lastly get the lat/long/alt

    latitudes_[iAnt].setDegrees(antCommon->location().latitude().getValue());
    longitudes_[iAnt].setDegrees(antCommon->location().longitude().getValue());
    altitudes_[iAnt].setMeters(antCommon->location().altitude().getValue());

    AntennaCommon::StateMonitorPointEnum::STATE state = antCommon->drive().state().getValue();

    tracking_[iAnt] = (state == AntennaCommon::StateMonitorPointEnum::TRACK ||
		       state == AntennaCommon::StateMonitorPointEnum::CLOSE);

    pad.validity_ = tracking_[iAnt];
    pad.ant_.tracking_ = tracking_[iAnt];
    pad.ant_.subarrayNumber_ = subarrayNos_[iAnt];

#if 0
    COUT("Antenna " << iAnt+1 << " pad = " << pad 
	 << std::endl << " tracking  = " << tracking_[iAnt] 
	 << std::endl << " azimuth   = " << azimuths_[iAnt] 
	 << std::endl << " elevation = " << elevations_[iAnt]
	 << std::endl << " latitude  = " << latitudes_[iAnt].degrees()
	 << std::endl << " longitude = " << longitudes_[iAnt].degrees()
	 << std::endl << " altitude  = " << altitudes_[iAnt].meters());
#endif
  }

}

/**.......................................................................
 * Once values have been read from the monitor system, iterate over
 * all antennas to calculate if any are shadowed.
 */
void ShadowingCalculator::updateShadowFlags()
{
  // Iterate over all antennas, regardless of subarray membership

  for(unsigned iAnt1=0; iAnt1 < nAnt_; iAnt1++) {

    // Initialize the shadowing flags to false at the start of each
    // loop

    internalShadowingFlags_[iAnt1]    = false;
    sweptVolumeShadowingFlags_[iAnt1] = false;

    CarmaConfig::PadLocation& pad1 = padLocations_[iAnt1];
    Angle& az = azimuths_[iAnt1];
    Angle& el = elevations_[iAnt1];

    // For this antenna, iterate over all other antennas, regardless
    // of subarray membership

    for(unsigned iAnt2=0; iAnt2 < nAnt_; iAnt2++) {

      // Obviously if this is the same antenna, just skip shadowing
      // calculations -- we can't shadow ourselves!

      if(iAnt1 == iAnt2)
	continue;
      
      CarmaConfig::PadLocation& pad2 = padLocations_[iAnt2];

      // If these two antennas are in the same subarray, and both are
      // actually tracking, calculate internal shadowing

      if(subarrayNos_[iAnt1] == subarrayNos_[iAnt2] && tracking_[iAnt1] && tracking_[iAnt2] && !sweptOverride_) {

	if(pad1.isShadowed(az, el, pad2, 
			   false, internalShadowing_.percent_, internalShadowing_.percentOfDiameter_)) {

#if 0
	  COUT("Pad " << pad1.padNumber_ << " is internally shadowed by pad " << pad2.padNumber_ << " for az = " << az << " el = " << el);
	  pad1.isShadowed(az, el, pad2, 
			  false, internalShadowing_.percent_, internalShadowing_.percentOfDiameter_, true);
#endif
	  internalShadowingFlags_[iAnt1] = true;
	}

	// Else they are in different subarrays, or one or more of
	// these antennas isn't tracking: in this case, we calculate
	// swept volume shadowing.

      } else {

	if(pad1.isShadowed(az, el, pad2, 
			   true, sweptVolumeShadowing_.percent_, sweptVolumeShadowing_.percentOfDiameter_)) {

#if 0
	  COUT("Pad " << pad1.padNumber_ << " is externally shadowed by pad " << pad2.padNumber_ << " for az = " << az << " el = " << el);
	  COUT("subArray1 = " << subarrayNos_[iAnt1] << " subarray2 = " << subarrayNos_[iAnt2] << " tracking1 = " << tracking_[iAnt1]
	       << " tracking2 = " << tracking_[iAnt2]);

	  pad1.isShadowed(az, el, pad2, 
			  true, sweptVolumeShadowing_.percent_, sweptVolumeShadowing_.percentOfDiameter_, true);
#endif
	  sweptVolumeShadowingFlags_[iAnt1] = true;
	  //	  COUT("Ant " << iAnt1 << " is shadowed by " << iAnt2);
	}

      }

      // If this iteration has left us with both shadowing flags set,
      // we don't need to carry on.  Ants can only be flagged as
      // shadowed once.

      if(internalShadowingFlags_[iAnt1] && sweptVolumeShadowingFlags_[iAnt1]) {
	break;
      }

    }
  }

}

std::vector<bool> ShadowingCalculator::getSweptVolumeShadowing()
{
  return sweptVolumeShadowingFlags_;
}

std::vector<bool> ShadowingCalculator::getInternalShadowing()
{
  return internalShadowingFlags_;
}

void ShadowingCalculator::setAzEl(sza::util::Angle az, sza::util::Angle el)
{
  for(unsigned iAnt=0; iAnt < nAnt_; iAnt++) {
    azimuths_[iAnt]   = az;
    elevations_[iAnt] = el;
    tracking_[iAnt]   = true;
    padLocations_[iAnt].validity_ = true;
    padLocations_[iAnt].ant_.tracking_ = true;
  }
}

void ShadowingCalculator::setHaDec(sza::util::HourAngle ha, sza::util::Declination declination)
{
  Lla lla;
  lla.setCoordSystem(COORD_GEODETIC);

  sza::util::PolarLengthVector azElVec;

  // Transform fom LLA + HA/DEC to AZ/EL for each antenna

  Geoid geoid(DATUM_GPS);

  for(unsigned iAnt=0; iAnt < nAnt_; iAnt++) {
    lla.latitude_  = latitudes_[iAnt];
    lla.longitude_ = longitudes_[iAnt];
    lla.altitude_  = altitudes_[iAnt];

    azElVec = geoid.geodeticLlaAndHaDecToAzEl(lla, ha, declination);

    azimuths_[iAnt]   = azElVec.az_;
    elevations_[iAnt] = azElVec.el_;
    tracking_[iAnt]   = true;
    padLocations_[iAnt].validity_ = true;
    padLocations_[iAnt].ant_.tracking_ = true;
  }
}

std::vector<sza::util::CarmaConfig::PadLocation>& ShadowingCalculator::getPadLocations()
{
  return padLocations_;
}

void ShadowingCalculator::defaultToSweptVolumeForAllAntennas(bool swept)
{
  sweptOverride_ = swept;
}
