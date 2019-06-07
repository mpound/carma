#include "carma/szautil/MonitorPoint.h"
#include "carma/szautil/MonitorPointManager.h"
#include "carma/szautil/Coordinates.h"
#include "carma/szautil/DelayAntennaLocation.h"
#include "carma/szautil/Debug.h"

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
DelayAntennaLocation::DelayAntennaLocation(RegMapDataFrameManager* frame) :
  defaultAdjustableDelay_(&default30GHzAdjustableDelay_), defaultFixedDelay_(&default30GHzFixedDelay_)
{
  monAxisMisalignment_ = 0;
  monUseAxisMisalignment_ = 0;
  monHasFrequency_ = 0;
  monitor_ = 0;

  if(frame != 0) 
    monitor_ = new MonitorPointManager(frame);

  // Set up alignment terms

  Length ln;
  ln.setMeters(0.0);
  setAxisMisalignment(ln);
  useAxisMisalignment(false);

  updateHasFrequency(false);

  // Set up the offset frequency, regardless of whether or not we will
  // use it

  offsetFrequency_.setGHz(5.0);

  // Set up xyz corrections

  dGeocentricXyz_.resize(3);
  
  dGeocentricXyz_[0] = 0.0;
  dGeocentricXyz_[1] = 0.0;
  dGeocentricXyz_[2] = 0.0;
  
  dTopocentricXyz_.resize(3);
  
  dTopocentricXyz_[0] = 0.0;
  dTopocentricXyz_[1] = 0.0;
  dTopocentricXyz_[2] = 0.0;
}

void DelayAntennaLocation::updateHasFrequency(bool hasFrequency)
{
  hasFrequency_ = hasFrequency;

  if(monHasFrequency_ != 0) 
    monHasFrequency_->writeReg(false, hasFrequency);
}

/**.......................................................................
 * Destructor.
 */
DelayAntennaLocation::~DelayAntennaLocation() 
{
  if(monitor_ != 0) {
    delete monitor_;
    monitor_ = 0;
  }
}

/**.......................................................................
 * Set an id for this antenna
 */
void DelayAntennaLocation::setAntId(AntNum::Id antId)
{
  antNum_.setId(antId);

  if(monitor_ != 0) { 
    unsigned iAnt = antNum_.getIntId();

    monHasFrequency_ = 
      monitor_->addMonitorPoint("delay", "hasFrequency",       iAnt);

    monLOFrequency_ = 
      monitor_->addMonitorPoint("delay", "LOFrequency",        iAnt);

    monSkyFrequency_ = 
      monitor_->addMonitorPoint("delay", "skyFrequency",       iAnt);

    monFixedDelay_ = 
      monitor_->addMonitorPoint("delay", "fixedDelay",         iAnt);

    monUseFixedDelay_ = 
      monitor_->addMonitorPoint("delay", "useFixedDelay",      iAnt);

    monAdjustableDelay_ = 
      monitor_->addMonitorPoint("delay", "adjustableDelay",    iAnt);

    monUseAdjustableDelay_ = 
      monitor_->addMonitorPoint("delay", "useAdjustableDelay", iAnt);

    monUseGeometricDelay_ = 
      monitor_->addMonitorPoint("delay", "useGeometricDelay",  iAnt);

    monUseTroposphericDelay_ = 
      monitor_->addMonitorPoint("delay", "useTroposphericDelay",  iAnt);

    monUseAxisMisalignment_ = 
      monitor_->addMonitorPoint("delay", "useNiaDelay",        iAnt);

    monAxisMisalignment_ = 
      monitor_->addMonitorPoint("delay", "nia",                iAnt);

    monLocation_ = 
      monitor_->addMonitorPoint("delay", "location",           iAnt);

    // Update our monitor points

    updateMonitors();
  }
}

void DelayAntennaLocation::updateMonitors()
{
  // Update monitor points from the base class

  DelayLocation::updateMonitors();

  // Now update our own

  if(monHasFrequency_ != 0) 
    monHasFrequency_->writeReg(false, hasFrequency_);

  if(monLOFrequency_ != 0) 
    monLOFrequency_->writeReg(false, LOFrequency_.Hz());

  if(monSkyFrequency_ != 0) 
    monSkyFrequency_->writeReg(false, skyFrequency_.Hz());

  if(monUseAxisMisalignment_ != 0) 
    monUseAxisMisalignment_->writeReg(false, useAxisMisalignment_);

  if(monAxisMisalignment_ != 0) 
    monAxisMisalignment_->writeReg(false, axisMisalignment_.meters());
}

void DelayAntennaLocation::updateUseAxisMisalignment()
{
  if(monUseAxisMisalignment_ != 0) 
    monUseAxisMisalignment_->writeReg(false, useAxisMisalignment_);
}

/**.......................................................................
 * Select a receiver for this antenna
 */
void DelayAntennaLocation::selectRx(sza::util::Rx::Id rxId)
{
  updateSkyFrequency(sza::util::Rx::getSkyFrequency(rxId));
  updateDefaultDelays(rxId);
  updateLOFrequency(sza::util::Rx::getLOFrequency(rxId));
  updateHasFrequency(true);

  markAsDiscontinuous(true);
}

void DelayAntennaLocation::updateLOFrequency(Frequency freq) 
{
  LOFrequency_  = freq;

  if(monLOFrequency_ != 0) 
    monLOFrequency_->writeReg(false, LOFrequency_.Hz());
}

void DelayAntennaLocation::updateSkyFrequency(Frequency freq) 
{
  skyFrequency_ = freq;

  // Update the frequency in our Atmosphere container too

  location().atmos().setFrequency(freq);

  if(monSkyFrequency_ != 0) 
    monSkyFrequency_->writeReg(false, skyFrequency_.Hz());
}

/**.......................................................................
 * Set an axis misalignment
 */
void DelayAntennaLocation::setAxisMisalignment(Length axisMisalignment)
{
  markAsDiscontinuous(true);
  axisMisalignment_ = axisMisalignment;

  updateAxisMisalignment();
}

void DelayAntennaLocation::updateAxisMisalignment()
{
  if(monAxisMisalignment_ != 0) 
    monAxisMisalignment_->writeReg(false, axisMisalignment_.meters());
}

/**.......................................................................
 * Set the adjustable delay associated with this location
 */
void DelayAntennaLocation::setDefaultAdjustableDelay(Delay delay, Rx::Id id) 
{
  switch(id) {
  case Rx::RX30GHZ:
    default30GHzAdjustableDelay_  = delay;
    break;
  case Rx::RX90GHZ:
    default90GHzAdjustableDelay_  = delay;
    break;
  case Rx::RX230GHZ:
    default230GHzAdjustableDelay_ = delay;
    break;
  default:
    ThrowError("Unrecognized receiver id: " << id);
    break;
  }
}

/**.......................................................................
 * Set the fixed delay associated with this location
 */
void DelayAntennaLocation::setDefaultFixedDelay(Delay delay, Rx::Id id) 
{
  switch(id) {
  case Rx::RX30GHZ:
    default30GHzFixedDelay_  = delay;
    break;
  case Rx::RX90GHZ:
    default90GHzFixedDelay_  = delay;
    break;
  case Rx::RX230GHZ:
    default230GHzFixedDelay_ = delay;
    break;
  default:
    ThrowError("Unrecognized receiver id: " << id);
    break;
  }
}

/**.......................................................................
 * Reset default delay pointers to the right one for this frequency
 */
void DelayAntennaLocation::updateDefaultDelays(Rx::Id id)
{
  switch(id) {
  case Rx::RX30GHZ:
    defaultAdjustableDelay_ = &default30GHzAdjustableDelay_;
    defaultFixedDelay_      = &default30GHzFixedDelay_;
    break;
  case Rx::RX90GHZ:
    defaultAdjustableDelay_ = &default90GHzAdjustableDelay_;
    defaultFixedDelay_      = &default90GHzFixedDelay_;
    break;
  case Rx::RX230GHZ:
    defaultAdjustableDelay_ = &default230GHzAdjustableDelay_;
    defaultFixedDelay_      = &default230GHzFixedDelay_;
    break;
  default:
    ThrowError("Unrecognized receiver id: " << id);
    break;
  }

  // Now install these delays

  setAdjustableDelay(*defaultAdjustableDelay_);
  setFixedDelay(*defaultFixedDelay_);
}

/**.......................................................................
 * Set up whether we are using the axis misalignment term
 */
void DelayAntennaLocation::useAxisMisalignment(bool use)
{
  if(use != useAxisMisalignment_)
    markAsDiscontinuous(true);
  useAxisMisalignment_ = use;
  
  updateUseAxisMisalignment();
}

/**.......................................................................
 * Update quantities needed for delay calculation
 */
void DelayAntennaLocation::
updateTransientDelayQuantities(double mjd, sza::util::Source* src)
{
  ha_  = getHa(mjd, src);
  dec_ = src->getDec(mjd);

  updateAzEl(ha_, dec_);
  updateAxisMisalignmentCorrection(ha_, dec_);
}

/**.......................................................................
 * Get the AZ/EL coordinates corresponding to this HA/DEC location
 */
void DelayAntennaLocation::updateAzEl(HourAngle ha, DecAngle dec)
{
  // Get the az/el coordinates corresponding to this ha/dec
  // combination for the antenna
  
  Vector<Angle> azEl = 
    Coordinates::laAndHaDecToAzEl(latitude(false), altitude(false), ha, dec, 
				  false);

  azimuth_   = azEl[0];
  elevation_ = azEl[1];

  // Correct the elevation for refraction, if possible

  if(atmos().canComputeRefraction()) {
    elevation_ += atmos_.offset(elevation_);
  }
}

/**.......................................................................
 * Get the AZ/EL coordinates corresponding to this HA/DEC location
 */
void DelayAntennaLocation::updateAxisMisalignmentCorrection(HourAngle ha, DecAngle dec)
{
  if(useAxisMisalignment_) {

    // Elevation axis misalignment is a N displacement when the
    // telescope is pointed at AZ=0

    double dN = axisMisalignment_.meters() * cos(azimuth_.radians());
    double dE = axisMisalignment_.meters() * sin(azimuth_.radians());

    // Update the geocentric (X, Y, Z) representation of this location
  
    dGeocentricXyz_  = Coordinates::laAndUenToXyz(latitude(true), altitude(true), 
					       up(), east()+dE, north()+dN, true);
    
    dGeocentricXyz_[0] -= geocentricXyz_[0];
    dGeocentricXyz_[1] -= geocentricXyz_[1];
    dGeocentricXyz_[2] -= geocentricXyz_[2];

    // Update the topocentric (X, Y, Z) representation of this location
    
    dTopocentricXyz_ = Coordinates::laAndUenToXyz(latitude(true), altitude(true), 
						  up(), east()+dE, north()+dN, false);

    dTopocentricXyz_[0] -= topocentricXyz_[0];
    dTopocentricXyz_[1] -= topocentricXyz_[1];
    dTopocentricXyz_[2] -= topocentricXyz_[2];
  }
}

double DelayAntennaLocation::X(bool ec) 
{
  if(ec) {
    return useAxisMisalignment_ ? geocentricXyz_[0] + dGeocentricXyz_[0] : geocentricXyz_[0];
  } else {
    return useAxisMisalignment_ ? topocentricXyz_[0] + dTopocentricXyz_[0] : topocentricXyz_[0];
  }
}

double DelayAntennaLocation::Y(bool ec) 
{
  if(ec) {
    return useAxisMisalignment_ ? geocentricXyz_[1] + dGeocentricXyz_[1] : geocentricXyz_[1];
  } else {
    return useAxisMisalignment_ ? topocentricXyz_[1] + dTopocentricXyz_[1] : topocentricXyz_[1];
  }
}

double DelayAntennaLocation::Z(bool ec) 
{
  if(ec) {
    return useAxisMisalignment_ ? geocentricXyz_[2] + dGeocentricXyz_[2] : geocentricXyz_[2];
  } else {
    return useAxisMisalignment_ ? topocentricXyz_[2] + dTopocentricXyz_[2] : topocentricXyz_[2];
  }
}
