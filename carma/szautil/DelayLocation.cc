#include "carma/szautil/DelayLocation.h"
#include "carma/szautil/DelayManager.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/MonitorPoint.h"

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
DelayLocation::DelayLocation()
{
  monAdjustableDelay_ = 0;
  monUseAdjustableDelay_ = 0;
  monFixedDelay_ = 0;
  monUseFixedDelay_ = 0;
  monGeometricDelay_ = 0;
  monUseGeometricDelay_ = 0;
  monTroposphericDelay_ = 0;
  monUseTroposphericDelay_ = 0;
  monLocation_ = 0;

  delayListener_      = 0;
  locationListener_   = 0;

  markAsDiscontinuous(true);

  // Register to be called when our location changes

  Location::registerLocationCallback(this);
}

/**.......................................................................
 * Destructor.
 */
DelayLocation::~DelayLocation() 
{
}

/**.......................................................................
 * Refer the fixed delay to a different reference point
 */
void DelayLocation::referFixedDelayTo(Delay delay)
{
  referredFixedDelay_ = fiducialFixedDelay_ - delay;

  updateFixedDelay();

  if(useFixedDelay_)
    markAsDiscontinuous(true);
}

/**.......................................................................
 * Refer the adjustable delay to a different reference point
 */
void DelayLocation::referAdjustableDelayTo(Delay delay) 
{
  DBPRINT(true, DEBUG_DELAY, "Old referred: " << referredAdjustableDelay_
	  << " Referring to: " << (fiducialAdjustableDelay_ - delay));

  referredAdjustableDelay_ = fiducialAdjustableDelay_ - delay;

  updateAdjustableDelay();

  if(useAdjustableDelay_)
    markAsDiscontinuous(true);
}

/**.......................................................................
 * Set the fixed delay associated with this location
 */
void DelayLocation::setFixedDelay(Delay delay) 
{
  // Get the old reference delay

  Delay oldRefDelay = fiducialFixedDelay_ - referredFixedDelay_;

  // Store the new fiducial

  fiducialFixedDelay_ = delay;

  // And refer the fixed delay relative to the new fiducial

  referFixedDelayTo(oldRefDelay);

  // Update the value of the fixed delay in the database

  updateFixedDelay();

  // If someone was registered to be notified, notify them now
  
  if(delayListener_ != 0)
    delayListener_->delaysChanged(this);
}

void DelayLocation::updateFixedDelay()
{
  //  std::cout << fiducialFixedDelay_.nanoSeconds() << std::endl;

  if(monFixedDelay_ != 0)
    monFixedDelay_->writeReg(false, fiducialFixedDelay_.nanoSeconds());
}

/**.......................................................................
 * Set the adjustable delay associated with this location
 */
void DelayLocation::setAdjustableDelay(Delay delay) 
{
  // Get the old reference delay

  Delay oldRefDelay = fiducialAdjustableDelay_ - referredAdjustableDelay_;

  // Store the new fiducial

  fiducialAdjustableDelay_ = delay;

  // And refer the adjustable delay relative to the new fiducial

  referAdjustableDelayTo(oldRefDelay);

  // If someone was registered to be notified, notify them now
  
  if(delayListener_ != 0)
    delayListener_->delaysChanged(this);
}

void DelayLocation::updateAdjustableDelay()
{
  if(monAdjustableDelay_ != 0)
    monAdjustableDelay_->writeReg(false, referredAdjustableDelay_.nanoSeconds());
}

void DelayLocation::updateUseFixedDelay()
{
  if(monUseFixedDelay_ != 0)
    monUseFixedDelay_->writeReg(false, useFixedDelay_);
}

void DelayLocation::updateUseAdjustableDelay()
{
  if(monUseAdjustableDelay_ != 0)
    monUseAdjustableDelay_->writeReg(false, useAdjustableDelay_);
}

void DelayLocation::updateUseGeometricDelay()
{
  if(monUseGeometricDelay_ != 0)
    monUseGeometricDelay_->writeReg(false, useGeometricDelay_);
}

void DelayLocation::updateUseTroposphericDelay()
{
  if(monUseTroposphericDelay_ != 0)
    monUseTroposphericDelay_->writeReg(false, useTroposphericDelay_);
}

void DelayLocation::updateLocation()
{
  if(monLocation_ != 0) {
    Vector<double>uen = getUen();
    monLocation_->writeReg(false, &uen[0]);
  }
}

void DelayLocation::updateMonitors()
{
  updateAdjustableDelay();
  updateFixedDelay();
  updateUseAdjustableDelay();
  updateUseGeometricDelay();
  updateUseTroposphericDelay();
  updateLocation();
}

/**.......................................................................
 * Register with this object to be notified when its parameters change.
 */
void DelayLocation::registerDelayCallback(DelayManager* delayListener)
{
  delayListener_ = delayListener;
}

/**.......................................................................
 * Toggle using various delays
 */
void DelayLocation::useFixedDelay(bool use)
{
  if(use != useFixedDelay_)
    markAsDiscontinuous(true);
  useFixedDelay_ = use;

  updateUseFixedDelay();
}

/**.......................................................................
 * Toggle using various delays
 */
void DelayLocation::useAdjustableDelay(bool use)
{
  if(use != useAdjustableDelay_)
    markAsDiscontinuous(true);
  useAdjustableDelay_ = use;

  updateUseAdjustableDelay();
}

void DelayLocation::useGeometricDelay(bool use)
{
  if(use != useGeometricDelay_)
    markAsDiscontinuous(true);
  useGeometricDelay_ = use;
  
  updateUseGeometricDelay();
}

void DelayLocation::useIonosphericDelay(bool use)
{
  if(use != useIonosphericDelay_)
    markAsDiscontinuous(true);
  useIonosphericDelay_ = use;
}

void DelayLocation::useTroposphericDelay(bool use)
{
  if(use != useTroposphericDelay_)
    markAsDiscontinuous(true);
  useTroposphericDelay_ = use;

  updateUseTroposphericDelay();
}

void DelayLocation::useThermalDelay(bool use)
{
  if(use != useThermalDelay_)
    markAsDiscontinuous(true);
  useThermalDelay_ = use;
}

/**.......................................................................
 * Get the total delay for an Ha Dec source position
 */
Delay DelayLocation::totalDelay(double mjd, sza::util::Source* src,
				DelayAntennaLocation* refDLoc,
				bool doMotionCorrection)
{
  Delay delay;

  if(useAdjustableDelay_)
    delay += adjustableDelay();
  if(useGeometricDelay_)
    delay += geometricDelay(refDLoc, doMotionCorrection);
  if(useTroposphericDelay_)
    delay += troposphericDelay(refDLoc);
  if(useFixedDelay_)
    delay += fixedDelay();
  
  return delay;
}


/**.......................................................................
 * Get the total delay for an Az El source position
 */
Delay DelayLocation::totalDelay(Angle az, Angle el,
				DelayAntennaLocation* refDLoc)
{
  Delay delay;

  if(useAdjustableDelay_)
    delay += adjustableDelay();
  if(useGeometricDelay_)
    delay += geometricDelay(az, el, refDLoc);
  if(useFixedDelay_)
    delay += fixedDelay();

  return delay;
}

/**.......................................................................
 * Get geometric delay for an Ha Dec source position
 */
Delay DelayLocation::geometricDelay(DelayAntennaLocation* refDLoc,
				    bool doMotionCorrection)
{
  lastGeometricDelay_ = Location::geometricDelay(refDLoc, doMotionCorrection);
  return lastGeometricDelay_;
}

/**.......................................................................
 * Get geometric delay for an Az El source position
 */
Delay DelayLocation::geometricDelay(Angle az, Angle el,
				    DelayAntennaLocation* refDLoc)
{
  lastGeometricDelay_ = Location::geometricDelay(az, el, refDLoc);
  return lastGeometricDelay_;
}

/**.......................................................................
 * Update the geometric delay in the register database
 */
void DelayLocation::updateGeometricDelay()
{
  if(monGeometricDelay_ != 0)
    monGeometricDelay_->writeReg(false, lastGeometricDelay_.nanoSeconds());
}

/**.......................................................................
 * Get tropospheric delay for an Ha Dec source position
 */
Delay DelayLocation::troposphericDelay(DelayAntennaLocation* refDLoc)
{
  lastTroposphericDelay_ = Location::troposphericDelay(refDLoc);
  return lastTroposphericDelay_;
}

/**.......................................................................
 * Update the tropospheric delay in the register database
 */
void DelayLocation::updateTroposphericDelay()
{
  if(monTroposphericDelay_ != 0)
    monTroposphericDelay_->writeReg(false, lastTroposphericDelay_.nanoSeconds());
}

/**.......................................................................
 * Register with this object to be notified when its location changes
 */
void DelayLocation::registerLocationCallback(DelayManager* locationListener)
{
  locationListener_ = locationListener;
}

/**.......................................................................
 * A function called whenever an outside caller changes a location
 */
void DelayLocation::locationChanged(Location* loc)
{ 
  DBPRINT(true, Debug::DEBUG4, "A location changed");

  // Update relevant monitor points

  updateLocation();
  markAsDiscontinuous(true);

  // And call anyone who's registered

  if(locationListener_ != 0)
    locationListener_->locationChanged(this);
}
