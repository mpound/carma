#include "carma/szautil/AntNum.h"
#include "carma/szautil/DelayManager.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/MonitorPoint.h"
#include "carma/szautil/MonitorPointManager.h"

using namespace sza::util;
using namespace std;

/**.......................................................................
 * Constructor.
 */
DelayManager::DelayManager(RegMapDataFrameManager* frame) 
{
  monitor_ = 0;

  monLon_    = 0;
  monLat_    = 0;
  monAlt_    = 0;
  monRefAnt_ = 0;

  if(frame != 0) {
    monitor_ = new MonitorPointManager(frame);

    monLon_    = monitor_->addMonitorPoint("delay", "siteFiducial", 0);
    monLat_    = monitor_->addMonitorPoint("delay", "siteFiducial", 1);
    monAlt_    = monitor_->addMonitorPoint("delay", "siteFiducial", 2);
    monRefAnt_ = monitor_->addMonitorPoint("delay", "referenceAntenna");
    monRefLoc_ = monitor_->addMonitorPoint("delay", "referenceLocation");
  }

  // Initialize the ref antenna pointer to NULL
  
  refLoc_ = &delayRefLocation_;
  refLoc_->registerLocationCallback(this);

  // Initialize antenna locations and register to be called when
  // anything changes

  for(AntNum antiter(AntNum::ANT0); antiter <= AntNum(AntNum::ANTMAX); antiter++) {

    DelayAntennaLocation* antLoc = new DelayAntennaLocation(frame);

    antennaLocations_.push_back(antLoc);

    // Set the antenna id of this antenna
    
    antLoc->setAntId(antiter.getId());

    // And register to be called back if anything changes

    antLoc->registerDelayCallback(this);
    antLoc->registerLocationCallback(this);
  }

  updateSiteMonitor(Angle(), Angle(), 0.0);
  updateRefAntMonitor("none");
  updateRefLocationMonitor();
}

/**.......................................................................
 * Record a new site in the database
 */
void DelayManager::updateSiteMonitor(Angle longitude, Angle latitude, double altitude)
{
  if(monLon_ != 0) 
    monLon_->writeReg(false, longitude.radians());

  if(monLat_ != 0) 
    monLat_->writeReg(false, latitude.radians());

  if(monAlt_ != 0) 
    monAlt_->writeReg(false, altitude);
}

/**.......................................................................
 * Destructor.
 */
DelayManager::~DelayManager() 
{
  for(unsigned iant=0; iant < antennaLocations_.size(); iant++)
    if(antennaLocations_[iant] != 0) {
      delete antennaLocations_[iant];
      antennaLocations_[iant] = 0;
    }
}

/**.......................................................................
 * Set a reference antenna.
 *
 * Passing AnNum::ANTNONE resets the reference location to the
 * arbitrary one.
 */
void DelayManager::setRefAntenna(AntNum::Id antId)
{
  // Invalid antenna id means reset to the arbitrary delay reference

  if(antId == AntNum::ANTNONE) {
    refLoc_ = &delayRefLocation_;
    updateRefAntMonitor("none");
  } else {
    AntNum antNum(antId);

    // Else set the requested antenna as the new delay reference
    
    unsigned iant = antNum.getIntId();
    refLoc_       = antennaLocations_[iant];

    updateRefAntMonitor((char*)antNum.getAntennaName().c_str());
  }

  // Update the delay reference location in the monitor database

  updateRefLocationMonitor();

  // And update referred delays for all antennas
  
  updateDelays();
}

void DelayManager::updateRefAntMonitor(char* ant)
{
  //  std::cout << "Inside updatRefAntMonitor: monRefAnt_ = " 
  //	    << (monRefAnt_ == NULL ? "NULL" : "not NULL") << std::endl;

  if(monRefAnt_ != 0)
    monRefAnt_->writeReg(false, (unsigned char*)ant);
}

void DelayManager::updateRefLocationMonitor()
{
  if(monRefLoc_ != 0) {
    Vector<double>uen = refLoc_->location().getUen();
    monRefLoc_->writeReg(false, &uen[0]);
  }
}

/**.......................................................................
 * Update delays for all antennas
 */
void  DelayManager::updateDelays()
{
  // Update the referred delays for all antennas
  
  for(unsigned iant=0; iant < antennaLocations_.size(); iant++) {
    DelayLocation* antLoc = antennaLocations_[iant];
    
    antLoc->referFixedDelayTo(refLoc_->fiducialFixedDelay());
    antLoc->referAdjustableDelayTo(refLoc_->fiducialAdjustableDelay());
  }
}

/**.......................................................................
 * Set the location of the global reference point in all relevant
 * containers.
 */
void DelayManager::setSite(Angle longitude, Angle latitude, double altitude)
{
  delayRefLocation_.location().setFiducialSite(longitude, latitude, altitude);
  
  for(unsigned iant=0; iant < antennaLocations_.size(); iant++)
    antennaLocations_[iant]->location().setFiducialSite(longitude, latitude, altitude);

  // And update the corresponding monitor points in the database

  updateSiteMonitor(longitude, latitude, altitude);
}

/**.......................................................................
 * Set a delay reference location
 */
void DelayManager::setDelayRefLocation(double up, double east, 
				       double north)
{
  refLoc_ = &delayRefLocation_;
  refLoc_->location().setOffset(up, east, north);

  // Update the ref ant in the database

  updateRefAntMonitor("none");

  // Update the delay reference location in the monitor database

  updateRefLocationMonitor();

  // And update referred delays for all antennas
  
  updateDelays();

}

/**.......................................................................
 * Return the current delay reference location
 */
DelayAntennaLocation* DelayManager::getDelayRef()
{
  return refLoc_;
}

/**.......................................................................
 * Set an antenna location
 */
void DelayManager::setAntennaLocation(AntNum::Id antId, 
				      double up, double east, double north)
{
  unsigned iant = AntNum::idToInt(antId);
  
  antennaLocations_[iant]->location().setOffset(up, east, north);
}

/**.......................................................................
 * Return an antenna location
 */
DelayAntennaLocation* DelayManager::getAntenna(AntNum::Id antId)
{
  unsigned iant = AntNum::idToInt(antId);

  return antennaLocations_[iant];
}

/**.......................................................................
 * A function called whenever an outside caller changes a delay
 */
void DelayManager::delaysChanged(DelayLocation* loc)
{
  // If it was our reference location, we must update all delays
  // referred to that reference

  DBPRINT(true, Debug::DEBUG4, "Inside delaysChanged: " << (refLoc_==loc));

  if(refLoc_ == loc) 
    updateDelays();
}

/**.......................................................................
 * A function called whenever an outside caller changes a
 * delay location
 */
void DelayManager::locationChanged(DelayLocation* loc)
{
  if(refLoc_ == loc)
    markAsDiscontinuous(AntNum::ANTALL, true);
}

/**.......................................................................
 * Mark parameters for one or more antennas as discontinuous
 */
void DelayManager::markAsDiscontinuous(AntNum::Id antennas, bool disc)
{
  AntNum antSet(antennas);

  for(AntNum antiter(AntNum::ANT0); antiter <= AntNum(AntNum::ANTMAX); antiter++)
    if(antSet.isSet(antiter)) 
      antennaLocations_[antiter.getIntId()]->markAsDiscontinuous(disc);
}

/**.......................................................................
 * Extend the ephemeris of the equation of the equinoxes.
 */
void DelayManager::extendEqnEqx(double mjdTt, double eqnEqx)
{
  delayRefLocation_.location().ephem().extendEqnEqx(mjdTt, eqnEqx);
  
  for(unsigned iant=0; iant < antennaLocations_.size(); iant++)
    antennaLocations_[iant]->location().ephem().extendEqnEqx(mjdTt, eqnEqx);
}

/**.......................................................................
 * Extend the UT1-UTC ephemeris
 */
void DelayManager::extendUt1Utc(double mjdUtc, double ut1Utc)
{
  delayRefLocation_.location().ephem().extendUt1Utc(mjdUtc, ut1Utc);
  
  for(unsigned iant=0; iant < antennaLocations_.size(); iant++)
    antennaLocations_[iant]->location().ephem().extendUt1Utc(mjdUtc, ut1Utc);
}
