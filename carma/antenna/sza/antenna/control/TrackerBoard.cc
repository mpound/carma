/**
 * With gcc.3.2.2, this must come first, or else the compiler gets
 * confused.
 */
#include "carma/antenna/sza/antenna/control/TrackerBoard.h"

/**
 * Put these second.
 */
#include "carma/szautil/PointingParameter.h"

#include "carma/antenna/sza/antenna/control/Atmosphere.h"
#include "carma/antenna/sza/antenna/control/AxisPositions.h"
#include "carma/antenna/sza/antenna/control/Model.h"
#include "carma/antenna/sza/antenna/control/PmacTarget.h"
#include "carma/antenna/sza/antenna/control/Pointing.h"
#include "carma/antenna/sza/antenna/control/Position.h"
#include "carma/antenna/sza/antenna/control/SzaShare.h"
#include "carma/antenna/sza/antenna/control/TrackerOffset.h"

#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/RegDate.h"
#include "carma/szautil/TrackerFlags.h"
#include "carma/szautil/TrackingStatus.h"

using namespace std;
using namespace sza::util;
using namespace sza::antenna::control;

/**.......................................................................
 * Constructor for the Tracker board class.
 */
TrackerBoard::TrackerBoard(SzaShare* share, char* name) : Board(share, name)
{
  // Initialize pointers to registers of this board

  lacking_     = 0;
  utc_         = 0;
  lst_         = 0;
  ut1utc_      = 0;
  eqneqx_      = 0;
  mode_        = 0;
  deck_mode_   = 0;
  refraction_  = 0;
  encoder_off_ = 0;
  encoder_mul_ = 0;
  az_limits_   = 0;
  el_limits_   = 0;
  dk_limits_   = 0;
  tilts_       = 0;
  flexure_     = 0;
  axis_        = 0;
  collimation_ = 0;
  siteActual_  = 0;
  siteFiducial_= 0;
  location_    = 0;
  source_      = 0;
  equat_geoc_  = 0;
  equat_off_   = 0;
  horiz_geoc_  = 0;
  horiz_topo_  = 0;
  horiz_mount_ = 0;
  horiz_off_   = 0;
  sky_xy_off_  = 0;
  counts_      = 0;
  rates_       = 0;
  actual_      = 0;
  expected_    = 0;
  errors_      = 0;
  state_       = 0;
  stateMask_   = 0;
  off_source_  = 0;

  // Look up relevant registers of the board

  lacking_     = findReg("lacking");
  utc_         = findReg("utc");
  lst_         = findReg("lst");
  ut1utc_      = findReg("ut1utc");
  eqneqx_      = findReg("eqneqx");
  mode_        = findReg("mode");
  deck_mode_   = findReg("deck_mode");
  refraction_  = findReg("refraction");
  encoder_off_ = findReg("encoder_off");
  encoder_mul_ = findReg("encoder_mul");
  az_limits_   = findReg("az_limits");
  el_limits_   = findReg("el_limits");
  dk_limits_   = findReg("dk_limits");
  tilts_       = findReg("tilts");
  flexure_     = findReg("flexure");
  axis_        = findReg("axis");
  collimation_ = findReg("collimation");
  siteActual_  = findReg("siteActual");
  siteFiducial_= findReg("siteFiducial");
  location_    = findReg("location");
  source_      = findReg("source");
  equat_geoc_  = findReg("equat_geoc");
  equat_off_   = findReg("equat_off");
  horiz_geoc_  = findReg("horiz_geoc");
  horiz_topo_  = findReg("horiz_topo");
  horiz_mount_ = findReg("horiz_mount");
  horiz_off_   = findReg("horiz_off");
  sky_xy_off_  = findReg("sky_xy_off");
  counts_      = findReg("counts");
  rates_       = findReg("rates");
  actual_      = findReg("actual");
  expected_    = findReg("expected");
  errors_      = findReg("errors");
  state_       = findReg("state");
  stateMask_   = findReg("stateMask");
  off_source_  = findReg("off_source");
}

/**.......................................................................
 * Archive the current and expected positions of the telescope axes.
 *
 * Input:
 *
 *  current    AxisPositions *  The current position of the telescope.
 *  commanded  Position      *  The requested position of the telescope.
 */
void TrackerBoard::archivePosition(AxisPositions *current, Position* commanded)
{
  LogStream errStr;
  signed s_elements[3]; // Signed register values to record 

  // We want the following registers to be archived with contemporary
  // values, so prevent archival while the tracker board is being
  // updated.

  DBPRINT(true, Debug::DEBUG14, "Inside");

  try {
    
    share_->lock();

    // Record the current positions of the telescope axes as returned
    // from the pmac.

    current->pack(s_elements);
    share_->writeRegNoLock(actual_, s_elements);

    // Record the expected positions of the telescope axes commanded
    // two ticks ago.
    
    commanded->pack(s_elements);  
    share_->writeRegNoLock(expected_, s_elements);
    
    // Record the difference between the expected and actual
    // positions, in mas
    
    double diff = (commanded->az_ - current->az_.topo_) * rtomas;
    s_elements[0] = static_cast<signed>(diff); 

    diff = (commanded->el_ - current->el_.topo_) * rtomas;
    s_elements[1] = static_cast<signed>(diff);

    diff = (commanded->pa_ - current->pa_.topo_) * rtomas;
    s_elements[2] = static_cast<signed>(diff);
    
    share_->writeRegNoLock(errors_, s_elements);

  } catch (Exception& err) {
    errStr.appendMessage(true, err.what());
  } catch(...) {
    errStr.appendMessage(true, "Caught an unknown exception\n");
  }

  // Release the archive.
  
  try {
    share_->unlock();
  } catch (Exception& err) {
    errStr.appendMessage(true, err.what());
  } catch(...) {
    errStr.appendMessage(true, "Caught an unknown exception\n");
  }

  if(errStr.isError())
    throw Error(errStr);
}
//-----------------------------------------------------------------------
// Private methods for packing data for archiving

/**.......................................................................
 * Pack the lst for archival into the register database.
 */
void TrackerBoard::packLst(unsigned* u_elements, double lst)
{
  u_elements[0] = static_cast<unsigned int>(lst * (daysec / twopi) * 1000.0);
}

/**.......................................................................
 * Pack the ut1-utc correction.
 */
void TrackerBoard::packUt1Utc(signed* s_elements, double ut1utc)
{
  s_elements[0] = static_cast<signed>(ut1utc * 1000.0);
}

/**.......................................................................
 * Pack the equation of the equinoxes.
 */
void TrackerBoard::packEqnEqx(signed* s_elements, double eqneqx)
{
  s_elements[0] = static_cast<signed>(eqneqx * daysec / twopi * 1000.0);
}

/*.......................................................................
 * Record the latest pointing parameters for archival.
 */
void TrackerBoard::archivePointing(unsigned* archived_ptr,
				   Atmosphere* atmosphere,
				   Model* model, 
				   PmacTarget *pmac,
				   Pointing* pointing, 
				   Site* site,
				   TrackerOffset* offset)
{
  LogStream errStr;
  unsigned u_elements[10];           // Unsigned register values to record 
  signed s_elements[10];             // Signed register values to record 
  double eqneqx;                     // The equation of the equinoxes 
  double ut1utc;                     // The value of UT1 - UTC 
  unsigned archived = *archived_ptr; // Store a copy of the bitmask

  // Get the UTC, the local apparent sidereal time and the terrestrial
  // time at the given time.

  RegDate date = pointing->getDate();

  double utc = date.mjd();
  double lst = share_->getLst(utc);
  double tt  = share_->getTt(utc);
  
  // Get the equation of the equinoxes and UT1-UTC.

  eqneqx = share_->getEqnEqx(tt);
  ut1utc = share_->getUt1Utc(utc);

  // We want all registers to be archived with contemporary values, so
  // prevent archival while the tracker board is being updated.
  //
  // Enclose this in a try{} construction so that a thrown exception
  // won't result in a lock remaining on the register database

  DBPRINT(true, Debug::DEBUG14, "Inside archivePointing()");

  try {

    share_->lock();

    // Record the MJD UTC in days and milli-seconds.

    share_->writeRegNoLock(utc_, date.data());

    // Record the local apparent sidereal time in milli-seconds.

    packLst(u_elements, lst);
    share_->writeRegNoLock(lst_, u_elements);

    // Record the value of UT1-UTC in milli-seconds.

    packUt1Utc(s_elements, ut1utc);
    share_->writeRegNoLock(ut1utc_, s_elements);

    // Record the value of the equation of the equinoxes from radians
    // to milliseconds of time (lst).

    packEqnEqx(s_elements, eqneqx);
    share_->writeRegNoLock(eqneqx_, s_elements);

    // Record the type of pointing.

    pmac->packMode(u_elements);
    share_->writeRegNoLock(mode_, u_elements);

    // Record the refraction parameters in micro-arcseconds.

    if(~archived & PointingParameter::ATMOSPHERE) {

      atmosphere->currentRefraction()->pack(s_elements);

      // Add in the applied refraction corrections

      s_elements[2] = static_cast<signed>(pointing->getRefraction() * rtomas);

      share_->writeRegNoLock(refraction_, s_elements);
      archived |= PointingParameter::ATMOSPHERE;

    } else {
      CoordRange range(2);
      int refracCorr = static_cast<signed>(pointing->getRefraction() * rtomas);
      share_->writeRegNoLock(refraction_, refracCorr, &range);
    };

    // Record the encoder zero points in milli-arcseconds.

    if(~archived & PointingParameter::ZEROS) {
      model->packEncoderZeros(s_elements);
      share_->writeRegNoLock(encoder_off_, s_elements);
    };

    // Record encoder counts/turn.

    if(~archived & PointingParameter::ENCODERS) {
      model->packEncoderMultipliers(s_elements);
      share_->writeRegNoLock(encoder_mul_, s_elements);
      archived |= PointingParameter::ENCODERS;
    };

    // Record the encoder limits as topocentric mount angles.

    if(~archived & PointingParameter::LIMITS) {

      // The azimuth limits (milli-arcsec).

      model->Encoder(Axis::AZ)->packLimits(s_elements);
      share_->writeRegNoLock(az_limits_, s_elements);

      // The elevation limits (milli-arcsec).

      model->Encoder(Axis::EL)->packLimits(s_elements);
      share_->writeRegNoLock(el_limits_, s_elements);

      // The deck limits (milli-arcsec).

      model->Encoder(Axis::PA)->packLimits(s_elements);
      share_->writeRegNoLock(dk_limits_, s_elements);
      archived |= PointingParameter::LIMITS;
    };

    // Record the axis tilts in milli-arcsec.

    if(~archived & PointingParameter::TILTS) {
      model->packTilts(s_elements);
      share_->writeRegNoLock(tilts_, s_elements);
      archived |= PointingParameter::TILTS;
    };

    // Record the gravitational flexure in milli-arcsec.

    if(~archived & PointingParameter::FLEXURE) {
      model->packFlexure(s_elements);
      share_->writeRegNoLock(flexure_, s_elements);
      archived |= PointingParameter::FLEXURE;
    };

    // Record whether radio collimation (else optical) is being used.

    model->packCollimationMode(u_elements);
    share_->writeRegNoLock(axis_, u_elements);

    // Record the radio collimation parameters in milli-arcseconds.

    if(~archived & PointingParameter::COLLIMATION) {

      model->packCollimation(s_elements);
      share_->writeRegNoLock(collimation_, s_elements);

      archived |= PointingParameter::COLLIMATION;
    };

    // Record the site-location parameters.

    if(~archived & PointingParameter::SITE) {

      site->packActual(s_elements);
      CoordRange range(0,2);
      share_->writeRegNoLock(siteActual_, s_elements, &range);

      COUT("Site is: " 
	   << s_elements[0] << " "  
	   << s_elements[1] << " "  
	   << s_elements[2] << " "  
	   << s_elements[3]);
      
      site->packFiducial(s_elements);
      share_->writeRegNoLock(siteFiducial_, s_elements);

      archived |= PointingParameter::SITE;
    };

    // Record the antenna-location parameters.

    if(~archived & PointingParameter::LOCATION) {
      site->packOffset(s_elements);
      share_->writeRegNoLock(location_, s_elements);
      archived |= PointingParameter::LOCATION;
    };

    // Pack the source name

    share_->writeRegNoLock(source_, pointing->getName());

    // Record the geocentric apparent equatorial coordinates of the
    // source.

    pointing->packEquatGeoc(s_elements);
    share_->writeRegNoLock(equat_geoc_, s_elements);

    // Record any temporary equatorial tracking offsets.

    offset->packEquatOffset(s_elements);
    share_->writeRegNoLock(equat_off_, s_elements);

    // Record the geocentric apparent az,el,pa of the source.

    pointing->packHorizGeoc(s_elements);
    share_->writeRegNoLock(horiz_geoc_, s_elements);

    // Record the topocentric az,el,pa of the source.

    pointing->packHorizTopo(s_elements);
    share_->writeRegNoLock(horiz_topo_, s_elements);

    // Record the instrumental-apparent az,el,pa of the source.

    pointing->packHorizMount(s_elements);
    share_->writeRegNoLock(horiz_mount_, s_elements);

    // Record any temporary az,el,dk tracking offsets.

    offset->packHorizOffset(s_elements);
    share_->writeRegNoLock(horiz_off_, s_elements);

    // Record any temporary sky-based tracking offsets.

    offset->packSkyOffset(s_elements);
    share_->writeRegNoLock(sky_xy_off_, s_elements);

    // Record the target encoder counts verbatim.

    pmac->packCounts(s_elements);
    share_->writeRegNoLock(counts_, s_elements);

    // Record the target encoder rates (milli-counts/second).

    pmac->packRates(s_elements);
    share_->writeRegNoLock(rates_, s_elements);

  } catch (Exception& err) {
    errStr.appendMessage(true, err.what());
  } catch(...) {
    errStr.appendMessage(true, "Caught an unknown exception\n");
  }

  // Release the archive.  Enclose in a try-catch clause so we don't
  // lose an exception we may ahve caught earlier if another is thrown
  // in the process.
  
  try {
    
    share_->unlock();
    
    // Return the new bitmask of archived paraters
    
    *archived_ptr = static_cast<PointingParameter::Parameter>(archived);
    
  } catch (Exception& err) {
    errStr.appendMessage(true, err.what());
  } catch(...) {
    errStr.appendMessage(true, "Caught an unknown exception\n");
  }

  // If we caught an exception, throw another.

  if(errStr.isError())
    throw Error(errStr);
}

/**.......................................................................
 * Record the current tracking status in the archive database
 */
void TrackerBoard::archiveStatus(unsigned state, unsigned off_source,
				 unsigned lacking)
{
  LogStream errStr;

  // Enclose this in a try{} construction so that a thrown exception
  // won't result in a lock remaining on the register database
  
  try {
    
    share_->lock();
    
    share_->writeRegNoLock(state_, (unsigned char)state);
    share_->writeRegNoLock(stateMask_, trackerStateToBit(state));

    share_->writeRegNoLock(off_source_, off_source);
    
    // Record the bit-mask of missing resources.
    
    share_->writeRegNoLock(lacking_, &lacking);

  } catch(Exception& err) {
    cout << err.what() << endl;
    errStr.appendMessage(true, "Caught an exception writing registers");
  }

  // Release the archive.

  try {
    share_->unlock();
  } catch (Exception& err) {
    errStr.appendMessage(true, err.what());
  } catch(...) {
    errStr.appendMessage(true, "Caught an unknown exception\n");
  }

  if(errStr.isError())
    throw Error(errStr);
}

/**.......................................................................
 * Convert from integer state to a bit mask
 */
unsigned char TrackerBoard::trackerStateToBit(int state)
{
  switch(state) {
  case TrackingStatus::LACKING:
    return TrackerFlags::LACKING;
    break;
  case TrackingStatus::TIME_ERROR:
    return TrackerFlags::TIME_ERROR;
    break;
  case TrackingStatus::UPDATING:
    return TrackerFlags::UPDATING;
    break;
  case TrackingStatus::HALTED:
    return TrackerFlags::HALT;
    break;
  case TrackingStatus::SLEWING:
    return TrackerFlags::SLEW;
    break;
  case TrackingStatus::TRACKING:
    return TrackerFlags::TRACK;
    break;
  case TrackingStatus::TOO_LOW:
    return TrackerFlags::TOO_LOW;
    break;
  case TrackingStatus::TOO_HIGH:
    return TrackerFlags::TOO_HIGH;
    break;
  default:
    ThrowError("Unrecognized state");
    break;
  }
}
