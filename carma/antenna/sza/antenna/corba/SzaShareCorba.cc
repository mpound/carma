#include "carma/antenna/sza/antenna/corba/CarmaMonitorPointHandler.h"
#include "carma/antenna/sza/antenna/corba/SzaShareCorba.h"

using namespace std;

using namespace sza::antenna::control;
using namespace sza::antenna::corba;
using namespace sza::util;

#if DIR_USE_ANT_CORBA
#include "carma/services/Angle.h"
#include "carma/services/Length.h"
#endif

/**.......................................................................
 * Constructor.
 */
SzaShareCorba::SzaShareCorba(std::string host) : SzaShare(host) 
{
  monitorPointHandler_    = 0;
  lastRequestedPosition_  = AZEL;
  nSinceLastRequest_      = 0;
  positionPending_        = false;

  rxId_                   = sza::util::Rx::RX1CM;
  tuningPending_          = false;

  ambientTemperature_.setK(300.0);
}

/**.......................................................................
 * Destructor.
 */
SzaShareCorba::~SzaShareCorba() 
{
  if(monitorPointHandler_) {
    delete monitorPointHandler_;
    monitorPointHandler_ = 0;
  }
}

/**.......................................................................
 * Overloaded method to pack data into the frame
 */
void SzaShareCorba::packData(RegMapBlock* blk, void* data, CoordRange* range, 
			     DataType::Type type, bool lock)
{
  RegMapDataFrameManager::packData(blk, data, range, type, lock);

  if(monitorPointHandler_) {
    monitorPointHandler_->packData(blk, data, range, type);
  }
}
	
/**.......................................................................
 * Overloaded method to pack data into the frame.  Extracts the block
 * descriptor corresponding to board and block and redirecets to the
 * method above
 */
void SzaShareCorba::packData(std::string board, std::string block, void* data, 
			     CoordRange* range, DataType::Type type, bool lock)
{
  RegMapBlock* blk = getReg(board, block);
  packData(blk, data, range, type, lock);
}
	
/**.......................................................................
 * Overloaded method to pack a fixed value into the frame.  
 */

void SzaShareCorba::packValue(RegMapBlock* blk, void* data, CoordRange* range, 
			      DataType::Type type, bool lock)
{
  RegMapDataFrameManager::packValue(blk, data, range, type, lock);

  if(monitorPointHandler_) {
    monitorPointHandler_->packValue(blk, data, range, type);
  }
}
	
/**.......................................................................
 * Overloaded method to pack a fixed values into the frame.  Extracts
 * the block descriptor corresponding to board and block and
 * redirecets to the method above
 */
void SzaShareCorba::packValue(std::string board, std::string block, void* data, 
			      CoordRange* range, DataType::Type type, bool lock)
{
  RegMapBlock* blk = getReg(board, block);
  packValue(blk, data, range, type, lock);
}

void SzaShareCorba::initialize(AntennaCorba* antCorba)
{
  monitorPointHandler_ = new CarmaMonitorPointHandler(antCorba);

  if(monitorPointHandler_ == 0) {
    ThrowError("Unable to initialize monitorPointHandler_ object");
  }
}

void SzaShareCorba::setSite(double longitude, double latitude, double altitude)
{
#if DIR_USE_ANT_CORBA

  carma::services::Angle  lng(longitude, "radians");
  carma::services::Angle  lat(latitude,  "radians");
  carma::services::Length alt(altitude,  "meters");

  astroTime_.setSite(lng, lat, alt);

#endif

  // Also set the site in the base-class, so we can compare
  // computations if we want

  SzaShare::setSite(longitude, latitude, altitude);
}

double SzaShareCorba::getLst(double utc)
{
#if DIR_USE_ANT_CORBA

  // Return the LST, converted from hours (as returned by AstroTime)
  // to radians (as expected by Tracker thread)

  double lstInRadians = astroTime_.localSiderealTime(utc) * M_PI/12;
#endif  

  //  COUT("LSTC = " << lstInRadians << " LSTS = " << SzaShare::getLst(utc) << " " << pthread_self());

  return lstInRadians;
}

void SzaShareCorba::setPosition(Position position)
{
  positionGuard_.lock();
  lastRequestedPosition_ = position;
  nSinceLastRequest_     = 0;
  positionPending_       = true;
  positionGuard_.unlock();
}

void SzaShareCorba::getPosition(Position& position, unsigned& nFrame, bool& pending)
{
  positionGuard_.lock();
  position = lastRequestedPosition_;
  nFrame   = nSinceLastRequest_;
  pending  = positionPending_;
  positionGuard_.unlock();
}

void SzaShareCorba::incrementFrame(bool acquired)
{
  positionGuard_.lock();

  // If the telescope is tracking, increment the count of frames
  // during which the telescope has been tracking.  Else reset the
  // count to zero

  if(acquired) {
    ++nSinceLastRequest_;
    if(positionPending_ && nSinceLastRequest_ > 2) {
      positionPending_ = false;
    }
  } else {
    nSinceLastRequest_ = 0;
  }

  positionGuard_.unlock();
}

void SzaShareCorba::setPending(bool pending)
{
  positionGuard_.lock();
  positionPending_ = pending;
  positionGuard_.unlock();
}

void SzaShareCorba::setInitialized(bool initialized)
{
  initGuard_.lock();
  initialized_ = initialized;
  initGuard_.unlock();
}

void SzaShareCorba::getInitialized(bool& initialized)
{
  initGuard_.lock();
  initialized = initialized_;
  initGuard_.unlock();
}

void SzaShareCorba::setRx(sza::util::Rx::Id rxId)
{
  rxGuard_.lock();
  rxId_ = rxId;
  tuningPending_ = true;
  rxGuard_.unlock();
}

void SzaShareCorba::getRx(sza::util::Rx::Id& rxId)
{
  rxGuard_.lock();
  rxId = rxId_;
  rxGuard_.unlock();
}

Rx::Id SzaShareCorba::getRx()
{
  Rx::Id rxId;
  rxGuard_.lock();
  rxId = rxId_;
  rxGuard_.unlock();

  return rxId;
}

void SzaShareCorba::setTuningPending(bool pending)
{
  tuningPendingGuard_.lock();
  tuningPending_ = pending;
  tuningPendingGuard_.unlock();
}

bool SzaShareCorba::getTuningPending()
{
  bool pending;
  tuningPendingGuard_.lock();
  pending = tuningPending_;
  tuningPendingGuard_.unlock();
  return pending;
}

void SzaShareCorba::setAmbientTemperature(Temperature temp)
{
  tempGuard_.lock();
  ambientTemperature_ = temp;
  tempGuard_.unlock();
}

sza::util::Temperature SzaShareCorba::getAmbientTemperature()
{
  sza::util::Temperature temp;

  tempGuard_.lock();
  temp = ambientTemperature_;
  tempGuard_.unlock();

  return temp;
}

void SzaShareCorba::setCarmaAzOffset(Angle az) 
{
  offsetGuard_.lock();
  azOffset_ = az;
  offsetGuard_.unlock();
}

void SzaShareCorba::setCarmaElOffset(Angle el) 
{
  offsetGuard_.lock();
  elOffset_ = el;
  offsetGuard_.unlock();
}

void SzaShareCorba::setCarmaMountAzOffset(Angle az) 
{
  offsetGuard_.lock();
  mountAzOffset_ = az;
  offsetGuard_.unlock();
}

void SzaShareCorba::setCarmaMountElOffset(Angle el) 
{
  offsetGuard_.lock();
  mountElOffset_ = el;
  offsetGuard_.unlock();
}

void SzaShareCorba::setCarmaApertureAzOffset(Rx::Id rxId, Angle az) 
{
  offsetGuard_.lock();

  if(rxId == Rx::RX30GHZ) {
    rx30GHzApertureAzOffset_ = az;
  } else if(rxId == Rx::RX90GHZ) {
    rx90GHzApertureAzOffset_ = az;
  }

  lastTimeCoefficientsChanged_.setToCurrentTime();

  offsetGuard_.unlock();
}

void SzaShareCorba::setCarmaApertureElOffset(Rx::Id rxId, Angle el) 
{
  offsetGuard_.lock();

  if(rxId == Rx::RX30GHZ) {
    rx30GHzApertureElOffset_ = el;
  } else if(rxId == Rx::RX90GHZ) {
    rx90GHzApertureElOffset_ = el;
  }

  lastTimeCoefficientsChanged_.setToCurrentTime();

  offsetGuard_.unlock();
}

/**.......................................................................
 * Store flexure terms for a receiver
 */
void SzaShareCorba::setCarmaApertureFlexure(Rx::Id rxId, Angle flexureSin, Angle flexureCos) 
{
  offsetGuard_.lock();

  if(rxId == Rx::RX30GHZ) {
    rx30GHzApertureFlexureSin_ = flexureSin;
    rx30GHzApertureFlexureCos_ = flexureCos;
  } else if(rxId == Rx::RX90GHZ) {
    rx90GHzApertureFlexureSin_ = flexureSin;
    rx90GHzApertureFlexureCos_ = flexureCos;
  }

  lastTimeCoefficientsChanged_.setToCurrentTime();

  offsetGuard_.unlock();
}

/**.......................................................................
 * Get the flexure terms for the currently selected receiver
 */
void SzaShareCorba::getCarmaApertureFlexure(Rx::Id rxId, Angle& flexureSin, Angle& flexureCos)
{
  offsetGuard_.lock();

  if(rxId == Rx::RX30GHZ) {
    flexureSin = rx30GHzApertureFlexureSin_;
    flexureCos = rx30GHzApertureFlexureCos_;
  } else if(rxId == Rx::RX90GHZ) {
    flexureSin = rx90GHzApertureFlexureSin_;
    flexureCos = rx90GHzApertureFlexureCos_;
  }

  offsetGuard_.unlock();
}

/**.......................................................................
 * Get the flexure terms for the currently selected receiver
 */
void SzaShareCorba::getFlexure(Angle& flexureSin, Angle& flexureCos)
{
  offsetGuard_.lock();

  if(rxId_ == Rx::RX30GHZ) {
    flexureSin = rx30GHzApertureFlexureSin_;
    flexureCos = rx30GHzApertureFlexureCos_;
  } else if(rxId_ == Rx::RX90GHZ) {
    flexureSin = rx90GHzApertureFlexureSin_;
    flexureCos = rx90GHzApertureFlexureCos_;
  }

  offsetGuard_.unlock();
}

double SzaShareCorba::getLastMjdCoefficientsChanged()
{
  double mjd;

  offsetGuard_.lock();
  mjd = lastTimeCoefficientsChanged_.getMjd();
  offsetGuard_.unlock();

  return mjd;
}

Angle SzaShareCorba::getCarmaAzOffset()
{
  Angle offset;

  offsetGuard_.lock();
  offset = azOffset_;
  offsetGuard_.unlock();

  return offset;
}

Angle SzaShareCorba::getCarmaElOffset()
{
  Angle offset;

  offsetGuard_.lock();
  offset = elOffset_;
  offsetGuard_.unlock();

  return offset;
}

Angle SzaShareCorba::getCarmaMountAzOffset()
{
  Angle offset;

  offsetGuard_.lock();
  offset = mountAzOffset_;
  offsetGuard_.unlock();

  return offset;
}

Angle SzaShareCorba::getCarmaMountElOffset()
{
  Angle offset;

  offsetGuard_.lock();
  offset = mountElOffset_;
  offsetGuard_.unlock();

  return offset;
}

Angle SzaShareCorba::getCarmaApertureAzOffset(sza::util::Rx::Id rxId)
{
  Angle offset;

  offsetGuard_.lock();

  if(rxId == Rx::RX30GHZ) {
    offset = rx30GHzApertureAzOffset_;
  } else if(rxId == Rx::RX90GHZ) {
    offset = rx90GHzApertureAzOffset_;
  }

  offsetGuard_.unlock();

  return offset;
}

Angle SzaShareCorba::getCarmaApertureElOffset(sza::util::Rx::Id rxId)
{
  Angle offset;

  offsetGuard_.lock();

  if(rxId == Rx::RX30GHZ) {
    offset = rx30GHzApertureElOffset_;
  } else if(rxId == Rx::RX90GHZ) {
    offset = rx90GHzApertureElOffset_;
  }

  offsetGuard_.unlock();

  return offset;
}

/**.......................................................................
 * Return the total collimation offset for the current receiver, as
 * CARMA defines it.  This is the sum of offsets specified by the
 * various offset commands: 
 *
 * setOffset() + setMountOffset() + setApertureOffset()
 */
Angle SzaShareCorba::getTotalXCollimationOffset()
{
  Angle offset;

  COUT("Inside getTotalXCollimationOffset: azOffset_ = " << azOffset_);

  offsetGuard_.lock();
  rxGuard_.lock();

  COUT("Inside getTotalXCollimationOffset: mountAzOffset_ = " << mountAzOffset_);

  offset = azOffset_ + mountAzOffset_;
  COUT("Offset is now (1): " << offset);

  offset = azOffset_ + mountAzOffset_ + (rxId_ == Rx::RX30GHZ ? rx30GHzApertureAzOffset_ : rx90GHzApertureAzOffset_);

  COUT("Inside getTotalXCollimationOffset: apertureAzOffset_ = " << (rxId_ == Rx::RX30GHZ ? rx30GHzApertureAzOffset_ : rx90GHzApertureAzOffset_));

  COUT("Offset is now (2): " << offset);

  rxGuard_.unlock();
  offsetGuard_.unlock();

  return offset;
}

/**.......................................................................
 * Return the total collimation offset for the current receiver, as
 * CARMA defines it.  This is the sum of offsets specified by the
 * various offset commands: 
 *
 * setOffset() + setMountOffset() + setApertureOffset()
 */
Angle SzaShareCorba::getTotalYCollimationOffset()
{
  Angle offset;

  offsetGuard_.lock();
  rxGuard_.lock();

  COUT("Inside getTotalYCollimationOffset: elOffset_         = " << elOffset_);
  COUT("Inside getTotalYCollimationOffset: mountElOffset_    = " << mountElOffset_);
  COUT("Inside getTotalYCollimationOffset: apertureElOffset_ = " << (rxId_ == Rx::RX30GHZ ? rx30GHzApertureElOffset_ : rx90GHzApertureElOffset_));

  offset = elOffset_ + mountElOffset_ + (rxId_ == Rx::RX30GHZ ? rx30GHzApertureElOffset_ : rx90GHzApertureElOffset_);

  rxGuard_.unlock();
  offsetGuard_.unlock();

  return offset;
}
