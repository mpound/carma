#include "carma/antenna/sza/antenna/control/AntennaMaster.h"

#include "carma/antenna/sza/antenna/corba/DriveProxy.h"
#include "carma/antenna/sza/antenna/corba/PointingModelProxy.h"
#include "carma/antenna/sza/antenna/corba/SzaShareCorba.h"

#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/Temperature.h"

#include "carma/szautil/Debug.h"

#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h"

using namespace carma::antenna::common;
using namespace carma::util;

using namespace sza::antenna::control;
using namespace sza::antenna::corba;
using namespace sza::util;
using namespace std;

sza::antenna::control::WrapMode::Mode 
carmaToSzaWrapMode(carma::antenna::common::DriveControl::AzWrapMode azWrapMode);

/**.......................................................................
 * Constructor with pointer to the parent AntennaDrive.
 *
 * @throws Exception
 */
DriveProxy::DriveProxy(AntennaMaster* parent) : Proxy(parent)
{
  // A sequence number of zero will indicate an invalid sequence
  // number

  seq_ = 0;

  pointingModel_ = 0;
  pointingModel_ = new PointingModelProxy(parent);
};

/**.......................................................................
 * Destructor.
 */
DriveProxy::~DriveProxy()
{
  if(pointingModel_ != 0) {
#ifdef SZA_PM
    delete pointingModel_;
#endif
    pointingModel_ = 0;
  }
};

#ifdef SZA_PM
/**.......................................................................
 * Serve up AntennaDrive's PointingModelProxy object as a remote CORBA
 * object.
 */
carma::antenna::common::PointingModelControl_ptr DriveProxy::PointingModel()
{
  return pointingModel_->_this();
};
#endif

/**.......................................................................
 * Stow the telescope
 */
void DriveProxy::stow(carma::antenna::common::DriveControl::Position position,
		 CORBA::ULong seq)
{
  switch (position) {
  case carma::antenna::common::DriveControl::ZENITH:
  case carma::antenna::common::DriveControl::SAFE:
    setAzel(sza::util::Axis::BOTH, 180.0, 85.0, seq);
    share_->setPosition(SzaShareCorba::STOW);
    break;
  case carma::antenna::common::DriveControl::SERVICE:
    setAzel(sza::util::Axis::EL, 0.0, 15.0, seq);
    share_->setPosition(SzaShareCorba::SERVICE);
    break;
  }
};

/**.......................................................................
 * Track to minimize snow accumulation
 */
void DriveProxy::trackSnow()
{
  stow(carma::antenna::common::DriveControl::SERVICE);
  share_->setPosition(SzaShareCorba::SNOW);
}

/**.......................................................................
 * Track to minimize wind damage
 */
void DriveProxy::trackWind()
{
  stow(carma::antenna::common::DriveControl::ZENITH);
};

/**.......................................................................
 * Set the maximum tracking rate.
 *
 * Not controllable for SZA antennas, so do nothing
 */
void DriveProxy::setMaxRate(float azRate, float elRate)
{
  COUT("DriveProxy::setMaxRates() stub");
};

/**.......................................................................
 * Set the maximum AZ tracking rate.
 *
 * Not controllable for SZA antennas, so do nothing
 */
void DriveProxy::setAzMaxRate(float azRate)
{
  COUT("DriveProxy::setAzMaxRates() stub");
};

/**.......................................................................
 * Set the maximum EL tracking rate.
 *
 * Not controllable for SZA antennas, so do nothing
 */

void DriveProxy::setElMaxRate(float elRate)
{
  COUT("DriveProxy::setElMaxRates() stub");
};

/**.......................................................................
 * Set the telescope location.  IDL says long/lat in radians, alt in
 * meters, same as my site message expects.
 *
 * Note however, that my convention for longitude is E longitude
 * positive, W longitude negative, in other words, the longitude must
 * be less either 0 to +pi (< +pi) or -pi to 0 (> -pi)
 */
void DriveProxy::setAntLocation(double longitude,
				double latitude,
				double altitude)
{
  // Ensure that longitude is a number between pi and -pi

  longitude = sza::util::Angle::radiansToPiMinusPi(longitude);

  AntennaMasterMsg msg;
  msg.getDriveMsg()->getTrackerMsg()->packSiteMsg(longitude, latitude, altitude);
  msg.getDriveMsg()->getTrackerMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);
};

/**.......................................................................
 * Utility function to convert from carma 'aperture' to pointing model
 */
sza::util::PointingMode::Type apertureToMode(carma::antenna::common::DriveControl::Aperture model)
{
  switch (model) {
  case carma::antenna::common::DriveControl::OPTICAL:
    return sza::util::PointingMode::OPTICAL;
    break;
  default:
    return sza::util::PointingMode::RADIO;
    break;
  }
}

/**.......................................................................
 * Utility function to convert from carma 'aperture' to Rx::Id
 */
sza::util::Rx::Id DriveProxy::apertureToRxId(carma::antenna::common::DriveControl::Aperture model)
{
  switch (model) {
  case carma::antenna::common::DriveControl::RADIO1CM:
    return sza::util::Rx::RX30GHZ;
    break;
  case carma::antenna::common::DriveControl::RADIO3MM:
    return sza::util::Rx::RX90GHZ;
    break;
  default:
    return sza::util::Rx::RXUNKNOWN;
    break;
  }
}

/**.......................................................................
 * Utility function to convert from Rx::Id to carma 'aperture'
 */
carma::antenna::common::DriveControl::Aperture DriveProxy::rxIdToAperture(sza::util::Rx::Id rxId)
{
  switch (rxId) {
  case sza::util::Rx::RX3MM:
    return carma::antenna::common::DriveControl::RADIO3MM;
    break;
  case sza::util::Rx::RX1MM:
    return carma::antenna::common::DriveControl::RADIO1MM;
    break;
  default:
    return carma::antenna::common::DriveControl::RADIO1CM;
    break;
  }
}

/**.......................................................................
 * Set the pointing mode of the telescope (radio or optical)
 */
void DriveProxy::selectAperture(carma::antenna::common::DriveControl::Aperture model)
{
  AntennaMasterMsg msg;
  msg.getDriveMsg()->getTrackerMsg()->packSelectModelMsg(sequenceNumber(), apertureToMode(model));
  msg.getDriveMsg()->getTrackerMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);

};

/**.......................................................................
 * Set the Azimuth wrap mode.
 */
void DriveProxy::setWrapMode(carma::antenna::common::DriveControl::AzWrapMode azWrapMode)
{
  COUT("DriveProxy::setWrapMode() stub");
  AntennaMasterMsg msg;
  msg.getDriveMsg()->getTrackerMsg()->packWrapModeMsg(carmaToSzaWrapMode(azWrapMode));
  msg.getDriveMsg()->getTrackerMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);
};


/**.......................................................................
 * Guess this is CARMA's slew command?  Convert angles from degrees to
 * radians
 */
void DriveProxy::setAzel(sza::util::Axis::Type axes, double az, double el, CORBA::ULong seq)
{
  Angle azAng(Angle::Degrees(), az);
  Angle elAng(Angle::Degrees(), el);

  AntennaMasterMsg msg;
  TrackerMsg* trackerMsg = msg.getDriveMsg()->getTrackerMsg();

  trackerMsg->packSlewMsg(sequenceNumber(),
			  "current", axes,
			  azAng.radians(),
			  elAng.radians(),
			  0.0);

  if(seq > 0)
    trackerMsg->setCarmaDriveSequenceNumber(seq);
  else
    trackerMsg->setCarmaSequenceNumber();

  parent_->fwdTaskMsg(&msg);

  // Set the requested position

  share_->setPosition(SzaShareCorba::AZEL);
}

/**.......................................................................
 * Move to a requested AZ/EL position
 */
void DriveProxy::setAzel(double az, double el, CORBA::ULong seq)
{
  setAzel(sza::util::Axis::BOTH, az, el, seq);
};

/**.......................................................................
 * Move to a requested AZ position
 */
void DriveProxy::setAz(double az, CORBA::ULong seq)
{
  setAzel(sza::util::Axis::AZ, az, 0.0, seq);
};

/**.......................................................................
 * Move to a requested EL position
 */
void DriveProxy::setEl(double el, CORBA::ULong seq)
{
  setAzel(sza::util::Axis::EL, 0.0, el, seq);
};

/**.......................................................................
 * Update the refraction corrections
 */
void DriveProxy::setRefrac(double a, double b )
{
  AntennaMasterMsg msg;
  msg.getDriveMsg()->getTrackerMsg()->packRefractionMsg(sza::util::PointingMode::CURRENT, a, b);
  msg.getDriveMsg()->getTrackerMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);
};

/**.......................................................................
 * Update the weather
 */
void DriveProxy::updateWeather(float ambientTemp,
			       float barometricPressure,
			       float relativeHumidity,
			       float dewpointTemp,
			       float windSpeed,
			       float windDirection)
{
  Temperature airTemp(Temperature::Celsius(), ambientTemp);

  AntennaMasterMsg msg;
  msg.getDriveMsg()->getTrackerMsg()->packWeatherMsg(airTemp.K(), relativeHumidity/100, barometricPressure);
  msg.getDriveMsg()->getTrackerMsg()->setCarmaSequenceNumber();

  share_->setAmbientTemperature(airTemp);

  parent_->fwdTaskMsg(&msg);
};

/**.......................................................................
 * CARMA uses setMountOffset() to mean 'another temporary collimation
 * offset', derived from an online radio pointing session for example,
 * which should persist in the system until the mount offset is
 * changed again.
 *
 * These get added to offsets set by the setOffset() and
 * setAperturePointingConstants() commands, so we command the total
 * offset as a collimation term each time this command is issued.
 */
void DriveProxy::setMountOffset(double az, double el, CORBA::ULong seq)
{
  Angle azAngle(Angle::ArcMinutes(), az);
  Angle elAngle(Angle::ArcMinutes(), el);

  COUT("Inside setMountOffset with azOffset = " << azAngle << " elOffset = " << elAngle);

  share_->setCarmaMountAzOffset(azAngle);
  share_->setCarmaMountElOffset(elAngle);

  setSkyOffsets(seq);
}

/**.......................................................................
 * See setMountOffset()
 */
void DriveProxy::setAzMountOffset(double az,
		      CORBA::ULong seq)
{
  Angle azAngle(Angle::ArcMinutes(), az);

  COUT("Inside setAzMountOffset with azOffset = " << azAngle);

  share_->setCarmaMountAzOffset(azAngle);

  setSkyOffsets(seq);
}

/**.......................................................................
 * See setMountOffset()
 */
void DriveProxy::setElMountOffset(double el,
		      CORBA::ULong seq)
{
  Angle elAngle(Angle::ArcMinutes(), el);

  COUT("Inside setElMountOffset with elOffset = " << elAngle);

  share_->setCarmaMountElOffset(elAngle);

  setSkyOffsets(seq);
}

/**.......................................................................
 * CARMA uses setOffset() to mean 'temporary collimation offsets'.
 *
 * These get added to setMountOffset() and
 * setAperturePointingConstants() offsets, so we command the total
 * offset as a collimation term each time this command is issued
 */
void DriveProxy::setOffset(OffsetMsg::Axis axes,
			   double az,
			   double el,
			   CORBA::ULong seq)
{
  Angle azAngle(Angle::ArcMinutes(), az);
  Angle elAngle(Angle::ArcMinutes(), el);

  COUT("Inside setOffset with azOffset = " << azAngle << " elOffset = " << elAngle);

  share_->setCarmaAzOffset(azAngle);
  share_->setCarmaElOffset(elAngle);

  setSkyOffsets(seq);
}

/**.......................................................................
 * Equivalent of the normal SZA mount offsets
 */
void DriveProxy::setOffset(double az,
			   double el,
			   CORBA::ULong seq)
{
  COUT("Inside setOffset with az = " << az << " el = " << el);
  setOffset(OffsetMsg::BOTH, az, el, seq);
}

void DriveProxy::setAzOffset(double az,
		 CORBA::ULong seq)
{
  COUT("Inside setAzOffset with az = " << az);
  setOffset(OffsetMsg::AZ, az, 0.0, seq);
}

void DriveProxy::setElOffset(double el,
		 CORBA::ULong seq)
{
  COUT("Inside setElOffset with el = " << el);
  setOffset(OffsetMsg::EL, 0.0, el, seq);
}

/**.......................................................................
 * Set pointing constants (collimation and sag terms)
 */
void DriveProxy::
setAperturePointingConstants(carma::antenna::common::DriveControl::Aperture
			     aperture,
			     float azOffset,
			     float elOffset,
			     float sag)
{
  COUT("Inside setAperturePointingConstants stub with: azOffset = " 
       << azOffset << " elOffset = " << elOffset << " sag = " << sag);

  Angle azAngle(Angle::ArcMinutes(), azOffset);
  Angle elAngle(Angle::ArcMinutes(), elOffset);

  share_->setCarmaApertureAzOffset(apertureToRxId(aperture), azAngle);
  share_->setCarmaApertureElOffset(apertureToRxId(aperture), elAngle);

  // Assert whatever terms are appropriate for the current receiver

  setSkyOffsets();

  // Now implementing the sag term.  This gets set as a
  // receiver-independent coefficient, hence the aperture to mode
  // (1cm/3mm --> radio) instead of rx id dependence.  For SZA, we
  // allowed both sin and cos coefficients, but physically there
  // should only be a cos EL dependence, and CARMA assumes that there
  // is only cos EL, so we explicitly zero the sin term here

  // This API specifies sag in arcminutes (per cos(EL)), but
  // setFlexure() method of szaDriveControl.idl expects Angles

  Angle flexureSin(Angle::ArcMinutes(), 0.0);
  Angle flexureCos(Angle::ArcMinutes(), sag);

  share_->setCarmaApertureFlexure(apertureToRxId(aperture), flexureSin, flexureCos);

  // Assert whatever terms are appropriate for the current receiver

  setFlexure();
};

/**.......................................................................
 * Tiltmeter stub
 */
void DriveProxy::setTiltmeterZero(float aftForward,
				  float leftRight)
{
  AntennaMasterMsg msg;
  msg.getRxMsg()->packTiltmeterMsg(sza::array::TILTMETER_ZEROS, sza::array::ON, 0.0, aftForward, leftRight);
  msg.getRxMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);
};

/**.......................................................................
 * Tolerance for target acquisition.  Not settable for SZA
 */
void DriveProxy::setTolerance(float toleranceInArcsecs)
{
  COUT("DriveProxy::setTolerance() stub");
};

/**.......................................................................
 * Set the software drive limits
 */
void DriveProxy::setSafeRange(float azLow,
			      float azHigh,
			      float elLow,
			      float elHigh)
{
  int countsPerTurn = 16777216;
  int azLowCount  = (int)((azLow / 360)  * countsPerTurn);
  int azHighCount = (int)((azHigh / 360) * countsPerTurn);
  int elLowCount  = (int)((elLow / 360)  * countsPerTurn);
  int elHighCount = (int)((elHigh / 360) * countsPerTurn);

  COUT("Would set azLow = " << azLowCount << " azHigh = " << azHigh << " elLow = " << elLowCount << " elHigh = " << elHighCount);

  //  setEncoderLimits(azLowCount, azHighCount, elLowCount, elHighCount);
};

/**.......................................................................
 * Stop the telescope immediately
 */
void DriveProxy::stop()
{
  AntennaMasterMsg msg;
  msg.getDriveMsg()->getTrackerMsg()->packHaltMsg(sequenceNumber());
  msg.getDriveMsg()->getTrackerMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);
}

/**.......................................................................
 * Set the equation of the equinoxes.  Not called by CARMA, but we
 * will need it for tracking.
 */
void DriveProxy::setEqnEqx(double mjd, double eqneqx)
{
  AntennaMasterMsg msg;
  msg.getDriveMsg()->getTrackerMsg()->packExtendEqnEqxMsg(mjd, eqneqx);
  msg.getDriveMsg()->getTrackerMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);
};

// Helper function
string azwrap2string(carma::antenna::common::DriveControl::AzWrapMode m)
{
  if (m == carma::antenna::common::DriveControl::ZERO) {
      return "ZERO";
  }
  else if (m == carma::antenna::common::DriveControl::SUB) {
      return "SUB";
  }
  else if (m == carma::antenna::common::DriveControl::ADD) {
      return "ADD";
  }
  return "UNKNOWN";
}

/**.......................................................................
 * Track a source
 */
void DriveProxy::track(const char* source,
		       const carma::antenna::common::DriveControl::RaDecTriplet& positionTriplet,
		       carma::antenna::common::DriveControl::AzWrapMode azWrapMode,
		       bool overTheTop,
		       CORBA::ULong seq)
{
  const ScopedLogNdc ndc( "DriveProxy::track" );
  string azwrapstr = azwrap2string(azWrapMode);
  std::ostringstream o;
  o << "track(" << source << ", azWrapMode=" << azwrapstr <<")";
  programLogInfo(o.str());
  COUT("Inside track command with sequence no = " << seq);
  // Reset the offsets
  Angle zeroAngle(Angle::ArcMinutes(), 0.0);
  share_->setCarmaAzOffset(zeroAngle);
  share_->setCarmaElOffset(zeroAngle);
  extendTrack(positionTriplet[0], true, seq, (char*)source, azWrapMode);
}

/**.......................................................................
 * Update the next RA/DEC position
 */
void DriveProxy::updateRaDec(const carma::antenna::common::DriveControl::RaDecEpoch& position,
			     carma::antenna::common::DriveControl::AzWrapMode azWrapMode)
{
  const ScopedLogNdc ndc( "DriveProxy::updateRaDec" );
  string azwrapstr = azwrap2string(azWrapMode);
  std::ostringstream o;
  o << "updateRaDec(azWrapMode=" << azwrapstr <<")";
  programLogInfo(o.str());
  COUT("Inside updateRaDec command with no sequence no");
  extendTrack(position, false, 0, 0, azWrapMode);
};

/**.......................................................................
 * Extend a track
 */
void DriveProxy::extendTrack(const carma::antenna::common::DriveControl::RaDecEpoch& position,
			     bool newSource, unsigned seq, char* source, 
			     carma::antenna::common::DriveControl::AzWrapMode azWrapMode)
{
  const ScopedLogNdc ndc( "DriveProxy::extendTrack" );
  string azwrapstr = azwrap2string(azWrapMode);
  std::ostringstream o;
  o << "extendTrack(newSource=" << boolalpha << newSource 
    << ", seq=" << seq << ", source=" << source
    << ", azWrapMode=" << azwrapstr <<")";
  programLogInfo(o.str());
  
  AntennaMasterMsg msg;
  TrackerMsg* trackerMsg = msg.getDriveMsg()->getTrackerMsg();

  // Only send a sequence number if this is the first update of
  // the source

  if(newSource) {
    trackerMsg->packTrackMsg(sequenceNumber(), source, position.mjd, position.ra, position.dec, 0.0, carmaToSzaWrapMode(azWrapMode));
    trackerMsg->setCarmaDriveSequenceNumber(seq);
    share_->setPosition(SzaShareCorba::EQUAT);
  } else {
    trackerMsg->packTrackMsg(-1, "", position.mjd, position.ra, position.dec, 0.0, carmaToSzaWrapMode(azWrapMode));
    trackerMsg->setCarmaSequenceNumber();
  }

  parent_->fwdTaskMsg(&msg);
}


/**.......................................................................
 * Set the tilts.  Do nothing for SZA
 */
void DriveProxy::setTilts(double azTilt1, double azTilt2,
			  double elTilt1, double elTilt2)
{
  COUT("DriveProxy::setTilts() stub");
};

/**.......................................................................
 * Set the UT1-UTC correction
 */
void DriveProxy::setUt1Utc(double mjd, double ut1utc)
{
  AntennaMasterMsg msg;
  msg.getDriveMsg()->getTrackerMsg()->packExtendUt1UtcMsg(mjd, ut1utc);
  msg.getDriveMsg()->getTrackerMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);
};

/**.......................................................................
 * Corba control method to set encoder calibration
 */
void DriveProxy::setEncoderCountsPerTurn(unsigned long azCountsPerTurn, unsigned long elCountsPerTurn)
{
  COUT("Inside setEncoderCountsPerTurn 0");
  setEncoderCountsPerTurn((unsigned) azCountsPerTurn, (unsigned) elCountsPerTurn);
  COUT("Inside setEncoderCountsPerTurn 1");
}

/**.......................................................................
 * Corba control method to set encoder limits
 */
void DriveProxy::setEncoderLimits(unsigned long azMinCount, unsigned long azMaxCount,
				  unsigned long elMinCount, unsigned long elMaxCount)
{
  setEncoderLimits((unsigned) azMinCount, (unsigned) azMaxCount,
		   (unsigned) elMinCount, (unsigned) elMaxCount);
}

/**.......................................................................
 * Corba control method to set encoder zeros
 */
void DriveProxy::setEncoderZeros(double azEncZeroDeg, double elEncZeroDeg)
{
  Angle azZero;
  azZero.setDegrees(azEncZeroDeg);

  Angle elZero;
  elZero.setDegrees(elEncZeroDeg);

  setEncoderZeros(azZero, elZero);
}

/**.......................................................................
 * Corba control method to set tilts -- calls local method with proper
 * Angle objects
 */
void DriveProxy::setTilts(double haTiltDeg, double latTiltDeg, double elTiltDeg)
{
  Angle haTilt, latTilt, elTilt;

  haTilt.setDegrees(haTiltDeg);
  latTilt.setDegrees(latTiltDeg);
  elTilt.setDegrees(elTiltDeg);

  setTilts(haTilt, latTilt, elTilt);
}

void DriveProxy::setCollimation(carma::antenna::common::DriveControl::Aperture aperture,
				double xCollimationDeg, double yCollimationDeg)
{
  Angle xColl;
  xColl.setDegrees(xCollimationDeg);

  Angle yColl;
  yColl.setDegrees(yCollimationDeg);

  setCollimation(apertureToMode(aperture), xColl, yColl);
}

void DriveProxy::setFlexure(sza::util::PointingMode::Type model, sza::util::Angle& sFlex, sza::util::Angle& cFlex)
{
  Proxy::setFlexure(model, sFlex, cFlex);
}

void DriveProxy::setFlexure(carma::antenna::common::DriveControl::Aperture aperture,
			    double sinCoeffDeg, double cosCoeffDeg)
{
  Angle sinCoeff;
  sinCoeff.setDegrees(sinCoeffDeg);

  Angle cosCoeff;
  cosCoeff.setDegrees(cosCoeffDeg);

  setFlexure(apertureToMode(aperture), sinCoeff, cosCoeff);
}

void DriveProxy::setMountPointingConstants(CORBA::ULong  azEncoderCountsPerTurn,     CORBA::ULong elEncoderCountsPerTurn,
					   CORBA::ULong  azMinEncoderCount,          CORBA::ULong azMaxEncoderCount,
					   CORBA::ULong  elMinEncoderCount,          CORBA::ULong elMaxEncoderCount,
					   double        azEncoderZeroDegrees,       double       elEncoderZeroDegrees,
					   double        haTiltDegrees,              double       latTiltDegrees,             double elTiltDegrees,
					   double        opticalXCollimationDegrees, double       opticalYCollimationDegrees,
					   double        opticalFlexureSinDegrees,   double       opticalFlexureCosDegrees,
					   double        radioXCollimationDegrees,   double       radioYCollimationDegrees,
					   double        radioFlexureSinDegrees,     double       radioFlexureCosDegrees)
{
  setEncoderCountsPerTurn(azEncoderCountsPerTurn, elEncoderCountsPerTurn);

  setEncoderLimits(azMinEncoderCount, azMaxEncoderCount, elMinEncoderCount, elMaxEncoderCount);

  setEncoderZeros(azEncoderZeroDegrees, elEncoderZeroDegrees);

  setTilts(haTiltDegrees, latTiltDegrees, elTiltDegrees);

  setCollimation(carma::antenna::common::DriveControl::OPTICAL,  opticalXCollimationDegrees, opticalYCollimationDegrees);
  setFlexure(carma::antenna::common::DriveControl::OPTICAL,      opticalFlexureSinDegrees,   opticalFlexureCosDegrees);

  setCollimation(carma::antenna::common::DriveControl::RADIO1CM, radioXCollimationDegrees,   radioYCollimationDegrees);
  setFlexure(carma::antenna::common::DriveControl::RADIO1CM,     radioFlexureSinDegrees,     radioFlexureCosDegrees);
}

//-----------------------------------------------------------------------
// Local methods, called on initialization
//-----------------------------------------------------------------------

void DriveProxy::setSite(sza::util::Angle& lng, sza::util::Angle& lat, sza::util::Length& alt)
{
  setAntLocation(lng.radians(), lat.radians(), alt.meters());
}

void DriveProxy::setLocation(sza::util::Length& up, sza::util::Length& east, sza::util::Length& north)
{
  AntennaMasterMsg msg;
  msg.getDriveMsg()->getTrackerMsg()->packLocationMsg(north.meters(), east.meters(), up.meters());
  msg.getDriveMsg()->getTrackerMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);
}

void DriveProxy::setEncoderCountsPerTurn(unsigned az, unsigned el)
{
  COUT("DEBUG Inside setEncoderCountsPerTurn az = " << az << " el = " << el);
  AntennaMasterMsg msg;
  msg.getDriveMsg()->getTrackerMsg()->packEncoderCountsPerTurnMsg(sequenceNumber(), az, el, 0);
  msg.getDriveMsg()->getTrackerMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);
}

void DriveProxy::setEncoderLimits(unsigned azMin, unsigned azMax, unsigned elMin, unsigned elMax)
{
  COUT("DEBUG Inside setEncoderLimits: azMin = " << azMin << " azMax = " << azMax << " elMin = " << elMin << " elMax = " << elMax);

  AntennaMasterMsg msg;
  msg.getDriveMsg()->getTrackerMsg()->packEncoderLimitsMsg(sequenceNumber(), azMin, azMax, elMin, elMax, 0, 0);
  msg.getDriveMsg()->getTrackerMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);
}

void DriveProxy::setEncoderZeros(sza::util::Angle& azZero, sza::util::Angle& elZero)
{
  COUT("DEBUG Inside setEncoderZeros: azZero = " << azZero << " elZero = " << elZero);

  AntennaMasterMsg msg;
  msg.getDriveMsg()->getTrackerMsg()->packEncoderZerosMsg(sequenceNumber(), azZero.radians(), elZero.radians(), 0);
  msg.getDriveMsg()->getTrackerMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);
}

void DriveProxy::setTilts(sza::util::Angle& haTilt, sza::util::Angle& latTilt, sza::util::Angle& elTilt)
{
  COUT("DEBUG Inside setTilts: haTilt = " << haTilt << " latTilt = " << latTilt << " elTilt = " << elTilt);

  AntennaMasterMsg msg;
  msg.getDriveMsg()->getTrackerMsg()->packTiltsMsg(sequenceNumber(), haTilt.radians(), latTilt.radians(), elTilt.radians());
  msg.getDriveMsg()->getTrackerMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);
}

void DriveProxy::setCollimation(sza::util::PointingMode::Type model, sza::util::Angle& x, sza::util::Angle& y)
{
  COUT("DEBUG Inside setCollimation: model = " << model << " x = " << x << " y = " << y);

  AntennaMasterMsg msg;
  msg.getDriveMsg()->getTrackerMsg()->packCollimationMsg(sequenceNumber(), model, x.radians(), y.radians(), OffsetMsg::SET);
  msg.getDriveMsg()->getTrackerMsg()->setCarmaSequenceNumber();
  parent_->fwdTaskMsg(&msg);
}

void DriveProxy::setFlexure()
{
  Proxy::setFlexure();
}

sza::antenna::control::WrapMode::Mode 
carmaToSzaWrapMode(carma::antenna::common::DriveControl::AzWrapMode azWrapMode)
{
  switch (azWrapMode) {
  case carma::antenna::common::DriveControl::SUB:
    return sza::antenna::control::WrapMode::SUBTRACT;
    break;
  case carma::antenna::common::DriveControl::ADD:
    return sza::antenna::control::WrapMode::ADD;
    break;
  default:
    return sza::antenna::control::WrapMode::NONE;
    break;
  }
}
