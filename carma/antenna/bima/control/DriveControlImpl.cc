/**
 * @file
 * Class definition of DriveControlImpl CORBA control implementation.
 *
 * @author Colby Gutierrez-Kraybill
 * Version: $Revision: 1.55 $
 * $Date: 2013/06/12 14:30:40 $
 * $Id: DriveControlImpl.cc,v 1.55 2013/06/12 14:30:40 friedel Exp $
 */

#include "carma/antenna/bima/Configuration.h"
#include "carma/antenna/bima/DriveCommand.h"
#include "carma/antenna/bima/SharedMemory.h"
#include "carma/antenna/bima/control/DriveControlImpl.h"
#include "carma/antenna/common/driveControlUtils.h"
#include "carma/monitor/AntennaCommon.h"
#include "carma/util/ErrorException.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"

using namespace std;
using namespace log4cpp;
using namespace carma::util;
using namespace carma::monitor;
using namespace carma::antenna::common;
using namespace carma::antenna::bima;

#define DWSET(N,V) DCSET(drvWriter_,N,V)
#define DWSETA(N,I,V) DCSETA(drvWriter_,N,I,V)

/**.......................................................................
 * Constructor.
 */
DriveControlImpl::DriveControlImpl ( string antenna, bool emulate )
 : log_(Program::getLogger()),
   emulate_(emulate),
   antenna_(antenna)
{
  CPTRACE( Trace::TRACE6, "Constructing DriveControl Object" );

  while (true)
  {
    int retrycnt = 0;

   try
    {
      CPTRACE( Trace::TRACE1, "Aquring handle to DRIVESIPQ" );
      drvWriter_ = new IPQwriter<DriveCommand>( DRIVESIPQ, false, IPQLEN );
      break;
    }
    catch ( carma::util::ErrorException &eex )
    {
      if ( retrycnt++ > 10 )
      {
        ostringstream oss;
        string msg;
        oss << "Unable to aquire DRIVES IPQ('" << DRIVESIPQ << "')!"
            << " Is bimaDriveMgr running? "
            << eex.what();
        msg = oss.str();
        throw CARMA_ERROR( msg );
      }
    }
    sleep(1);
  }

  CPTRACE( Trace::TRACE6, "Creating handle to SharedMemory file for: " << antenna_ );
  bimaShm_ = new SharedMemory( antenna_.c_str() );
  bimaShm_->map();

  _safeCalled = false;
}

/**.......................................................................
 * Destructor.
 */
DriveControlImpl::~DriveControlImpl() {}

//
// IDL interface
// Sent to the gulag
/*
PointingModelControl* DriveControlImpl::PointingModel()
{
  log_ << Priority::INFO << "DriveControl.PointingModel()";
  // STUB
  return NULL;
}
*/

void DriveControlImpl::stow (
    carma::antenna::common::DriveControl::Position position,
    const CORBA::ULong seq )
{

  setNextSequenceNo( seq );

  string thePositionString;
  // Preset to avoid compiler warnings
  DriveCommand::PositionType thePosition = DriveCommand::ZENITH;

  if ( position == carma::antenna::common::DriveControl::ZENITH ) {
    thePositionString = "ZENITH";
    thePosition = DriveCommand::ZENITH;
  } else if ( position == carma::antenna::common::DriveControl::SERVICE ) {
    thePositionString = "SERVICE";
    thePosition = DriveCommand::SERVICE;
  } else if ( position == carma::antenna::common::DriveControl::SAFE ) {
    thePositionString = "SAFE";
    thePosition = DriveCommand::SAFE;
    if ( _safeCalled == false )
      throw CARMA_ERROR( "stow(SAFE) called before setSafeRange(...)" );
  }

  log_ << Priority::INFO << "DriveControl.stow( " << thePositionString << " )";

  DWSET(command,DriveCommand::STOW);
  DWSET(stowPosition,thePosition);
  drvWriter_->write();
}

/**
 *  Stop the antenna
 */
void DriveControlImpl::stop()
{
  log_ << Priority::INFO << "DriveControl.stop()";
  DWSET(command,DriveCommand::STOP);
  drvWriter_->write();
}

/**
 *  Track wind to prevent snow build-up on dish
 */
void DriveControlImpl::trackSnow()
{
  log_ << Priority::INFO << "DriveControl.trackSnow()";
  DWSET(command,DriveCommand::SNOW);
  drvWriter_->write();
}

/**
 * Minimize antenna profile to heavy winds.
 */
void DriveControlImpl::trackWind()
{
    log_ << Priority::INFO << "DriveControl.trackWind()";
}

/**
 *  Set maximum drive rate that can be requested by the drive sytem
 *  software
 */
void DriveControlImpl::setMaxRate ( float azRate, float elRate )
{
  log_ << Priority::INFO
      << "DriveControl.setMaxRates( azRate=" << azRate << ", elRate=" << elRate << " )";

  setAzMaxRate(azRate);
  setElMaxRate(elRate);
}

/**
 *  Set maximum drive rate that can be requested by the drive sytem
 *  software
 */
void DriveControlImpl::setAzMaxRate ( float azRate )
{
  log_ << Priority::INFO
      << "DriveControl.setAzRate( azRate=" << azRate << " )";

  // 6m dishes max out at ~2.0deg/sec in AZ
  if ( azRate > 2.0 || azRate < 0.0 )
    throw CARMA_EXCEPTION( UserException,
	"Az Max Rate can only be [0.0,2.0] deg/sec for 6m dish" );

  DWSET(command,DriveCommand::SET_MAX_AZ_RATE);
  DWSET(maxRateAz,azRate);
  drvWriter_->write();
}

/**
 *  Set maximum drive rate that can be requested by the drive sytem
 *  software
 */
void DriveControlImpl::setElMaxRate ( float elRate )
{
  log_ << Priority::INFO
      << "DriveControl.setElRate( elRate=" << elRate << " )";

  // 6m dishes max out at ~1.5deg/sec
  if ( elRate > 1.5 || elRate < 0.0 )
    throw CARMA_EXCEPTION( UserException,
	"El Max Rate can only be [0.0,1.5] deg/sec for 6m dish" );

  DWSET(command,DriveCommand::SET_MAX_EL_RATE);
  DWSET(maxRateEl,elRate);
  drvWriter_->write();
}

// Deprecated/removed?
//
///**
// *  Sst encoder limits
// */
//void
//DriveControlImpl::setEncoderLimits( long azMin, long azMax,
//                                long elMin, long elMax )
//{
//
//  log_ << Priority::INFO << "DriveControl.setEncoderLimits("
//      << " azMin=" << azMin
//      << " azMax=" << azMax
//      << " elMin=" << elMin
//      << " elMax=" << elMax << " )";
//
//  // STUB
//}

// Deprecated/removed?
//
///**
// *  Set Encoder counts per 1 revolution
// */
//void
//DriveControlImpl::setEncoderCountsPerTurn( long az, long el )
//{
//  log_ << Priority::INFO << "DriveControl.setEncoderCountsPerTurn("
//      << " az=" << az
//      << " el=" << el << " )";
//
//  // STUB
//}

/**
 *  Set the antenna location (absolute)
 */
void DriveControlImpl::setAntLocation( double longitude,
                                   double latitude,
				   double altitude )
{
  log_ << Priority::INFO << "DriveControl.setAntLocation("
      << " longitude=" << longitude << ","
      << " latitude=" << latitude << ","
      << " altitude=" << altitude << " )";

  DWSET(command,DriveCommand::SET_ANTENNA_LOCATION);
  DWSET(longitude,longitude);
  DWSET(latitude,latitude);
  DWSET(altitude,altitude);
  drvWriter_->write();
}

/**
 *  Set the value of the sequence number to be associated with the
 *  next Drive subsystem command.
 */
void DriveControlImpl::setNextSequenceNo(CORBA::ULong seq)
{
  DWSET( command, DriveCommand::SET_NEXT_SEQ_NO );
  DWSET( seq, seq );
  drvWriter_->write();
}

/**
 *  set the pointing mode. (Enumeration)
 *
void
DriveControlImpl::
setPointing(carma::antenna::common::DriveControl::Model
            pointing) {
  log_ << Priority::INFO << "DriveControl.setPointing( ... )";

  // STUB
}
*/


/**
 *  Go to requested Az/El
 */
void DriveControlImpl::setAzel ( const double az,
                                 const double el,
                                 const CORBA::ULong seq )
{
  log_ << Priority::INFO << "DriveControl.setAzel"
    << "( az=" << az << ", el=" << el << ", seq=" << seq << " )";

  if ( az < -90.0 || az > 450.0 )
    throw CARMA_EXCEPTION( UserException, "Target Az outside of [-90,450] range" );

  if ( el < 1.0 || el > 170.0 )
    throw CARMA_EXCEPTION( UserException, "Target El outside of [1,170] range" );

  setNextSequenceNo( seq );
  setAz(az);
  setEl(el);
}

/**
 *  Go to requested Az
 */
void DriveControlImpl::setAz ( const double az, const CORBA::ULong seq )
{
  log_ << Priority::INFO << "DriveControl.setAz( az=" << az
    << ", seq=" << seq << " )";

  setNextSequenceNo( seq );
  setAz( az );
}

void DriveControlImpl::setAz ( const double az )
{
  DWSET(command,DriveCommand::SET_AZ);
  DWSET(az,az);
  drvWriter_->write();
}

/**
 *  Go to requested El
 */
void DriveControlImpl::setEl ( const double el, const CORBA::ULong seq )
{
  log_ << Priority::INFO << "DriveControl.setEl( el=" << el
    << ", seq=" << seq << " )";

  setNextSequenceNo( seq );
  setEl( el );
}

void DriveControlImpl::setEl ( const double el )
{
  DWSET(command,DriveCommand::SET_EL);
  DWSET(el,el);
  drvWriter_->write();
}

// Deprecated/removed?
//
///**
// *  Set the slew rate
// */
//void DriveControlImpl::setSlewRate ( unsigned long axes,
//                                 long azRate, long elRate )
//{
//  log_ << Priority::INFO << "DriveControl.setSlewRate("
//      << ", axes=" << axes
//      << ", azRate=" << azRate
//      << ", elRate=" << elRate << " )";
//
//  // STUB
//}

/**
 *  Set Ra/Dec triplet with an informative source name.
 */
void DriveControlImpl::track (
    const char* source,
    const carma::antenna::common::DriveControl::RaDecTriplet & positionTriplet,
    carma::antenna::common::DriveControl::AzWrapMode azWrapMode,
    CORBA::Boolean overTheTop,
    const CORBA::ULong seq )
{
  log_ << Priority::INFO
      << "DriveControl.track( source='" << source
       << ", triplet=<unlogged>" // << raDecTripletToString( positionTriplet )
       << ", seq=" << seq << " )";

  // zero out offsets with each new source
  setOffset(0.0, 0.0, seq + 10);

  setNextSequenceNo( seq );

  // Preset this with a default to avoid compiler warning...
  DriveCommand::AzWrapModeType dcWrapMode = DriveCommand::ZERO;

  switch (azWrapMode)
  {
    case carma::antenna::common::DriveControl::ZERO:
      dcWrapMode = DriveCommand::ZERO;
      break;
    case carma::antenna::common::DriveControl::ADD:
      dcWrapMode = DriveCommand::ADD;
      break;
    case carma::antenna::common::DriveControl::SUB:
      dcWrapMode = DriveCommand::SUB;
      break;
  }

  bool discontinuity = true; // True for first run, false for subsequent runs
  const CORBA::Long count = positionTriplet.length( );
  for ( CORBA::Long i = 0; i < count; ++i ) {
    const double mjd = positionTriplet[i].mjd;
    const double ra = positionTriplet[i].ra;
    const double dec = positionTriplet[i].dec;

    bimaShm_->putData( "SOURCE", source, 10 );
    DWSET(command,DriveCommand::SET_RA_DEC);
    DWSET(mjd,mjd);
    DWSET(ra,ra);
    DWSET(dec,dec);
    DWSET(discontinuity,discontinuity);
    DWSET(azWrapMode,dcWrapMode);
    DWSET(overTheTop,overTheTop);
    drvWriter_->write();
    discontinuity = false;
  }
}

void
DriveControlImpl::updateRaDec(
    const carma::antenna::common::DriveControl::RaDecEpoch & position,
    carma::antenna::common::DriveControl::AzWrapMode azWrapMode )
{
  log_ << Priority::INFO
      << "DriveControl.track(position='" << raDecEpochToString(position) << ")";

  // Preset this with a default to avoid compiler warning...
  DriveCommand::AzWrapModeType dcWrapMode = DriveCommand::ZERO;

  switch (azWrapMode)
  {
    case carma::antenna::common::DriveControl::ZERO:
      dcWrapMode = DriveCommand::ZERO;
      break;
    case carma::antenna::common::DriveControl::ADD:
      dcWrapMode = DriveCommand::ADD;
      break;
    case carma::antenna::common::DriveControl::SUB:
      dcWrapMode = DriveCommand::SUB;
      break;
  }

  const bool discontinuity = false; // Always false for update cycle
  const double mjd = position.mjd;
  const double ra = position.ra;
  const double dec = position.dec;

  DWSET(command,DriveCommand::SET_RA_DEC);
  DWSET(mjd,mjd);
  DWSET(ra,ra);
  DWSET(dec,dec);
  DWSET(discontinuity,discontinuity);
  DWSET(azWrapMode,dcWrapMode);
  drvWriter_->write();
}

void DriveControlImpl::setMountOffset( double az, double el,
                                       const CORBA::ULong seq  )
{
  log_ << Priority::INFO
       << "DriveConrolImpl.setMountOffset( az=" << az
       << ", el=" << el << ", ...)";

  setNextSequenceNo( seq );
  setAzMountOffset(az);
  setElMountOffset(el);
}

void DriveControlImpl::setAzMountOffset( double az, const CORBA::ULong seq )
{
  log_ << Priority::INFO
       << "DriveConrolImpl.setAzMountOffset( az=" << az
       << ", seq=" << seq << " )";

  setNextSequenceNo( seq );
  setAzMountOffset( az );
}

void DriveControlImpl::setAzMountOffset( const double az )
{
  DWSET(command,DriveCommand::SET_AZ_MOUNT_OFFSET);
  DWSET(azOffsetMount,az);
  drvWriter_->write();
}

void DriveControlImpl::setElMountOffset( double el, const CORBA::ULong seq )
{
  log_ << Priority::INFO
       << "DriveConrolImpl.setElMountOffset( az=" << el
       << ", seq=" << seq << " )";

  setNextSequenceNo( seq );
  setElMountOffset( el );
}

void DriveControlImpl::setElMountOffset( const double el )
{
  DWSET(command,DriveCommand::SET_EL_MOUNT_OFFSET);
  DWSET(elOffsetMount,el);
  drvWriter_->write();
}

void DriveControlImpl::setOffset( double az, double el, const CORBA::ULong seq )
{
  log_ << Priority::INFO
       << "DriveConrolImpl.setOffset( az=" << az
       << ", el=" << el << ", seq= " << seq << " )";

  setNextSequenceNo( seq );
  setAzOffset(az);
  setElOffset(el);
}

void DriveControlImpl::setAzOffset( double az, const CORBA::ULong seq )
{
  log_ << Priority::INFO
       << "DriveConrolImpl.setOffset( az=" << az << ", seq=" << seq << " )";

  setNextSequenceNo( seq );
  setAzOffset( az );
}

void DriveControlImpl::setAzOffset( const double az )
{
  DWSET(command,DriveCommand::SET_AZ_OFFSET);
  DWSET(azOffset,az);
  drvWriter_->write();
}

void DriveControlImpl::setElOffset( double el, const CORBA::ULong seq )
{
  log_ << Priority::INFO
       << "DriveConrolImpl.setElOffset( el=" << el << ", seq=" << seq << " )";

  setNextSequenceNo( seq );
  setElOffset( el );
}

void DriveControlImpl::setElOffset( const double el )
{
  DWSET(command,DriveCommand::SET_EL_OFFSET);
  DWSET(elOffset,el);
  drvWriter_->write();
}

void DriveControlImpl::updateWeather( float ambTemp, float baroPressure,
              float relHumidity, float dewpoint,
              float windSpeed, float windDir)
{
  log_ << Priority::INFO
       << "DriveConrolImpl.updateWeather( ambTemp=" << ambTemp
       << ", baroPressure=" << baroPressure
       << ", relHumidity="  << relHumidity
       << ", dewpoint="     << dewpoint
       << ", windSpeed=" << windSpeed
       << ", windDir=" << windDir << " )";

  DWSET(command,DriveCommand::UPDATE_WEATHER);
  DWSET(ambientTemp,ambTemp);
  DWSET(barometricPressure,baroPressure);
  DWSET(relativeHumidity,relHumidity);
  DWSET(dewpointTemp,dewpoint);
  DWSET(windSpeed,windSpeed);
  DWSET(windDirection,windDir);
  drvWriter_->write();
}

void
DriveControlImpl::
selectAperture(carma::antenna::common::DriveControl::Aperture
               aperture)
{
  log_ << Priority::INFO << "DriveControl.selectAperture( "
       << " aperture=" << aperture
       << " )";

  // Preset this with a default to avoid compiler warning...
  DriveCommand::ApertureType dcAperture = DriveCommand::RADIO3MM;

  switch(aperture) {
    case carma::antenna::common::DriveControl::OPTICAL:
      dcAperture=DriveCommand::OPTICAL;
      break;
    case carma::antenna::common::DriveControl::RADIO1MM:
      dcAperture=DriveCommand::RADIO1MM;
      break;
    case carma::antenna::common::DriveControl::RADIO3MM:
      dcAperture=DriveCommand::RADIO3MM;
      break;
    case carma::antenna::common::DriveControl::RADIO1CM:
      dcAperture=DriveCommand::RADIO1CM;
      break;
  }

  DWSET(command,DriveCommand::SELECT_APERTURE);
  DWSET(aperture,dcAperture);
  drvWriter_->write();
}

void
DriveControlImpl::
setTiltmeterZero(float aftForward, float leftRight)
{
  log_ << Priority::INFO << "DriveControlImpl.setTiltmeterZero( "
       << " aftForwar= " << aftForward
       << " leftRight= " << leftRight
       << " )";

  // STUB
}


void
DriveControlImpl::
setAperturePointingConstants(
       carma::antenna::common::DriveControl::Aperture aperture,
       float azOffset, float elOffset, float sag)
{
  log_ << Priority::INFO << "DriveControlImpl.setAperturePointingConstants( "
       << " aperture= " << aperture
       << " azOffset= " << azOffset
       << " elOffset= " << elOffset
       << " sag= "      << sag
       << " )";

  // Preset this with a default to avoid compiler warning...
  DriveCommand::ApertureType dcAperture = DriveCommand::RADIO3MM;

  switch(aperture) {
    case carma::antenna::common::DriveControl::OPTICAL:
      dcAperture=DriveCommand::OPTICAL;
      break;
    case carma::antenna::common::DriveControl::RADIO1MM:
      dcAperture=DriveCommand::RADIO1MM;
      break;
    case carma::antenna::common::DriveControl::RADIO3MM:
      dcAperture=DriveCommand::RADIO3MM;
      break;
    case carma::antenna::common::DriveControl::RADIO1CM:
      dcAperture=DriveCommand::RADIO1CM;
      break;
  }


  DWSET(command,DriveCommand::SET_APERTURE_POINTING_CONSTANTS);
  DWSET(aperture,dcAperture);
  DWSET(apertureOffsetAz,azOffset);
  DWSET(apertureOffsetEl,elOffset);
  DWSET(sag,sag);
  drvWriter_->write();
}

void
DriveControlImpl::
setPointingModelCoefs(
  const ::carma::antenna::bima::control::DriveControl::sequence_double& dazCoefs,
  const ::carma::antenna::bima::control::DriveControl::sequence_double& delCoefs)
{
  if(dazCoefs.length() > DriveCommand::dazCoefsCount
  || delCoefs.length() > DriveCommand::delCoefsCount) {
    ostringstream oss;
    oss << "Too many coefficients ("
      << dazCoefs.length() << "," << delCoefs.length() << ")";
    CARMA_EXCEPTION(carma::util::UserException,oss.str().c_str());
  }

  unsigned int i;
  ostringstream oss;

  oss << "DriveControlImpl.setPointingModelCoefs( [";
  for(i = 0; i < dazCoefs.length(); ++i) {
    oss << dazCoefs[i];
    oss << ((i < dazCoefs.length()-1) ? "," : "],[");
  }
  for(unsigned int i = 0; i < delCoefs.length(); ++i) {
    oss << delCoefs[i];
    oss << ((i < delCoefs.length()-1) ? "," : "])");
  }
  log_ << Priority::INFO << oss;

  DWSET(command,DriveCommand::SET_POINTING_MODEL_COEFS);
  // daz ceofs
  for(i=0; i<dazCoefs.length(); ++i) {
    DWSETA(dazCoefs,i,dazCoefs[i]);
  }
  for(;i<DriveCommand::dazCoefsCount;++i) {
    DWSETA(dazCoefs,i,0);
  }
  // del ceofs
  for(i=0; i<delCoefs.length(); ++i) {
    DWSETA(delCoefs,i,delCoefs[i]);
  }
  for(;i<DriveCommand::delCoefsCount;++i) {
    DWSETA(delCoefs,i,0);
  }
  drvWriter_->write();
}



void DriveControlImpl::setTolerance( float arcsec )
{
  DWSET( command, DriveCommand::SET_TOLERANCE );
  DWSET( tolerance, arcsec );
  drvWriter_->write();
}

void DriveControlImpl::setSafeRange( float azLow, float azHigh,
	                             float elLow, float elHigh )
{
  _safeCalled = true;

  if ( azLow < azHigh && elLow < elHigh )
  {
    DWSET( command, DriveCommand::SET_SAFE_RANGE );
    DWSET( azLow, azLow );
    DWSET( azHigh, azHigh );
    DWSET( elLow, elLow );
    DWSET( elHigh, elHigh );

    drvWriter_->write();
  }
  else
  {
    ostringstream oss;
    string msg;

    oss << "Invalid safe range ";
    if ( azLow > azHigh )
      oss << " azLow > azHigh";

    if ( elLow > elHigh )
      oss << " elLow > elHigh";

    msg = oss.str();
    throw CARMA_EXCEPTION( UserException, msg.c_str() );
  }
}
