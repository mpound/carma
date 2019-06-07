/**@file
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.72 $
 * $Date: 2012/12/17 20:51:19 $
 * $Id: DriveMgrThread.cc,v 1.72 2012/12/17 20:51:19 plambeck Exp $
 */


#include <cmath>

// CARMA includes
#include "carma/antenna/bima/DriveMgrThread.h"
#include "carma/services/Location.h"
#include "carma/services/Ephemeris.h"
#include "carma/services/Physical.h"
#include "carma/util/QuadraticInterpolatorPositiveAngle.h"
#include "carma/util/QuadraticInterpolatorSignedAngle.h"
#include "carma/util/Time.h"

using namespace std;
using namespace log4cpp;
using namespace carma::util;
using namespace carma::services;
using namespace carma::antenna::bima;

bool DriveMgrThread::_ok = true;

#define DRGET(N) DCGET(_drvReader, N )
#define DRGETA(N,I) DCGETA(_drvReader, N , I )

DriveMgrThread::DriveMgrThread
(
 Configuration& config,
 Drives &drives
 ):
 _drives( drives ),
 _config( config ),
 _logger( Program::getLogger() )
{
  _name = string( "DriveMgrThread" );

  _drvReader = new IPQreader<DriveCommand>( DRIVESIPQ, true, IPQLEN );

  // Reset stall indicator only at start up
  _drives.zeroStallMJD();

  // reset drive power check
  _drives.setAzCheckDrivePower(false);
  _drives.setElCheckDrivePower(false);

  // Zero out offsets at start up, subarrayInit installs new ones after
  // init..
  _azOffset = _elOffset = _azMountOffset = _elMountOffset
    = _azApertureOffset = _elApertureOffset = 0.0;

  _lastAzOffsetRads = _lastElOffsetRads = 0.0;

  _lastObsFreq = 0.0;

  // Default _lastCUEEwhat
  _lastCUEEwhat = string("");
}

bool DriveMgrThread::isOk()
{
  return _ok;
}

void DriveMgrThread::run()
{

  CARMA_ASSERT( _drvReader != static_cast<IPQreader<DriveCommand>*>(NULL) );
  _drvReader->setNoneAvailable();

  double dazrad=0, delrad=0;
  Location location("carma");
  QuadraticInterpolatorPositiveAngle raqi(0.0);
  QuadraticInterpolatorSignedAngle decqi(0.0);
  Ephemeris eph(location);
  Drives::StateType driveState = Drives::STOP;
  Units units;
  bool discontinuity = false;

  eph.setAzElOffsets(dazrad,delrad);
  eph.setRefraction(false);

  int strobe = 0;

  while ( true )
  {
    strobe++;

    try
    {
      // This will check for any commands that may have
      // been issued since the last time we checked.
      // This command check will happen approximately
      // every .08 seconds
      if ( _drvReader->isDataAvailable() )
      {
        _drvReader->read();
        _command = DRGET(command);

        switch ( _command )
        {
          case DriveCommand::NOOP:
            break;

          case DriveCommand::STOW:
            {
              if ( _drives.getState() != Drives::DISABLE )
              {
                DriveCommand::PositionType stowPosition = DRGET(stowPosition);
		driveState = Drives::STOW;
                _logger << Priority::DEBUG << "DriveMgrThread got stow(" << stowPosition << ")";
                _drives.setStowPosition(stowPosition);
		_drives.setState( driveState );
		_drives.setMode( Drives::STOWMODE );
              }
            }
            break;

          case DriveCommand::STOP:
            {
              _logger << Priority::DEBUG << "DriveMgrThread got stop()";
              driveState = Drives::STOP;
              _drives.stop();
              _drives.setStallRetries( 0 );
              _drives.setState(driveState);
              _drives.setMode( Drives::STOPMODE );
            }
            break;

          case DriveCommand::SNOW:
            {
              if ( _drives.getState() != Drives::DISABLE )
              {
                _logger << Priority::DEBUG << "DriveMgrThread got snow()";
                driveState = Drives::SNOW;
                _drives.snow();
                _drives.setState(driveState);
		_drives.setMode( Drives::SNOWMODE );
              }
            }
            break;

          case DriveCommand::SET_ANTENNA_LOCATION:
            {
              double longitude = DRGET(longitude);
              double latitude = DRGET(latitude);
              double altitude = DRGET(altitude);
              _logger << Priority::DEBUG << "DriveMgrThread got setAntennaLocation("
                << longitude << ","
                << latitude << ","
                << altitude << ")";
              location.setLongitude(longitude,"radians");
              location.setLatitude(latitude,"radians");
              location.setAltitude(altitude,"meters");
              eph.setLocation(location);

	      _drives.setLatitude( units.convert(latitude,"radians","degrees") );
	      _drives.setLongitude( units.convert(longitude,"radians","degrees") );
	      _drives.setAltitude( altitude );
            }
            break;

          case DriveCommand::SET_NEXT_SEQ_NO:
            {
              _drives.setNextSequenceNo( DRGET(seq) );
	      _drives.resetSettle(); // make sure seqno is handled properly
            }
            break;

          case DriveCommand::SET_WRAP:
            {
              _drives.setWrapLogic( (Drives::WrapLogic)DRGET(azWrapMode) );
              // DEFER
            }
            break;

          case DriveCommand::SET_AZ:
            {
              if ( (_drives.isComputerCtl()==false) || // if manual mode
                   (_drives.isKey()==true) ) {         // or disabled by key,
                driveState = Drives::DISABLE; // update driveState 
		        _drives.setState(driveState); // and shared memory, and then move on.
              } else {  // we're in computer control and the key is enabled, so do it.
                double az = DRGET(az);
                _logger << Priority::DEBUG << "DriveMgrThread got setAz(" << az << ")";
                double now = carma::util::Time::MJD();
                _drives.setTargetAz(10, now, az, true);
		        _drives.setRawTargetAz( az );
		        driveState = Drives::TRACK;
		        _drives.setState(driveState);
		        _drives.setMode( Drives::AZEL );
	         	_drives.resetSettle(); // make sure seqno is handled properly
              }
            }
            break;

          case DriveCommand::SET_EL:
            {
              if ( (_drives.isComputerCtl()==false) || // if manual mode
                   (_drives.isKey()==true) ) {         // or disabled by key,
                driveState = Drives::DISABLE; // update driveState 
		        _drives.setState(driveState); // and shared memory, and then move on.
              } else {  // we're in computer control and the key is enabled, so do it.
                double el = DRGET(el);
                _logger << Priority::DEBUG << "DriveMgrThread got setEl(" << el << ")";
                double now = carma::util::Time::MJD();
                _drives.setTargetEl(now, el, true);
		        _drives.setRawTargetEl( el );
		        driveState = Drives::TRACK;
                _drives.setState(driveState);
		        _drives.setMode( Drives::AZEL );
		        _drives.resetSettle(); // make sure seqno is handled properly
              }
            }
            break;

          case DriveCommand::SET_MAX_AZ_RATE:
            {
              _drives.setMaxAzRate( DRGET( maxRateAz ) );
            }
            break;

          case DriveCommand::SET_MAX_EL_RATE:
            {
              _drives.setMaxElRate( DRGET( maxRateEl ) );
            }
            break;

          case DriveCommand::SET_RA_DEC:
            {
              if ( (_drives.isComputerCtl()==false) || // if manual mode
                   (_drives.isKey()==true) ) {         // or disabled by key,
                driveState = Drives::DISABLE; // update driveState 
		        _drives.setState(driveState); // and shared memory, and then move on.
              } else {  // we're in computer control and the key is enabled, so do it.
                double mjd = DRGET(mjd);
                double ra = DRGET(ra);
                double dec = DRGET(dec);
		        bool disconnect = DRGET(discontinuity);
		        Drives::WrapLogic wrapMode = (Drives::WrapLogic)DRGET(azWrapMode);
		        bool overTop = DRGET(overTheTop);

                _logger << Priority::DEBUG <<
		           "DriveMgrThread got setRaDec(" << mjd << "," << ra << "," << dec <<
		           "," << discontinuity << "," << wrapMode << "," << overTop << ")";

   		        driveState = Drives::TRACK;
		        _drives.setMode( Drives::EQUAT );
                raqi.lock();
                decqi.lock();

                if ( disconnect == true ) {
                  raqi.empty();
                  decqi.empty();
                  discontinuity = true;
                }

                raqi.extend(mjd,ra);
                decqi.extend(mjd,dec);

                raqi.unlock();
                decqi.unlock();

                _drives.setRA(ra);
                _drives.setDec(dec);
	 	        _drives.setWrapLogic( wrapMode );
		        _drives.setOverTheTop( overTop );
                _drives.setState(driveState);
		        _drives.resetSettle(); // make sure seqno is handled properly
              }
            }
            break;

          case DriveCommand::UPDATE_WEATHER:
            {
	      _logger << Priority::DEBUG << "DriveMgrThread got weather(Ta=" << DRGET(ambientTemp) << ")";
	      // Used in monitor stream for "sky temp" ... durrr...
	      _drives.setAmbWeatherTemp( DRGET(ambientTemp)
		  - carma::services::constants::Physical::ABS_ZERO);

              eph.setWeather(
                  DRGET(barometricPressure),
                  _drives.getAmbWeatherTemp(),
                  DRGET(relativeHumidity));
              _drives.setWindDir( DRGET(windDirection) );
            }
            break;

          case DriveCommand::SET_AZ_MOUNT_OFFSET:
            {
              _azMountOffset = DRGET( azOffsetMount );
              _drives.setMountOffsetAz( _azMountOffset );
            }
            break;

          case DriveCommand::SET_EL_MOUNT_OFFSET:
            {
              _elMountOffset = DRGET( elOffsetMount );
              _drives.setMountOffsetEl( _elMountOffset );
            }
            break;

          case DriveCommand::SET_AZ_OFFSET:
            {
              _azOffset = DRGET(azOffset);
              _logger << Priority::DEBUG << "DriveMgrThread got setAzOffset(" << _azOffset;
              _drives.setOffsetAz( _azOffset );
	      // The next line forces the current target to be updated with the new
	      // user offset
	      // Only change this in AZEL Mode!
	      if ( _drives.getMode() == Drives::AZEL )
	      {
		_drives.setTargetAz( 11, Time::MJD(),
		    _drives.getRawTargetAz(), true );
	      }

	      _drives.resetSettle(); // make sure seqno is handled properly

            }
            break;

          case DriveCommand::SET_EL_OFFSET:
            {
              _elOffset = DRGET(elOffset);
              _logger << Priority::DEBUG << "DriveMgrThread got setElOffset(" << _elOffset;
              _drives.setOffsetEl( _elOffset );
	      // The next line forces the current target to be updated with the new
	      // user offset
	      // Only change this in AZEL Mode!
	      if ( _drives.getMode() == Drives::AZEL )
		_drives.setTargetEl( Time::MJD(),
		    _drives.getRawTargetEl(), true );

	      _drives.resetSettle(); // make sure seqno is handled properly

            }
            break;

          case DriveCommand::SET_OFFSET_PATH:
            {
              // DEFER
            }
            break;

          case DriveCommand::SET_APERTURE_POINTING_CONSTANTS:
            {
	      DriveCommand::ApertureType ap = DRGET( aperture );
              _azApertureOffset = DRGET( apertureOffsetAz );
              _elApertureOffset = DRGET( apertureOffsetEl );

              _drives.setApertureOffsetAz( ap, _azApertureOffset );
              _drives.setApertureOffsetEl( ap, _elApertureOffset );
	      _drives.setApertureOffsetSag( ap, DRGET( sag ) );

	      // if these apply to the current aperture
	      // force the update to all pointing info b
	      // re-calling setAperture
	      if ( _drives.getAperture() == ap )
		_drives.setAperture( ap );

	      _drives.resetSettle(); // make sure seqno is handled properly
            }
            break;

          case DriveCommand::SELECT_APERTURE:
            {
              _logger << Priority::DEBUG
                << "DriveMgrThread got selectAperture(" << DRGET( aperture ) << ")";
              switch ( DRGET( aperture ) )
              {
                case DriveCommand::OPTICAL:
                  _drives.setModel( Drives::OPTICAL );
		  _drives.setAperture( DRGET( aperture ) );
		  // Make sure we have uptodate freq
		  eph.setFreq( 4e14 ); // Tail end of infrared
		  eph.setRefraction(true); 
                  break;
                case DriveCommand::RADIO1MM:
                case DriveCommand::RADIO3MM:
                case DriveCommand::RADIO1CM:
                default:
                  _drives.setModel( Drives::RADIO );
		  _drives.setAperture( DRGET( aperture ) );

		  // Freq update also happens in the control loop because
		  // the freq might change without any model change
		  // for the radio.  This is as opposed to OPTICAL
		  // which will always have the same freq for
		  // refraction corrections.
		  double obsfreq = 0.0;
		  if ( DRGET( aperture ) == DriveCommand::RADIO1MM )
		    obsfreq = 235e9;
		  else if ( DRGET( aperture ) == DriveCommand::RADIO3MM )
		    obsfreq = 88.5e9;
		  else if ( DRGET( aperture ) == DriveCommand::RADIO1CM )
		    obsfreq = 33e9;
 
		  eph.setFreq( obsfreq );
		  eph.setRefraction(true); // don't apply refract until known working
                  break;
              }

	      _drives.resetSettle(); // make sure seqno is handled properly
            }
            break;

          case DriveCommand::SET_POINTING_MODEL_COEFS:
            {
              double dazCoefs[DriveCommand::dazCoefsCount];
              double delCoefs[DriveCommand::delCoefsCount];
              for(unsigned int i=0; i<DriveCommand::dazCoefsCount; ++i) {
                dazCoefs[i] = DRGETA(dazCoefs,i);
              }
              for(unsigned int i=0; i<DriveCommand::delCoefsCount; ++i) {
                delCoefs[i] = DRGETA(delCoefs,i);
              }

              ostringstream oss;
              oss << "DriveMgrThread got setPointingModelCoefs([";
              for(unsigned int i = 0; i < DriveCommand::dazCoefsCount; ++i) {
                oss << dazCoefs[i];
                oss << ((i < DriveCommand::dazCoefsCount-1) ? "," : "],[");
              }
              for(unsigned int i = 0; i < DriveCommand::delCoefsCount; ++i) {
                oss << delCoefs[i];
                oss << ((i < DriveCommand::delCoefsCount-1) ? "," : "])");
              }
              _logger << Priority::DEBUG << oss.str();

              _drives.setPointingModelCoefs(
                  dazCoefs, DriveCommand::dazCoefsCount,
                  delCoefs, DriveCommand::delCoefsCount);

	      _drives.resetSettle(); // make sure seqno is handled properly
            }
            break;

          case DriveCommand::SET_TOLERANCE:
            {
              _drives.setTolerance( DRGET( tolerance ) );
            }
            break;
	  
	  case DriveCommand::SET_SAFE_RANGE:
	    {

	      _drives.setSafeRange(
		  DRGET( azLow ),
		  DRGET( azHigh ),
		  DRGET( elLow ),
		  DRGET( elHigh ) );

	    }

        } // switch _drvReader->getCommand()
      } // if _drvReader->isDataAvailable()

      // Work on tracking in EQUAT mode
      // If not in EQUAT (i.e. in AZEL), then we're tracking
      // a given AZ/EL and we should track on it, but there's
      // no need to go through the ra/dec math
      if( driveState == Drives::TRACK &&
	  _drives.getMode() == Drives::EQUAT ) {
        raqi.lock();
        decqi.lock();
        // 2 seconds into the future
        double mjd = carma::util::Time::MJD() + 2.0/(24*60*60);
        double ra = raqi.evaluate(mjd);
        double dec = decqi.evaluate(mjd);
        raqi.unlock();
        decqi.unlock();

        eph.setMJD(mjd);
	// Use ephem class to apply user az/el offsets in sky coords.
	// If user offsets have been modified since last call, restart
	// the interpolator
	if ( _lastAzOffsetRads != getAzOffsetRadians() ||
	     _lastElOffsetRads != getElOffsetRadians() )
	{
	  _lastAzOffsetRads = getAzOffsetRadians();
	  _lastElOffsetRads = getElOffsetRadians();
	  discontinuity = true;
	}
	eph.setAzElOffsets( getAzOffsetRadians(), getElOffsetRadians() );

	/*  Don't do this anymore, see bug # 307
	// Update the ephem with the current observing freq
	// if we're in the radio and the obs freq has not changed
	if ( _lastObsFreq != _drives.getObsFreq() && 
	    _drives.getModel() == Drives::RADIO )
	{
	  _lastObsFreq = _drives.getObsFreq();
	  eph.setFreq( _lastObsFreq );
	}
	*/

        Vector<double> azel = eph.getAzEl(mjd,ra,dec);
	float refract = eph.getRefrac() * 60.0; // now in arcmin, but updated via the previous getAzEl() 

        double az = units.convert(azel[0],"radians","degrees");
        double el = units.convert(azel[1],"radians","degrees");

	CSHMTRC( _drives, "DRVMGRRA", ra );
	CSHMTRC( _drives, "DRVMGRDEC", dec );
	CSHMTRC( _drives, "DRVMGRAZ", az );
	CSHMTRC( _drives, "DRVMGREL", el );

	_drives.setRefract( refract );

        CPTRACE( Trace::TRACE3,
		 "mjd=" << mjd << " ra=" << ra << " dec=" << dec << " az=" << az << "(" << azel[0] << ") el=" << el << "(" << azel[1] << ") refrac=" << refract );

	// OLD AUTOMATIC WRAP LOGIC
	// Now wrap logic is handled by the control system
	// We're either going to start slewing onto source or tracking on
	// it already.  To help with the wraps, filter the target az
	// to allow for moving through 0 or above 360 degrees
	//
	/* 
	if ((az < 45.) && (_drives.getAz() > 225.))
	{
	  az += 360;
	  CPTRACE( Trace::TRACE3, " smart wrapping az: " << az );
	}
	else if ((az > 315.) && (_drives.getAz() < 135.))
	{
	  az -= 360;
	  CPTRACE( Trace::TRACE3, " smart wrapping az: " << az );
	}
	*/

	float azSoftLow, azSoftHigh, ello, elhi;
	_drives.getSoftLimits( azSoftLow, azSoftHigh, ello, elhi );
	if ( _drives.getWrapLogic() == Drives::ADD )
	{
	  // If the control system (SubarrayControl) selects a position
	  // that combines with a wrap selection that exceeds the
	  // software/hardware limit, then it will not be modified
	  if ( az + 360. < azSoftHigh )
	    az += 360.;

	  CPTRACE( Trace::TRACE3, " control system added wrap az: " << az );
	}
	else if ( _drives.getWrapLogic() == Drives::SUB )
	{
	  if ( az - 360. > azSoftLow )
	    az -= 360.;

	  CPTRACE( Trace::TRACE3, " control system added wrap az: " << az );
	}

        _drives.setTargetAz( 13, mjd, az, discontinuity );
        _drives.setTargetEl( mjd, el, discontinuity );
        discontinuity = false;

	if ( _drives.isComputerCtl() == true )
	  _drives.setState(Drives::TRACK);
	else
	  _drives.setState(Drives::DISABLE);
      }

      // Let drives object know that we're not dead
      if ( _config.isEmulating() == false )
	_drives.toggle();
      else
	if ( strobe % 1000 == 0 )
	  _logger << Priority::INFO << "FYI, Drive Manager is Emulating ("
	    << strobe << ")";

      //      if ( _drives.getActiveState() == Drives::SLEW ||
      //	   _drives.getActiveState() == Drives::CLOSE )

      // Always updating at .08s now - colby 23oct2007
      usleep( _drives.getWaitInUSec() ); // encoder update rate

      // Update a counter to show activity
      CSHMTRC( _drives, "DRVCNTR", strobe );

      //      else
      //        usleep(500000);

    } catch ( carma::util::ErrorException &cuee ) {

      // If many exceptions of the same type
      // are being thrown, but we don't wish to quit
      // for the sake of robustness, then throttle the number of messages
      // that get pushed into the logger
      if ( _lastCUEEwhat == cuee.what() )
      {
        if ( _cueeThrottle < time(NULL) )
        {
          _logger << Priority::ERROR << cuee.getErrorMessage();
          _cueeThrottle = time(NULL) + 10;
        }
      }
      else
      {
        _cueeThrottle = time(NULL) + 5;
        _lastCUEEwhat = cuee.what();
      }
    }
    catch ( ... )
    {
      _logger << Priority::ERROR << "Un-caught exception thrown!  Ignoring...";
    }

  } // while true
} // void thread

void DriveMgrThread::thread( DriveMgrThread &This )
{
  This.run();
}

// User offset
double DriveMgrThread::getAzOffsetRadians()
{
  Units units;

  return (units.convert( _drives.getOffsetAz(), "arcminutes", "radians" ));
}

// User offset
double DriveMgrThread::getElOffsetRadians()
{
  Units units;
  return (units.convert( _drives.getOffsetEl(), "arcminutes", "radians" ));
}
