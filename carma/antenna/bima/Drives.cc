/**@file
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.108 $
 * $Date: 2012/12/12 18:51:14 $
 * $Id: Drives.cc,v 1.108 2012/12/12 18:51:14 plambeck Exp $
 */

#include <cmath>
#include <limits>

// CARMA includes
#include "carma/antenna/bima/Drives.h"

using namespace std;
using namespace log4cpp;
using namespace carma::util;
using namespace carma::services;
using namespace carma::antenna::bima;

#define NOW 0

Drives::Drives( Configuration& config, bool checkTelemHost ) :
  TelemetryClient( config, checkTelemHost ),
  _config( config ),
  _logger( Program::getLogger() )

{
  _name = string( "drives" );
  _forceVelLimit = 0;

  _lastToggleUpdate = time(NULL);
  _lastToggle = 0;
  _toggle = 0x8000;

  // Default to moving both axis
  pegAz( false );
  pegEl( false );

  loadDrivesConfig();

  _elDigWOMismatch = _azDigWOMismatch = 0;
  _lastAzRate = _lastElRate = _azStalls = _azStallCnt =
    _elStalls = _elStallCnt = 0;

  _rawTilts = new int[TILTSAMPLES];

  setState( STOP );
  setActiveState( STOP );

  if ( checkTelemHost )
  {
    _rx = new Rx( _config );
    _calwheel = new CalWheel( _config );
  }

  // Init az/el linear interpolator data points if needed
  double liInit[4];
  double mjd = Time::MJD();
  getData( "AZLINTPL", liInit, 4 );
  if(liInit[0] == 0.0) {
    setTargetAz( 0, mjd, 180.0, true );
  }
  setRawTargetAz( 180.0 );
  getData( "ELLINTPL", liInit, 4 );
  if(liInit[0] == 0.0) {
    setTargetEl( mjd, 9.0, true );
  } 
  setRawTargetEl( 9.0 );

  // Used to limit acceleration in software
  gettimeofday( &_lastLimChkAz, NULL );
  gettimeofday( &_lastLimChkEl, NULL );

  _azLastStallChk = Time::MJD();
  _elLastStallChk = Time::MJD();

  _settle = 0;  // avoid unset data from mucking up settle times
  _stallsStarted = time(NULL); // same, but for limitAxisAccel()
  _lastManWarn = 0;

  setAmbWeatherTemp( 0.0 ); // initialize value

  // Set some wide open defaults...
  // These only apply to collision "safe"
  // These values are typically the full mechanical range
  // of the 6m dishes
  // But in E-Array, oh baby, do they matter
  _azSafeLow = -70.0;
  _azSafeHigh = 450.0;
  _elSafeLow = 0.0;
  _elSafeHigh = 150.0;
  setSafeRange( _azSafeLow, _azSafeHigh, _elSafeLow, _elSafeHigh );

  setSoftLimits( _swAzLoLim, _swAzHiLim, _swElLoLim, _swElHiLim );

  // TO DO: Make these const
  setHardLimits( -70.0, 430.0, 0.0, 150.0 );
  setSafeCalled(0);
  setWrapLogic( Drives::ZERO );
}

void Drives::loadDrivesConfig()
{

  Table drvtab( _config.getDrivesConfFile() );
  vector<string> ants = drvtab.getColumn( "name" );

  CPTRACE( Trace::TRACE7, "Opened " << _config.getDrivesConfFile() );
  CPTRACE( Trace::TRACE7, "Looking for: " << _config.getAntenna() );

  int entry = 0;
  vector<string>::iterator i;
  for ( i = ants.begin(); i != ants.end(); ++i )
  {
    CPTRACE( Trace::TRACE7, "  Looking at: " << *i );
    if ( i->compare( _config.getAntenna() ) == 0 )
    {
      CPTRACE( Trace::TRACE7, "    Found it!" );
  
      _azOrigMaxSlew = _azMaxSlew = drvtab.getIntColumn( "azslew").at( entry );
      _elOrigMaxSlew = _elMaxSlew = drvtab.getIntColumn( "elslew").at( entry );

      _azRampDown = drvtab.getDoubleColumn( "azdown").at( entry );
      _elRampDown = drvtab.getDoubleColumn( "eldown").at( entry );

      _azRampStartSpeed = drvtab.getIntColumn( "azramp").at( entry );
      _elRampStartSpeed = drvtab.getIntColumn( "elramp").at( entry );

      _azCoarseOffset = drvtab.getIntColumn( "azoff").at( entry );
      _elCoarseOffset = drvtab.getIntColumn( "eloff").at( entry );

      _swAzLoLim = drvtab.getDoubleColumn( "azlolim" ).at( entry );
      _swAzHiLim = drvtab.getDoubleColumn( "azhilim" ).at( entry );
      _swElLoLim = drvtab.getDoubleColumn( "ellolim" ).at( entry );
      _swElHiLim = drvtab.getDoubleColumn( "elhilim" ).at( entry );

      _limitAccel = drvtab.getBoolColumn( "swslew" ).at( entry );

      _azOrigAccelLim = _azAccelLim = drvtab.getIntColumn( "azaccellim" ).at( entry );
      _elOrigAccelLim = _elAccelLim = drvtab.getIntColumn( "elaccellim" ).at( entry );

      _azSlewMod = (float)drvtab.getDoubleColumn( "azslewmod" ).at( entry );
      _elSlewMod = (float)drvtab.getDoubleColumn( "elslewmod" ).at( entry );

      _gain = (float)drvtab.getDoubleColumn( "servogain" ).at( entry );
      _mjdGainLookAhead =  _gain/(24*60*60);
      _gainConst = 1./_gain;
      putData( "DRVGAIN", &_gain );
      putData( "DRVMJDGAIN", &_mjdGainLookAhead );
      putData( "DRVGAINCST", &_gainConst );

      double waitInSec = (float)drvtab.getDoubleColumn( "servowait" ).at( entry );
      _waitInUsec = (int)(waitInSec * 1000000);
      putData( "DRVWAITSEC", &waitInSec );

      break;
    }
    entry++;
  }

  if ( i == ants.end() )
    throw CARMA_ERROR( "Unable to find " + _config.getAntenna()
        + " in " + _config.getDrivesConfFile() );
}

void Drives::toggle()
{
  time_t current = time(NULL);

  double curAz = getAz();
  double curEl = getEl();

  if ( isComputerCtl() == false )
  {
    setState( DISABLE );
    setActiveState( DISABLE );
    return;
  }

  getOneSecRateAz(); // compute and save current rate
  getOneSecRateEl(); // need for monitor stream to pick it up

  CPTRACE( Trace::TRACE3, "toggle() lastToggle: " << _lastToggle );
  _lastToggle = current - _lastToggleUpdate;

  CPTRACE( Trace::TRACE3, "toggle() lastToggle: " << _lastToggle );
  StateType state = getState();
  CPTRACE( Trace::TRACE3, "  toggle::before switch, state: " << state );
//  if ( _lastToggle > 1 && state != DISABLE && state != STOP )
  // Just always toggle when going through here.  This speeds up
  // responsiveness to commands.
  if ( state != DISABLE && state != STOP )
  {
    _lastToggleUpdate = current;
    _toggle = ( _toggle == 0 ) ? 0x8000 : 0;
  }
  CPTRACE( Trace::TRACE3, "toggle(): toggle: 0x" <<hex<< _toggle );

  updateSettle();

  // Avoid having the receivers be open to Cloudsat signals at zenith
  // Cloud sat broadcasts a 94GHz signal straight down from LEO
  // Since we're being slewed, close the ambient flap very early
  //
  // First, check if we're in the danger elevation
  //   If so, move the flap into position over the 3mm band (B)
  //   and set cloudsat to true
  //   Upon each new call, a check against cloudsat being false
  //   will keep the loop from calling setPosition again
  // Then, if we fall out of the dangerzone, and cloudsat is true
  //   we know that it's time to move the amb flap out of position
  //   and we reset cloudsat to false, so that the sky position
  //   is not called over and over again.
  // Note, getEl() is the fully corrected elevation
  static bool cloudsat = false;
  if ( curEl >= 89 && curEl <= 91 )
  {
    // this mucks up slewing/tracking around 90deg
    // this will have to wait until setPosition is
    // an async call...
    //    if ( cloudsat == false )
    //      _calwheel->setPosition( CalWheel::AMB, 'B' );

    cloudsat = true;
  }
  else if ( cloudsat == true )
  {
    cloudsat = false;
    // TODO:
    // the following may be a bad assumption.  What if someone
    // set the calwheel to somewhere else while at zenith
    // and this code fought that change.  It should actually go
    // back to the last commanded Position
    //   _calwheel->setPosition( CalWheel::SKY, _rx->getBand() );
  }

  switch ( state )
  {
    case TRACK:
    case CLOSE:
      //setMode( EQUAT );
      if ( ( fabs( getAzRequested() - curAz ) > _azRampDown ) || 
          ( fabs( getElRequested() - curEl ) > _elRampDown ) )
      {
        slew();
      }
      else
        servo();
      break;
    case SLEW:
      //   Should never get into here right now!!! 07mar06 - colby

      /*
      CPTRACE( Trace::TRACE3, "  toggle::SLEW reqaz: " << getAzRequested()
          << " reqel: " << getElRequested() << " az: " << getAz() << " el: " << getEl() );

      //setMode( AZEL );
      if ( onTarget() == true && ( _azVel == 0 && _elVel == 0 ) )
      {
        //setState( STOP );
        setCurSequenceNo( getNextSequenceNo() );
      }
      else
        slew();
        */

      break;

    case STOW:
      //setMode( STOWMODE );
      if ( onTarget() == true && ( _azVel == 0 && _elVel == 0 ) )
      {
        stow();
        setCurSequenceNo( getNextSequenceNo() );
      }
      else
        slew();

      break;

    case SNOW:
      //setMode( SNOWMODE );
      break;
    case DISABLE:
      stop();
      setActiveState( DISABLE );
      setCurSequenceNo( getNextSequenceNo() );
      break;
    case STOP:
      //setMode( STOPMODE );
    default:
      stop();
      break;
  }
}

double Drives::getTargetAz( double mjd )
{
  if(mjd <= 0) {
    mjd = Time::MJD();
  }
  double targetAz = evaluateLinearInterpolator( "AZLINTPL", mjd);
  return targetAz;
}

double Drives::getTargetEl( double mjd )
{
  if(mjd <= 0) {
    mjd = Time::MJD();
  }
  double targetEl = evaluateLinearInterpolator( "ELLINTPL", mjd);
  return targetEl;
}

void Drives::setTargetAz( int whence, double mjd, double az, bool discontinuity )
{
  putData( "TARGAZ", &whence );
  if ( mjd <= 0 )
  {
    mjd = Time::MJD();
  }

  // Now apply user selected offset if we are in AZEL mode
  // In EQUAT mode, these offsets are applied by the ephem
  // class in the DriveMgrThread.
  if ( getMode() == AZEL )
    az = az + getOffsetAz()/60.;

  updateLinearInterpolator( "AZLINTPL", mjd, az, discontinuity );
  setAzRequested(getTargetAz());
}

void Drives::setRawTargetAz( double az )
{
  putData( "RAWTRGAZ", &az );
}

double Drives::getRawTargetAz()
{
  double az;
  getData( "RAWTRGAZ", &az, 1 );
  return az;
}


void Drives::setRawTargetEl( double el )
{
  putData( "RAWTRGEL", &el );
}

double Drives::getRawTargetEl()
{
  double el;
  getData( "RAWTRGEL", &el, 1 );
  return el;
}

void Drives::setTargetEl( double mjd, double el, bool discontinuity )
{
  if ( mjd <= 0 )
  {
    mjd = Time::MJD();
  }

  // Now apply user selected offset if we are in AZEL mode
  // In EQUAT mode, these offsets are applied by the ephem
  // class in the DriveMgrThread.
  if ( getMode() == AZEL )
    el = el + getOffsetEl()/60.;

  updateLinearInterpolator( "ELLINTPL", mjd, el, discontinuity );
  setElRequested(getTargetEl());
}

void Drives::stop()
{
  unsigned short zero = 0;

  setActiveState( STOP );
  setStallRetries( 0 );
  setTargetAz( 1, Time::MJD(), getAz(), true );
  setTargetEl( Time::MJD(), getEl(), true );

  tpoke( "AZDVELF", zero );
  tpoke( "ELDVELF", zero );

  // To appease those that want to reset
  // slowless on the bima telescopes
  _stallsStarted = 0;
  _elLastStallChk = 0.0;
  _azLastStallChk = 0.0;
}

void Drives::slew( )
{
  unsigned short iAz, iEl;
  iAz = iEl = 0;

  // Get out if we're in DISABLE state
  if ( getState() == DISABLE )
  {
    setActiveState( DISABLE );
    return;
  }

  setActiveState( SLEW );
  // setState( SLEW );

  // In old code, normally there would be a check for
  // safe position here, but that's not included here
  // until a need to implement comes up...
  double currentAz = getAz();
  double currentEl = getEl();

  double targetAz = getTargetAz();
  double targetEl = getTargetEl();

  _distAz = targetAz - currentAz;
  _distEl = targetEl - currentEl;

  SHMTRC( "DISTAZ", _distAz );
  SHMTRC( "DISTEL", _distEl );

  CPTRACE( Trace::TRACE4, "slew start: _azVel: 0x" << hex << _azVel << " _elVel: 0x" <<hex<< _elVel );
  CPTRACE( Trace::TRACE4, "  iAz: 0x" <<hex<< iAz << " iEl: 0x" <<hex<< iEl );
  CPTRACE( Trace::TRACE4, "  targetAz: " << targetAz << " targetEl: " << targetEl
      << " currentAz: " << currentAz << " currentEl: " << currentEl );

  // Right now fault bits are always set because of a hardware problem
  // with all antennas' telemetry cards.
  //  if ( areFaultBits() == true )
  //    return;

  if ( _peggedAz )
  {
    _azVel = 0;
  }
  else
  {
    double mjd;

    SHMTRC( "AZMAXSLEW", _azMaxSlew );
    _azVel = ( _distAz > 0 ) ? _azMaxSlew : -_azMaxSlew;
    CPTRACE( Trace::TRACE3, " _azVel': " << _azVel );

    if ( fabs( _distAz ) < _azRampDown )
    {

      // Old style ramdown.  Now uses MJD look ahead to compute
      // a vel that is closer to tracking so that landing on a target
      // with subarc sec accuracy is possible
      // int rampdown = (int)( ( fabs( _distAz ) + .03 ) / _azRampDown * _azRampStartSpeed );
      // _azVel = ( _distAz > 0 ) ? rampdown : -rampdown;

      // 1 second in the future
      mjd = Time::MJD() + .00001157407;
      // Make sure vel doesn't go to zero before getting to within an arcsec
      // of the target...  Therefore, if rawVel is < 1, but we're not on target
      // force a slow vel to make sure we make that last .5 arc sec
      double rawVel = ( getTargetAz(mjd) - getAz() ) / 4.70e-4;
      _azVel = (int)(fabs(rawVel) > 1 ? (int)rawVel : (rawVel > 0 ? 3 : -3 ));
      SHMTRC( "AZRAWVEL", rawVel );

      CPTRACE( Trace::TRACE2, " _azVel''': " << _azVel << " targetAz: " << getTargetAz(mjd));
    }

    // Peg the speed to a particular amount
    if ( _azVel != 0 && _forceVelLimit != 0 )
      _azVel = ( _azVel > 0 ) ? _forceVelLimit: -_forceVelLimit;
  }

  if ( _peggedEl )
  {
    _elVel = 0;
  }
  else
  {
    double mjd = 0.;

    SHMTRC( "ELMAXSLEW", _elMaxSlew );
    _elVel = ( _distEl > 0 ) ? _elMaxSlew : -_elMaxSlew;
    CPTRACE( Trace::TRACE3, " _elVel': " << _elVel );

    if ( fabs( _distEl ) < _elRampDown )
    {
      // CPTRACE( Trace::TRACE3, " _distEl:" << _distEl << " _elRampDown:" << _elRampDown );
      // int rampdown = (int)( ( fabs( _distEl ) + .03 ) / _elRampDown * _elRampStartSpeed );
      // _elVel = ( _distEl > 0 ) ? rampdown : -rampdown;
      // 1 second in the future
      mjd = Time::MJD() + .00001157407;
      double rawVel = ( getTargetEl(mjd) - getEl() ) / 3.60e-4;
      _elVel = (int)(fabs(rawVel) > 1 ? (int)rawVel : (rawVel > 0 ? 3 : -3 ));
      SHMTRC( "ELRAWVEL", rawVel );
    }    

    // Peg the speed to a particular amount
    if ( _elVel != 0 && _forceVelLimit != 0 )
      _elVel = ( _elVel > 0 ) ? _forceVelLimit: -_forceVelLimit;

    CPTRACE( Trace::TRACE4, "    _elVel''': " << _elVel << " targetEl: " << getTargetEl(mjd));
  }

  CPTRACE( Trace::TRACE4, "    Drives::slew() azVel: " << _azVel
      << " elVel: " << _elVel );

  SHMTRC( "AZVELPREL", _azVel );
  SHMTRC( "ELVELPREL", _elVel );

  // start Artificially lower acceleration

  _azVel = limitAxisAccel( _azStalls, _azVel, getAzDigVeFilter(), _azAccelLim, _azOrigAccelLim,
      _azMaxSlew, _azOrigMaxSlew, _azSlewMod, _lastLimChkAz );
  _elVel = limitAxisAccel( _elStalls, _elVel, getElDigVeFilter(), _elAccelLim, _elOrigAccelLim,
      _elMaxSlew, _elOrigMaxSlew, _elSlewMod, _lastLimChkEl );

  markAzStalls( _azStalls );
  markElStalls( _elStalls );

  //   end Artificially lower acceleration

  SHMTRC( "AZVEL", _azVel );
  SHMTRC( "ELVEL", _elVel );

  // Now we have either a positive or neg velocity
  // But we want this number to be positive
  // Also note, these are different from the old BIMA system, where a neg velocity
  // went clockwise
  markAzVel( _azVel );
  markElVel( _elVel );
  iAz = ( _azVel > 0 ) ? _azVel : ( -_azVel | 0x1000 );
  iEl = ( _elVel > 0 ) ? ( _elVel | 0x1000 ) : -_elVel;
  iAz |= _toggle;
  iEl |= _toggle;

  CPTRACE( Trace::TRACE4, "slew end: _azVel: 0x" << hex << _azVel << " _elVel: 0x" <<hex<< _elVel );
  CPTRACE( Trace::TRACE4, "  iAz: 0x" <<hex<< iAz << " iEl: 0x" <<hex<< iEl );
  CPTRACE( Trace::TRACE4, "  targetAz: " << targetAz << " targetEl: " << targetEl
      << " currentAz: " << currentAz << " currentEl: " << currentEl );

  if ( isKey() == true ) // this means it's off.. brilliant.
  {
    iEl = 0;
    iAz = 0;
    setState( DISABLE );
    setActiveState( DISABLE );
  }

  // Last sanity check for manual mode
  if ( isComputerCtl() == false )
  {
    iEl = 0;
    iAz = 0;
    setState( DISABLE );
    setActiveState( DISABLE );
  }

  if ( isElLim() || isElULim() || isAzLim() || isAzULim() )
  {
    setHardLimit();
    iEl = 0;
    iAz = 0;
  }

  if ( currentAz < _swAzLoLim && _azVel < 0 ) 
  {
    setSoftLimit();
    iAz = 0;
  }

  if ( currentAz > _swAzHiLim && _azVel > 0 ) 
  {
    setSoftLimit();
    iAz = 0;
  }

  if ( currentEl < _swElLoLim && _elVel < 0 )
  {
    setSoftLimit();
    iEl = 0;
  }

  if ( currentEl > _swElHiLim && _elVel > 0 )
  {
    setSoftLimit();
    iEl = 0;
  }

  SHMTRC( "IAZPOSTLIM", iAz );
  SHMTRC( "IELPOSTLIM", iEl );

  if ( onTargetAz() )
    iAz = _azVel = 0;

  if ( onTargetEl() )
    iEl = _elVel = 0;

  SHMTRC( "IAZPOSTTRG", iAz );
  SHMTRC( "IELPOSTTRG", iEl );

  // STALL Detection & Handling -------------------------------------
  if ( ( Time::MJD() - _elLastStallChk ) > .000011574069 ) // after 1 sec
  {
    if ( isElStalled() )
      _elStallCnt++;
    else
      _elStallCnt = 0;

    _elLastStallChk = Time::MJD();
  }

  if ( ( Time::MJD() - _azLastStallChk ) > .000011574069 ) // after 1 sec
  {
    if ( isAzStalled() )
      _azStallCnt++;
    else
      _azStallCnt = 0;

    _azLastStallChk = Time::MJD();
  }

  SHMTRC( "AZLSTSTLCHK", _azLastStallChk );
  SHMTRC( "ELLSTSTLCHK", _elLastStallChk );
  SHMTRC( "AZSTALLCNT", _azStallCnt );
  SHMTRC( "ELSTALLCNT", _elStallCnt );

  /* 
     No longer needs to check for control switch this way
  if ( (_elStallCnt > 5) && (_azStallCnt > 5) )
  {
    // Throttle warning messages
    if ( _lastManWarn < (time(NULL) - 1200) )
    {
      _lastManWarn = time(NULL);
      _logger << Priority::ERROR
        << "Stall detected on azimuth and elevation, MANUAL SWITCH IS SET?";
      _logger << Priority::ERROR
        << "Disabling antenna!";
    }

    stop();
    setState( DISABLE );
    setActiveState( DISABLE );

    return;
  }
  */

  // Give the antenna time to get up to speed
  if ( _elStallCnt > 5 )
  {
    _logger << Priority::WARN
      << "Stall detected on elevation drive at " << getEl()
      << " deg, target is " << getTargetEl() << " deg, retrying!";
    _elStallCnt = 0;

    if ( getStallRetries() == 0 )
      markElStallMJD();

    setStallRetries( getStallRetries() + 1 );
  }

  // Give the antenna time to get up to speed
  if ( _azStallCnt > 5 )
  {
    _logger << Priority::WARN
      << "Stall detected on azimuth drive at " << getAz()
      << " deg, target is " << getTargetAz() << " deg, retrying!";
    _azStallCnt = 0;

    if ( getStallRetries() == 0 )
      markAzStallMJD();

    setStallRetries( getStallRetries() + 1 );
  }

  CPTRACE( Trace::TRACE2, "  Time::MJD() - getAzStallMJD() = " <<
      Time::MJD() - getAzStallMJD() );
  if ( Time::MJD() - getAzStallMJD() < .00001157407 ) // one sec in MJD
  {
    // If stall was detected within last second, force speed to 0
    // to back off pushing the drive
    iAz = 0;
    setActiveState( ERROR );
  }

  if ( Time::MJD() - getElStallMJD() < .00001157407 ) // one sec in MJD
  {
    // If stall was detected within last second, force speed to 0
    // to back off pushing the drive
    iEl = 0;
    setActiveState( ERROR );
  }

  if ( getStallRetries() > 4 )
  {
    if ( (Time::MJD() - getAzStallMJD() < .01388888888) || 
        (Time::MJD() - getElStallMJD() < .01388888888) ) // 20 minutes in MJD
    {
      _logger << Priority::ERROR 
        << "Multiple stalls detected within last 20 minutes, disabling antenna!";
      stop();
      setState( DISABLE );
      setActiveState( DISABLE );
    }

    if ( (Time::MJD() - getAzStallMJD() > .01388888888) &&
        (Time::MJD() - getElStallMJD() > .01388888888) ) // 20 minutes in MJD
    {
      // Clear out number of stalls if there haven't been any in 20+ minutes
      setStallRetries( 0 );
    }
  }

  SHMTRC( "IAZPOSTSTL", iAz );
  SHMTRC( "IELPOSTSTL", iEl );

  // END Stall Detection & Handling ---------------------------------

  CPTRACE( Trace::TRACE3, "  Drives::slew() lastiAzVel: 0x" << hex << _lastiAzVel << " iAz: 0x" << hex << iAz << " " << " _lastToggle: " << _lastToggle );
  if ( iAz != _lastiAzVel || _lastToggle > 2 )
  {
    tpoke( "AZDVELF", iAz );
    _lastiAzVel = iAz;
    SHMTRC( "IAZ", iAz );
  }

  CPTRACE( Trace::TRACE3, "  Drives::slew() lastiElVel: 0x" << hex << _lastiElVel << " iEl: 0x" << hex << iEl << " " << " _lastToggle: " << _lastToggle );
  if ( iEl != _lastiElVel || _lastToggle > 2 )
  {
    tpoke( "ELDVELF", iEl );
    _lastiElVel = iEl;
    SHMTRC( "IEL", iEl );
  }

}

void Drives::setOverTheTop( bool overTop )
{
  int ans = (overTop == true ? 1 : 0 );
  putData( "OVERTOP", &ans );
}

bool Drives::getOverTheTop()
{
  int ans;
  getData( "OVERTOP", &ans );
  return ( ans == 1 );
}

bool Drives::areFaultBits()
{
  int bits;
  bool status = false;  // default no fault/manual switch

  getData( "BASECTRL", &bits );

  if ( ( bits & 0xc786 ) != 0x4700 )
    status = true;  // there are fault bits or the manual switch is set

  return status;
}

bool Drives::isKeyOn()
{
  int bits;
  bool status = true;  // key is on by default

  getData( "BASECTRL", &bits );

  if ( bits & 0x800 )
  {
    // Old code gave it 5 ticks and checked again
    usleep( 50000 );
    getData( "BASECTRL", &bits );
    bits >>= 8;

    if ( bits & 0x800 )
      status = false;  // key is not on!
  }

  return status;
}

int Drives::getInstantAzResolver()
{
  int resolver;
  getData( "AZRSOLV", &resolver, 1 );

  return resolver;
}

int Drives::getInstantElResolver()
{
  int resolver;
  getData( "ELRSOLV", &resolver, 1 );

  return resolver;
}

int Drives::getSmoothedAzResolver()
{
  int resolver;
  getData( "AZRSOLVSM", &resolver, 1 );

  return resolver;
}

int Drives::getSmoothedElResolver()
{
  int resolver;
  getData( "ELRSOLVSM", &resolver, 1 );

  return resolver;
}

double Drives::getAzFine()
{
  int sin, cos;
  double fine;

  cos = getInstantAzCosEnc();
  sin = getInstantAzSinEnc();

  if ( sin == 0 && cos == 0 )
  {
    if ( status() == true && getConfig().isEmulating() == false )
      throw CARMA_ERROR( "Bad Inductosyn/Encoder" );
  }


  fine = atan2( (double)sin, (double)cos ) * RD;

  if ( fine < 0. )
    fine += 360.;

  fine /= 180.;

  return fine;
}

double Drives::getElFine()
{
  int sin, cos;
  double fine;

  cos = getInstantElCosEnc();
  sin = getInstantElSinEnc();

  if ( sin == 0 && cos == 0 )
  {
    if ( status() == true && getConfig().isEmulating() == false )
      throw CARMA_ERROR( "Bad Inductosyn/Encoder" );
  }

  fine = atan2( (double)sin, (double)cos ) * RD;

  if ( fine < 0. )
    fine += 360.;

  fine /= 180.;

  return fine;
}

double Drives::getAz()
{
  double az,el,sinaz,cosaz,sin2az,cos2az,cosel,cosz,tanel,daz,del;
  int iterate;
  const double azdeg = getAzSansPointing();
  const double eldeg = getElSansPointing();

  double apc[DriveCommand::dazCoefsCount];
  getData( "APC", apc, DriveCommand::dazCoefsCount );
  double epc[DriveCommand::delCoefsCount];
  getData( "EPC", epc, DriveCommand::delCoefsCount );


  // iteration 1 - begin with 0th order corrections
  az = (azdeg - apc[0]/60.) * DR;
  el = (eldeg - epc[0]/60.) * DR;

  sinaz  = sin(az); 
  cosaz  = cos(az); 
  sin2az = sin(2*az); 
  cos2az = cos(2*az); 
  cosel  = cos(el); 
  cosz   = cos(M_PI_2-el);
  tanel  = cosz / cosel;

  // changed limits from +/-0.02 to +/-0.01
  if(-0.01 < cosel && cosel < 0.0) {
    cosel = -0.01;
  } else if(0.0 <= cosel && cosel < 0.01) {

    cosel = 0.01;
  }
  if(cosz < 0.01) {
    cosz = 0.01;
  }

  // daz is in arcminutes of *horizon* offset (NB: NOT *sky* offset).
  // (Equation 4 from Hat Creek divided by cos(el)
  // to give horizon offset rather than sky offset).
  //
  // apc[3] == aperture collimation offset
  //   The offset is subtracted here because:
  //     Let ephem output == E
  //     The antenna's reported position is B
  //     Observer adds offsets to make the requested
  //     B's move a known target into position, this
  //     is equivilent to E+C=B
  //     Now, those are put into the aperture offset
  //     coefs.  So the eqn looks like E=B-C.
  // To match up with OVRO antennas, mount offset is
  // seen as a time dependant collimation error.  This
  // is equiv to variable collimation term, and therefore
  // is put in with the ApertureOffset term
  
  daz =
    apc[0]       
    + apc[1]*sinaz*tanel 
    + apc[2]*cosaz*tanel
    + (apc[3] + getApertureOffsetAz(getAperture()) + getMountOffsetAz())/cosel
    + apc[4]*tanel 
    + apc[5]*sinaz       
    + apc[6]*cosaz
    + apc[7]*sin2az      
    + apc[8]*cos2az;

  // if iterate != 0, compute 1st order el also, redo calculation
  getData( "ITERATE", &iterate, 1 ) ;
  if (iterate != 0) {
    del =
      (epc[0] + getApertureOffsetEl(getAperture()) + getMountOffsetEl())
      + epc[1]*sinaz
      + epc[2]*cosaz
      + epc[3]*cosz  
      + (epc[4] + getApertureOffsetSag(getAperture()))*cosel
      + epc[5]*sin2az
      + epc[6]*cos2az
      + epc[7]*cosel/cosz;
    az = (azdeg - daz/60.) * DR;
    el = (eldeg - del/60.) * DR;
    sinaz  = sin(az); 
    cosaz  = cos(az); 
    sin2az = sin(2*az); 
    cos2az = cos(2*az); 
    cosel  = cos(el); 
    cosz   = cos(M_PI_2-el);
    tanel  = cosz / cosel;

    if(-0.01 < cosel && cosel < 0.0) {
      cosel = -0.01;
    } else if(0.0 <= cosel && cosel < 0.01) {
      cosel = 0.01;
    }
    if(cosz < 0.01) {
      cosz = 0.01;
    }

    daz =
      apc[0]       
      + apc[1]*sinaz*tanel 
      + apc[2]*cosaz*tanel
      + (apc[3] + getApertureOffsetAz(getAperture()) + getMountOffsetAz())/cosel
      + apc[4]*tanel 
      + apc[5]*sinaz       
      + apc[6]*cosaz
      + apc[7]*sin2az      
      + apc[8]*cos2az;
    }

  // Now apply mount offset, Aperture offset was applied above as part of
  // apc[3] user offsets are applied in setTargetAz() in AZEL mode and in
  // the ephem class in EQUAT mode
  
  return azdeg - daz/60.0;
}

double Drives::getAzSansPointing()
{
  double fine, angle;
  int coarse, degrees, twodeg;

  coarse = getInstantAzResolver();
  fine = getAzFine();

  coarse -= _azCoarseOffset;           // offset read from drives.tab file
  if ( coarse < 0 )
    coarse += 65536;

  degrees = (coarse * 360) / 65536;    // 360deg == 65536; round to next lower integer
  twodeg = degrees & -2;               // round to next lower even degrees (e.g., 11->10, 12->12)
  if ( (fine > 1.5) && (twodeg == degrees) )     
    twodeg -= 2;
  else if ( (fine < .5) && (twodeg != degrees))
    twodeg += 2;

  angle = fine + twodeg;

  // at this point angle is in the old BIMA system: 0 degrees is due South and
  //   increases CCW
  // now convert to the CARMA system where 0 is due North and increases CCW
  // 'westerly' indicates that N is 360, rather than 0 
  // this bit only needed for AZ

  if ( fabs( angle ) > 90. )
  { 
    int westerly; 
    getData( "AZDVELF", &westerly, 1 );
    westerly &= 0x8000;

    if ( angle > 0. )
    { 
      if ( westerly != 0 )
        angle -= 360.;
    }
    else
    {
      if ( westerly == 0 )
        angle += 360.;
    }
  }

  angle = 180 - angle;

  return angle;
}


// changed to iterate twice if iterate != 0
double Drives::getEl()
{
  double az,el,sinaz,cosaz,sin2az,cos2az,cosel,cosz,tanel,daz,del;
  int iterate;
  const double azdeg = getAzSansPointing();
  const double eldeg = getElSansPointing();

  double apc[DriveCommand::dazCoefsCount];
  getData( "APC", apc, DriveCommand::dazCoefsCount );
  double epc[DriveCommand::delCoefsCount];
  getData( "EPC", epc, DriveCommand::delCoefsCount );


  // iteration 1 - begin with 0th order corrections
  az = (azdeg - apc[0]/60.) * DR;
  el = (eldeg - epc[0]/60.) * DR;

  sinaz  = sin(az); 
  cosaz  = cos(az); 
  sin2az = sin(2*az); 
  cos2az = cos(2*az); 
  cosel  = cos(el); 
  cosz   = cos(M_PI_2-el);
  tanel  = cosz / cosel;

  // changed limits from +/-0.02 to +/-0.01
  if(-0.01 < cosel && cosel < 0.0) {
    cosel = -0.01;
  } else if(0.0 <= cosel && cosel < 0.01) {

    cosel = 0.01;
  }
  if(cosz < 0.01) {
    cosz = 0.01;
  }

  // del is in arcminutes
  // (Equation 4 from Hat Creek)
  //
  // epc[0] == aperture collimation offset
  //   The offset is subtracted here because:
  //     Let ephem output == E
  //     The antenna's reported position is B
  //     Observer adds offsets to make the requested
  //     B's move a known target into position, this
  //     is equivilent to E+C=B
  //     Now, those are put into the aperture offset
  //     coefs.  So the eqn looks like E=B-C.
  // epc[4] == sag term (TODO CHECK THIS!)
  // To match up with OVRO antennas, mount offset is
  // seen as a time dependant collimation error.  This
  // is equiv to variable collimation term, and therefore
  // is put in with the ApertureOffset term
  
  del =
    (epc[0] + getApertureOffsetEl(getAperture()) + getMountOffsetEl())
    + epc[1]*sinaz
    + epc[2]*cosaz
    + epc[3]*cosz  
    + (epc[4] + getApertureOffsetSag(getAperture()))*cosel
    + epc[5]*sin2az
    + epc[6]*cos2az
    + epc[7]*cosel/cosz;

  // if iterate != 0, recalculate el
  getData( "ITERATE", &iterate, 1 ) ;
  if (iterate != 0) {
    daz =
      apc[0]       
      + apc[1]*sinaz*tanel 
      + apc[2]*cosaz*tanel
      + (apc[3] + getApertureOffsetAz(getAperture()) + getMountOffsetAz())/cosel
      + apc[4]*tanel 
      + apc[5]*sinaz       
      + apc[6]*cosaz
      + apc[7]*sin2az      
      + apc[8]*cos2az;
    az = (azdeg - daz/60.) * DR;
    el = (eldeg - del/60.) * DR;
    sinaz  = sin(az); 
    cosaz  = cos(az); 
    sin2az = sin(2*az); 
    cos2az = cos(2*az); 
    cosel  = cos(el); 
    cosz   = cos(M_PI_2-el);
    tanel  = cosz / cosel;

    if(-0.01 < cosel && cosel < 0.0) {
      cosel = -0.01;
    } else if(0.0 <= cosel && cosel < 0.01) {
      cosel = 0.01;
    }
    if(cosz < 0.01) {
      cosz = 0.01;
    }
    del =
      (epc[0] + getApertureOffsetEl(getAperture()) + getMountOffsetEl())
      + epc[1]*sinaz
      + epc[2]*cosaz
      + epc[3]*cosz  
      + (epc[4] + getApertureOffsetSag(getAperture()))*cosel
      + epc[5]*sin2az
      + epc[6]*cos2az
      + epc[7]*cosel/cosz;
  }

  // Now apply mount offset, Aperture offset was applied above as part of
  // epc[0] user offsets are applied in setTargetEl() in AZEL mode and in
  // the ephem class in EQUAT mode
  
  return eldeg - del/60.0;
}


double Drives::getElSansPointing()
{
  double fine, angle, degrees;
  int coarse, twodeg;

  coarse = getInstantElResolver();
  fine = getElFine();

  coarse -= _elCoarseOffset; // offset read from desc.tab file

  if ( coarse < 0 )
    coarse += 65536;
  degrees = (coarse * 360.) / 65536; // 360deg == 65536

  twodeg =  (int)(degrees - fine + 0.5 );   // should round to nearest two degree increment

  // old code - must declare degrees as integer for this
  // twodeg = degrees & -2;
  // if ( (fine > 1.5) && (twodeg == degrees) )
  //    twodeg -= 2;
  //  else if ( (fine < .5) && (twodeg != degrees))
  //    twodeg += 2;

  angle = fine + twodeg;
  if ( angle > 180. )
    angle -= 360.;

  return angle;
}

double Drives::getAzRequested()
{
  double angle;

  getData( "AZIREQ", &angle, 1 );

  return angle;
}

void Drives::setAzRequested( double az )
{
  putData( "AZIREQ", &az );
}

void Drives::setElRequested( double el )
{
  putData( "ELREQ", &el );
}

double Drives::getElRequested()
{
  double angle;

  getData( "ELREQ", &angle, 1 );

  return angle;
}

int Drives::getAzDigVeFilter()
{
  int value;

  getData( "AZDVELF", &value );

  // Mask off high status bits
  // Also note, the sense of the 
  // direction bit is reversed for az when compared to el
  if ( (value & 0x1000) == 0x1000 )
  {
    value = abs(value) & 0x0FFF;
    value = -value;
  }
  else
  {
    value = abs(value) & 0x0FFF;
  }

  SHMTRC( "AZDTRC", value );

  return value;
}

int Drives::getAzDigVeFilterWriteOut()
{
  int value = _lastiAzVel;

  if ( (value & 0x1000) == 0x1000 )
  {
    value = abs(value) & 0x0FFF;
    value = -value;
  }
  else
  {
    value = abs(value) & 0x0FFF;
  }

  SHMTRC( "AZDWTRC", value );

  return value;
}

int Drives::getElDigVeFilter()
{
  int value;

  getData( "ELDVELF", &value );

  // Mask off high status bits
  if ( (value & 0x1000) == 0x1000 )
  {
    value = abs(value) & 0x0FFF;
  }
  else
  {
    value = abs(value) & 0x0FFF;
    value = -value;
  }
  SHMTRC( "ELDTRC", value );

  return value;
}

int Drives::getElDigVeFilterWriteOut()
{
  int value = _lastiElVel;

  if ( (value & 0x1000) == 0x1000 )
  {
    value = abs(value) & 0x0FFF;
  }
  else
  {
    value = abs(value) & 0x0FFF;
    value = -value;
  }

  SHMTRC( "ELDWTRC", value );

  return value;
}

int Drives::getInstantAzCosEnc()
{
  int value;

  getData( "AZCOSEN", &value );
  dataInvert(value);

  return value;
}

int Drives::getInstantElCosEnc()
{
  int value;

  getData( "ELCOSEN", &value );
  dataInvert(value);

  return value;
}

int Drives::getInstantAzSinEnc()
{
  int value;

  getData( "AZSINEN", &value );
  dataInvert(value);

  return value;
}

int Drives::getInstantElSinEnc()
{
  int value;

  getData( "ELSINEN", &value );
  dataInvert(value);

  return value;
}


void Drives::setTiltOutliers( const char *name, int outliers )
{
  putData( name, &outliers );
}

int Drives::getTilt1Outliers()
{
  int outliers;
  getData( "TILTMTR1O", &outliers );
  return outliers;
}

int Drives::getTilt2Outliers()
{
  int outliers;
  getData( "TILTMTR2O", &outliers );
  return outliers;
}


int Drives::getTilt1Counts()
{
  int outliers, rms;
  int counts = getTiltCounts( "TILT1SAMPLES", outliers, rms );
  setTiltOutliers( "TILTMTR1O", outliers );
  putData( "TILT1RMS", &rms );

  return counts;
}

double Drives::getTilt1Arcmin()
{
  int ave = getTilt1Counts();

  return (double)( ave * VOLTS4 * 12.0 );
}

double Drives::getTilt2Arcmin()
{
  int ave = getTilt2Counts();

  return (double)( ave * VOLTS4 * 12.0 ); 
}

int Drives::getTiltCounts( const char *name, int &outliers, int &rms1 )
{
  int cooked[TILTSAMPLES], tiltsSansOutliers[TILTSAMPLES], inliers = 0;
  int ave, rms;

  getData( name, _rawTilts, TILTSAMPLES );

  for ( int i = 0; i < TILTSAMPLES; i++ )
    cooked[i] = atodin( _rawTilts[i] );

  getAveRms( cooked, TILTSAMPLES, ave, rms );
  rms1 = rms; // first rms is used to tracking in shared mem

  for ( int i = 0; i < TILTSAMPLES; i++ )
  {
    CPTRACE( Trace::TRACE6, "getTiltsCounts, rms: " << rms );
    if ( abs( cooked[i] - ave ) < (rms*2) )
    {
      tiltsSansOutliers[inliers] = cooked[i];
      inliers++;
    }
  }

  CPTRACE( Trace::TRACE3, "GETTING ave/rms after removing outliers" );
  getAveRms( tiltsSansOutliers, inliers, ave, rms );

  outliers = TILTSAMPLES - inliers;

  return ave;
}

int Drives::getTilt2Counts()
{
  int outliers, rms;
  int counts = getTiltCounts( "TILT2SAMPLES", outliers, rms );
  setTiltOutliers( "TILTMTR2O", outliers );
  putData( "TILT2RMS", &rms );

  return counts;
}

void Drives::servo()
{
  double mjd;
  unsigned short iAz = 0, iEl = 0;
  double distance = 0.;

  // Kick out if we're disabled
  if ( getState() == DISABLE )
  {
    setActiveState( DISABLE );
    return;
  }

  //setState( TRACK );

  // Set state according to distance from
  // target Az/El in sky error terms.
  // If not within tracking tolerance, then mark state
  // as CLOSE.
  // If within tracking tolerance, start count to allow
  // antenna to settle (~3 seconds +-.5).  Combined with
  // delays in the monitor stream, this gives the subarray
  // controller between 3-4 seconds before it recognizes the antenna
  // has completed the track command and is on target
  if ( getModel() == OPTICAL )
    distance = 3.0; // optical pointing tolerance set to 3arcsec by default
  else
    distance = getTolerance();

  if ( fabs( hypot(getAzErrSky(), getElErr()) ) - distance  > 0 )
  {
    setActiveState( CLOSE );
    // Okay, settle is getting moved forward whenever we're
    // setting the CLOSE state.  Once tracking, the settle
    // is now static at +1 seconds in the future and time()
    // will catch up...
    resetSettle();
  }
  else
  {
    setActiveState( TRACK );
  }

  CPTRACE( Trace::TRACE1, "  activestate: " << ( getActiveState() == TRACK ? "TRACK":"NOTTRACK") << " _settle: " << _settle << " close: " << fabs( getAzErrSky() ) - distance );

  if ( getActiveState() == TRACK && settled()  )
    setCurSequenceNo( getNextSequenceNo() );

  mjd = Time::MJD() + _mjdGainLookAhead;
  SHMTRC( "GAINMJD", mjd );

  double targAz = getTargetAz(mjd);
  double currAz = getAz();
  _azVel = (int)(roundf((( targAz - currAz ) / 4.70e-4) * _gainConst));
  if ( abs(_azVel) > _azMaxSlew )
  {
    if ( _azVel > 0 )
      _azVel = _azMaxSlew;
    else
      _azVel = -_azMaxSlew;
  }

  SHMTRC( "AZTARG", targAz );
  SHMTRC( "AZCURR", currAz );
  SHMTRC( "AZLKVEL", _azVel );

  double targEl = getTargetEl(mjd);
  double currEl = getEl();
  _elVel = (int)(roundf((( targEl - currEl ) / 3.60e-4) * _gainConst));
  if ( abs(_elVel) > _elMaxSlew )
  {
    if ( _elVel > 0 )
      _elVel = _elMaxSlew;
    else
      _elVel = -_elMaxSlew;
  }

  SHMTRC( "ELTARG", targEl );
  SHMTRC( "ELCURR", currEl );
  SHMTRC( "ELLKVEL", _elVel );

  markAzVel( _azVel );
  markElVel( _elVel );

  //iAz = (unsigned short)(( _azVel > 0 ) ? _azVel : ( -_azVel | 0x1000 ));
  if ( _azVel > 0 )
    iAz = (unsigned short)_azVel;
  else
    iAz = (unsigned short)(-_azVel | 0x1000);

  //iEl = (unsigned short)(( _elVel > 0 ) ? ( _elVel | 0x1000 ) : -_elVel);
  if ( _elVel > 0 )
    iEl = (unsigned short)( _elVel | 0x1000 );
  else
    iEl =  (unsigned short)(-_elVel);

  iAz |= _toggle;
  iEl |= _toggle;

  // Make sure we aren't up against a limit...
  if ( isElLim() || isElULim() || isAzLim() || isAzULim() )
  {   
    setHardLimit();
    iEl = 0;
    iAz = 0;
  }   

  if ( currAz < _swAzLoLim && _azVel < 0 ) 
  {   
    setSoftLimit();
    iAz = 0;
  }

  if ( currAz > _swAzHiLim && _azVel > 0 )
  {   
    setSoftLimit(); 
    iAz = 0;
  }   

  if ( currEl < _swElLoLim && _elVel < 0 )
  {   
    setSoftLimit();
    iEl = 0;
  }

  if ( currEl > _swElHiLim && _elVel > 0 )
  { 
    setSoftLimit();
    iEl = 0;
  }

  SHMTRC( "IAZ", iAz );
  SHMTRC( "IEL", iEl );

  tpoke( "AZDVELF", iAz );
  tpoke( "ELDVELF", iEl );
}


void Drives::pegVelocities( int limit )
{
  _forceVelLimit = limit;
}

void Drives::pegAz( bool peggit )
{
  _peggedAz = peggit;
}


void Drives::pegEl( bool peggit )
{
  _peggedEl = peggit;
}

bool Drives::isAzPegged()
{
  return _peggedAz;
}

bool Drives::isElPegged()
{
  return _peggedEl;
}

int Drives::computeAzCoarseOffset( int degreeshint )
{
  int coarse, coarseoffset;
  double fine;

  coarse = getInstantAzResolver();  
  fine = getAzFine();  
  fine *= (65536./180./360.);

  coarseoffset = coarse - ( degreeshint * 65536/360 + (int)fine );

  return coarseoffset;
}

int Drives::computeElCoarseOffset( int degreeshint )
{
  int coarse, coarseoffset;
  double fine;

  coarse = getInstantElResolver();
  fine = getElFine();
  fine *= (65536./180./360.);

  coarseoffset = coarse - ( degreeshint * 65536/360 + (int)fine );

  return coarseoffset;
}

void Drives::getAveRms( int *l, int size, int &ave, int &rms )
{
  long sum = 0;
  double sq = 0.; // long and unsigned long gets overflowed..

  for ( int i = 0; i < size; i++ )
  {
    sum += l[i];
    sq += (l[i] * l[i]);
  }

  if ( size == 0 )
    size = 1;

  ave = (int)(sum/size);
  rms = (int)(sqrtl((sq/size)));
  CPTRACE( Trace::TRACE5, "getAveRms for tilts, sum: " << sum << " sq: " << sq
      << " ave: " << ave << " rms: " << rms );
}

void Drives::setState( StateType state )
{
  unsigned short ustate = (unsigned short)state;
  CPTRACE( Trace::TRACE3, "  Drives::setState( " << state << ")" );
  putData( "DRVSTATE", &ustate );
}

void Drives::setActiveState( StateType state )
{
  unsigned short ustate = (unsigned short)state;
  putData( "DRVASTATE", &ustate );
}

Drives::StateType Drives::getState()
{
  unsigned short state;

  getData( "DRVSTATE", &state );

  return (StateType)state;
}

Drives::StateType Drives::getActiveState()
{
  unsigned short state;

  getData( "DRVASTATE", &state );

  return (StateType)state;
}

void Drives::stow()
{
  setState( STOP );
  setActiveState( STOW );
}

void Drives::setStowPosition( DriveCommand::PositionType position )
{
  unsigned short pos;
  pos = position;

  putData( "POSTYPE", &pos );

  setTargetAz( 3, Time::MJD(), 180.0, true);

  switch ( position )
  {
    case DriveCommand::ZENITH:
      setTargetEl( Time::MJD(), 89.1, true );
      break;

    case DriveCommand::SAFE:
      float az, el;
      getSafeTarget( az, el );
      CPTRACE( Trace::TRACE1, "safeaz:" << az << " safeel:" << el );
      setTargetAz( 4, Time::MJD(), az, true );
      setTargetEl( Time::MJD(), el, true );
      break;

    case DriveCommand::SERVICE:
    default:
      setTargetEl( Time::MJD(), 9.0, true );
      break;

  }
}

DriveCommand::PositionType Drives::getStowPosition()
{
  unsigned short pos;
  getData( "POSTYPE", &pos );

  return (DriveCommand::PositionType)pos;
}

void Drives::snow()
{
  setState( SNOW );
}

bool Drives::onTarget()
{
  return( onTargetAz() && onTargetEl() );
}

bool Drives::onTargetAz()
{
  return( fabs( getAzRequested() - getAz() ) < .00033 ); // .00033 ~= 1 arc sec
}

bool Drives::onTargetEl()
{
  return( fabs( getElRequested() - getEl() ) < .00033 ); // .00033 ~= 1 arc sec
}

void Drives::setSoftLimit()
{
  setActiveState( SWLIMIT );
}

void Drives::setHardLimit()
{
  stop();
  setActiveState( HWLIMIT );
}

Drives::WrapLogic Drives::getWrapLogic()
{
  unsigned short logic;

  getData( "WRAPLOGIC", &logic );

  return (WrapLogic)logic;
}

void Drives::setWrapLogic( Drives::WrapLogic logic )
{
  unsigned short l = (unsigned short)logic;

  putData( "WRAPLOGIC", &l );
}

Drives::ModeType Drives::getMode()
{
  unsigned short mode;
  getData( "DRVMODE", &mode );
  return (ModeType)mode;
}

void Drives::setMode( Drives::ModeType mode )
{
  unsigned short m = (unsigned short)mode;

  putData( "DRVMODE", &m );
}

Drives::RefractModel Drives::getModel()
{
  unsigned short r;
  getData( "REFRACTMOD", &r );
  return (RefractModel)r;
}

void Drives::setModel( Drives::RefractModel model )
{
  unsigned short r = model;
  putData( "REFRACTMOD", &r );
}  

float Drives::getRefract()
{
  float r;
  getData("REFRACT", &r);
  return r;
}

void Drives::setRefract( float refract)
{
  putData("REFRACT", &refract);
}

// getFastPacketRxMJD() returns the mjd for when the last set of fast packets
// was received.
double Drives::getFastPacketRxMJD() {
  double mjd;
  getData( "FPKT1MJD", &mjd );
  return mjd;
}

// return arcsec
double Drives::getAzErr()
{
  // getAz() returns the last measured azimuth.
  // getFastPacketRxMJD() returns the mjd for when the last set of fast packets
  // was received (which approximates when the azimuth was last measured).
  double mjd = getFastPacketRxMJD();
  double targetAz = getTargetAz(mjd);
  double err = targetAz - getAz();

  if ( ::isinf( err ) ) 
    return std::numeric_limits<double>::max();
  else 
      return ( err * 3600. );

}

double Drives::getAzErrSky()
{
  const double el = getEl() * DR;

  if ( ::isinf( el ) ) 
      return std::numeric_limits<double>::max();
  else 
    return getAzErr() * cos(el);
}

// return arcsec
double Drives::getElErr()
{
  // getEl() returns the last measured elevation.
  // getFastPacketRxMJD() returns the mjd for when the last set of fast packets
  // was received (which approximates when the elevation was last measured).
  double mjd = getFastPacketRxMJD();
  double targetEl = getTargetEl(mjd);
  double err = targetEl - getEl();

  if ( ::isinf( err ) ) 
    return std::numeric_limits<double>::max();
  else 
    return ( err * 3600. );
}

void Drives::setRA( double angle )
{
  putData( "RA", &angle );
}

double Drives::getRA()
{
  double angle;
  getData( "RA", &angle );
  return angle;
}

void Drives::setDec( double angle )
{
  putData( "DEC", &angle );
}

double Drives::getDec()
{
  double angle;
  getData( "DEC", &angle );
  return angle;
}

double Drives::getOffsetAz()
{
  double angle;
  getData( "OFFSETAZ", &angle );
  return angle;
}

void Drives::setOffsetAz( double angle )
{
  putData( "OFFSETAZ", &angle );
}

double Drives::getOffsetEl()
{
  double angle;
  getData( "OFFSETEL", &angle );
  return angle;
}

void Drives::setOffsetEl( double angle )
{
  putData( "OFFSETEL", &angle );
}


double Drives::getMountOffsetAz()
{
  double angle;
  getData( "MOUNTOFFAZ", &angle );
  return angle;
}

void Drives::setMountOffsetAz( double angle )
{
  putData( "MOUNTOFFAZ", &angle );
}

double Drives::getMountOffsetEl()
{
  double angle;
  getData( "MOUNTOFFEL", &angle );
  return angle;
}

void Drives::setMountOffsetEl( double angle )
{
  putData( "MOUNTOFFEL", &angle );
}

void Drives::setWindDir( float dir )
{
  putData( "WINDDIR", &dir );
}

float Drives::getWindDir()
{
  float dir;
  getData( "WINDDIR", &dir );
  return dir;
}

void Drives::setAperture(  DriveCommand::ApertureType ap )
{
  int iap = (int)ap;

  putData( "APER", &iap );

}

DriveCommand::ApertureType Drives::getAperture()
{
  int iap;

  getData( "APER", &iap );

  return (static_cast<DriveCommand::ApertureType>(iap));
}

double Drives::getApertureOffsetAz( DriveCommand::ApertureType ap )
{
  double o = 0.;

  switch ( ap )
  {
    case DriveCommand::OPTICAL:
      getData( "OPAPOFFAZ", &o );
      break;
    case DriveCommand::RADIO1MM:
      getData( "RAD1OFFAZ", &o );
      break;
    case DriveCommand::RADIO3MM:
      getData( "RAD3OFFAZ", &o );
      break;
    case DriveCommand::RADIO1CM:
      getData( "RADCMOFFAZ", &o );
      break;
    default:
      throw CARMA_ERROR( "Unknown aperture!" );
  }

  return o;
}

void Drives::setApertureOffsetAz( DriveCommand::ApertureType ap, double o )
{
  switch ( ap )
  {
    case DriveCommand::OPTICAL:
      putData( "OPAPOFFAZ", &o ); 
      break;
    case DriveCommand::RADIO1MM:
      putData( "RAD1OFFAZ", &o );
      break;
    case DriveCommand::RADIO3MM:
      putData( "RAD3OFFAZ", &o );
      break;
    case DriveCommand::RADIO1CM:
      putData( "RADCMOFFAZ", &o );
      break;
    default:
      throw CARMA_ERROR( "Unknown aperture!" );
  }
}

double Drives::getApertureOffsetEl( DriveCommand::ApertureType ap )
{
  double o = 0.;

  switch ( ap )
  {
    case DriveCommand::OPTICAL:
      getData( "OPAPOFFEL", &o );
      break;
    case DriveCommand::RADIO1MM:
      getData( "RAD1OFFEL", &o );
      break;
    case DriveCommand::RADIO3MM:
      getData( "RAD3OFFEL", &o );
      break;
    case DriveCommand::RADIO1CM:
      getData( "RADCMOFFEL", &o );
      break;
    default:
      throw CARMA_ERROR( "Unknown aperture!" );
  }

  return o;
}

void Drives::setApertureOffsetEl( DriveCommand::ApertureType ap, double o )
{
  switch ( ap )
  {
    case DriveCommand::OPTICAL:
      putData( "OPAPOFFEL", &o ); 
      break;
    case DriveCommand::RADIO1MM:
      putData( "RAD1OFFEL", &o );
      break;
    case DriveCommand::RADIO3MM:
      putData( "RAD3OFFEL", &o );
      break;
    case DriveCommand::RADIO1CM:
      putData( "RADCMOFFEL", &o );
      break;
    default:
      throw CARMA_ERROR( "Unknown aperture!" );
  }
}

void Drives::setApertureOffsetSag( DriveCommand::ApertureType ap, double o )
{
  switch ( ap )
  {
    case DriveCommand::OPTICAL:
      putData( "OPSAG", &o );
      break;
    case DriveCommand::RADIO1MM:
      putData( "RAD1SAG", &o );
      break;
    case DriveCommand::RADIO3MM:
      putData( "RAD3SAG", &o );
      break;
    case DriveCommand::RADIO1CM:
      putData( "RADCMSAG", &o );
      break;
    default:
      throw CARMA_ERROR( "Unknown aperture!" );
  }

}

double Drives::getApertureOffsetSag( DriveCommand::ApertureType ap )
{
  double o = 0.;

  switch ( ap )
  {   
    case DriveCommand::OPTICAL:
      getData( "OPSAG", &o );
      break;              
    case DriveCommand::RADIO1MM:
      getData( "RAD1SAG", &o );
      break;                          
    case DriveCommand::RADIO3MM:          
      getData( "RAD3SAG", &o );
      break;                                      
    case DriveCommand::RADIO1CM:                      
      getData( "RADCMSAG", &o );
      break;
    default:                                                
      throw CARMA_ERROR( "Unknown aperture!" );               
  }                                                             

  return o;
}


float Drives::getMaxAzRate()
{
  float r;
  getData( "MAXAZRATE", &r );
  return r;
}

void Drives::setMaxAzRate( float r )
{
  // r comes in as deg/sec
  // must be converted to counts
  // 6m az max abs counts -> 4095
  // equates to ~120deg/min for az
  // or ~2deg/sec
  
  _azMaxSlew = _azOrigMaxSlew = (int)(r * 2047.5);
  putData( "MAXAZRATE", &r );
}

float Drives::getMaxElRate()
{
  float r;
  getData( "MAXELRATE", &r );
  return r;
}

void Drives::setMaxElRate( float r )
{
  // r comes in as deg/sec
  // must be converted to counts
  // 6m el max abs counts -> 4095
  // equates to ~90deg/min for el
  // or ~1.5deg/sec

  _elMaxSlew = _elOrigMaxSlew = (int)(r * 2730.0);
  putData( "MAXELRATE", &r );
} 

void
Drives::setPointingModelCoefs(
    double * dazCoefs, unsigned int dazCoefsCount,
    double * delCoefs, unsigned int delCoefsCount)
{
  // TODO Move this to a more modularized pointing model!!!
  unsigned int i;
  double dazPointingCoefs[10];
  double delPointingCoefs[10];
  for(i=0; i<DriveCommand::dazCoefsCount; ++i) {
    dazPointingCoefs[i] = (i < dazCoefsCount) ? dazCoefs[i] : 0.0;
  }
  for(i=0; i<DriveCommand::delCoefsCount; ++i) {
    delPointingCoefs[i] = (i < delCoefsCount) ? delCoefs[i] : 0.0;
  }
  putData( "APC", dazPointingCoefs, DriveCommand::dazCoefsCount );
  putData( "EPC", delPointingCoefs, DriveCommand::delCoefsCount );
  setCoefChange( Time::MJD() );
}

void Drives::setCoefChange( double mjd )
{
  putData( "COEFCHGMJD", &mjd );
}

double Drives::getCoefChange()
{
  double mjd;

  getData( "COEFCHGMJD", &mjd );

  return mjd;
}

double Drives::getOneSecRateAz()
{
  static double lastAz = getAzSansPointing();
  static struct timeval tv1;
  static bool first = true;
  static double lastRate = 0.;
  double dconv, delta, nowAz;

  struct timeval tv2;

  if ( first )
  {
    first = false;
    gettimeofday( &tv2, NULL );
    tv1.tv_sec = tv2.tv_sec;
    tv1.tv_usec = tv2.tv_usec;
  }

  gettimeofday( &tv2, NULL );

  double interval =
    ( tv2.tv_sec + ( tv2.tv_usec * .000001 ) )
    - ( tv1.tv_sec + ( tv1.tv_usec * .000001 ) );

  SHMTRC( "AZINTERVAL", interval );
  // Only recompute once every second
  if ( interval > 1 )
  {
    SHMTRC( "AZINTHIT", interval );
    dconv = 1/interval;
    delta = ((nowAz = getAzSansPointing()) - lastAz);
    SHMTRC( "AZDLTA", delta );
    lastRate = delta * dconv * 60; // make deg/min
    lastAz = nowAz;
    tv1.tv_sec = tv2.tv_sec;
    tv1.tv_usec = tv2.tv_usec;
    markOneSecAzRate( lastRate );

    CPTRACE( Trace::TRACE4, "    OneSecRateAz  interval > .5 !" );
    CPTRACE( Trace::TRACE4, "     delta: " << delta << " dconv: "
        << dconv << " lastAz: " << lastAz);
  }
  else
    CPTRACE( Trace::TRACE4, "    OneSecRateAz  interval = " << interval );


  return ( lastRate );
}

double Drives::getOneSecRateEl()
{
  static double lastEl = getElSansPointing();
  static struct timeval tv1;
  static bool first = true;
  static double lastRate = 0.;

  struct timeval tv2;
  double dconv, delta, nowEl;

  if ( first )
  {
    first = false;
    gettimeofday( &tv2, NULL );
    tv1.tv_sec = tv2.tv_sec;
    tv1.tv_usec = tv2.tv_usec;
  }

  gettimeofday( &tv2, NULL );

  double interval =
    ( tv2.tv_sec + ( tv2.tv_usec * .000001 ) )
    - ( tv1.tv_sec + ( tv1.tv_usec * .000001 ) );

  SHMTRC( "ELINTERVAL", interval );
  // Only recompute once every second
  if ( interval > 1 )
  {
    SHMTRC( "ELINTHIT", interval );
    dconv = 1/interval;
    delta = ((nowEl = getElSansPointing()) - lastEl);
    SHMTRC( "ELDLTA", delta );
    lastRate = delta * dconv * 60; // make deg/min
    lastEl = nowEl;
    tv1.tv_sec = tv2.tv_sec;
    tv1.tv_usec = tv2.tv_usec;
    markOneSecElRate( lastRate );

    CPTRACE( Trace::TRACE4, "    OneSecRateEl  interval > .5 !" );
    CPTRACE( Trace::TRACE4, "     delta: "
        << delta << " dconv: " << dconv << " lastEl: " << lastEl);
  }
  else
    CPTRACE( Trace::TRACE4, "    OneSecRateEl  interval = " << interval );

  return ( lastRate );
}

bool Drives::isAzStalled()
{
  bool status = false;

  CPTRACE( Trace::TRACE3, "stall detect: az vel=" << getAzDigVeFilter()
      << " rate=" << fabs( getOneSecRateAz() ) );

  int azfil = abs(getAzDigVeFilter());
  int azfilwo = abs(getAzDigVeFilterWriteOut());
  double azr = fabs(getOneSecRateAz()); 

  SHMTRC( "AZFIL", azfil );
  SHMTRC( "AZFILWO", azfilwo );
  SHMTRC( "AZR", azr );
  SHMTRC( "AZLSTR", _lastAzRate );

  if (  azfil > 700 && (azr < ((azfil/34.125)*.75)) )  // 75% of exepected deg/min
  {
    // final chance, see if the elr is increasing
    // if so, assume there's no out right stall
    if ( _lastAzRate - azr < .6 )  // .6 gets us above the encoder readback noise
      status = true; // potential stall

  }
  else
  {
    // Do one more check to insure that power has not been tripped
    // off to the drives.  In this case, counts will be written
    // to the digvel card, but it will not be returning back those
    // counts, thus passing the 700 counts test above...
    // So, we check what counts are getting written out, and then
    // check if the digvel card readout reflects those counts or not...
    // First, check if stallcnts have gone above 3, this provides
    // a buffer between the delay of writes going out and the hardware
    // underneath reflecting the change...
    if ( azfilwo > 700 && azfil == 0 )
    {
      if ( ++_azDigWOMismatch > 5 )
        status = true; // power to drive tripped
    }
    else
    {
      _azDigWOMismatch = 0;
      status = false;
    }

    setAzCheckDrivePower(status);
  }

  _lastAzRate = azr;

  return status;
}

bool Drives::isElStalled()
{
  bool status = false;
  CPTRACE( Trace::TRACE3, "stall detect: el vel=" << getElDigVeFilter()
      << " rate=" << fabs( getOneSecRateEl() ) );

  int elfil = abs(getElDigVeFilter());
  int elfilwo = abs(getElDigVeFilterWriteOut());
  double elr = fabs(getOneSecRateEl());

  SHMTRC( "ELFIL", elfil );
  SHMTRC( "ELFILWO", elfilwo );
  SHMTRC( "ELR", elr );
  SHMTRC( "ELLSTR", _lastElRate );

  if (  elfil > 700 && (elr < ((elfil/51.1875)*.75)) )  // 75% of exepected deg/min
  {
    // final chance, see if the elr is increasing
    // if so, assume there's no out right stall
    if ( _lastElRate - elr < .6 ) // .6 gets us above the encoder readback noise
      status = true; // stall
  }
  else
  {
    // Do one more check to insure that power has not been tripped
    // off to the drives.  In this case, counts will be written
    // to the digvel card, but it will not be returning back those
    // counts, thus passing the 700 counts test above...
    // So, we check what counts are getting written out, and then
    // check if the digvel card readout reflects those counts or not...
    // First, check if stallcnts have gone above 3, this provides
    // a buffer between the delay of writes going out and the hardware
    // underneath reflecting the change...
    if ( elfilwo > 700 && elfil == 0 )
    {
      if ( ++_elDigWOMismatch > 5 )
        status = true; // power to drive box tripped!
    }
    else
    {
      _elDigWOMismatch = 0;
      status = false;
    }

    setElCheckDrivePower(status);
  }

  _lastElRate = elr;

  return status;
}

void Drives::zeroStallMJD()
{
  double zero = 0.;

  putData( "AZSTALLMJD", &zero );
  putData( "ELSTALLMJD", &zero );
}

void Drives::markAzStalls( int stalls )
{
  putData( "AZSTALLS", &stalls );
}

void Drives::markElStalls( int stalls )
{
  putData( "ELSTALLS", &stalls );
}

int Drives::getAzStalls()
{
  int s;
  getData( "AZSTALLS", &s );
  return s;
}

int Drives::getElStalls()
{
  int s;
  getData( "ELSTALLS", &s );
  return s;
}

void Drives::markAzStallMJD()
{
  double mjd = Time::MJD();

  putData( "AZSTALLMJD", &mjd );
}

void Drives::markElStallMJD()
{
  double mjd = Time::MJD();

  putData( "ELSTALLMJD", &mjd );
}

double Drives::getAzStallMJD()
{
  double mjd;

  getData( "AZSTALLMJD", &mjd );

  return mjd;
}

double Drives::getElStallMJD()
{
  double mjd;

  getData( "ELSTALLMJD", &mjd );

  return mjd;
}

void Drives::setStallRetries( int cnt )
{
  putData( "STALLRTRY", &cnt );
}

int Drives::getStallRetries()
{
  int cnt;
  getData( "STALLRTRY", &cnt );

  return cnt;
}

double Drives::getAzRate()
{
  double rate;
  getData( "AZSECRATE", &rate );
  return rate;
}

void Drives::markOneSecAzRate( double rate )
{
  putData( "AZSECRATE", &rate );
}

double Drives::getElRate()
{
  double rate;
  getData( "ELSECRATE", &rate );
  return rate;
}

void Drives::markOneSecElRate( double rate )
{
  putData( "ELSECRATE", &rate );
}

void Drives::setTolerance( double arcsec )
{
  putData( "TOLERANCE", &arcsec );
}

double Drives::getTolerance()
{
  double arcsec;
  getData( "TOLERANCE", &arcsec );
  return arcsec;
}

// The sequence track is assigned long.
// as long as int and long have the same
// sizes, this is okay for storing.
// If they do not, then a long putData()
// needs to be created.  TODO - colby
void Drives::setNextSequenceNo( unsigned long seq )
{
  int is = (int)seq;
  // Will be converted back to unsigned on read out...
  putData( "DRVNXTSEQ", &is );
}

unsigned long Drives::getNextSequenceNo()
{
  int is;

  getData( "DRVNXTSEQ", &is );

  return (unsigned long)is;
}

void Drives::setCurSequenceNo( unsigned long seq )
{
  int is = (int)seq;
  // Will be converted back to unsigned on read out...
  putData( "DRVCURSEQ", &is );
}

unsigned long Drives::getCurSequenceNo()
{
  int is;

  getData( "DRVCURSEQ", &is );

  return (unsigned long)is;
}

int Drives::limitAxisAccel( int &stalls, int newcounts, int lastcounts, int &accelLim, int origLim,
    int &maxSlew, int origMax, float slewMod, struct timeval &tv1 )
{
  struct timeval tv2;
  int axisLim;

  if ( stalls < 4 )
    if ( getStallRetries() > stalls )
    {
      stalls++;

      accelLim = accelLim/stalls;

      // Sanity accel...
      if ( accelLim < 50 )
        accelLim = 50;

      if ( stalls > 2 )
        maxSlew = (int)(maxSlew*slewMod);

      // Sanity speed...
      if ( maxSlew < 1500 )
        maxSlew = 1500;

      _stallsStarted = time(NULL);
    }

  int timetconv = (int)_stallsStarted;
  SHMTRC( "STALLSTART", timetconv );
  // Impose a 20 minute time limit to Axis acceleration limits
  if ( stalls > 0 && ((time(NULL) - _stallsStarted) > 300) )
  {
    _stallsStarted = 0;
    stalls = 0;
    maxSlew = origMax;
    accelLim = origLim;
  }

  axisLim = newcounts; // Default to returning new counts
  SHMTRC( "AXISLIM1", axisLim );
  SHMTRC( "AXISLIMLAST", lastcounts );
  SHMTRC( "AXISLIMSTLS", stalls );
  SHMTRC( "AXISLIMMAX", maxSlew );
  SHMTRC( "AXISLIMDVL", accelLim );

  if ( _limitAccel && stalls > 0 )
  {
    gettimeofday( &tv2, NULL );

    // Only care if the counts are above
    // stall detection threshold, which is,
    // 700 counts!
    if ( abs(newcounts) > 700 )
    {
      double dt =
        ( tv2.tv_sec + ( tv2.tv_usec * .000001 ) )
        - ( tv1.tv_sec + ( tv1.tv_usec * .000001 ) );

      // if this hasn't been called in a while (haven't slewed)
      // force calculation to operate under the impression it's
      // been a tenth of a sec.  Generally, that's what the
      // dt is anyway, and it helps get phantom large changes to
      // go through when the antenna hasn't been doing anything
      // lately...
      if ( dt > .1 )
        dt = .1;

      SHMTRC( "AXISLIMDT", dt );

      tv1.tv_sec = tv2.tv_sec;
      tv1.tv_usec = tv2.tv_usec;

      int dVel = (int)((newcounts - lastcounts) * dt);
      int lim = (int)(accelLim * dt);

      SHMTRC( "AXISLIMDVEL", dVel );
      SHMTRC( "AXISLIMDLIM", lim );

      // if slewing at maximum rate already, do not bother
      // artificially changing the neg acceleration,
      // the ants have no problem doing this.
      if ( (abs(lastcounts) != maxSlew) && (abs(dVel) > lim) )
      {
        if ( dVel > 0 )
          axisLim = lastcounts + lim;
        else
          axisLim = lastcounts - lim;
      }
    }
  } // if _limitAccel

  // final sanity check
  if ( axisLim > maxSlew )
    axisLim = maxSlew;
  else if ( axisLim < -maxSlew )
    axisLim = -maxSlew;

  SHMTRC( "AXISLIM2", axisLim );

  return axisLim;
}

void Drives::setAzCheckDrivePower( bool status )
{
  int is = (int)status;

  putData( "AZCHKDRVPOW", &is );
}

void Drives::setElCheckDrivePower( bool status )
{
  int is = (int)status;

  putData( "ELCHKDRVPOW", &is );
}

bool Drives::getAzCheckDrivePower()
{
  int is;

  getData( "AZCHKDRVPOW", &is );

  return (bool)is;
}

bool Drives::getElCheckDrivePower()
{
  int is;

  getData( "ELCHKDRVPOW", &is );

  return (bool)is;
}


// 6 is the number of .08s cycles that should pass before assuming we're on target
// and updating the monitor info for the drive seq number
bool Drives::settled()
{
  if ( _settle > 6 )
    return true;
  else
    return false;
}

void Drives::updateSettle()
{
  if ( _settle < 7 )
    _settle++;
}

void Drives::resetSettle()
{
  _settle = 0;
}

// Used to expose Drives' handle to the rx,
// this is only used to fill in observing freq
// for refraction correction in the ephem obj
double Drives::getObsFreq()
{
  double freq = 80.;

  if ( _rx != 0 )
    freq = _rx->getObsFreq();

  return freq;
}

void Drives::setLatitude( double lat )
{
  putData( "ANTLAT", &lat );
}

double Drives::getLatitude()
{
  double lat = 0.;
  getData( "ANTLAT", &lat );
  return lat;
}

void Drives::setLongitude( double longi )
{
  putData( "ANTLONG", &longi );
}

double Drives::getLongitude()
{
  double longi = 0.;
  getData( "ANTLONG", &longi );
  return longi;
}

void Drives::setAltitude( double alt )
{
  putData( "ANTALT", &alt );
}

double Drives::getAltitude()
{
  double alt = 0.;
  getData( "ANTALT", &alt );
  return alt;
}

void Drives::setAmbWeatherTemp( float temp )
{
  putData( "AMBWEATHTEMP", &temp );
}

float Drives::getAmbWeatherTemp()
{
  float temp;
  getData( "AMBWEATHTEMP", &temp );
  return temp;
}

void Drives::markAzVel( int vel )
{
  putData( "IAZVEL", &vel );
}

void Drives::markElVel( int vel )
{
  putData( "IELVEL", &vel );
}

int Drives::getAzVel()
{
  int vel;
  getData( "IAZVEL", &vel );
  return(vel);
}

int Drives::getElVel()
{
  int vel;
  getData( "IELVEL", &vel );
  return(vel);
}

ostream& operator<<( ostream& os, Drives& drives )
{
  os << "  Drives configuration info:" << endl;
  os << "   file: " << drives.getConfig().getDrivesConfFile() << endl;
  os << "    azslew: " << drives.getAzMaxSlew() << endl;
  os << "    elslew: " << drives.getElMaxSlew() << endl;
  os << "    azdown: " << drives.getAzRampDown() << endl;
  os << "    eldown: " << drives.getElRampDown() << endl;
  os << "    azramp: " << drives.getAzRampStartSpeed() << endl;
  os << "    elramp: " << drives.getAzRampStartSpeed() << endl;
  os << "    azoff: " << drives.getAzCoarseOffset() << endl;
  os << "    eloff: " << drives.getElCoarseOffset() << endl;
  os << "    azlolim: " << drives.getswAzLoLim() << endl;
  os << "    azhilim: " << drives.getswAzHiLim() << endl;
  os << "    ellolim: " << drives.getswElLoLim() << endl;
  os << "    elhilim: " << drives.getswElHiLim() << endl;
  os << "    swslew: " << (boolalpha) << drives.getswSlewLim() << endl;
  os << "    azaccellim: " << drives.getAzAccelLim() << endl;
  os << "    elaccellim: " << drives.getElAccelLim() << endl;
  os << "    azslewmod: " << drives.getAzSlewMod() << endl;
  os << "    elslewmod: " << drives.getElSlewMod() << endl;
  os << "    gain: " << drives.getGain() << endl;

  return os;
}

// vim: set expandtab sw=2 ts=2 cindent :


