/**@file
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.37 $
 * $Date: 2012/10/30 22:42:03 $
 * $Id: RxMgrThread.cc,v 1.37 2012/10/30 22:42:03 plambeck Exp $
 */


#include <cmath>

// CARMA includes
#include "carma/antenna/bima/RxMgrThread.h"

using namespace std;
using namespace log4cpp;
using namespace carma::util;
using namespace carma::antenna::bima;

bool RxMgrThread::_ok = true;

RxMgrThread::RxMgrThread(
    Configuration& config,
    Rx &rx,
    LO &lo,
    Secondary &secondary,
    CalWheel &calwheel )
: _config( config ),
  _rx( rx ),
  _lo( lo ),
  _secondary( secondary ),
  _calwheel( calwheel ),
  _logger( Program::getLogger() )
{
  _name = string( "RxMgrThread" );

  _rxReader = new IPQreader<RxCommand>( RXIPQ, true, IPQLEN );
  _relockTimer = 0;
  _relockRetries = 0;
  _rx.disableRelock();

}

bool RxMgrThread::isOk()
{
  return _ok;
}

void RxMgrThread::run()
{

  CARMA_ASSERT( _rxReader != static_cast<IPQreader<RxCommand>*>(NULL) );
  _rxReader->setNoneAvailable();

  while ( true )
  {

    if ( _config.isEmulating() )
    {
      _logger << Priority::INFO << "RxMgrThread in emulate mode, sleeping for 100 sec";
      sleep( 100 );
    }
    else
    {

      try
      {
	// This will check for any commands that may have
	// been issued since the last time we checked.
	// This command check will happen approximately
	// every 1 second
	if ( _rxReader->isDataAvailable() )
	{
	  _rx.disableRelock();
	  _rxReader->read();
	  _command = RRGET(command);

	  switch ( _command )
	  {
	    case RxCommand::MEASURETOTPOW:
	      {
		try
		{
		  //          RxCommand::CalPos pos = RRGET(cPos);
		  // TODO _rx.measureTotalPower( (Calwheel::Position)pos ); 

		}
		catch ( ... )
		{
		  BIMASTATUSINFO( _rx, "Exception caught!  Tuning may have failed!" );
		  _logger << Priority::WARN
		    << "Exception caught while tuning, tuning may have failed!";
		}

		// seq no should be set no matter what happened
		_rx.setCurTuneSequenceNo( _rx.getNextTuneSequenceNo() );
	      }
	      break;
	      
	    case RxCommand::FORCERELOCK:
	      { 
	        _logger << Priority::INFO << "forceRelock";
		    BIMASTATUSINFO( _rx, "forceRelock" );
	        _rx.forceRelock();
	      }
	      break;

	    case RxCommand::SETFREQ:
	      {
		try
		{
		  double lo1 = RRGET(lofreq);
		  double yigfreq = RRGET(yigfreq);
		  unsigned char band;

		  bool leaveAbsorber    = RRGET( leaveAbsorber );
		  bool optimizeReceiver = RRGET( optimizeReceiver );

		  CPTRACE( Trace::TRACE2, "SETFREQ: lo1=" << lo1 << " xfreq=" << yigfreq );
		  band = (lo1 > 150.) ? 'D' : 'B' ;
                  if (lo1 < 50.) band = 'E' ;
		  double oscfreq = (lo1 > 120.) ? lo1/3. : lo1 ;

		  BIMASTATUSINFO( _rx, "Setting LO1Freq: " << lo1 );
		  _rx.setLO1Freq( lo1 );

		  BIMASTATUSINFO( _rx, "Setting OscFreq: " << oscfreq );
		  _rx.setOscFreq( oscfreq ) ;

		  BIMASTATUSINFO( _rx, "Setting Band: " << (char)band );
		  _rx.setBand( band ); 

		  CPTRACE( Trace::TRACE2, "Setting LOTermRFout to 0.90" );
		  BIMASTATUSINFO( _rx, "Setting LOTermRFout: 0.90" );
		  _rx.setLOTermRFout( (float)0.90 );

		  CPTRACE( Trace::TRACE2, "Locking X-Band" );
		  BIMASTATUSINFO( _rx, "Locking X-Band: " << yigfreq );
		  _lo.lockX( yigfreq );

                  if (band != 'E' ) {
		    CPTRACE( Trace::TRACE2, "Locking MM osc" );
		    BIMASTATUSINFO( _rx, "Locking MM osc: " << oscfreq );
		    _rx.lockmm( oscfreq );

		    CPTRACE( Trace::TRACE2, "Optimizing SIS bias" );
		    BIMASTATUSINFO( _rx, "Optimizing SIS bias: " << lo1 );
		    _rx.tune( lo1, leaveAbsorber, optimizeReceiver );
                  }

		  else {
		    _rx.turnMMOscOff();   // turn mm osc off when doing 1cm observations
		  }

		  BIMASTATUSINFO( _rx, "Finished setFreq, lo1: " << lo1 << " yig: " << yigfreq );

		  // _rx.enableRelock(); // re-enabling relock 22 dec 08 - colby
		  // _relockRetries = 3; // only reset this when setfreq works
		}
		catch ( const char *msg )
		{
		  BIMASTATUSINFO( _rx, msg );
		  _logger << Priority::WARN
		    << msg;
		}
		catch ( carma::util::ErrorException &msg )
		{
		  BIMASTATUSINFO( _rx, msg.getMessage() );
		  _logger << Priority::WARN
		    << msg.what();
		}
		catch ( ... )
		{
		  BIMASTATUSINFO( _rx, "Exception caught!  Tuning may have failed!" );
		  _logger << Priority::WARN
		    << "Exception caught while tuning, tuning may have failed!";
		}
		// seq no should be set no matter what happened.
		_rx.setCurTuneSequenceNo( _rx.getNextTuneSequenceNo() );
	      }
	      break;

	    case RxCommand::SETOBSFREQ:
	      {
		try
		{
		  _rx.setObsFreq( RRGET(obsfreq) );
		}
		catch ( ... )
		{
		  BIMASTATUSINFO( _rx, "Exception caught!  Tuning may have failed!" );
		  _logger << Priority::WARN
		    << "Exception caught while tuning, tuning may have failed!";
		}
		// Seq no should be updated no matter what happened
		_rx.setCurTuneSequenceNo( _rx.getNextTuneSequenceNo() );
	      }
	      break;

	    case RxCommand::SETBAND:
	      {
		unsigned char bimaband = 'A';

		switch ( RRGET( band ) )
		{
		  case RxCommand::RX1CM:
		    bimaband = 'E';
		    break;
		  case RxCommand::RX1MM:
		    bimaband = 'D';
		    break;
		  case RxCommand::RX3MM:
		    bimaband = 'B';
		    break;
		  case RxCommand::RXANY:
		  default:
		    bimaband = 'A';
		    break;
		}

		_rx.setBand( bimaband );
	      }
	      break;

	    case RxCommand::SETYIG:
	      {
		_rx.setLOTermRFout( (float)0.950 );
		_lo.lockX( RRGET(yigfreq) );
	      }
	      break;

	    case RxCommand::SETLO:
	      {
		BIMASTATUSINFO( _rx, "Setting LO Freq: " << RRGET(lofreq) );
		_rx.lockmm( RRGET( lofreq ) );
	      }
	      break;

	    case RxCommand::SETLOTERMATTN:
	      {
		_rx.setLOTermAtten( (int)RRGET( lotermatten ) );
	      }
	      break;

	    case RxCommand::SETFOCUSZ:
	      {
		_secondary.setFocus( RRGET( fPos ) );
		_rx.setCurOpticsSequenceNo( _rx.getNextOpticsSequenceNo() );
		// _rx.enableRelock(); // re-enabling relock 22 dec 08 - colby
	      }
	      break;

	    case RxCommand::SETCALPOS:
	      {
		string posString;
		CalWheel::Positions pos;

		switch( RRGET( cPos ) )
		{
		  case RxCommand::SKY:
		    posString = "SKY";
		    pos = CalWheel::SKY;
		    break;
		  case RxCommand::AMBIENT:
		    posString = "AMBIENT";
		    pos = CalWheel::AMB;
		    break;
		  case RxCommand::FIXEDTEMP:
		    posString = "FIXEDTEMP";
		    pos = CalWheel::FIXED;
		    break;
		  default:
		    posString = "UNKNOWN POSITION PASSED";
		    pos = CalWheel::UNKNOWN;
		}

		if ( pos != CalWheel::UNKNOWN )
		{
		  _calwheel.setPosition( pos, _rx.bandCheck() );
		  _logger << Priority::INFO
		    << "  _calwheel.setPosition( " << posString << " )";
		}
		else
		  _logger << Priority::WARN
		    << "CalWheel given unknown position: " << RRGET( cPos );

		//_rx.enableRelock(); // re-enabling relock 22 may 08 - colby
	      }
	      break;

	    case RxCommand::SETCALNEXTSEQNO:
	      {
		_calwheel.setNextSequenceNo( (int)RRGET( calSeqNo ) );
	      }
	      break;
	    case RxCommand::SETTUNENEXTSEQNO:
	      {
		_rx.setNextTuneSequenceNo( (int)RRGET( tuneSeqNo ) );
	      }
	      break;
	    case RxCommand::SETOPTICNEXTSEQNO:
	      {
		_rx.setNextOpticsSequenceNo( (int)RRGET( opticSeqNo ) );
	      }
	      break;
	    case RxCommand::USENEXTOPTICSEQNO:
	      {
		_rx.setCurOpticsSequenceNo( _rx.getNextOpticsSequenceNo() );
	      }
	      break;
	    case RxCommand::DOIVCURVE:
	      {
		// Move code for creating iv curve out of bimaSis and into 
		// other class...
	      }
	      break;
	    default:
	      {
		_logger << Priority::WARN
		  << "Unknown command sent to RxMgrThread: "
		  << _command;
	      }
	      break;
	  } // switch _command
	} // if _rxReader->isDataAvailable()

	// Check if it would be useful to attempt a relock of the mm
	// osc.  This can help work around some problems when we're
	// scratching our heads over hardware.
	// Only reattempt once every 600 counts.  On average this will
	// be once every 10 minutes.  However, sometimes operations
	// that are set off above will consume more than 1 sec of
	// wall clock time (i.e. tuning/calwheel move/etc...)
	// Which is good, cause that keeps this check, in check,
	// so to speak.
	if ( _rx.isRelockEnabled() )
	{
	  if ( _relockTimer < 0 )
	  {

	    _relockTimer = 600; // Make sure we try only once every 600 counts

	    // Now, see if there's a good reason for no mm lock
	    // If the YIG isn't locked, there's no reason to bother
	    // trying to relock
	    if ( _lo.xLockStatus() == true
		 && _rx.mmLockStatus() == false )
	    {
	      if ( _relockRetries > 0 )
	      {
		_logger << Priority::WARN << " relockTimer < 0, "
		  << "xband is locked and mmlock is not "
		  << "attempting relockmm(), _relockRetries: " << _relockRetries;
		_rx.relockmm(); // Okay!  Try to relock!
		_relockRetries--;
		BIMASTATUSINFO( _rx, "Attempted relockmm(), " << Time::getTimeString()
		    << _relockRetries << " tries left" );
	      }
	      else
	      {
		_logger << Priority::WARN << "Too many automatic relockmm() call"
		  << " attempts! _relockRetries: " << _relockRetries;
		BIMASTATUSINFO( _rx, "Too many relockmm() retries!" );
	      }
	    }
	  }
	}
	else
	{
	  _relockTimer = 0;
	}

	_relockTimer--;
	
	{
	  unsigned short shortRelockCount;
	  shortRelockCount = ( 3 - _relockRetries < 0 ? 0 : 3 - _relockRetries );
	  _rx.putData( "RELOCKCNT", &shortRelockCount );
	}

	// Sleep until next time
	usleep(1000000);
      } catch ( carma::util::ErrorException &cuee ) {
	// TO DO, Make this properly report where it came from...
	_logger << Priority::ERROR << cuee.getErrorMessage();
      } catch ( ... ) {
	_logger << Priority::ERROR << "Caught unknown exception!";
      }
    } // if not emulating... 

  } // while true
} // void thread

void RxMgrThread::thread( RxMgrThread &This )
{
  This.run();
}

