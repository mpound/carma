/**
 * @file
 * BIMA LOControl implementation.
 *
 * @author Colby Gutierrez-Kraybill
 *
 * $Revision: 1.11 $
 * $Id: LOControlImpl.cc,v 1.11 2012/02/21 21:06:58 abeard Exp $
 */


// Carma includes
#include "carma/antenna/bima/control/LOControlImpl.h"
#include "carma/util/ExceptionUtils.h"

using namespace std;
using namespace log4cpp;
using namespace carma::util;
using namespace carma::antenna::bima;

// -----------------------------------------------------------------------------
LOControlImpl::LOControlImpl( Configuration &config ) :
  RxClient( config ),
  log_(Program::getLogger()),
  _config( config )
{
  // Nothing
}

// -----------------------------------------------------------------------------
LOControlImpl::~LOControlImpl()
{
  // Nothing
}

// -----------------------------------------------------------------------------
  void LOControlImpl::setFrequency(double yigFreq, double LOfreq)
{
  try
  {
    log_ << Priority::INFO
      << "setFrequency( yigFreq=" << yigFreq
      << ", LOfreq=" << LOfreq << " )";

    if ( yigFreq < 8.0 || yigFreq > 12.5 )
      throw 1;

    RWSET( command, RxCommand::SETYIG );
    RWSET( yigfreq, yigFreq );
    this->rxWrite();

    setLoFrequency( LOfreq );

    // Need to flesh out what setting LO here means...
  }
  catch ( int a )
  {
    ostringstream oss;
    oss << "yigFreq: " << yigFreq << " is out of range (8.0,12.5)";
    log_ << Priority::WARN << oss.str();

    throw CARMA_USREX( oss.str() );
  }
  catch (...)
  {
    // log and rethrow as user ex
    logCaughtAndRethrowAsUser( Priority::ERROR );
  }
}

// -----------------------------------------------------------------------------
  void LOControlImpl::toggleSweep(bool on)
{
  log_ << Priority::INFO
    << "toggleSweep( on=" << on << " ) - not implemented on bima antennas";
  // Nothing
}

// -----------------------------------------------------------------------------
  void LOControlImpl::toggleYigSweep( bool on )
{
    log_ << Priority::INFO
          << "toggleYigSweep( on=" << on << " ) - not implemented on bima antennas";
      // Nothing
}

// -----------------------------------------------------------------------------
  void LOControlImpl::setYigFrequency( const CORBA::Double yigFreq )
try {
        log_ << Priority::INFO
            << "setFrequency( yigFreq=" << yigFreq << " )";

        if ( yigFreq < 8.0 || yigFreq > 12.5 )
            throw 1;

        RWSET( command, RxCommand::SETYIG );
        RWSET( yigfreq, yigFreq );
        this->rxWrite();

} catch ( int a ) {
    ostringstream oss;
    oss << "yigFreq: " << yigFreq << " is out of range (8.0,12.5)";
    log_ << Priority::WARN << oss.str();

    throw CARMA_USREX( oss.str() );
} catch (...) {
    // log and rethrow as user ex
    logCaughtAndRethrowAsUser( Priority::ERROR );
} // setYigFrequency

// -----------------------------------------------------------------------------
  void LOControlImpl::setLoFrequency(double freq)
{

  try
  {
    log_ << Priority::INFO
      << "setLoFrequency( freq=" << freq << " )";

    if (  freq < 75. || freq > 150. )
      throw 1;

    RWSET( command, RxCommand::SETLO );
    RWSET( lofreq, freq );
    this->rxWrite();

    // Need to flesh out what setting LO here means...
  }
  catch ( int a )
  {
    ostringstream oss;
    oss << "LOFreq: " << freq << " is out of range (75.0,150.0)";
    log_ << Priority::WARN << oss.str();

    throw CARMA_USREX( oss.str() );
  }
  catch (...)
  {
    // log and rethrow as user ex
    logCaughtAndRethrowAsUser( Priority::ERROR );
  }
}

// -----------------------------------------------------------------------------
  void LOControlImpl::setLoTerminatorAttenuation(::CORBA::UShort atten)
{
  try
  {

    log_ << Priority::INFO
      << "setLoTerminatorAttenuation( atten=" << atten << " )";

    if ( atten > 31 )
    {
      atten = 31;
      log_ << Priority::WARN << " capping atten value to 31 db";
    }

    RWSET( command, RxCommand::SETLOTERMATTN );
    RWSET( lotermatten, atten );
    this->rxWrite();
  }
  catch ( ... )
  {
    // log and rethrow as user ex
    logCaughtAndRethrowAsUser( Priority::ERROR );
  }
}
