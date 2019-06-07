/**
 * @file
 * BIMA CryoControl implementation.
 *
 * @author Colby Gutierrez-Kraybill
 *
 * $Revision: 1.33 $
 * $Id: RxControlImpl.cc,v 1.33 2012/03/12 06:36:53 abeard Exp $
 */

#include "carma/antenna/bima/control/RxControlImpl.h"
#include "carma/corba/Client.h"
#include "carma/corba/Server.h"
#include "carma/util/ExceptionUtils.h"

using namespace std;
using namespace log4cpp;
using namespace carma::util;
using namespace carma::antenna::common;
using namespace carma::antenna::bima;


// -----------------------------------------------------------------------------
RxControlImpl::RxControlImpl
(
 Configuration &config,
 LOControl_ptr loPtr,
 FrontEndControl_ptr fePtr,
 OpticsControl_ptr opPtr,
 PolarizationControl_ptr poPtr
 ) :
  RxClient( config ),
  log_(Program::getLogger()),
  _config( config ),
  _loPtr( loPtr ),
  _fePtr( fePtr ),
  _opPtr( opPtr ),
  _poPtr( poPtr ),
  _ifPol1Name( "carma." + config.getAntenna() + "." + IF1_NAME ),
  _ifPol2Name( "carma." + config.getAntenna() + "." + IF2_NAME ),
  _client( Program::getProgram().getCorbaClient() )
{
    // Nothing
}

// -----------------------------------------------------------------------------
RxControlImpl::~RxControlImpl()
{
  // Nothing
}

// -----------------------------------------------------------------------------
  carma::antenna::common::LOControl_ptr RxControlImpl::LO()
{
  log_ << Priority::INFO << "LO()";

  return carma::antenna::common::LOControl::_duplicate( _loPtr );
}

// -----------------------------------------------------------------------------
carma::antenna::common::IFControl_ptr RxControlImpl::IF(
    carma::antenna::common::RxControl::IF_Type pol )
{

  if (
      pol != carma::antenna::common::RxControl::IF1 &&
      pol != carma::antenna::common::RxControl::IF2 )
  {
    logCaughtAndRethrowAsUser( log_, Priority::ERROR );
  }

  try
  {

    if ( pol == carma::antenna::common::RxControl::IF1 )
    {

      log_ << Priority::INFO << "IFPol1()";

      if ( CORBA::is_nil( _ifPol1Control ) )
      {
	try
	{
	  CARMA_CPTRACE( Trace::TRACE3, "_ifPol1Control ref is nil, attempting "
	      " to fetch it with name: " << _ifPol1Name );

	  _ifPol1Control = _client.resolveName<IFControl>( _ifPol1Name );
	}
	catch (...)
	{
	  _ifPol1Control = IFControl::_nil();
	  throw;
	}
      }
    }
    else if ( pol == carma::antenna::common::RxControl::IF2 )
    {


      log_ << Priority::INFO << "IFPol2()";

      if ( CORBA::is_nil( _ifPol2Control ) )
      {
	try
	{
	  CARMA_CPTRACE( Trace::TRACE3, "_ifPol2Control ref is nil, attempting "
	      " to fetch it with name: " << _ifPol2Name );

	  _ifPol2Control = _client.resolveName<IFControl>( _ifPol2Name );
	}
	catch (...)
	{
	  _ifPol2Control = IFControl::_nil();
	  throw;
	}
      } // if CORBA::is_nil
    } // else if IF2
  }
  catch (...)
  {
    logCaughtAndRethrowAsUser( log_, Priority::ERROR );
  }

  // By default, if1 is returned
  return ( pol == carma::antenna::common::RxControl::IF2 ? _ifPol2Control._retn() : _ifPol1Control._retn() );
}

// -----------------------------------------------------------------------------
carma::antenna::common::FrontEndControl_ptr RxControlImpl::FrontEnd(
        carma::antenna::common::RxControl::Pol_Type pol )
{
  log_ << Priority::INFO << "FrontEnd()";

  return carma::antenna::common::FrontEndControl::_duplicate( _fePtr );
}

// -----------------------------------------------------------------------------
  carma::antenna::common::OpticsControl_ptr RxControlImpl::Optics()
{
  log_ << Priority::INFO << "Optics()";

  return carma::antenna::common::OpticsControl::_duplicate( _opPtr );
}

// -----------------------------------------------------------------------------
  carma::antenna::common::PolarizationControl_ptr RxControlImpl::Polarization()
{
  log_ << Priority::INFO << "Polarization()";

  return carma::antenna::common::PolarizationControl::_duplicate( _poPtr );
}

// -----------------------------------------------------------------------------
void RxControlImpl::setFrequency( ::CORBA::Double yigFreq,
                                  ::CORBA::Double LOFreq,
                                  ::CORBA::Boolean endWithAbsorberInBeam,
                                  ::CORBA::Boolean optimizeReceiver,
                                  ::CORBA::Boolean forceRelock)
{
  try
  {
    if ( LOFreq < 75 )
      RWSET( band, RxCommand::RX1CM );
    else if ( LOFreq > 150 )
      RWSET( band, RxCommand::RX1MM );
    else
      RWSET( band, RxCommand::RX3MM );

    RWSET( command, RxCommand::SETBAND );
    this->rxWrite();
    
    if (forceRelock) {
        RWSET(command, RxCommand::FORCERELOCK);
    }
    this->rxWrite();

    RWSET( command, RxCommand::SETFREQ );
    RWSET( yigfreq, yigFreq );
    RWSET( lofreq, LOFreq );
    RWSET( leaveAbsorber, endWithAbsorberInBeam );
    RWSET( optimizeReceiver, optimizeReceiver );
    this->rxWrite();

  }
  catch ( ErrorException &eex )
  {
    log_ << Priority::WARN << "setFrequency( " << yigFreq
      << ", " << LOFreq << ") FAILED";
  }

  // Nothing
}

// -----------------------------------------------------------------------------
void RxControlImpl::setFrequency(
    ::CORBA::Double yigFreq,
    ::CORBA::Double LOFreq,
    ::CORBA::Boolean endWithAbsorberInBeam,
    ::CORBA::Boolean optimizeReceiver,
    ::CORBA::Boolean forceRelock,
    ::CORBA::ULong seqNo )
{
  try
  {
    log_ << Priority::INFO << boolalpha
      << "setFrequency( yigFreq=" << yigFreq
      << ", LOFreq=" << LOFreq
      << ", endWithAbsorberInBeam=" << endWithAbsorberInBeam
      << ", optimizeReceiver=" << optimizeReceiver
      << ", forceRelock=" << forceRelock
      << " seqNo=" << seqNo << " )";

    RWSET( command, RxCommand::SETTUNENEXTSEQNO );
    RWSET( tuneSeqNo, seqNo );
    this->rxWrite();

    setFrequency(yigFreq, LOFreq, 
            endWithAbsorberInBeam, optimizeReceiver, forceRelock);
  }
  catch ( ... )
  {
    logCaughtAndRethrowAsUser( Priority::ERROR );
  }
}

// -----------------------------------------------------------------------------
void RxControlImpl::setObservingFrequency( ::CORBA::Double freq )
{
  try
  {
    log_ << Priority::INFO
      << "setObservingFrequency( freq=" << freq << " )";

    RWSET( command, RxCommand::SETOBSFREQ );
    RWSET( obsfreq, freq );
    this->rxWrite();
  }
  catch ( ... )
  {
    logCaughtAndRethrowAsUser( Priority::ERROR );
  }

}

// -----------------------------------------------------------------------------
void RxControlImpl::setObservingFrequency( ::CORBA::Double freq,
    ::CORBA::ULong seqNo )
{
  try
  {
    log_ << Priority::INFO
      << "setObservingFrequency( freq=" << freq << " seqNo=" << seqNo << " )";

    RWSET( command, RxCommand::SETTUNENEXTSEQNO );
    RWSET( tuneSeqNo, seqNo );
    this->rxWrite();

    setObservingFrequency( freq );
  }
  catch ( ... )
  {
    logCaughtAndRethrowAsUser( Priority::ERROR );
  }
}

// -----------------------------------------------------------------------------
void RxControlImpl::measureTotalPower(
    carma::antenna::common::CalibratorControl::Position position)
{
  log_ << Priority::INFO
    << "toggleFastSampling( position=" << position  << " )";

  // Nothing
}

// -----------------------------------------------------------------------------
void RxControlImpl::measureTotalPower(
    carma::antenna::common::CalibratorControl::Position position,
    ::CORBA::ULong seqNo)
{
  try
  {
    log_ << Priority::INFO
      << "measureTotalPower( position="
      << position  << " seqNo=" << seqNo << " )";

    RWSET( command, RxCommand::SETTUNENEXTSEQNO );
    RWSET( tuneSeqNo, seqNo );
    this->rxWrite();

    measureTotalPower( position );
  }
  catch ( ... )
  {
    logCaughtAndRethrowAsUser( Priority::ERROR );
  }
}

// -----------------------------------------------------------------------------
void RxControlImpl::toggleFastSampling(CORBA::ULong channel, bool start)
{
  log_ << Priority::INFO
    << "toggleFastSampling( channel=" << channel
    << ", start=" << (start ? "true" : "false" ) << " )";

}

// -----------------------------------------------------------------------------
void RxControlImpl::setIFPresetPower( )
{
    log_ << Priority::INFO << "RxControlImpl::setIFPresetPower( ).";
    if ( CORBA::is_nil( _ifPol1Control ) )
        _ifPol1Control = _client.resolveName<IFControl>( _ifPol1Name );
    if ( CORBA::is_nil( _ifPol2Control ) )
        _ifPol2Control = _client.resolveName<IFControl>( _ifPol2Name );

    if ( CORBA::is_nil( _ifPol1Control ) || CORBA::is_nil( _ifPol2Control ) ) {
        log_ << Priority::ERROR << "RxControlImpl::setIFPresetPower( ) "
            << "Unable to obtain a reference to IF1 or IF2.";
        return;
    }
    _ifPol1Control->setPresetPower();
    _ifPol2Control->setPresetPower();
}

// -----------------------------------------------------------------------------
void RxControlImpl::setIFAtten(
    CORBA::Float atten,
    carma::antenna::common::RxControl::IF_Type ifType )
{
    log_ << Priority::INFO
         << "RxControlImpl::setIFAtten( atten=" << atten << " ).";

    if ( CORBA::is_nil( _ifPol1Control ) )
        _ifPol1Control = _client.resolveName<IFControl>( _ifPol1Name );
    if ( CORBA::is_nil( _ifPol2Control ) )
        _ifPol2Control = _client.resolveName<IFControl>( _ifPol2Name );

    if ( CORBA::is_nil( _ifPol1Control ) || CORBA::is_nil( _ifPol2Control ) ) {
        log_ << Priority::ERROR << "RxControlImpl::setIFPresetPower( ) "
            << "Unable to obtain a reference to IF1 or IF2.";
        return;
    }

    if ( ifType == carma::antenna::common::RxControl::IF1 ) {
        _ifPol1Control->setAtten( atten );
    } else if ( ifType == carma::antenna::common::RxControl::IF2 ) {
        _ifPol2Control->setAtten( atten );
    } else if ( ifType == carma::antenna::common::RxControl::BOTH ) {
        _ifPol1Control->setAtten( atten );
        _ifPol2Control->setAtten( atten );
    }
}

// -----------------------------------------------------------------------------
void RxControlImpl::setIFPower( CORBA::Float power )
{
    log_ << Priority::INFO
         << "RxControlImpl::setIFPower( power=" << power << " ).";

    if ( CORBA::is_nil( _ifPol1Control ) )
        _ifPol1Control = _client.resolveName<IFControl>( _ifPol1Name );
    if ( CORBA::is_nil( _ifPol2Control ) )
        _ifPol2Control = _client.resolveName<IFControl>( _ifPol2Name );

    if ( CORBA::is_nil( _ifPol1Control ) || CORBA::is_nil( _ifPol2Control ) ) {
        log_ << Priority::ERROR << "RxControlImpl::setIFPresetPower( ) "
            << "Unable to obtain a reference to IF1 or IF2.";
        return;
    }

    _ifPol1Control->setPower( power );
    _ifPol2Control->setPower( power );
}
