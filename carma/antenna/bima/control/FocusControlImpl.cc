/**
 * @file
 * BIMA CryoControl implementation.
 *
 * @author Colby Gutierrez-Kraybill
 *
 * $Revision: 1.14 $
 * $Id: FocusControlImpl.cc,v 1.14 2013/02/21 16:02:02 friedel Exp $
 */

#include "carma/antenna/bima/control/FocusControlImpl.h"
#include "carma/antenna/bima/control/IDLutils.h"
#include "carma/util/ExceptionUtils.h"

using namespace std;
using namespace log4cpp;
using namespace carma::util;
using namespace carma::antenna::bima;

// -----------------------------------------------------------------------------
FocusControlImpl::FocusControlImpl
(
 Configuration &config
):
  RxClient( config ),
  log_( Program::getLogger() ),
  _config( config )
{
  CPTRACE( Trace::TRACE5, "Constructing FocusControl Object..." );
  _tmmFocusPosition = -1000.0;
  int startattempts = 0;
  while(true)
    {
      try
	{
	  bimaShm = new SharedMemory( config.getAntenna().c_str() );
	  break;
	}
      catch ( ... )
	{
	  if ( startattempts++ < 10 ) 
	    {
	      log_ << Priority::WARN << "Unable to open shared memory file, retrying in 1 second";
	      sleep(1);
	    }
	  else
	    throw CARMA_ERROR( "Unable to open shared memory file after 9 attempts!" );
	}
    }
}

// -----------------------------------------------------------------------------
FocusControlImpl::~FocusControlImpl()
{
  // Nothing
}

// -----------------------------------------------------------------------------
  void FocusControlImpl::setZ( ::CORBA::Float position )
{
  try
  {
    log_ << Priority::INFO
      << "setZ( position=" << position << " )";

    if ( position < -25.0 || position > 25.0 )
      throw 1;
    if(_tmmFocusPosition < -30.0){
      _tmmFocusPosition = position;
      log_ << Priority::INFO << "Storing focus " << _tmmFocusPosition;
      bimaShm->putData("TMMFOCUS", &_tmmFocusPosition);
    }

    RWSET( command, RxCommand::SETFOCUSZ );
    RWSET( fPos, position );
    this->rxWrite();
  }
  catch ( int a )
  {
    ostringstream oss;
    oss << " focus position out of range [-25.0,25.0]: " << position;
    log_ << Priority::WARN << oss.str();

    throw CARMA_USREX( oss.str() );
  }
  catch ( ... )
  {
    logCaughtAndRethrowAsUser( Priority::ERROR );
  }
}

// -----------------------------------------------------------------------------
void FocusControlImpl::setZ( ::CORBA::Float position,
    ::CORBA::ULong seqNo )
{
  try
  {
    log_ << Priority::INFO
      << "setZWithSeqNo( position=" << position << " seqNo=" << seqNo << " )";

    RWSET( command, RxCommand::SETOPTICNEXTSEQNO );
    RWSET( opticSeqNo, seqNo );
    this->rxWrite();

    setZ( position );
  }
  catch ( ... )
  {
    logCaughtAndRethrowAsUser( Priority::ERROR );
  }
}

// -----------------------------------------------------------------------------
  void FocusControlImpl::setY( ::CORBA::Float position )
{
  try
  {
  log_ << Priority::INFO
    << "setY( position=" << position << " )"
    << " - call ignored by bima antenna.";
    RWSET( command, RxCommand::USENEXTOPTICSEQNO );
    this->rxWrite();
  }
  catch ( ... )
  {
    logCaughtAndRethrowAsUser( Priority::ERROR );
  }
}

// -----------------------------------------------------------------------------
void FocusControlImpl::setY( ::CORBA::Float position,
    ::CORBA::ULong seqNo )
{
  try
  {
    log_ << Priority::INFO
      << "setYWithSeqNo( position=" << position << " seqNo=" << seqNo << " )";

    RWSET( command, RxCommand::SETOPTICNEXTSEQNO );
    RWSET( opticSeqNo, seqNo );
    this->rxWrite();

    setY( position );
  }
  catch ( ... )
  {
    logCaughtAndRethrowAsUser( Priority::ERROR );
  }
}


// -----------------------------------------------------------------------------
  void FocusControlImpl::setX( ::CORBA::Float position )
{
  try
  {
    log_ << Priority::INFO
      << "setX( position=" << position << " )"
      << " - call ignored by bima antenna.";

    RWSET( command, RxCommand::USENEXTOPTICSEQNO );
    this->rxWrite();
  }
  catch ( ... )
  {
    logCaughtAndRethrowAsUser( Priority::ERROR );
  }

}

// -----------------------------------------------------------------------------
void FocusControlImpl::setX( ::CORBA::Float position,
    ::CORBA::ULong seqNo )
{
  try
  {
    log_ << Priority::INFO
      << "setXWithSeqNo( position=" << position << " seqNo=" << seqNo << " )";

    RWSET( command, RxCommand::SETOPTICNEXTSEQNO );
    RWSET( opticSeqNo, seqNo );
    this->rxWrite();

    setX( position );
  }
  catch ( ... )
  {
    logCaughtAndRethrowAsUser( Priority::ERROR );
  }
}


// -----------------------------------------------------------------------------
  void FocusControlImpl::doZTracking( ::CORBA::Boolean tracking )
{
  log_ << Priority::INFO
    << "doZTracking( tracking=" << tracking << " )"
    << " - call ignored by bima antenna.";

    RWSET( command, RxCommand::USENEXTOPTICSEQNO );
    this->rxWrite();
}

// -----------------------------------------------------------------------------
void FocusControlImpl::doZTracking( ::CORBA::Boolean tracking,
    ::CORBA::ULong seqNo )
{
  try
  {
    log_ << Priority::INFO
      << "doZTrackingWithSeqNo( tracking=" << tracking << " seqNo=" << seqNo << " )";

    RWSET( command, RxCommand::SETOPTICNEXTSEQNO );
    RWSET( opticSeqNo, seqNo );
    this->rxWrite();

    doZTracking( tracking );
  }
  catch ( ... )
  {
    logCaughtAndRethrowAsUser( Priority::ERROR );
  }
}

