/**
 * @file
 * Class definition of CalibratorControlImpl CORBA control implementation.
 *
 * @author Colby Gutierrez-Kraybill
 * Version: $Revision: 1.15 $
 * $Date: 2012/06/05 22:39:41 $
 * $Id: CalibratorControlImpl.cc,v 1.15 2012/06/05 22:39:41 abeard Exp $
 */

// system includes
#include <iostream>

// Carma includes
#include "carma/antenna/bima/control/CalibratorControlImpl.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"

// Carma Tools includes
#include <log4cpp/Priority.hh>


using namespace std;
using namespace log4cpp;
using namespace carma::util;
using namespace carma::antenna::bima;
using namespace carma::antenna::common;

// -----------------------------------------------------------------------------
CalibratorControlImpl::CalibratorControlImpl
(
 Configuration &config
 ) :
  RxClient( config ),
  log_(Program::getLogger()),
  _config( config )
{
  CPTRACE( Trace::TRACE6, "Creating CalibratorControl objid..." );
}

// -----------------------------------------------------------------------------
CalibratorControlImpl::~CalibratorControlImpl()
{
  // Nothing
}

// -----------------------------------------------------------------------------
void CalibratorControlImpl::setPos (
    carma::antenna::common::CalibratorControl::Position position )
{

  try
  {
    string posString;

    switch ( position )
    {
      case carma::antenna::common::CalibratorControl::SKY:
	posString = "SKY";
	RWSET( cPos, RxCommand::SKY );
	break;
      case carma::antenna::common::CalibratorControl::AMBIENT:
	posString = "AMBIENT";
	RWSET( cPos, RxCommand::AMBIENT );
	break;
      case carma::antenna::common::CalibratorControl::FIXEDTEMP:
	posString = "FIXEDTEMP";
	RWSET( cPos, RxCommand::FIXEDTEMP );
	break;
      case carma::antenna::common::CalibratorControl::PARTIAL:
	posString = "PARTIAL (NOT USED IN BIMA)";
        break;
      default:
	throw 1;
	break;
    }

    log_ << Priority::INFO << "Calibrator.setPos( " << position 
        << " ( " << posString << " ) )";

    RWSET( command, RxCommand::SETCALPOS );
    this->rxWrite();
  }
  catch ( int a )
  {
    log_ << Priority::WARN << "Unknown position " << position << "!";

    throw CARMA_USREX( string("Unknown calibrator position!") );
  }
  catch ( ... )
  {
    logCaughtAndRethrowAsUser( Priority::ERROR );
  }
}

// -----------------------------------------------------------------------------
void CalibratorControlImpl::setPos(
    carma::antenna::common::CalibratorControl::Position position,
    ::CORBA::ULong seqNo)
{
  try
  {
    log_ << Priority::INFO
      << "setPosWithSeqNo( position=" << position
      << " seqNo=" << seqNo << " )";

    RWSET( command, RxCommand::SETCALNEXTSEQNO );
    RWSET( calSeqNo, seqNo );
    this->rxWrite();

    setPos( position );
  }
  catch ( ... )
  {
    logCaughtAndRethrowAsUser( Priority::ERROR );
  }
}


