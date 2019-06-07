/**
 * @file
 * BIMA CryoControl implementation.
 *
 * @author Colby Gutierrez-Kraybill
 *
 * $Revision: 1.16 $
 * $Id: FrontEndControlImpl.cc,v 1.16 2012/02/21 21:06:58 abeard Exp $
 */

#include "carma/antenna/bima/control/FrontEndControlImpl.h"
#include "carma/util/ExceptionUtils.h"

using namespace std;
using namespace carma::util;
using namespace log4cpp;
using namespace carma::antenna::bima;

// -----------------------------------------------------------------------------
FrontEndControlImpl::FrontEndControlImpl
(
 Configuration &config
 )
: RxClient( config ),
  log_(Program::getLogger()),
  _config( config )
{
}

// -----------------------------------------------------------------------------
FrontEndControlImpl::~FrontEndControlImpl()
{
    // Nothing
}

// -----------------------------------------------------------------------------
void FrontEndControlImpl::setFrequency(double freq)
{
  log_ << Priority::INFO
       << "setFrequency( freq=" << freq << " )";

}

// -----------------------------------------------------------------------------
void FrontEndControlImpl::setSISVj(float voltage)
{
  log_ << Priority::INFO
       << "setSISVj( voltage=" << voltage << " )";

    // Nothing
}

// -----------------------------------------------------------------------------
void FrontEndControlImpl::setSISIj(float current)
{
  log_ << Priority::INFO
       << "setSISIj( current=" << current << " )";

    // Nothing
}

// -----------------------------------------------------------------------------
void FrontEndControlImpl::doIVcurve(
        ::CORBA::Float startVjInMv,
        ::CORBA::Float stopVjInMv,
        ::CORBA::Float stepVjInMv,
        ::CORBA::UShort deltaInMs,
        ::CORBA::Boolean doPower )
{
  log_ << Priority::INFO
       << "doIVcurve()";

  RWSET( command, RxCommand::USENEXTOPTICSEQNO );
  this->rxWrite();

    // Nothing... yet
}

// -----------------------------------------------------------------------------
void FrontEndControlImpl::doIVcurve(
    ::CORBA::Float startVjInMv,
    ::CORBA::Float stopVjInMv,
    ::CORBA::Float stepVjInMv,
    ::CORBA::UShort deltaInMs,
    ::CORBA::Boolean doPower,
	::CORBA::ULong seqNo )
{
  try
  {
    log_ << Priority::INFO
      << "doIVcurveWithSeqNo( start=" << startVjInMv
      << " stop=" << stopVjInMv
      << " step=" << stepVjInMv
      << " delta=" << deltaInMs
      << " seqNo=" << seqNo << " )";

    RWSET( command, RxCommand::SETOPTICNEXTSEQNO );
    RWSET( opticSeqNo, seqNo );
    this->rxWrite();

    doIVcurve( startVjInMv, stopVjInMv, stepVjInMv, deltaInMs, doPower );
  }
  catch ( ... )
  {
    logCaughtAndRethrowAsUser( Priority::ERROR );
  }
}

// -----------------------------------------------------------------------------
carma::antenna::common::IVCurve * FrontEndControlImpl::getIVCurve( )
{
  return new carma::antenna::common::IVCurve;
}

// -----------------------------------------------------------------------------
void FrontEndControlImpl::setVG(
    carma::antenna::common::FrontEndControl::Amp amp,
    carma::antenna::common::FrontEndControl::Stage stage,
    float voltage)
{
  log_ << Priority::INFO
    << "setVG( amp=" << amp << ", stage=" << stage << " )";
  // Nothing
}

// -----------------------------------------------------------------------------
void FrontEndControlImpl::setVD(
    carma::antenna::common::FrontEndControl::Amp amp,
    carma::antenna::common::FrontEndControl::Stage stage,
    float voltage)
{
  log_ << Priority::INFO
    << "setVD( amp=" << amp << ", stage=" << stage << " )";
  // Nothing
}

// -----------------------------------------------------------------------------
void FrontEndControlImpl::setID(
    carma::antenna::common::FrontEndControl::Amp amp,
    carma::antenna::common::FrontEndControl::Stage stage,
    float voltage)
{
  log_ << Priority::INFO
    << "setMixer( voltage=" << voltage << " )";
  // Nothing
}

// -----------------------------------------------------------------------------
  void FrontEndControlImpl::setMixer(float voltage)
{
  log_ << Priority::INFO
    << "setMixer( voltage=" << voltage << " )";
  // Nothing
}
