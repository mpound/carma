/**
 * @file
 * BIMA CryoControl implementation.
 *
 * @author Colby Gutierrez-Kraybill
 *
 * $Revision: 1.10 $
 * $Id: PolarizationControlImpl.cc,v 1.10 2012/02/21 21:06:58 abeard Exp $
 */


// Carma includes
#include "carma/antenna/bima/control/PolarizationControlImpl.h"
#include "carma/util/ExceptionUtils.h"

using namespace carma::antenna::common;
using namespace carma::antenna::bima;
using namespace carma::util;
using namespace log4cpp;

float PolarizationControlImpl::observingFreq_;


// -----------------------------------------------------------------------------
PolarizationControlImpl::PolarizationControlImpl
(
 Configuration &config
 )
: RxClient( config ),
  log_(Program::getLogger()),
  _config( config )

{
    // Nothing
}

// -----------------------------------------------------------------------------
PolarizationControlImpl::~PolarizationControlImpl()
{
    // Nothing
}

// -----------------------------------------------------------------------------
void PolarizationControlImpl::setObservingFreq(float freq)
{
   observingFreq_ = freq;
}

// -----------------------------------------------------------------------------
void PolarizationControlImpl::setState(
    carma::antenna::common::PolarizationControl::State poltype)
{
    try {

        CPTRACE(Trace::TRACE5, "PolarizationControlImpl::setState() - "
            "Setting polarization state to " << poltype );

	// STUBBED OUT FOR BIMA ANTENNAS UNTIL DESIGN CHANGES
	// TO HARDWARE ARE AVAILABLE

        switch (poltype) {
            case carma::antenna::common::PolarizationControl::POLH:
                break;
            case carma::antenna::common::PolarizationControl::POLV:
                break;
            case carma::antenna::common::PolarizationControl::POLRCP:
                break;
            case carma::antenna::common::PolarizationControl::POLLCP:
                break;
            default:
                break;
        }

    } catch (const carma::util::ErrorException &ex) {
        log_ << Priority::ERROR << ""
            "PolarizationControlImpl::setState() - carma::util::ErrorException "
            "caught. " << ex.what();
    } catch (...) {
        log_ << Priority::ERROR << "carma::antenna::bima::"
            "PolarizationControlImpl::setState() - Unknown exception caught";
    }
}

// -----------------------------------------------------------------------------
void PolarizationControlImpl::setState(
    carma::antenna::common::PolarizationControl::State poltype,
    ::CORBA::ULong seqNo )
{
  try
  {
    log_ << Priority::INFO
      << "setState( poltype=" << poltype << " seqNo=" << seqNo << " )";

    RWSET( command, RxCommand::SETOPTICNEXTSEQNO );
    RWSET( opticSeqNo, seqNo );
    this->rxWrite();

    setState( poltype );
  }
  catch ( ... )
  {
    logCaughtAndRethrowAsUser( Priority::ERROR );
  }
}


