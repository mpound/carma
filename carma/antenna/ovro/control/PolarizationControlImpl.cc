/**
 * @file
 * Class definition of PolarizationControlImpl Corba control implementation.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * Version: $Revision: 1.23 $
 * $Date: 2012/02/23 22:31:24 $
 * $Id: PolarizationControlImpl.cc,v 1.23 2012/02/23 22:31:24 abeard Exp $
 */

// Carma includes
#include "carma/antenna/ovro/canbus/Optics.h"
#include "carma/antenna/ovro/control/PolarizationControlImpl.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/BaseException.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"

// Carma Tools includes
#include <log4cpp/Category.hh>
#include <log4cpp/Priority.hh>

using namespace carma::antenna::common;
using namespace carma::antenna::ovro;
using namespace carma::util;
using namespace log4cpp;
using namespace std;

float PolarizationControlImpl::observingFreq_;

// -----------------------------------------------------------------------------
PolarizationControlImpl::PolarizationControlImpl(
    carma::antenna::ovro::Optics& optics,
    carma::antenna::common::RxControl::Type type ) :
    optics_(optics),
    log_(Program::getLogger()),
    type_(type)
{
    string objectId;
    string rx = (type == RxControl::RX1CM ? "RX1CM" : (type == RxControl::RX1MM
                ? "RX1MM" : (type == RxControl::RX3MM ? "RX3MM" : "RXANY")));

    CARMA_CPTRACE(Trace::TRACE6, "PolarizationControlImpl() - Creating "
        "polarization control object for " << rx << ".");
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
    const carma::antenna::common::PolarizationControl::State poltype,
    const ::CORBA::ULong seqNo )
try {
    log_ << Priority::WARN << "carma::antenna::ovro::"
         << "PolarizationControlImpl::setState() - N/A on 10m Dishes.";
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser();
}

// -----------------------------------------------------------------------------
void PolarizationControlImpl::setParameters(float gridAngle, float backshortPos)
try {
        log_ << Priority::INFO << "PolarizationControlImpl::setParameters() "
            << "- Setting grid angle to " << gridAngle
            << ", setting backshortPos to " << backshortPos << " for rx type "
            << type_ << ".";
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser();
}
