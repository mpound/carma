/**
 * @file
 * Class definition for CryoControlImpl.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.18 $
 * $Date: 2012/02/15 21:05:00 $
 * $Id: CryoControlImpl.cc,v 1.18 2012/02/15 21:05:00 abeard Exp $
 */

// Carma includes
#include "carma/antenna/ovro/control/CryoControlImpl.h"
#include "carma/antenna/ovro/canbus/CryoCompressor.h"
#include "carma/antenna/common/SwitchState.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"

// Carma Tools includes
#include <log4cpp/Category.hh>
#include <log4cpp/Priority.hh>

using namespace carma;
using namespace carma::antenna::ovro;
using namespace carma::util;
using namespace log4cpp;

// -----------------------------------------------------------------------------
CryoControlImpl::CryoControlImpl(
    CryoCompressor& compressor ) :
    compressor_(compressor),
    log_(Program::getLogger())
{
    CARMA_CPTRACE(Trace::TRACE6, "CryoControlImpl() - "
        "Creating cryo control object.");
}

// -----------------------------------------------------------------------------
CryoControlImpl::~CryoControlImpl()
{
    CARMA_CPTRACE(Trace::TRACE6, "CryoControlImpl() - Destroying cryo "
        "control object.");
}

// -----------------------------------------------------------------------------
void CryoControlImpl::turnCompressor(carma::antenna::common::SwitchState state)
try {
    bool on = (state == carma::antenna::common::ON);
    log_ << Priority::INFO << "CryoControlImpl::turnCompressor() - "
        << (on ? "En" : "Dis") << "abling helium compressor.";
    compressor_.enableCompressor(on);
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser();
}

// -----------------------------------------------------------------------------
void CryoControlImpl::resetCompressor()
try {
    log_ << Priority::INFO << "CryoControlImpl::resetCompressor() - "
        << "Resetting helum compressor.";
    compressor_.resetCompressor();
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser();
}

// -----------------------------------------------------------------------------
void CryoControlImpl::fillCompressor()
try {
    log_ << Priority::INFO << "CryoControlImpl::fillCompressor() - "
         << "Filling helium compressor.";
    compressor_.fillCompressor();
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser();
}

// -----------------------------------------------------------------------------
void CryoControlImpl::purgeCompressor()
try {
    log_ << Priority::INFO << "CryoControlImpl::purgeCompressor() - "
         << "Purging helium compressor.";
    compressor_.purgeCompressor();
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser();
}

// -----------------------------------------------------------------------------
void CryoControlImpl::reset()
try {
    log_ << Priority::INFO << "CryoControlImpl::reset() - "
         << "Software resetting Compressor CAN module.";
    compressor_.reset();
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser();
}

// -----------------------------------------------------------------------------
void CryoControlImpl::turnTempServoLoop( antenna::common::SwitchState state )
try {
    bool on = (state == carma::antenna::common::ON);
    // Log it
    log_ << Priority::INFO << "CryoControlImpl::turnTempServoLoop() - "
        "Switching temperature servo loop " << (on ? "ON" : "OFF") << ".";

    compressor_.enableTemperatureServo(on);
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser();
}

// -----------------------------------------------------------------------------
void CryoControlImpl::setInletLouverPosition(float volts)
try {
    log_ << Priority::INFO << "CryoControlImpl::setInletLouverPosition() - "
         << "Setting inlet louver position to " << volts << "V.";
    compressor_.setInletLouver(volts);
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser();
}

// -----------------------------------------------------------------------------
void CryoControlImpl::setOutletLouverPosition(float volts)
try {
    log_ << Priority::INFO << "CryoControlImpl::setOutletLouverPosition() "
         << "- Setting outlet louver position to " << volts << "V.";
    compressor_.setOutletLouver(volts);
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser();
}
