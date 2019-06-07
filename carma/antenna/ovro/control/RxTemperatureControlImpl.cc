/**
 * @file
 * RxTemperatureControl Corba control implementation.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * Version: $Revision: 1.7 $
 * $Date: 2012/02/15 21:05:00 $
 * $Id: RxTemperatureControlImpl.cc,v 1.7 2012/02/15 21:05:00 abeard Exp $
 */

// Carma includes
#include "carma/antenna/ovro/control/RxTemperatureControlImpl.h"

#include "carma/antenna/ovro/canbus/RxTemperatures.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"

// Carma Tools includes
#include <log4cpp/Category.hh>
#include <log4cpp/Priority.hh>

using namespace carma::antenna::ovro;
using namespace carma::util;
using namespace log4cpp;
using namespace std;

// -----------------------------------------------------------------------------
RxTemperatureControlImpl::RxTemperatureControlImpl( RxTemperatures& rxtemp ) :
    rxtemp_(rxtemp),
    log_(Program::getLogger())
{
    CARMA_CPTRACE(Trace::TRACE6, "RxTemperatureControlImpl() - Creating 10-m Rx"
        " Electronics Temperature Control object.");
}

// -----------------------------------------------------------------------------
RxTemperatureControlImpl::~RxTemperatureControlImpl()
{
    CARMA_CPTRACE(Trace::TRACE6, "~RxTemperatureControlImpl() - Destroy!");
}

// -----------------------------------------------------------------------------
void RxTemperatureControlImpl::setTemperature (
    carma::antenna::ovro::RxTemperatureControl::LoopId loop,
    float temp)
try {

        log_ << Priority::INFO << "RxTemperatureControlImpl::setTemperature() -"
            << " Setting loop " << (static_cast<int>(loop) + 1)
            << " temperature to " << temp << ".";

        rxtemp_.setTemperature(
            static_cast<RxTemperatures::LoopId>(loop),
            temp);

} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( );
}

// -----------------------------------------------------------------------------
void RxTemperatureControlImpl::regulateTemperature (
    carma::antenna::ovro::RxTemperatureControl::LoopId loop,
    carma::antenna::ovro::RxTemperatureControl::OpMode mode,
    float pwr)
try {

    log_ << Priority::INFO
        << "RxTemperatureControlImpl::regulateTemperature() -"
        << " Regulating loop " << (static_cast<int>(loop) + 1)
        << " temperature.";

    rxtemp_.regulateTemperature(
            static_cast<RxTemperatures::LoopId>(loop),
            static_cast<RxTemperatures::OpMode>(mode),
            pwr);

} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( );
}

// -----------------------------------------------------------------------------
void RxTemperatureControlImpl::setLoopGain (
    carma::antenna::ovro::RxTemperatureControl::LoopId loop,
    float gain)
try {

        log_ << Priority::INFO
            << "RxTemperatureControlImpl::setLoopGain() -"
            << " Setting loop " << (static_cast<int>(loop) + 1)
            << " gain to " << gain << ".";

        rxtemp_.setLoopGain(
            static_cast<RxTemperatures::LoopId>(loop),
            gain);

} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( );
}

// -----------------------------------------------------------------------------
void RxTemperatureControlImpl::setLoopIntegrationConstant (
    carma::antenna::ovro::RxTemperatureControl::LoopId loop,
    float integration)
try {
        log_ << Priority::INFO
            << "RxTemperatureControlImpl::setLoopIntegrationConstant() -"
            << " Setting loop " << (static_cast<int>(loop) + 1)
            << " integration constant to " << integration << ".";

        rxtemp_.setLoopIntegrationConstant (
            static_cast<RxTemperatures::LoopId>(loop),
            integration);

} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( );
}

// -----------------------------------------------------------------------------
void RxTemperatureControlImpl::setLoopRateConstant (
    carma::antenna::ovro::RxTemperatureControl::LoopId loop,
    float rate)
try {

        log_ << Priority::INFO
            << "RxTemperatureControlImpl::setLoopRateConstant() -"
            << " Setting loop " << (static_cast<int>(loop) + 1)
            << " rate constant to " << rate << ".";

        rxtemp_.setLoopRateConstant(
            static_cast<RxTemperatures::LoopId>(loop),
            rate);

} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( );
}

// -----------------------------------------------------------------------------
void RxTemperatureControlImpl::setLoopBandwidth(
    carma::antenna::ovro::RxTemperatureControl::LoopId loop,
    float bandwidth)
try {

        log_ << Priority::INFO
            << "RxTemperatureControlImpl::setLoopBandwidth() -"
            << " Setting loop " << (static_cast<int>(loop) + 1)
            << " bandwidth to " << bandwidth << ".";

        rxtemp_.setLoopBandwidth(
            static_cast<RxTemperatures::LoopId>(loop),
            bandwidth);

} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( );
}

// -----------------------------------------------------------------------------
void RxTemperatureControlImpl::writeParametersToEEPROM()
try {

        log_ << Priority::INFO
            << "RxTemperatureControlImpl::writeParametersToEEPROM() -"
            << " Writing current loop settings to EEPROM.";

        rxtemp_.writeParametersToEEPROM( );

} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( );
}

// -----------------------------------------------------------------------------
void RxTemperatureControlImpl::reset()
try {
        log_ << Priority::INFO
            << "RxTemperatureControlImpl::reset() - Reset invoked.";
        rxtemp_.reset();
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( );
}
