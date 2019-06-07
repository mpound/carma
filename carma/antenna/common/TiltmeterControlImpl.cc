/** @file
 * Class definition for the TiltmeterControl Corba implementation.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.5 $
 * $Date: 2012/02/21 21:06:59 $
 * $Id: TiltmeterControlImpl.cc,v 1.5 2012/02/21 21:06:59 abeard Exp $
 */

// Carma includes
#include "carma/antenna/common/Tiltmeter.h"
#include "carma/antenna/common/TiltmeterControlImpl.h"
#include "carma/util/BaseException.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"

// Carma Tools includes
#include <log4cpp/Category.hh>
#include <log4cpp/Priority.hh>

using namespace carma::antenna::common;
using namespace carma::antenna::ovro;
using namespace carma::util;
using namespace std;
using namespace log4cpp;

// -----------------------------------------------------------------------------
TiltmeterControlImpl::TiltmeterControlImpl( Tiltmeter& tiltmeter ) :
    tilt_( tiltmeter ),
    log_(Program::getLogger())
{
    CARMA_CPTRACE(Trace::TRACE6, "TiltmeterControlImpl() - Creating Tiltmeter "
        "control object.");
}

// -----------------------------------------------------------------------------
TiltmeterControlImpl::~TiltmeterControlImpl()
{
    CARMA_CPTRACE(Trace::TRACE6, "~TiltmeterControlImpl() - Destroying "
        "Tiltmeter control object.");
}

// -----------------------------------------------------------------------------
void TiltmeterControlImpl::setTemperature(float temp)
{
    try {
        log_ << Priority::INFO << "TiltmeterControlImpl::setTemperature() - "
            << "Setting temperature to " << temp << " C.";
        tilt_.setTemperature(temp);
    } catch (const std::exception &ex) {
        log_ << Priority::ERROR
            << "TiltmeterControlImpl::setTemperature()"
            << ex.what();
        throw CARMA_EXCEPTION(UserException, ex.what());
    } catch (...) {
        log_ << Priority::ERROR
            << "TiltmeterControlImpl::setTemperature() - "
            << "Unknown exception caught.";
        throw CARMA_EXCEPTION(UserException, "Unknown exception.");
    }
}

// -----------------------------------------------------------------------------
void TiltmeterControlImpl::regulateTemperature(
    carma::antenna::common::TiltmeterControl::OpMode mode,
    float pwrfract)
{
    try {
        log_ << Priority::INFO << "TiltmeterControlImpl::regulateTemperature()"
            << " - OpMode " << static_cast<int>(mode) << ", with pwrfract "
             << pwrfract;
        tilt_.regulateTemperature(
            static_cast<Tiltmeter::OpMode>(mode), pwrfract);

    } catch (const std::exception &ex) {
        log_ << Priority::ERROR
            << "TiltmeterControlImpl::regulateTemperature()"
            << ex.what();
        throw CARMA_EXCEPTION(UserException, ex.what());
    } catch (...) {
        log_ << Priority::ERROR
            << "TiltmeterControlImpl::regulateTemperature() - "
            << "Unknown exception caught.";
        throw CARMA_EXCEPTION(UserException, "Unknown exception.");
    }
}

// -----------------------------------------------------------------------------
void TiltmeterControlImpl::setLoopGain(float gain)
{
    try {
        log_ << Priority::INFO << "TiltmeterControlImpl::setLoopGain() - "
            << "Setting loop gain to " << gain << ".";

        tilt_.setLoopGain(gain);

    } catch (const std::exception &ex) {
        log_ << Priority::ERROR
            << "TiltmeterControlImpl::setLoopGain()"
            << ex.what();
        throw CARMA_EXCEPTION(UserException, ex.what());
    } catch (...) {
        log_ << Priority::ERROR
            << "TiltmeterControlImpl::setLoopGain() - "
            << "Unknown exception caught.";
        throw CARMA_EXCEPTION(UserException, "Unknown exception.");
    }
}

// -----------------------------------------------------------------------------
void TiltmeterControlImpl::setLoopIntegrationConstant(float loopInteg)
{
    try {
        log_ << Priority::INFO
            << "TiltmeterControlImpl::setLoopIntegrationConstant() - "
            << "Setting loop integration constant to " << loopInteg << ".";
        tilt_.setLoopIntegrationConstant(loopInteg);
    } catch (const std::exception &ex) {
        log_ << Priority::ERROR
            << "TiltmeterControlImpl::setLoopIntegrationConstant()"
            << ex.what();
        throw CARMA_EXCEPTION(UserException, ex.what());
    } catch (...) {
        log_ << Priority::ERROR
            << "TiltmeterControlImpl::setLoopIntegrationConstant() - "
            << "Unknown exception caught.";
        throw CARMA_EXCEPTION(UserException, "Unknown exception.");
    }
}

// -----------------------------------------------------------------------------
void TiltmeterControlImpl::setLoopRateConstant(float rateConst)
{
    try {
        log_ << Priority::INFO
            << "TiltmeterControlImpl::setLoopRateConstant() - "
            << "Setting loop rate constant to " << rateConst << ".";
        tilt_.setLoopRateConstant(rateConst);
    } catch (const std::exception &ex) {
        log_ << Priority::ERROR
            << "TiltmeterControlImpl::setLoopRateConstant()"
            << ex.what();
        throw CARMA_EXCEPTION(UserException, ex.what());
    } catch (...) {
        log_ << Priority::ERROR
            << "TiltmeterControlImpl::setLoopRateConstant() - "
            << "Unknown exception caught.";
        throw CARMA_EXCEPTION(UserException, "Unknown exception.");
    }
}

// -----------------------------------------------------------------------------
void TiltmeterControlImpl::setLoopBandwidth(float bw)
{
    try {
        log_ << Priority::INFO << "TiltmeterControlImpl::setLoopBandwidth() - "
            "Setting loop bandwidth to " << bw << ".";
        tilt_.setLoopBandwidth(bw);
    } catch (const std::exception &ex) {
        log_ << Priority::ERROR
            << "TiltmeterControlImpl::setLoopBandwidth() - "
            << ex.what();
        throw CARMA_EXCEPTION(UserException, ex.what());
    } catch (...) {
        log_ << Priority::ERROR
            << "TiltmeterControlImpl::setLoopBandwidth() - "
            << "Unknown exception caught.";
        throw CARMA_EXCEPTION(UserException, "Unknown exception.");
    }
}

// -----------------------------------------------------------------------------
void TiltmeterControlImpl::writeLoopParametersToEEPROM()
{
    try {
        log_ << Priority::INFO << "TiltmeterControlImpl::"
            << "writeLoopParametersToEEPROM() - Writing current loop params "
            << "to EEPROM.";
        tilt_.writeLoopParametersToEEPROM();
    } catch (const std::exception &ex) {
        log_ << Priority::ERROR
            << "TiltmeterControlImpl::writeLoopParametersToEEPROM() - "
            << ex.what();
        throw CARMA_EXCEPTION(UserException, ex.what());
    } catch (...) {
        log_ << Priority::ERROR
            << "TiltmeterControlImpl::writeLoopParametersToEEPROM() - "
            << "Unknown exception caught.";
        throw CARMA_EXCEPTION(UserException, "Unknown exception.");
    }
}

