/** @file
 * Class definition for the EnvironmentalControl Corba control implementation.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.10 $
 * $Date: 2012/02/15 21:05:00 $
 * $Id: EnvironmentalControlImpl.cc,v 1.10 2012/02/15 21:05:00 abeard Exp $
 */

// Carma includes
#include "carma/antenna/ovro/control/EnvironmentalControlImpl.h"

#include "carma/antenna/ovro/canbus/EnvironmentalMonitor.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"

// Carma Tools includes
#include <log4cpp/Category.hh>
#include <log4cpp/Priority.hh>

using namespace carma::antenna::ovro;
using namespace carma::util;
using namespace std;
using namespace log4cpp;

// -----------------------------------------------------------------------------
EnvironmentalControlImpl::EnvironmentalControlImpl(
    EnvironmentalMonitor& env ) :
    env_(env),
    log_(Program::getLogger())
{
    CARMA_CPTRACE(Trace::TRACE6, "EnvironmentalControlImpl() - "
        "Creating Environmental control object.");
}

// -----------------------------------------------------------------------------
EnvironmentalControlImpl::~EnvironmentalControlImpl()
{
    CARMA_CPTRACE(Trace::TRACE6, "EnvironmentalControlImpl() - Destroying "
        "Environmental control object.");
}

// -----------------------------------------------------------------------------
void EnvironmentalControlImpl::enableCamera(const ::CORBA::Boolean on)
try {
        log_ << Priority::INFO
            << "EnvironmentalControlImpl::enableCamera() - Turning camera "
            << ( on ? "on." : "off.");
        env_.enableCamera( static_cast<bool>( on ) );
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( );
}

// -----------------------------------------------------------------------------
void EnvironmentalControlImpl::turnSidecabPowerOff()
try {
        log_ << Priority::INFO
            << "EnvironmentalControlImpl::turnSidecabPowerOff()"
            << " - Turning sidecab power off.";
        env_.turnSidecabPowerOff();
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( );
}

// -----------------------------------------------------------------------------
void EnvironmentalControlImpl::enable24vPs( const ::CORBA::UShort supplyNo, 
                                            const ::CORBA::Boolean on )
try {
        log_ << Priority::INFO
            << "EnvironmentalControlImpl::enable24vPs() - "
            << (on ? "En" : "Dis") << "abling power supply " << supplyNo;
        env_.enable24vPs(supplyNo, on);
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( );
}
