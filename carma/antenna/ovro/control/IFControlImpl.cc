/**
 * @file
 * Class definition for IFControlImpl Corba control implementation.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * Version: $Revision: 1.19 $
 * $Date: 2012/02/15 21:05:00 $
 * $Id: IFControlImpl.cc,v 1.19 2012/02/15 21:05:00 abeard Exp $
 */

#include "carma/antenna/ovro/control/IFControlImpl.h"

// Carma includes
#include "carma/antenna/ovro/canbus/AntennaIF.h"
#include "carma/util/BaseException.h"
#include "carma/util/ExceptionUtils.h"
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

namespace {

string
ifTypeAsString( const carma::antenna::common::RxControl::IF_Type ifType )
{
    if ( ifType == carma::antenna::common::RxControl::IF1 )
        return "IF1";
    else if ( ifType == carma::antenna::common::RxControl::IF2 )
        return "IF2";
    else
        return "<error>";
}

} // namespace <unnamed>

// -----------------------------------------------------------------------------
IFControlImpl::IFControlImpl(
    AntennaIF& antennaIf,
    carma::antenna::common::RxControl::Type type,
    carma::antenna::common::RxControl::IF_Type ifType )
    :   antennaIf_( antennaIf ),
        log_( Program::getLogger( ) ),
        rxType_( type ),
        ifType_( ifType )
{
    CARMA_CPTRACE( Trace::TRACE6,
                   "IFControlImpl::IFControlImpl() - C'tor "
                   "for receiver type " << rxType_.rxAsString( ) << ".");
}

// -----------------------------------------------------------------------------
IFControlImpl::~IFControlImpl( )
{
    CARMA_CPTRACE( Trace::TRACE6,
                   "IFControlImpl::~IFControlImpl() - D'tor "
                   "for receiver type " << rxType_.rxAsString( ) << ".");
}

// -----------------------------------------------------------------------------
void
IFControlImpl::selectRx()
try {

    log_ << Priority::INFO << "IFControlImpl::selectRx() - "
         << "Selecting for " << ifTypeAsString( ifType_ )
         << " receiver type " << rxType_.rxAsString( ) << ".";

    const unsigned short band = rxType_.rxAsIfSwitchPosition( );

    antennaIf_.selectBand( band );

} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
}

// -----------------------------------------------------------------------------
IFTotalPowerVec
IFControlImpl::getIFTotalPower( ) const
{
    return antennaIf_.getIFTotalPower();
}

// -----------------------------------------------------------------------------
void
IFControlImpl::selectBand( ::CORBA::UShort band )
try {
    log_ << Priority::INFO << "IFControlImpl::selectBand( " << band << " )"
         << "for " << ifTypeAsString( ifType_ )
         << " Rx " << rxType_.rxAsString( ) << ".";

    antennaIf_.selectBand( static_cast<unsigned short>( band ) );

} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
}

// -----------------------------------------------------------------------------
void
IFControlImpl::reset( )
try {
    log_ << Priority::INFO << "IFControlImpl::reset() - "
         << "Sending reset message to " << ifTypeAsString( ifType_ )
         << " module.";
    antennaIf_.reset( );
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
}

// -----------------------------------------------------------------------------
void
IFControlImpl::setAtten( ::CORBA::Float atten )
try {
    log_ << Priority::INFO << "IFControlImpl::setAtten() - "
         << "Setting " << ifTypeAsString( ifType_ )
         << " attenuation to " << atten << "dB for receiver type "
         << rxType_.rxAsString( ) << ".";
    antennaIf_.setIFtotalAttenuation( static_cast<float>( atten ) );
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
}

// -----------------------------------------------------------------------------
void
IFControlImpl::setPower( ::CORBA::Float power )
try {
    log_ << Priority::INFO << "IFControlImpl::setPower() - "
         << "Setting " << ifTypeAsString( ifType_ )
         << " total power to " << power << "mW for receiver type "
         << rxType_.rxAsString( ) << ".";
    antennaIf_.setIFlevel( static_cast<float>( power ) );
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
}

// -----------------------------------------------------------------------------
void
IFControlImpl::saveCurrentPower( )
try {

    log_ << Priority::INFO << "IFControlImpl::saveCurrentPower() - Called "
        << "on " << ifTypeAsString( ifType_ ) << ".";

} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
}

// -----------------------------------------------------------------------------
void
IFControlImpl::setPresetPower( )
try {
    log_ << Priority::INFO << "IFControlImpl::setPresetPower() - "
         << "Setting " << ifTypeAsString( ifType_ )
         << " total power to preset for receiver type "
         << rxType_.rxAsString( ) << ".";
    antennaIf_.setOutputPowerToPreset();
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
}
