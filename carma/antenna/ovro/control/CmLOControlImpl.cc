/**
 * @file
 * Class definition for LOControlImpl Corba control implementation.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * Version: $Revision: 1.11 $
 * $Date: 2013/04/22 17:41:05 $
 * $Id: CmLOControlImpl.cc,v 1.11 2013/04/22 17:41:05 mpound Exp $
 */

#include "carma/antenna/ovro/control/CmLOControlImpl.h"

#include "carma/antenna/common/LOReferenceMonitor.h"
#include "carma/antenna/common/loggingUtils.h"
#include "carma/antenna/common/Varactor.h"
#include "carma/antenna/ovro/canbus/GunnPll.h"
#include "carma/antenna/ovro/canbus/YigPll.h"
#include "carma/antenna/ovro/control/LOControlImpl.h"
#include "carma/util/BaseException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/PthreadAttr.h"
#include "carma/util/StartPthread.h"
#include "carma/util/Trace.h"

// Carma Tools includes
#include <log4cpp/Category.hh>
#include <log4cpp/Priority.hh>

using namespace carma::antenna::common;
using namespace carma::antenna::ovro;
using namespace carma::util;
using namespace log4cpp;
using namespace std;

namespace { // Anonymous namespace

    const Trace::TraceLevel TRACE_SET_FREQ = Trace::TRACE5;
    const Trace::TraceLevel TRACE_CTOR_DTOR = Trace::TRACE5;

    // Const map for common strings.
    typedef std::map<enum SwitchState, std::string> Switch2StringMap;

    const Switch2StringMap::value_type ssa[] = // Switch String Array
    {
        Switch2StringMap::value_type(ON, "ON"),
        Switch2StringMap::value_type(OFF, "OFF")
    };

    Switch2StringMap switch2string(
        ssa,
        ssa + sizeof(ssa) / sizeof(ssa[0]) );

    typedef struct {
        double min;
        double max;
    } FreqLimitType;

    // Limits in GHz - all are exactly representable.
    const unsigned MAX_YIG_LOCK_ATTEMPTS = 2;
    const FreqLimitType YIG_LIMITS   = {  8.0,  12.5};
    const FreqLimitType LO1CM_LIMITS = { 25.0,  37.0};
    const FreqLimitType LO3MM_LIMITS = { 70.0, 116.0};
    const FreqLimitType LO1MM_LIMITS = {210.0, 270.0};

    typedef map<RxControl::Type, FreqLimitType> FreqLimitMap;

    const FreqLimitMap::value_type freqLimitArray[] =
    {
        FreqLimitMap::value_type(RxControl::RX1CM, LO1CM_LIMITS),
        FreqLimitMap::value_type(RxControl::RX1MM, LO1MM_LIMITS),
        FreqLimitMap::value_type(RxControl::RX3MM, LO3MM_LIMITS)
    };

    FreqLimitMap loLimits(
        freqLimitArray,
        freqLimitArray + sizeof(freqLimitArray)/sizeof(freqLimitArray[0]));

} // End namespace <unnamed>

CmLOControlImpl::CmLOControlImpl(
    YigPll & yig,
    GunnPll & gunn,
    Varactor & varactor,
    LOReferenceMonitor & loref ) :
yig_( yig ),
gunn_( gunn ),
varactor_( varactor ),
loref_( loref ),
log_( Program::getLogger( ) ),
typeInfo_( carma::antenna::common::RxControl::RX1CM )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR,
                   "CmLOControlImpl() - Creating LO control object for "
                   "receiver of type " << typeInfo_.rxAsString( ) << "." );
}

CmLOControlImpl::~CmLOControlImpl()
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "~CmLoControlImpl() - Deleting LO control "
                  "object of type " << typeInfo_.rxAsString( ) << "!");
}

bool
CmLOControlImpl::yigFreqOutOfRange( const ::CORBA::Double yigFreq )
{
    return LOControlImpl::yigFreqOutOfRange( yigFreq );
}

bool
CmLOControlImpl::loFreqOutOfRange( const ::CORBA::Double loFreq )
{
    const RxControl::Type rxType = typeInfo_.rxAsRxControlType( );

    return ( loFreq > loLimits[rxType].max || loFreq < loLimits[rxType].min );
}

void
CmLOControlImpl::toggleSweep( const ::CORBA::Boolean on )
try {
    ostringstream msg;
    msg << "CmLOControlImpl::toggleSweep( ) - Toggling sweep "
        << (on ? "on" : "off") << ".";

    logInfoWithRxNdc( typeInfo_, msg.str( ) );

    varactor_.enableSweep(on);

} catch (...) {
    logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
}

void
CmLOControlImpl::toggleYigSweep( const ::CORBA::Boolean on )
try {
    ostringstream msg;
    msg << "LOControlImpl::toggleYigSweep() - Toggling sweep "
        << (on ? "on" : "off") << ".";

    logInfoWithRxNdc( typeInfo_, msg.str( ) );

    yig_.toggleSweep(on);

} catch (...) {
    logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
}

void
CmLOControlImpl::setYigFrequency( const ::CORBA::Double yigFreq )
try {
    ostringstream msg;
    msg << "CmLOControlImpl::setYigFrequency( yigFreq= " << yigFreq << "  )";

    logInfoWithRxNdc( typeInfo_, msg.str( ) );

    yig_.setYigFrequencyAndLockNoBlock( yigFreq );

} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
}

void
CmLOControlImpl::setLoFrequency( const ::CORBA::Double Frequency )
try {
    ostringstream msg;
    msg << "LOControlImpl::setLoFrequency( Frequency=" << Frequency << " )";

    logInfoWithRxNdc( typeInfo_, msg.str( ) );

    gunn_.disableAllGunns();

    varactor_.enableGunn( true );
    varactor_.setDummyLoFreq( Frequency );

} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
}

void
CmLOControlImpl::setLoTerminatorAttenuation( const ::CORBA::UShort atten )
try {
    ostringstream msg;
    msg << "LOControlImpl::setLoTerminatorAttenuation( atten=" << atten << " )";

    logInfoWithRxNdc( typeInfo_, msg.str( ) );

    loref_.setLOTerminatorAttenuation( static_cast<unsigned char>( atten ) );

} catch (...) {
    logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
}

void
CmLOControlImpl::setLoTerminatorPowerToPreset( )
try {
    logInfoWithRxNdc( typeInfo_,
                      "CmLoControlImpl::setLoTerminatorPowerToPreset()" );

    loref_.setPowerLevelToPreset( );

} catch (...) {
    logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
}

void
CmLOControlImpl::setLoTerminatorPowerLevel( const ::CORBA::Double power )
try {
    ostringstream msg;
    msg << "CmLOControlImpl::setLoTerminatorPowerLevel( power = "
        << power << " ).";

    logInfoWithRxNdc( typeInfo_, msg.str( ) );

    loref_.setPowerLevel( power );

} catch (...) {
    logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
}

void
CmLOControlImpl::extractTuneTable( )
try {
    logInfoWithRxNdc( typeInfo_, "CmLOControlImpl::extractTuneTable( )" );

    yig_.extractTuneTable();

} catch (...) {
    logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
}

void
CmLOControlImpl::setYigOutputFrequency( const ::CORBA::Double freq )
try {
    ostringstream msg;
    msg << "CmLOControlImpl::setYigOutputFrequency( freq=" << freq << " )";

    logInfoWithRxNdc( typeInfo_, msg.str( ) );

    yig_.setYigFrequencyWithoutLock(freq);

} catch (...) {
    logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
}

void
CmLOControlImpl::setDampingResistance( const ::CORBA::UShort resistance )
try {
    ostringstream msg;
    msg << "CmLOControlImpl::setDampingResistance( resistance=" << resistance
        << " )";

    logInfoWithRxNdc( typeInfo_, msg.str( ) );

    yig_.setDampingResistance( resistance );

} catch ( ... ) {
    logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
}

void
CmLOControlImpl::resetYigPll( )
try {
    logInfoWithRxNdc( typeInfo_, "CmLOControlImpl::resetYigPll( )" );

    yig_.reset();

} catch ( ... ) {
    logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
}

void CmLOControlImpl::setGunnLoopGainResistance(
    const ::CORBA::UShort resistanceInOhms )
try {
    ostringstream msg;
    msg << "CmLOControlImpl::setGunnLoopGain( resistanceInOhms="
        << resistanceInOhms << " )";

    logInfoWithRxNdc( typeInfo_, msg.str( ) );

    varactor_.setLoopGainResistance( resistanceInOhms );

} catch ( ... ) {
    logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
}

void
CmLOControlImpl::turnGunn( const carma::antenna::common::SwitchState state )
try {
    ostringstream msg;
    msg << "CmLOControlImpl::turnGunn( state="
        << switch2string[ state ] << " )";

    logInfoWithRxNdc( typeInfo_, msg.str( ) );

    varactor_.enableGunn( state == ON );

} catch ( ... ) {
    logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
}

void
CmLOControlImpl::turnIfMonitor(
    const carma::antenna::common::SwitchState state )
try {
    ostringstream msg;
    msg << "CmLOControlImpl::turnIfMonitor( state=" << switch2string[state]
        << " )";

    logInfoWithRxNdc( typeInfo_, msg.str( ) );

    varactor_.enableIFmonitor( state == ON );

} catch ( ... ) {
    logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
}

void
CmLOControlImpl::resetGunn( )
try {
    logInfoWithRxNdc( typeInfo_, "CmLOControlImpl::resetGunn()" );

    varactor_.reset();

} catch ( ... ) {
    logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
}
