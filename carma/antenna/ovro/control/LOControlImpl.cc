/**
 * @file
 * Class definition for LOControlImpl Corba control implementation.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * Version: $Revision: 1.45 $
 * $Date: 2013/04/22 17:41:05 $
 * $Id: LOControlImpl.cc,v 1.45 2013/04/22 17:41:05 mpound Exp $
 */

#include "carma/antenna/ovro/control/LOControlImpl.h"

#include "carma/antenna/common/LOReferenceMonitor.h"
#include "carma/antenna/common/loggingUtils.h"
#include "carma/antenna/common/Varactor.h"
#include "carma/antenna/ovro/canbus/GunnPll.h"
#include "carma/antenna/ovro/canbus/YigPll.h"
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

// -----------------------------------------------------------------------------
LOControlImpl::LOControlImpl(
    YigPll & yig,
    GunnPll & gunn,
    Varactor & varactor,
    LOReferenceMonitor & loref,
    carma::antenna::common::RxControl::Type type ) :
yig_( yig ),
gunn_( gunn ),
varactor_( varactor ), 
loref_( loref ),
log_( Program::getLogger( ) ),
typeInfo_( type )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR,
                   "LOControlImpl() - Creating LO control object for "
                   "receiver of type " << typeInfo_.rxAsString( ) << "." );
}

// -----------------------------------------------------------------------------
LOControlImpl::~LOControlImpl()
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "~LoControlImpl() - Deleting LO control "
                  "object of type " << typeInfo_.rxAsString( ) << "!");
}

// -----------------------------------------------------------------------------
bool LOControlImpl::yigFreqOutOfRange( const ::CORBA::Double yigFreq )
{
    return ( yigFreq > YIG_LIMITS.max || yigFreq < YIG_LIMITS.min );
}

// -----------------------------------------------------------------------------
bool LOControlImpl::loFreqOutOfRange( const ::CORBA::Double loFreq )
{
    const RxControl::Type rxType = typeInfo_.rxAsRxControlType( );

    return ( loFreq > loLimits[rxType].max || loFreq < loLimits[rxType].min );
}

// -----------------------------------------------------------------------------
YigPll::LockResultType
LOControlImpl::setYigFrequencyAndWaitForLockOrTimeout(
    const ::CORBA::Double yigFreq )
{
    unsigned attempt = 0;
    YigPll::LockResultType lockstate = YigPll::YIG_UNLOCKED;

    while ( lockstate != YigPll::YIG_LOCKED &&
            attempt < MAX_YIG_LOCK_ATTEMPTS )
    {
        lockstate = yig_.setYigFrequencyAndLock( yigFreq );
        ++attempt;
    }

    return lockstate;
}

// -----------------------------------------------------------------------------
void LOControlImpl::toggleSweep( const ::CORBA::Boolean on )
try {
    ostringstream msg;
    msg << "LOControlImpl::toggleSweep( ) - Toggling sweep "
        << (on ? "on" : "off") << ".";

    logInfoWithRxNdc( typeInfo_, msg.str( ) );

    gunn_.toggleSweep(on);

} catch (...) {
    logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
}

// -----------------------------------------------------------------------------
void LOControlImpl::toggleYigSweep( const ::CORBA::Boolean on )
try {
    ostringstream msg;
    msg << "LOControlImpl::toggleYigSweep() - Toggling sweep "
        << (on ? "on" : "off") << ".";

    logInfoWithRxNdc( typeInfo_, msg.str( ) );

    yig_.toggleSweep(on);

} catch (...) {
    logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
}

// -----------------------------------------------------------------------------
void LOControlImpl::setYigFrequency( const ::CORBA::Double yigFreq )
try {
    ostringstream msg;
    msg << "LOControlImpl::setYigFrequency( yigFreq= " << yigFreq << "  )";

    logInfoWithRxNdc( typeInfo_, msg.str( ) );

    yig_.setYigFrequencyAndLockNoBlock( yigFreq );

} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
}

// -----------------------------------------------------------------------------
void LOControlImpl::setLoFrequency( const ::CORBA::Double Frequency )
try {
    const timespec sleepytime = { 0, 10000000L }; // 1ms

    ostringstream msg;
    msg << "LOControlImpl::setLoFrequency( Frequency=" << Frequency << " )";

    logInfoWithRxNdc( typeInfo_, msg.str( ) );

    varactor_.enableGunn( false );
    gunn_.disableAllGunns();

    nanosleep( &sleepytime, 0 ); // Modules can't handle high rx rates.

    gunn_.enableGunn( true ); // Explicitly enable the gunn.

    nanosleep( &sleepytime, 0 ); // Modules can't handle high rx rates.

    gunn_.setLoFrequency( Frequency );

} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
}

// -----------------------------------------------------------------------------
void LOControlImpl::setLoTerminatorAttenuation( const ::CORBA::UShort atten )
try {
    ostringstream msg;
    msg << "LOControlImpl::setLoTerminatorAttenuation( atten=" << atten << " )";

    logInfoWithRxNdc( typeInfo_, msg.str( ) );

    loref_.setLOTerminatorAttenuation( static_cast<unsigned char>( atten ) );

} catch (...) {
    logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
}

// -----------------------------------------------------------------------------
void LOControlImpl::setLoTerminatorPowerToPreset( )
try {
    logInfoWithRxNdc( typeInfo_,
                      "LoControlImpl::setLoTerminatorPowerToPreset()" );

    loref_.setPowerLevelToPreset( );

} catch (...) {
    logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
}

// -----------------------------------------------------------------------------
void LOControlImpl::setLoTerminatorPowerLevel( const ::CORBA::Double power )
try {
    ostringstream msg;
    msg << "LOControlImpl::setLoTerminatorPowerLevel( power = "
        << power << " ).";

    logInfoWithRxNdc( typeInfo_, msg.str( ) );

    loref_.setPowerLevel( power );

} catch (...) {
    logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
}

// -----------------------------------------------------------------------------
void LOControlImpl::extractTuneTable( )
try {
    logInfoWithRxNdc( typeInfo_, "LOControlImpl::extractTuneTable( )" );

    yig_.extractTuneTable();

} catch (...) {
    logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
}

// -----------------------------------------------------------------------------
void LOControlImpl::setYigOutputFrequency( const ::CORBA::Double freq )
try {
    ostringstream msg;
    msg << "LOControlImpl::setYigOutputFrequency( freq=" << freq << " )";

    logInfoWithRxNdc( typeInfo_, msg.str( ) );

    yig_.setYigFrequencyWithoutLock(freq);

} catch (...) {
    logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
}

// -----------------------------------------------------------------------------
void LOControlImpl::setDampingResistance( const ::CORBA::UShort resistance )
try {
    ostringstream msg;
    msg << "LOControlImpl::setDampingResistance( resistance=" << resistance
        << " )";

    logInfoWithRxNdc( typeInfo_, msg.str( ) );

    yig_.setDampingResistance( resistance );

} catch ( ... ) {
    logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
}

// -----------------------------------------------------------------------------
void LOControlImpl::resetYigPll( )
try {
    logInfoWithRxNdc( typeInfo_, "LOControlImpl::resetYigPll( )" );

    yig_.reset();

} catch ( ... ) {
    logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
}

// -----------------------------------------------------------------------------
void LOControlImpl::setGunnVoltage( const ::CORBA::Float volts )
try {
    ostringstream msg;
    msg << "LOControlImpl::setGunnVoltage( volts=" << volts << " )";

    logInfoWithRxNdc( typeInfo_, msg.str( ) );

    gunn_.setGunnOperatingVoltage( volts );
} catch ( ... ) {
    logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
}

// -----------------------------------------------------------------------------
void LOControlImpl::setGunnLoopGain( const ::CORBA::Float percent )
try {
    ostringstream msg;
    msg << "LOControlImpl::setGunnLoopGain( percent=" << percent << " )";

    logInfoWithRxNdc( typeInfo_, msg.str( ) );

    gunn_.setLoopGain( percent );

} catch ( ... ) {
    logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
}

// -----------------------------------------------------------------------------
void LOControlImpl::turnGunn( const carma::antenna::common::SwitchState state )
try {
    ostringstream msg;
    msg << "LOControlImpl::turnGunn( state=" << switch2string[ state ] << " )";

    logInfoWithRxNdc( typeInfo_, msg.str( ) );

    gunn_.enableGunn( state == ON );

} catch ( ... ) {
    logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
}

// -----------------------------------------------------------------------------
void LOControlImpl::turnIfMonitor(
    const carma::antenna::common::SwitchState state )
try {
    ostringstream msg;
    msg << "LOControlImpl::turnIfMonitor( state=" << switch2string[state]
        << " )";

    logInfoWithRxNdc( typeInfo_, msg.str( ) );

    gunn_.enableIfMonitorOutput( state == ON );

} catch ( ... ) {
    logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
}

// -----------------------------------------------------------------------------
void LOControlImpl::setTuner( const ::CORBA::ULong position )
try {
    ostringstream msg;
    msg << "LOControlImpl::setTuner( position= " << position << " )";

    logInfoWithRxNdc( typeInfo_, msg.str( ) );

    gunn_.setTuner( position );

} catch ( ... ) {
    logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
}

// -----------------------------------------------------------------------------
void LOControlImpl::setBackshort( const ::CORBA::ULong position )
try {
    ostringstream msg;
    msg << "LOControlImpl::setBackshort( position=" << position << " )";

    logInfoWithRxNdc( typeInfo_, msg.str( ) );

    gunn_.setBackshort( position );

} catch ( ... ) {
    logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
}

// -----------------------------------------------------------------------------
void LOControlImpl::setAttenuator( const ::CORBA::ULong position )
try {
    ostringstream msg;
    msg << "LOControlImpl::setAttenuator( position=" << position << " )";

    logInfoWithRxNdc( typeInfo_, msg.str( ) );

    gunn_.setAttenuator(position);

} catch ( ... ) {
    logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
}

// -----------------------------------------------------------------------------
void LOControlImpl::jogTuner( ::CORBA::Short microsteps )
try {
    ostringstream msg;
    msg << "LOControlImpl::jogTuner( microsteps=" << microsteps << " )";

    logInfoWithRxNdc( typeInfo_, msg.str( ) );

    gunn_.jogTuner(microsteps);

} catch ( ... ) {
    logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
}

// -----------------------------------------------------------------------------
void LOControlImpl::jogBackshort( ::CORBA::Short microsteps )
try {
    ostringstream msg;
    msg << "LOControlImpl::jogBackshort( microsteps=" << " )";

    logInfoWithRxNdc( typeInfo_, msg.str( ) );

    gunn_.jogBackshort(microsteps);

} catch ( ... ) {
    logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
}

// -----------------------------------------------------------------------------
void LOControlImpl::jogAttenuator( ::CORBA::Short microsteps )
try {
    ostringstream msg;
    msg << "LOControlImpl::jogAttenuator( microsteps=" << " )";

    logInfoWithRxNdc( typeInfo_, msg.str( ) );

    gunn_.jogAttenuator(microsteps);

} catch ( ... ) {
    logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
}

// -----------------------------------------------------------------------------
void LOControlImpl::resetGunn( )
try {
    logInfoWithRxNdc( typeInfo_, "LOControlImpl::resetGunn()" );

    gunn_.reset();

} catch ( ... ) {
    logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
}
