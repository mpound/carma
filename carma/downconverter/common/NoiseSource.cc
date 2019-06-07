/**@file
 * Class Definition for Noise Sourc Device API 96.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.49 $
 * $Date: 2012/08/28 21:43:06 $
 * $Id: NoiseSource.cc,v 1.49 2012/08/28 21:43:06 abeard Exp $
 */

#include "carma/downconverter/common/NoiseSource.h"

#include "carma/canbus/exceptions.h"
#include "carma/canbus/Utilities.h"
#include "carma/monitor/DownconverterCommon.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedPthreadMutexLock.h"
#include "carma/util/Trace.h"

using namespace carma;
using namespace carma::canbus;
using namespace carma::canbus::devices;
using namespace carma::downconverter;
using namespace carma::util;
using namespace std;

namespace { // Anonymous namespace for local constants and typedefs

    // API Identifier
    const carma::canbus::apiType API_ID                  = 97;

    // Version of API document
    const char API_VERSION                               = 'A';

    // Fast monitor point message ids.
    const carma::canbus::msgType BLANKING_FRAME_PACKET_1 = 0x0E0;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_2 = 0x0E1;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_3 = 0x0E2;

    // Slow monitors.  Most of these are handled by the XacDevice
    // base class.  Any system monitors handled here, overload routines
    // in that class.
    const carma::canbus::msgType SYSTEM_MONITOR_PACKET_4 = 0x123;

    // The controls
    const carma::canbus::msgType SET_NOISE_OUT_TO_PRESET      = 0x040;
    const carma::canbus::msgType SET_NOISE_OUT_TO_REQUESTED   = 0x041;
    const carma::canbus::msgType SET_NOISE_ATTENUATION        = 0x042;
    const carma::canbus::msgType ENABLE_NOISE_SOURCE          = 0x043;
    const carma::canbus::msgType SET_TONE_OUT_TO_PRESET       = 0x044;
    const carma::canbus::msgType SET_TONE_OUT_TO_REQUESTED    = 0x045;
    const carma::canbus::msgType SET_TONE_ATTENUATION         = 0x046;
    const carma::canbus::msgType ENABLE_TONE_SOURCE           = 0x047;

    namespace CM = carma::monitor;

} // End anonymous namespace

NoiseSource::Shared::Shared( ) :
    noiseEnabled( false ),
    simNoiseEnabled( false ),
    mutex( )
{
    // Nothing
}


// -----------------------------------------------------------------------------
NoiseSource::NoiseSource(
    nodeType node,
    CanOutput &io,
    monitor::NoiseSourceContainer & nsMon ) :
        XacDevice(API_ID, node, io),
        state_( node == 0 ? 0 : &( nsMon.state() ) ),
        mon_( node == 0 ? 0 : &( nsMon.noiseSource() ) ),
        xacMon_( node == 0 ? 0 : &( nsMon.xac() ) ),
        shared_( )
{
    setBoardType(0);
    setSerialNumber(0);
    setApiVersion(API_VERSION);

    CPTRACE(Trace::TRACE6, "NoiseSource::NoiseSource() - Device class created"
        " for node " << getNode());
}

// -----------------------------------------------------------------------------
NoiseSource::~NoiseSource()
{
    CPTRACE(Trace::TRACE6, "NoiseSource::~NoiseSource() - Device class "
        "destroyed for node " << getNode());
}

// -----------------------------------------------------------------------------
apiType NoiseSource::getApiId()
{
    return API_ID;
}

// -----------------------------------------------------------------------------
void NoiseSource::updateFrameData()
{
    state_->setValue(static_cast< CM::StateMonitorPointEnum::STATE>(getState()));
}

// -----------------------------------------------------------------------------
map<msgType, string> NoiseSource::getHalfSecMonitors() const
{
    static bool init = false;
    static map<msgType, string> mons;

    if (!init) {
        mons[BLANKING_FRAME_PACKET_1] = "Blanking frame packet 1";
        mons[BLANKING_FRAME_PACKET_2] = "Blanking frame packet 2";
        mons[BLANKING_FRAME_PACKET_3] = "Blanking frame packet 3";
        init = true;
    }
    return mons;
}

// -----------------------------------------------------------------------------
map<msgType, string> NoiseSource::getSlowMonitors() const
{
    return XacDevice::getSlowMonitors();
}

// -----------------------------------------------------------------------------
void NoiseSource::processMsg(msgType mid, DataVector & data, bool sim)
{
    // If state is ONLINE, check if the packet is late.
    if (getState() == ONLINE) {
        if (isPacketLate(200.0))
            incrementLatePacketCount();
    }

    CPTRACE(Trace::TRACEALL, "NoiseSource::processMsg() - Processing "
            << (sim ? "simulated" : "unsimulated") << " msg 0x"
            << hex << mid << dec << " for node " << getNode() << ".");

    switch (mid) {
        case BLANKING_FRAME_PACKET_1:
            processBlankingFramePacket1(data);
            break;
        case BLANKING_FRAME_PACKET_2:
            processBlankingFramePacket2(data);
            break;
        case BLANKING_FRAME_PACKET_3:
            processBlankingFramePacket3(data);
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_1:
        case XacDevice::SYSTEM_MONITOR_PACKET_2:
        case XacDevice::SYSTEM_MONITOR_PACKET_3:
        case XacDevice::SYSTEM_MONITOR_PACKET_4:
        case XacDevice::SYSTEM_MONITOR_PACKET_5:
            XacDevice::processSystemMonitorPacket( mid, data, *xacMon_ );
            break;
        default:
            break;
    }
}

// -----------------------------------------------------------------------------
carma::canbus::Message NoiseSource::simulateMsg(msgType mid)
{
    carma::canbus::Message msg;

    CPTRACE(Trace::TRACEALL, "NoiseSource::simulateMsg() - Creating "
            << "simulated msg 0x"
            << hex << mid << dec << " for node " << getNode() << ".");

    switch (mid) {
        case BLANKING_FRAME_PACKET_1:
            msg = simBlankingFramePacket1();
            break;
        case BLANKING_FRAME_PACKET_2:
            msg = simBlankingFramePacket2();
            break;
        case BLANKING_FRAME_PACKET_3:
            msg = simBlankingFramePacket3();
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_1:
        case XacDevice::SYSTEM_MONITOR_PACKET_2:
        case XacDevice::SYSTEM_MONITOR_PACKET_3:
        case XacDevice::SYSTEM_MONITOR_PACKET_4:
        case XacDevice::SYSTEM_MONITOR_PACKET_5:
            msg = simSystemMonitorPacket( mid );
            break;
        default:
            break;
    }

    return msg;
}

// -----------------------------------------------------------------------------
void NoiseSource::processBlankingFramePacket1( DataVector & data )
{
    short ps5v, ps5vMinus, ps5vDig, ps7v;

    // The below condition is checked and an exception is thrown in order
    // to provide a slightly higher level of error correction...  If this
    // condition were to be true and the below exception wasn't thrown here
    // it would still be thrown from the dataToType calls below.
    if (data.size() < 8) {
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "NoiseSource::processBlankingFramePacket1() - Invalid data size.");
    }

    // Unpack the data
    ps5v = dataToShort(data);
    ps5vMinus = dataToShort(data);
    ps5vDig = dataToShort(data);
    ps7v = dataToShort(data);

    // Convert and place into monitor stream
    mon_->ps5v1().setValue(ps5v * 0.001);
    mon_->ps5v2().setValue(ps5vMinus * 0.001);
    mon_->ps5v3().setValue(ps5vDig * 0.001);
    mon_->ps7v().setValue(ps7v * 0.001);

}

// -----------------------------------------------------------------------------
void NoiseSource::processBlankingFramePacket2( DataVector & data )
{
    short ps12v, ps24v, pOut;
    unsigned char noiseAtten, toneAtten;
    if (data.size() < 8) {
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "NoiseSource::processBlankingFramePacket2() - Invalid data size.");
    }

    // Unpack the data
    ps12v = dataToShort(data);
    ps24v = dataToShort(data);
    noiseAtten = dataToUbyte(data);
    toneAtten = dataToUbyte(data);
    pOut = dataToShort(data);

    // Convert and place into monitor stream
    mon_->ps12v().setValue(ps12v * 0.001);
    mon_->ps24v().setValue(ps24v * 0.001);
    mon_->noiseAtten().setValue(noiseAtten);
    mon_->toneAtten().setValue(toneAtten);
    mon_->powerOut().setValue(pOut * 0.01);

}

// -----------------------------------------------------------------------------
void NoiseSource::processBlankingFramePacket3( DataVector & data )
{
    if (data.size() < 7) {
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "NoiseSource::processBlankingFramePacket3() - Invalid data size.");
    }
    
    // Unpack the data
    const unsigned short temp = dataToShort(data);
    const unsigned char noiseStatus = dataToUbyte( data );
    const unsigned char toneStatus = dataToUbyte( data );
    const unsigned char noiseSwitchState = dataToUbyte( data );
    const unsigned char toneSwitchState = dataToUbyte( data );
    const unsigned char outputSwitchState = dataToUbyte( data );

    { // Set state info
        ScopedPthreadMutexLock scopelock( shared_.mutex );
        shared_.noiseEnabled = (noiseStatus == 1);
    }

    const float LSBS_PER_DEGREE = 100.0;
    mon_->temp().setValue(temp / LSBS_PER_DEGREE );

    mon_->noiseStatus().setValue(
        static_cast<CM::NoiseStatusMonitorPointEnum::NOISESTATUS>(noiseStatus) );
    typedef carma::monitor::NoiseSource NS;
    mon_->toneStatus().setValue(
        static_cast<NS::ToneStatusMonitorPointEnum::TONESTATUS>( toneStatus ) );
    mon_->noiseSwitchState().setValue( static_cast<bool>( noiseSwitchState ) );
    mon_->toneSwitchState().setValue( static_cast<bool>( toneSwitchState ) );
    mon_->outputSwitchState().setValue( static_cast<bool>(outputSwitchState) );
}

// -----------------------------------------------------------------------------
carma::canbus::Message NoiseSource::simBlankingFramePacket1()
{
    short ps5v = 5000, psNeg5v = -5000, ps7v = 7000;
    carma::canbus::Message msg = createMsgToHost(BLANKING_FRAME_PACKET_1);
    msg << ps5v << psNeg5v << ps5v << ps7v;
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message NoiseSource::simBlankingFramePacket2()
{
    short ps12v = 12000, ps24v = 24010, pout = 10000;
    unsigned char atten = 1;
    carma::canbus::Message msg = createMsgToHost(BLANKING_FRAME_PACKET_2);
    msg << ps12v << ps24v << atten << atten << pout;
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message NoiseSource::simBlankingFramePacket3()
{
    const short temp = 5000;
    unsigned char noiseEnabled; 
    unsigned char noiseSwitch;
    unsigned char outputSwitch;
    { 
        ScopedPthreadMutexLock scopelock( shared_.mutex );
        noiseEnabled = shared_.simNoiseEnabled ? 1 : 0;
        noiseSwitch = shared_.simNoiseEnabled ? 1 : 0;
        outputSwitch = shared_.simNoiseEnabled ? 1 : 0;
    }
    const unsigned char toneEnabled = 2;
    const unsigned char toneSwitch = 0;
    carma::canbus::Message msg = createMsgToHost(BLANKING_FRAME_PACKET_3);
    msg << temp << noiseEnabled << toneEnabled << noiseSwitch;
    msg << toneSwitch << outputSwitch;
    return msg;
}

// -----------------------------------------------------------------------------
void NoiseSource::setNoiseOutputToPreset()
try {
    programLogInfoIfPossible( "NoiseSource::setNoiseOutputToPreset()" );
    carma::canbus::Message msg = createMsgToNode( SET_NOISE_OUT_TO_PRESET );
    io_.postMessage(msg);
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( );
}

// -----------------------------------------------------------------------------
void NoiseSource::setNoiseOutput( const CORBA::Double pout )
try {
    ostringstream os;
    os << "NoiseSource::setNoiseOutput( pout=" << pout << " )";
    programLogInfoIfPossible( os.str() );
    carma::canbus::Message msg = createMsgToNode( SET_NOISE_OUT_TO_REQUESTED );
    const double LSBS_PER_DBM = 100.0;
    msg << ( static_cast<short>( pout * LSBS_PER_DBM ) );
    io_.postMessage(msg);
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( );
}

// -----------------------------------------------------------------------------
void NoiseSource::setNoiseAttenuation( const CORBA::UShort atten )
try {
    ostringstream os;
    os << "NoiseSource::setNoiseAttenuation( atten=" << atten << " )";
    programLogInfoIfPossible( os.str() );
    carma::canbus::Message msg = createMsgToNode(SET_NOISE_ATTENUATION);
    msg << ( static_cast<unsigned char>( atten ) );
    io_.postMessage(msg);
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( );
}

// -----------------------------------------------------------------------------
void NoiseSource::setToneOutputToPreset( )
try {
    programLogInfoIfPossible( "NoiseSource::setToneOutputToPreset()" );
    carma::canbus::Message msg = createMsgToNode(SET_TONE_OUT_TO_PRESET);
    io_.postMessage( msg );
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( );
}

// -----------------------------------------------------------------------------
void NoiseSource::setToneOutput( const CORBA::Double powerIndBm )
try {
    ostringstream os;
    os << "NoiseSource::setToneOutput( powerIndBm=" << powerIndBm << " )";
    programLogInfoIfPossible( os.str() );
    carma::canbus::Message msg = createMsgToNode(SET_TONE_OUT_TO_PRESET);
    const double LSBS_PER_DBM = 100.0;
    msg << static_cast<short>( powerIndBm * LSBS_PER_DBM );
    io_.postMessage( msg );
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( );
}

// -----------------------------------------------------------------------------
void NoiseSource::setToneAttenuation( const CORBA::UShort atten )
try {
    ostringstream os;
    os << "NoiseSource::setToneAttenuation( powerIndBm=" << atten << " )";
    programLogInfoIfPossible( os.str() );
    carma::canbus::Message msg = createMsgToNode(SET_TONE_ATTENUATION);
    msg << ( static_cast<unsigned char>(atten) );
    io_.postMessage(msg);
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( );
}

// -----------------------------------------------------------------------------
void NoiseSource::enableToneSource( const CORBA::Boolean enable )
try {
    ostringstream os;
    os << "NoiseSource::enableToneSource( enable=" << enable << " )";
    programLogInfoIfPossible( os.str() );
    const unsigned char cbool = (enable ? 0x01 : 0x00);
    carma::canbus::Message msg = createMsgToNode(ENABLE_TONE_SOURCE);
    msg << cbool;
    io_.postMessage(msg);
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( );
}

// -----------------------------------------------------------------------------
void NoiseSource::enableNoiseSource( const CORBA::Boolean enable )
try {
    ostringstream os;
    os << "NoiseSource::enableNoiseSource( enable=" << enable << " )";
    programLogInfoIfPossible( os.str() );
    const unsigned char cbool = (enable ? 0x01 : 0x00);
    carma::canbus::Message msg = createMsgToNode(ENABLE_NOISE_SOURCE);
    msg << cbool;
    io_.postMessage(msg);
    
    ScopedPthreadMutexLock scopelock( shared_.mutex );
    shared_.simNoiseEnabled = enable;
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( );
}

// -----------------------------------------------------------------------------
bool NoiseSource::isEnabled()
{
    ScopedPthreadMutexLock scopelock( shared_.mutex );
    return shared_.noiseEnabled;
}
