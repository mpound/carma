/** @file
 * CAN Device class definition for the Varactor-Tuned Gunn PLL (API 48).
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.2 $
 * $Date: 2011/01/03 18:48:05 $
 * $Id: Varactor.cc,v 1.2 2011/01/03 18:48:05 iws Exp $
 *
 * $CarmaCopyright$
 */

#include "carma/antenna/common/Varactor.h"

#include "carma/canbus/Utilities.h"
#include "carma/monitor/CanbusCommon.h"
#include "carma/monitor/Varactor.h"
#include "carma/util/programLogging.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"

#include <iomanip>

using namespace carma;
using namespace carma::canbus;
using namespace carma::antenna::common;
using namespace carma::util;
using namespace std;

namespace { 
    
    const carma::canbus::apiType VARACTOR_API = 48;

    const Trace::TraceLevel TRACE_CTOR_DTOR = Trace::TRACE3;

    // Controls
    const canbus::msgType ENABLE_GUNN             = 0x080;
    const canbus::msgType ENABLE_SWEEP            = 0x081;
    const canbus::msgType ENABLE_IF_MONITOR       = 0x082;
    const canbus::msgType SET_LOOP_GAIN_OHMS      = 0x083;

    // Monitors
    const canbus::msgType BLANKING_FRAME_PACKET_1 = 0x0E0;
    const canbus::msgType BLANKING_FRAME_PACKET_2 = 0x0E1;
    const canbus::msgType BLANKING_FRAME_PACKET_3 = 0x0E2;
    const canbus::msgType BLANKING_FRAME_PACKET_4 = 0x0E3;
    const canbus::msgType BLANKING_FRAME_PACKET_5 = 0x0E4;
 
    const double PACKET_LATE_THRESH_MS = 150.0;
    const float VOLTS_PER_LSB = 1.0e-3;

    namespace CM = carma::monitor;

} // namespace < unnamed >

Varactor::Varactor( canbus::nodeType node,
                    canbus::CanOutput & co,
                    monitor::VaractorModule & varMon,
                    monitor::Xac & xacMon,
                    monitor::StateMonitorPointEnum & stateMon ) :
    XacDevice( VARACTOR_API, node, co ),
    state_( stateMon ),
    varMon_( varMon ),
    xacMon_( xacMon ),
    freqInGHz_( 0.0 )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "Varactor::Varactor( node=" << node << ")" );
}

Varactor::~Varactor( ) 
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "Varactor::~Varactor( )" );
}

canbus::MsgIdInfoMap
Varactor::getHalfSecMonitors( ) const
{
    static MsgIdInfoMap idInfo;
    static bool init = false;

    if ( !init ) {
        idInfo[BLANKING_FRAME_PACKET_1] = "Varactor::BLANKING_FRAME_PACKET_1";
        idInfo[BLANKING_FRAME_PACKET_2] = "Varactor::BLANKING_FRAME_PACKET_2";
        idInfo[BLANKING_FRAME_PACKET_3] = "Varactor::BLANKING_FRAME_PACKET_3";
        idInfo[BLANKING_FRAME_PACKET_4] = "Varactor::BLANKING_FRAME_PACKET_4";
        idInfo[BLANKING_FRAME_PACKET_5] = "Varactor::BLANKING_FRAME_PACKET_5";
    }

    return idInfo;
}

canbus::MsgIdInfoMap
Varactor::getSlowMonitors( ) const
{
    return XacDevice::getSlowMonitors( );
}

void
Varactor::processMsg( const canbus::msgType mid,
                      canbus::DataVector & data,
                      const bool sim )
{
    // If state is ONLINE, check if the packet is late...
    if ( getState( ) == ONLINE ) {
        if ( isPacketLate( PACKET_LATE_THRESH_MS ) ) {
            incrementLatePacketCount( );
        }
    }

    CARMA_CPTRACE( Trace::TRACEALL, "Varactor::processMsg() - Processing "
        << (sim ? "simulated" : "real") << " msg 0x" << hex << mid << dec 
        << " for node " << getNode() << "."); 

    switch (mid) {
        case BLANKING_FRAME_PACKET_1:
            processBlankingFramePacket1( data );
            break;
        case BLANKING_FRAME_PACKET_2:
            processBlankingFramePacket2( data );
            break;
        case BLANKING_FRAME_PACKET_3:
            processBlankingFramePacket3( data );
            break;
        case BLANKING_FRAME_PACKET_4:
            processBlankingFramePacket4( data );
            break;
        case BLANKING_FRAME_PACKET_5:
            processBlankingFramePacket5( data );
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_1:
            XacDevice::processSystemMonitorPacket1( data, xacMon_ );
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_2:
            XacDevice::processSystemMonitorPacket2( data, xacMon_ );
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_3:
            XacDevice::processSystemMonitorPacket3( data, xacMon_ );
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_4:
            XacDevice::processSystemMonitorPacket4( data, xacMon_ );
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_5:
            XacDevice::processSystemMonitorPacket5( data, xacMon_ );
            break;
        default:
            {
                // I don't know how to process this message id!  Just log it.
                ostringstream dbg; 
                dbg << "Varactor::processMsg() - Switch does not match any case:"
                    << " Unknown mid " << mid << ". Node " << getNode();
                programLogDebugIfPossible( dbg.str() );
                CARMA_CPTRACE( Trace::TRACEALL, dbg.str() );
            }
            break;
    }

}

canbus::Message 
Varactor::simulateMsg( const msgType mid )
{
    CARMA_CPTRACE( Trace::TRACEALL, "Varactor::simulateMsg() - Simulating msg "
        "0x" << hex << mid << dec << " for node " << getNode() << "." );
    
    carma::canbus::Message msg;
    
    switch ( mid ) {
        case BLANKING_FRAME_PACKET_1:
            msg = simBlankingFramePacket1();
            break;
        case BLANKING_FRAME_PACKET_2:
            msg = simBlankingFramePacket2();
            break;
        case BLANKING_FRAME_PACKET_3:
            msg = simBlankingFramePacket3();
            break;
        case BLANKING_FRAME_PACKET_4:
            msg = simBlankingFramePacket4();
            break;
        case BLANKING_FRAME_PACKET_5:
            msg = simBlankingFramePacket5();
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_1:
            msg = XacDevice::simSystemMonitorPacket1();
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_2:
            msg = XacDevice::simSystemMonitorPacket2();
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_3:
            msg = XacDevice::simSystemMonitorPacket3();
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_4:
            msg = XacDevice::simSystemMonitorPacket4();
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_5:
            msg = XacDevice::simSystemMonitorPacket5();
            break;
        default:
            // I don't know how to simulate this message!
            // Not a problem, just log it - but should I throw???
            {
                ostringstream dbg;

                dbg << "Varactor::simulateMsg - Switch does not match any case: "
                    << " mid 0x" << hex << mid << dec 
                    << " node " << getNode() << ".";

                programLogDebugIfPossible( dbg.str() );

                CARMA_CPTRACE( Trace::TRACEALL, dbg.str() );
            }
            break;
    }
    return msg;
}

void 
Varactor::updateFrameData( ) 
{
    state_.setValue( 
        static_cast<CM::StateMonitorPointEnum::STATE>( getState( ) ) );
    varMon_.commandedFreq( ).setValue( freqInGHz_ );
}

void
Varactor::enableGunn( const bool on ) const 
{
    const unsigned char enable = ( on ? 0x01 : 0x00 );
    canbus::Message msg = createMsgToNode( ENABLE_GUNN );
    msg << enable;
    io_.postMessage( msg );
}

void
Varactor::enableSweep( const bool on ) const
{
    const unsigned char enable = ( on ? 0x01 : 0x00 );
    canbus::Message msg = createMsgToNode( ENABLE_SWEEP );
    msg << enable;
    io_.postMessage( msg );
}

void
Varactor::enableIFmonitor( const bool on ) const
{
    const unsigned char enable = ( on ? 0x01 : 0x00 );
    canbus::Message msg = createMsgToNode( ENABLE_IF_MONITOR );
    msg << enable;
    io_.postMessage( msg );
}

void
Varactor::setLoopGainResistance( const unsigned short resistanceInOhms ) const
{
    canbus::Message msg = createMsgToNode( SET_LOOP_GAIN_OHMS );
    msg << resistanceInOhms;
    io_.postMessage( msg );
}

void 
Varactor::setDummyLoFreq( const float freqInGHz ) 
{
    freqInGHz_ = freqInGHz;
}

void 
Varactor::processBlankingFramePacket1( canbus::DataVector & data )
{
    const unsigned char lockStat = dataToUbyte( data );
    const unsigned char refStat = dataToUbyte( data );
    const unsigned char sweepStat = dataToUbyte( data );
    const unsigned char gunnStat = dataToUbyte( data );
    const unsigned short loopGain = dataToUshort( data );
    const short temp = dataToShort( data );

    typedef monitor::VaractorModule::LockStatusMonitorPointEnum::LOCKSTATUS LockStatus;
    varMon_.lockStatus().setValue( static_cast< LockStatus >( lockStat ) );

    typedef monitor::VaractorModule::RefStatusMonitorPointEnum::REFSTATUS RefStatus;
    varMon_.refStatus().setValue( static_cast< RefStatus >( refStat ) );

    varMon_.sweepEnabled( ).setValue( sweepStat == 0x01 );
    varMon_.gunnEnabled( ).setValue( gunnStat == 0x01 );
    varMon_.loopGainResistance().setValue( loopGain );
    varMon_.temperature().setValue( temp * 1.0e-2 );  
}

void 
Varactor::processBlankingFramePacket2( canbus::DataVector & data )
{
    const unsigned char ifMonStat = dataToUbyte( data );
    const unsigned char dataValid = dataToUbyte( data );
    const short noiseMeter = dataToShort( data );

    varMon_.ifPortEnabled().setValue( ifMonStat == 0x01 ); 
    varMon_.dataValid().setValue( dataValid == 0x01 );
    varMon_.noiseMeter().setValue( noiseMeter * VOLTS_PER_LSB );
}

void 
Varactor::processBlankingFramePacket3( canbus::DataVector & data )
{
    const short ifLevel = dataToShort( data );
    const short vError = dataToShort( data );
    const short gunnCurrent = dataToShort( data );
    const short ps24 = dataToShort( data );

    varMon_.ifLevel().setValue( ifLevel * VOLTS_PER_LSB );
    varMon_.vError().setValue( vError * VOLTS_PER_LSB );
    varMon_.gunnCurrent().setValue( static_cast< float >( gunnCurrent ) );
    varMon_.ps24V().setValue( ps24 * VOLTS_PER_LSB );
}

void 
Varactor::processBlankingFramePacket4( canbus::DataVector & data )
{
    const short ps5v = dataToShort( data );
    const short ps15v = dataToShort( data );
    const short ps12v = dataToShort( data );
    const short ps6v = dataToShort( data );

    varMon_.ps5vDigital().setValue( ps5v * VOLTS_PER_LSB );
    varMon_.ps15vAnalog().setValue( ps15v * VOLTS_PER_LSB );
    varMon_.ps12vAnalog().setValue( ps12v * VOLTS_PER_LSB );
    varMon_.ps6vAnalog().setValue( ps6v * VOLTS_PER_LSB );
}

void 
Varactor::processBlankingFramePacket5( canbus::DataVector & data )
{
    const short ps15v = dataToShort( data );
    const short ps5v = dataToShort( data );
    const short ps9v = dataToShort( data );

    varMon_.psNeg15vAnalog().setValue( ps15v * VOLTS_PER_LSB );
    varMon_.ps5vAnalog().setValue( ps5v * VOLTS_PER_LSB );
    varMon_.ps9vAnalog().setValue( ps9v * VOLTS_PER_LSB );
}

canbus::Message 
Varactor::simBlankingFramePacket1()
{
    // Use the current time to add some variability to some mps.
    const util::frameType now = Time::computeCurrentFrame();

    const unsigned char lockStat = (0x0008 & now ) >> 3; // Toggle every 8 frms
    const unsigned char refStat = (0x0010 & now ) >> 4; // Toggle every 16 frms
    const unsigned char sweepStat = ( 0x0001 & now ); // Toggle every frame
    const unsigned char gunnStat = (0x0002 & now ) >> 1; // Toggle every 2 frms
    const unsigned short loopGain = 5;
    const unsigned short temp = 5000; // 50C

    canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_1 );
    
    msg << lockStat << refStat << sweepStat << gunnStat << loopGain << temp;

    return msg;
}

canbus::Message 
Varactor::simBlankingFramePacket2()
{
    const unsigned char ifMonStat = 0x01;
    const unsigned char dataValid = 0x01;
    const short noiseV = 1300; 
    
    canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_2 );

    msg << ifMonStat << dataValid << noiseV;

    return msg;
}

canbus::Message 
Varactor::simBlankingFramePacket3()
{
    const short ifLevel = 2300;
    const short vError = 330;
    const short gunnCurrent = 440;
    const short ps24v = 24100;

    canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_3 );
    
    msg << ifLevel << vError << gunnCurrent << ps24v;

    return msg;
}

canbus::Message 
Varactor::simBlankingFramePacket4()
{
    const short ps5 = 5200;
    const short ps15 = 15300;
    const short ps12 = 12400;
    const short ps6 = 6500;

    canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_4 );

    msg << ps5 << ps15 << ps12 << ps6;

    return msg;
}

canbus::Message 
Varactor::simBlankingFramePacket5()
{
    const short ps15v = -15600;
    const short ps5v = 5700;
    const short ps9v = 9800;

    canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_5 );

    msg << ps15v << ps5v << ps9v;

    return msg;
}
