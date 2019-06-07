/** @file
 * Implementation of 10-M Secondary Mirror CAN Device.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.24 $
 * $Date: 2011/01/03 18:48:06 $
 * $Id: SecondaryMirror.cc,v 1.24 2011/01/03 18:48:06 iws Exp $
 *
 * $CarmaCopyright$
 */

// Carma includes
#include "carma/antenna/ovro/canbus/SecondaryMirror.h"
#include "carma/antenna/ovro/canbus/SharedOpticsSeqNo.h"
#include "carma/canbus/exceptions.h"
#include "carma/canbus/Utilities.h"
#include "carma/monitor/AntennaCommon.h"
#include "carma/monitor/OvroSubsystem.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/ScopedPthreadMutexLock.h"
#include "carma/util/Trace.h"

// Carma Tools includes
#include <log4cpp/Category.hh>
#include <log4cpp/Priority.hh>

using namespace carma;
using namespace carma::antenna::ovro;
using namespace carma::canbus;
using namespace carma::canbus::devices;
using namespace carma::monitor;
using namespace carma::util;
using namespace log4cpp;
using namespace std;

namespace {

    // API Id for this device.
    const carma::canbus::apiType API_ID                    = 56;

    // API version this class was implemented from 
    const char API_VERSION                                 = 'F';
    
    // Late packet timeout in ms
    const double PACKET_LATE_THRESHOLD                     = 150.0;
    
    // Control command message ids.
    const carma::canbus::msgType SET_X_POSITION            = 0x080;
    const carma::canbus::msgType SET_Y_POSITION            = 0x081;
    const carma::canbus::msgType SET_Z_POSITION            = 0x082;
    const carma::canbus::msgType LVDT_POWER_CYCLE_RESET    = 0x083;
    const carma::canbus::msgType STOP_MOTION               = 0x084;

    // Blanking frame message ids.
    const carma::canbus::msgType BLANKING_FRAME_PACKET_1   = 0x0E0;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_2   = 0x0E1;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_3   = 0x0E2;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_4   = 0x0E3;

    const carma::util::Trace::TraceLevel TRACE_CTOR_DTOR   = Trace::TRACE3;

    const unsigned int MOVE_WAIT_FRAMES = 2;
    const unsigned int ZTRACK_WAIT_FRAMES = 1;

    short 
    positionToMicrons( const float posInMM )
    {
        const float umPerMM = 1000.0; // um/mm 
        return static_cast<short>( roundl( posInMM * umPerMM ) );
    }

} // namespace < unnamed >

struct SecondaryMirror::Shared {

    Shared( );

    bool zTracking;
    carma::util::PthreadMutex mutex;
};

SecondaryMirror::Shared::Shared( ) : zTracking( false ) { }

// -----------------------------------------------------------------------------
SecondaryMirror::SecondaryMirror( 
        const nodeType node, 
        CanOutput & io,
        OvroSubsystem & ovroSubsys,
        SharedOpticsSeqNo & sharedSeqNo ) :
    XacDevice( API_ID, node, io ),
    comMon_( ovroSubsys.antennaCommon( ) ),
    mon_( ovroSubsys.secondary( ) ),
    sharedSeqNo_( sharedSeqNo ),
    shared_( new Shared( ) )
{
    CPTRACE( TRACE_CTOR_DTOR, "SecondaryMirror::SecondaryMirror() - Device "
        "class created for api " << API_ID << " node " << node);
}

// -----------------------------------------------------------------------------
SecondaryMirror::~SecondaryMirror()
{
    CPTRACE( TRACE_CTOR_DTOR, "SecondaryMirror::~SecondaryMirror() - Device "
        "class destroyed for api " << API_ID << " node " << getNode( ) );
}

// -----------------------------------------------------------------------------
carma::canbus::MsgBriefMap SecondaryMirror::getHalfSecMonitors( ) const
{
    static canbus::MsgBriefMap tmp;
    static bool init = false;

    if ( !init ) {
        tmp[BLANKING_FRAME_PACKET_1] = 
            "SecondaryMirror::BLANKING_FRAME_PACKET_1";
        tmp[BLANKING_FRAME_PACKET_2] = 
            "SecondaryMirror::BLANKING_FRAME_PACKET_2";
        tmp[BLANKING_FRAME_PACKET_3] = 
            "SecondaryMirror::BLANKING_FRAME_PACKET_3";
        tmp[BLANKING_FRAME_PACKET_4] = 
            "SecondaryMirror::BLANKING_FRAME_PACKET_4";
        init = true;
    }
    return tmp;
}

// -----------------------------------------------------------------------------
carma::canbus::MsgBriefMap SecondaryMirror::getSlowMonitors( ) const
{
    return XacDevice::getSlowMonitors();
}

// -----------------------------------------------------------------------------
void SecondaryMirror::updateFrameData( )
{
    // This method gets called once every half second FOR EACH INSTANCE OF
    // THIS DEVICE REGISTERED WITH MASTER! Use it to update information 
    // pertinent to the frame time but not updated every message.

    sharedSeqNo_.writeToMonitorSystem( );
    
    // Set the state of the device...
    mon_.state( ).setValue(
        static_cast<StateMonitorPointEnum::STATE>( getState( ) ) );

    // Note ztracking request is a toggle so it can be pending for ON or OFF.
    sharedSeqNo_.markFocusZTrackingRequestCompleteIfPending(ZTRACK_WAIT_FRAMES); 
    // Set zTracking bool
    ScopedPthreadMutexLock scopelock( shared_->mutex );
    
    mon_.zTracking( ).setValue(
        ( shared_->zTracking ?  
            OvroSubsystem::ZTrackingMonitorPointEnum::ON :
            OvroSubsystem::ZTrackingMonitorPointEnum::OFF ) );
}

// -----------------------------------------------------------------------------
void SecondaryMirror::processMsg( const msgType mid, 
                                  DataVector & data, 
                                  const bool sim )
{
    // If state is ONLINE, check if the packet is late...
    if ( getState( ) == ONLINE ) {
        if ( isPacketLate( PACKET_LATE_THRESHOLD ) ) {
            incrementLatePacketCount( );
        }
    }

    CPTRACE(Trace::TRACEALL, "SecondaryMirror::processMsg() - Processing "
        << (sim ? "simulated" : "unsimulated") << " msg 0x"
        << hex << mid << dec << " for node " << getNode() << ".");
        
    // Dispatch the data to the appropriate message processing routine
    switch(mid) {
        case BLANKING_FRAME_PACKET_1:
            processBlankingFramePacket1(data);
            break;
        case BLANKING_FRAME_PACKET_2:
            processBlankingFramePacket2(data);
            break;
        case BLANKING_FRAME_PACKET_3:
            processBlankingFramePacket3(data);
            break;
        case BLANKING_FRAME_PACKET_4:
            processBlankingFramePacket4(data);
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_1:
            XacDevice::processSystemMonitorPacket1(data, mon_.xac());
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_2:
            XacDevice::processSystemMonitorPacket2(data, mon_.xac());
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_3:
            XacDevice::processSystemMonitorPacket3(data, mon_.xac());
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_4:
            XacDevice::processSystemMonitorPacket4(data, mon_.xac());
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_5:
            XacDevice::processSystemMonitorPacket5(data, mon_.xac());
            break;
        default:
            ostringstream dbg;
            dbg << "SecondaryMirror::processMsg() - Switch matches no case mid "
                << "0x" << hex << mid << dec << ", node " << getNode() << ".";
            programLogDebugIfPossible( dbg.str( ) );
            CPTRACE( Trace::TRACE6, dbg.str( ) );
            break;
    }
}

// -----------------------------------------------------------------------------
carma::canbus::Message SecondaryMirror::simulateMsg(msgType mid)
{
    CPTRACE(Trace::TRACEALL, "SecondaryMirror::simulateMsg() - Simulating msg "
        "0x" << hex << mid << dec << " for node " << getNode() << ".");

    carma::canbus::Message msg;

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
        case BLANKING_FRAME_PACKET_4:
            msg = simBlankingFramePacket4();
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
            // I don't know how to simulate this message!  Just log it.
            ostringstream dbg;
            dbg << "SecondaryMirror::simulateMsg() - Switch matches no case mid"
                << " 0x" << hex << mid << dec << ", node " << getNode() << ".";
            programLogDebugIfPossible( dbg.str( ) );
            CPTRACE( Trace::TRACE6, dbg.str( ) );
            break;
    }
    return msg;
}

// -----------------------------------------------------------------------------
void SecondaryMirror::processBlankingFramePacket1( DataVector & data )
{
    if ( data.size() < 8 )
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "SecondaryMirror::processBlankingFramePacket1 - Size != 8.");

    // Unpack the data
    const short ps5mv = dataToShort(data);
    const short ps12mv = dataToShort(data);
    const short psNeg12mv = dataToShort(data);
    const short ps24mv = dataToShort(data);

    const float voltPerMv = 1.0E-3; 

    // Convert and place into monitor stream.
    mon_.psPos5v().setValue( ps5mv * voltPerMv );
    mon_.psPos12v().setValue( ps12mv * voltPerMv );
    mon_.psNeg12v().setValue( psNeg12mv * voltPerMv );
    mon_.psPos24v().setValue( ps24mv * voltPerMv );
}

// -----------------------------------------------------------------------------
void SecondaryMirror::processBlankingFramePacket2( DataVector & data )
{
    if (data.size() < 8)
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "SecondaryMirror::processBlankingFramePacket2 - Size != 8.");

    // Unpack the data
    const short xPosMicrons = dataToShort( data );
    const short yPosMicrons = dataToShort( data );
    const short zPosMicrons = dataToShort( data );
    const short tempHundredthsC = dataToShort( data );

    const double mmPerMicron = 1.0E-3;
    const double cPerHundredthsC = 1.0E-2;

    // Convert and place into monitor stream...
    comMon_.secondary().focusZ().setValue( zPosMicrons * mmPerMicron ); 

    mon_.xPosition().setValue( xPosMicrons * mmPerMicron ); // um to mm
    mon_.yPosition().setValue( yPosMicrons * mmPerMicron ); // um to mm
    mon_.zPosition().setValue( zPosMicrons * mmPerMicron ); // um to mm
    mon_.modTemp().setValue( tempHundredthsC * cPerHundredthsC ); // Units C
}

// -----------------------------------------------------------------------------
void SecondaryMirror::processBlankingFramePacket3( DataVector & data )
{
    if ( data.size() < 4 )
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "SecondaryMirror::processBlankingFramePacket3 - Size < 4.");

    typedef OvroSubsystem::XMovementStatusMonitorPointEnum::XMOVEMENTSTATUS XSTAT;
    typedef OvroSubsystem::YMovementStatusMonitorPointEnum::YMOVEMENTSTATUS YSTAT;
    typedef OvroSubsystem::ZMovementStatusMonitorPointEnum::ZMOVEMENTSTATUS ZSTAT;

    // Unpack the data
    const XSTAT xStat = static_cast<XSTAT>( dataToUbyte( data ) );
    const YSTAT yStat = static_cast<YSTAT>( dataToUbyte( data ) );
    const ZSTAT zStat = static_cast<ZSTAT>( dataToUbyte( data ) );
    const unsigned char lvdtReset = dataToUbyte( data );

    // Convert and place into monitor stream...
    mon_.xMovementStatus().setValue( xStat );
    mon_.yMovementStatus().setValue( yStat );
    mon_.zMovementStatus().setValue( zStat );
    mon_.lvdtResetting().setValue( static_cast<bool>( lvdtReset ) );

    if ( xStat != OvroSubsystem::XMovementStatusMonitorPointEnum::MOVING ) 
        sharedSeqNo_.markFocusPositionRequestCompleteIfPending( 
            SharedOpticsSeqNo::X, MOVE_WAIT_FRAMES );

    if ( yStat != OvroSubsystem::YMovementStatusMonitorPointEnum::MOVING ) 
        sharedSeqNo_.markFocusPositionRequestCompleteIfPending( 
            SharedOpticsSeqNo::Y, MOVE_WAIT_FRAMES );

    if ( zStat != OvroSubsystem::ZMovementStatusMonitorPointEnum::MOVING ) 
        sharedSeqNo_.markFocusPositionRequestCompleteIfPending( 
            SharedOpticsSeqNo::Z, MOVE_WAIT_FRAMES );
}

// -----------------------------------------------------------------------------
void SecondaryMirror::processBlankingFramePacket4(vector<byteType> &data)
{
    if ( data.size( ) < 8 )
        throw CARMA_EXCEPTION( carma::canbus::BadDataSizeException,
            "SecondaryMirror::processBlankingFramePacket4 - Size < 2." );

    const short adcZeroMv = dataToShort( data );
    const unsigned char pX = dataToUbyte( data );
    const unsigned char mX = dataToUbyte( data );
    const unsigned char pY = dataToUbyte( data );
    const unsigned char mY = dataToUbyte( data );
    const unsigned char pZ = dataToUbyte( data );
    const unsigned char mZ = dataToUbyte( data );

    mon_.adcZero( ).setValue( adcZeroMv / 1.0E3 ); // mV to V
    mon_.xLimitPlus().setValue(
        static_cast<OvroSubsystem::XLimitPlusMonitorPointEnum::XLIMITPLUS>(pX));
    mon_.xLimitMinus().setValue(
        static_cast<OvroSubsystem::XLimitMinusMonitorPointEnum::XLIMITMINUS>(mX));
    mon_.yLimitPlus().setValue(
        static_cast<OvroSubsystem::YLimitPlusMonitorPointEnum::YLIMITPLUS>(pY));
    mon_.yLimitMinus().setValue(
        static_cast<OvroSubsystem::YLimitMinusMonitorPointEnum::YLIMITMINUS>(mY));
    mon_.zLimitPlus().setValue(
        static_cast<OvroSubsystem::ZLimitPlusMonitorPointEnum::ZLIMITPLUS>(pZ));
    mon_.zLimitMinus().setValue(
        static_cast<OvroSubsystem::ZLimitMinusMonitorPointEnum::ZLIMITMINUS>(mZ));
}

// -----------------------------------------------------------------------------
carma::canbus::Message SecondaryMirror::simBlankingFramePacket1()
{
    canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_1 );

    const short ps5mv = 5000;
    const short ps12mv = 12000;
    const short psNeg12mv = -12000;
    const short ps24mv = 24000;
    
    msg << ps5mv << ps12mv << psNeg12mv << ps24mv;

    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message SecondaryMirror::simBlankingFramePacket2()
{
    canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_2 );

    const short xMicrons = 1000;
    const short yMicrons = -1000;
    const short zMicrons = 0;
    const short tempHundredthsC = 5000;

    msg << xMicrons << yMicrons << zMicrons << tempHundredthsC;

    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message SecondaryMirror::simBlankingFramePacket3()
{
    canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_3 );

    const unsigned char xStatus = 0x00; // IDLE
    const unsigned char yStatus = 0x09; // ERROR
    const unsigned char zStatus = 0x0A; // STOP
    const unsigned char lvdtResetting = 0x00; // No

    msg << xStatus << yStatus << zStatus << lvdtResetting;

    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message SecondaryMirror::simBlankingFramePacket4()
{
    canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_4 );

    const short adcZero = 500;
    const unsigned char limit = 0x00;
    const unsigned char notAtLimit = 0x01;

    msg << adcZero << limit << notAtLimit; // X
    msg << notAtLimit << limit;
    msg << limit << limit;

    return msg;
}

// -----------------------------------------------------------------------------
void SecondaryMirror::setXPosition( const float posInMM, const long seqNo )
{
    sharedSeqNo_.setFocusPositionRequestPending( seqNo, 
                                                 SharedOpticsSeqNo::X );
    canbus::Message msg = createMsgToNode( SET_X_POSITION );
    msg << positionToMicrons( posInMM );
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void SecondaryMirror::setYPosition( const float posInMM, const long seqNo )
{
    sharedSeqNo_.setFocusPositionRequestPending( seqNo,
                                                 SharedOpticsSeqNo::Y );
    canbus::Message msg = createMsgToNode( SET_Y_POSITION );
    msg << positionToMicrons( posInMM );
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void SecondaryMirror::setZPosition( const float posInMM, const long seqNo )
{
    sharedSeqNo_.setFocusPositionRequestPending( seqNo, 
                                                 SharedOpticsSeqNo::Z );
    canbus::Message msg = createMsgToNode( SET_Z_POSITION );
    msg << positionToMicrons( posInMM );
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void SecondaryMirror::cycleLvdtPower()
{
    canbus::Message msg = createMsgToNode( LVDT_POWER_CYCLE_RESET );
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void SecondaryMirror::stopMotion()
{
    canbus::Message msg = createMsgToNode( STOP_MOTION );
    io_.postMessage(msg);
}
    
// -----------------------------------------------------------------------------
void SecondaryMirror::doZTracking( const bool ztrack, const long seqNo )
{
    sharedSeqNo_.setFocusZTrackingRequestPending( seqNo );
    ScopedPthreadMutexLock scopelock( shared_->mutex );
    shared_->zTracking = ztrack;
}

