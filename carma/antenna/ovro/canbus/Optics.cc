/** @file
 * Implementation of 10-M Antenna Optics CAN Device.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.23 $
 * $Date: 2012/05/16 00:17:22 $
 * $Id: Optics.cc,v 1.23 2012/05/16 00:17:22 abeard Exp $
 *
 * $CarmaCopyright$
 */

// Carma includes
#include "carma/antenna/ovro/canbus/Optics.h"
#include "carma/canbus/exceptions.h"
#include "carma/canbus/Utilities.h"
#include "carma/monitor/OvroSubsystem.h"
#include "carma/util/Program.h"
#include "carma/util/ScopedPthreadMutexLock.h"
#include "carma/util/Time.h"
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

    typedef AntennaCommon::CalStateMonitorPointEnum CommonCalStateMPE;

    const carma::canbus::apiType API_ID                = 72;
    const string                 API_VERSION           = "D";
    
    // Late packet timeout in ms
    const double                 PACKET_LATE_THRESHOLD = 150.0;
    
    // Control command message ids.
    const carma::canbus::msgType SELECT_RECEIVER       = 0x080;
    const carma::canbus::msgType SET_CAL_POSITION      = 0x081;
    const carma::canbus::msgType SET_MM_RX             = 0x082;
    const carma::canbus::msgType MOVE_TERTIARY         = 0x083;
    const carma::canbus::msgType SET_MM_CAL_POSITION   = 0x084;
    const carma::canbus::msgType SET_CM_CAL_POSITION   = 0x085;
    const carma::canbus::msgType SET_MM_RX_ANGLE       = 0x086;

    // Blanking frame message ids.
    const carma::canbus::msgType BLANKING_FRAME_PACKET_1   = 0x0E0;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_2   = 0x0E1;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_3   = 0x0E2;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_4   = 0x0E3;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_5   = 0x0E4;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_6   = 0x0E5;

    const unsigned int CAL_POS_WAIT_ATLEAST_FRAMES = 2;
    const unsigned int CAL_POS_MAX_WAIT_FRAMES = 20; 
    
} // namespace < unnamed >

// -----------------------------------------------------------------------------
Optics::Optics( nodeType node, 
                CanOutput & io,
                OvroSubsystem & ovroSubsys ) :
    XacDevice(API_ID, node, io),
    log_(Program::getLogger()),
    common_( ovroSubsys.antennaCommon()),
    mon_( ovroSubsys.optics( ) ),
    currentSequenceNo_( 0 ),
    lastInstantaneousCalState_( OvroCalStateMPE::UNKNOWN )
{
    CPTRACE(Trace::TRACE6, "Optics::Optics() - Device "
        "class created for api " << API_ID << " node " << node);

    calPosRequest_.requestPending = false;
    calPosRequest_.requestPendingFrames = 0;
    calPosRequest_.pendingSequenceNo = 0;
    calPosRequest_.sequenceNoFromRx = false;
}

// -----------------------------------------------------------------------------
Optics::~Optics()
{
    CPTRACE(Trace::TRACE6, "Optics::~Optics() - Device "
        "class destroyed for api " << API_ID << " node " << getNode());
}

// -----------------------------------------------------------------------------
map<msgType, string> Optics::getHalfSecMonitors() const
{
    static map<msgType, string> tmp;
    static bool init = false;

    if (!init) {
        tmp[BLANKING_FRAME_PACKET_1] = 
            "Optics::BLANKING_FRAME_PACKET_1";
        tmp[BLANKING_FRAME_PACKET_2] = 
            "Optics::BLANKING_FRAME_PACKET_2";
        tmp[BLANKING_FRAME_PACKET_3] = 
            "Optics::BLANKING_FRAME_PACKET_3";
        tmp[BLANKING_FRAME_PACKET_4] = 
            "Optics::BLANKING_FRAME_PACKET_4";
        tmp[BLANKING_FRAME_PACKET_5] = 
            "Optics::BLANKING_FRAME_PACKET_5";
        tmp[BLANKING_FRAME_PACKET_6] = 
            "Optics::BLANKING_FRAME_PACKET_6";
        init = true;
    }
    return tmp;
}

// -----------------------------------------------------------------------------
map<msgType, string> Optics::getSlowMonitors() const
{
    return XacDevice::getSlowMonitors();
}

// -----------------------------------------------------------------------------
void Optics::updateFrameData()
{
    // This method gets called once every half second FOR EACH INSTANCE OF
    // THIS DEVICE REGISTERED WITH MASTER! Use it to update information 
    // pertinent to the frame time but not updated every message.
    
    // Set the state of the device...
    mon_.state().setValue( 
        static_cast<StateMonitorPointEnum::STATE>(getState()));

}

// -----------------------------------------------------------------------------
void Optics::processMsg(msgType mid, vector<byteType> &data, bool sim)
{
    // If state is ONLINE, check if the packet is late...
    if (getState() == ONLINE) {
        if (isPacketLate(PACKET_LATE_THRESHOLD)) {
            incrementLatePacketCount();
        }
    }

    CPTRACE(Trace::TRACEALL, "Optics::processMsg() - Processing "
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
        case BLANKING_FRAME_PACKET_5:
            processBlankingFramePacket5(data);
            break;
        case BLANKING_FRAME_PACKET_6:
            processBlankingFramePacket6(data);
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
            break; // Drop it on the floor.
    }
}

// -----------------------------------------------------------------------------
carma::canbus::Message Optics::simulateMsg(msgType mid)
{
    carma::canbus::Message msg;
    CPTRACE(Trace::TRACEALL, "Optics::simulateMsg() - Simulating msg "
        "0x" << hex << mid << dec << " for node " << getNode() << ".");
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
        case BLANKING_FRAME_PACKET_5:
            msg = simBlankingFramePacket5();
            break;
        case BLANKING_FRAME_PACKET_6:
            msg = simBlankingFramePacket6();
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
            // Not a problem, just log it.
            log_ << Priority::DEBUG << "Optics::simulateMsg - "
                << "Switch does not match any case: mid " << mid;
            CPTRACE(Trace::TRACE6, "Optics::simulateMsg() - "
                "Switch doesn't match any case.  mid 0x" << hex  << mid
                << dec << " node " << getNode() << ".");
            break;
    }
    return msg;
}

// -----------------------------------------------------------------------------
void Optics::processBlankingFramePacket1(vector<byteType> &data)
{
    if ( data.size() != 8 )
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "Optics::processBlankingFramePacket1 - Size != 8.");

    const unsigned char rxSelState = dataToUbyte( data );
    const unsigned char instantaneousCalState = dataToUbyte( data );
    const short ambLoadTemp = dataToShort( data );
    const short hotLoadTemp = dataToShort( data );
    const unsigned short hotLoadCurrent = dataToShort( data );

    const OvroCalStateMPE::CALSTATE instantaneousCalStateMp = 
        static_cast< OvroCalStateMPE::CALSTATE >( instantaneousCalState );

    typedef OvroSubsystem::RxSelectStateMonitorPointEnum::RXSELECTSTATE 
    RxSelectStateSPE;

    mon_.rxSelectState().setValue( static_cast<RxSelectStateSPE>( rxSelState) );
    mon_.calState().setValue( instantaneousCalStateMp ); 
    mon_.ambLoadTemp().setValue( ambLoadTemp / 100.0 ); // 0.01 C
    mon_.hotLoadTemp().setValue( hotLoadTemp / 100.0 ); // 0.01 C
    mon_.hotLoadCurrent().setValue( hotLoadCurrent / 1000.0 ); // mA
    
    setAntennaCommonCalibratorState( instantaneousCalStateMp ); 

    handlePendingCalibratorPositionRequests( instantaneousCalStateMp );
}

// -----------------------------------------------------------------------------
void Optics::processBlankingFramePacket2(vector<byteType> &data)
{
    if ( data.size() != 8 )
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "Optics::processBlankingFramePacket2 - Size != 8.");

    // Unpack the data
    const unsigned char mmPos = dataToUbyte( data );
    const unsigned char tertPos = dataToUbyte( data );
    const unsigned char mmCalPos = dataToUbyte( data );
    const unsigned char cmCalPos = dataToUbyte( data );
    const short mmLoadTemp = dataToShort( data );
    const short cmLoadTemp = dataToShort( data );

    typedef OvroSubsystem::MmSelectPosMonitorPointEnum::MMSELECTPOS 
        MmSelectPosSPE;
    typedef OvroSubsystem::TertiaryPosMonitorPointEnum::TERTIARYPOS
        TertPosSPE;
    typedef OvroSubsystem::MmCalStateMonitorPointEnum::MMCALSTATE
        MmCalStateSPE;
    typedef OvroSubsystem::CmCalStateMonitorPointEnum::CMCALSTATE 
        CmCalStateSPE;

    mon_.mmSelectPos().setValue( static_cast<MmSelectPosSPE>( mmPos ) );
    mon_.tertiaryPos().setValue( static_cast<TertPosSPE>( tertPos ) );
    mon_.mmCalState().setValue( static_cast<MmCalStateSPE>( mmCalPos ) ); 
    mon_.cmCalState().setValue( static_cast<CmCalStateSPE>( cmCalPos ) );
    mon_.mmLoadTemp().setValue( mmLoadTemp / 100.0 );
    mon_.cmLoadTemp().setValue( cmLoadTemp / 100.0 );
}

// -----------------------------------------------------------------------------
void Optics::processBlankingFramePacket3(vector<byteType> &data)
{
    if ( data.size() != 8 )
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "Optics::processBlankingFramePacket3 - Size != 8.");

    // Unpack the data
    const short ps24v = dataToShort(data);
    const short ps5v = dataToShort(data);
    const short ps12vPos = dataToShort(data);
    const short ps12vNeg = dataToShort(data);
    
    // Convert and place into monitor stream...
    mon_.ps24v().setValue( ps24v / 1000.0 );
    mon_.ps5v().setValue( ps5v / 1000.0 );
    mon_.psPos12v().setValue( ps12vPos / 1000.0 );
    mon_.psNeg12v().setValue( ps12vNeg / 1000.0 );
}

// -----------------------------------------------------------------------------
void Optics::processBlankingFramePacket4(vector<byteType> &data)
{
    if ( data.size() != 8 )
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "Optics::processBlankingFramePacket4 - Size != 8.");
    
    // Unpack the data
    const short modTemp = dataToShort( data );
    const unsigned short mmSelAngle = dataToUshort( data );
    const short refVoltage = dataToShort( data );
    const unsigned char fpgaMaj = dataToUbyte( data );
    const unsigned char fpgaMin = dataToUbyte( data );

    // Convert and place into monitor stream...
    mon_.modTemp().setValue( modTemp / 100.0 );
    mon_.mmSelAngle().setValue( mmSelAngle / 10.0 );
    mon_.refVoltage().setValue( refVoltage / 1000.0 );
    ostringstream oss;
    oss << static_cast<int>( fpgaMaj ) << "." << static_cast<int>( fpgaMin);
    mon_.fpgaVersion().setValue( oss.str() );
}

// -----------------------------------------------------------------------------
void Optics::processBlankingFramePacket5(vector<byteType> &data)
{
    if ( data.size() < 6 )
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "Optics::processBlankingFramePacket5 - Size < 6.");

    const unsigned char reqRx = dataToUbyte( data );
    const short angleOffset = dataToShort( data );
    const short angleError = dataToShort( data );
    const unsigned char dataValid = dataToUbyte( data );

    typedef OvroSubsystem::RequestedRxMonitorPointEnum::REQUESTEDRX
        RequestedRxSPE;

    mon_.requestedRx().setValue( static_cast< RequestedRxSPE >( reqRx ) );
    mon_.angleOffset().setValue( angleOffset / 10.0 );
    mon_.angleError().setValue( angleError / 10.0 );
    mon_.dataValid().setValue( dataValid == 0x01 );
}

// -----------------------------------------------------------------------------
void Optics::processBlankingFramePacket6(vector<byteType> &data)
{
    if ( data.size() < 7 )
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "Optics::processBlankingFramePacket6 - Size < 7.");

    const unsigned char mmCalStat = dataToUbyte( data );
    const unsigned char mmSelStat = dataToUbyte( data );
    const unsigned short setVoltage = dataToUshort( data );
    const unsigned char servoState = dataToUbyte( data );
    const unsigned char cmCalStat = dataToUbyte( data );
    const unsigned char tertStat = dataToUbyte( data );

    // Seriously - Binary?! Ok, if pulling out the API and looking up the real
    // meaning is any easier for you.  
    mon_.mmCalBits().setValue( mmCalStat );
    mon_.mmSelBits().setValue( mmSelStat );
    mon_.setVoltage().setValue( setVoltage / 1000.0 );
    mon_.servoState().setValue( 
        static_cast<OvroSubsystem::ServoStateMonitorPointEnum::SERVOSTATE>( 
            servoState ) );
    mon_.cmCalBits().setValue( cmCalStat );
    mon_.tertBits().setValue( tertStat );
}

// -----------------------------------------------------------------------------
carma::canbus::Message Optics::simBlankingFramePacket1()
{
    const unsigned char rxSelState = 3;
    unsigned char calState;
    const short ambTemp = 3500;
    const short loadTemp = 5000;
    const unsigned short loadCurrent = 1000;

    if ( calPosRequest_.requestPending ) {
    
        if ( calPosRequest_.requestPendingFrames < 2 ) {
            calState = static_cast< unsigned char >( 
                lastInstantaneousCalState_ );
         } else if ( calPosRequest_.requestPendingFrames < 4 ) {
            calState = 1; // Moving
         } else {
            calState = static_cast< unsigned char >( 
                calPosRequest_.requestedPosition );
         }

    } else {
        calState = static_cast< unsigned char >( 
            calPosRequest_.requestedPosition );
    }

    canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_1 );

    msg << rxSelState << calState << ambTemp << loadTemp << loadCurrent;

    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message Optics::simBlankingFramePacket2()
{
    const unsigned char mmSel = 0;
    const unsigned char tertPos = 0;
    const unsigned char mmCalPos = 0;
    const unsigned char cmCalPos = 1;
    const short mmLoadTemp = 5000;
    const short cmLoadTemp = 5010;

    canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_2 );

    msg << mmSel << tertPos << mmCalPos << cmCalPos << mmLoadTemp << cmLoadTemp; 
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message Optics::simBlankingFramePacket3()
{
    const short ps24v = 24000;
    const short ps5v = 5000;
    const short ps12v = 12000;
    const short ps12vNeg = -12000;

    canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_3 );

    msg << ps24v << ps5v << ps12v << ps12vNeg;

    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message Optics::simBlankingFramePacket4()
{
    const short modTemp = 4200;
    const unsigned short mmSelAngle = 450;
    const short refVolt = 2500;
    const unsigned char fpgaMaj = 0;
    const unsigned char fpgaMin = 1;

    canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_4 );

    msg << modTemp << mmSelAngle << refVolt << fpgaMaj << fpgaMin;

    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message Optics::simBlankingFramePacket5()
{
    const unsigned char reqRx = 1;
    const short angleOffset = 40;
    const short angleError = 4;
    const unsigned char dataValid = 0;
    
    canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_5 );

    msg << reqRx << angleOffset << angleError << dataValid;

    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message Optics::simBlankingFramePacket6()
{
    const unsigned char mmCalStat = 1;
    const unsigned char mmSelStat = 2;
    const unsigned short setVoltage = 1500;
    const unsigned char servoState = 1;
    const unsigned char cmCalStat = 4;
    const unsigned char tertStat = ~( mmCalStat | mmSelStat | cmCalStat);

    canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_6 );

    msg << mmCalStat << mmSelStat << setVoltage << servoState 
        << cmCalStat << tertStat;

    return msg;
}

// -----------------------------------------------------------------------------
void Optics::selectReceiver( const enum RECEIVER rx )
{
    canbus::Message msg = this->createMsgToNode( SELECT_RECEIVER );

    msg << static_cast<unsigned char>( rx );

    io_.postMessage( msg );
}

// -----------------------------------------------------------------------------
void Optics::setCalPosition( const enum CAL_POS pos ) 
{
    canbus::Message msg = this->createMsgToNode( SET_CAL_POSITION );

    msg << static_cast<unsigned char>( pos );
    
    io_.postMessage( msg );
}

// -----------------------------------------------------------------------------
void Optics::setCalPosition( const enum CAL_POS pos,
                             const unsigned long sequenceNo,
                             const bool sequenceNoFromRx )
{
    setCalPosition( pos );

    ScopedPthreadMutexLock scopelock( calPosRequest_.mutex );

    switch ( pos ) {
        case Optics::SKY:
            calPosRequest_.requestedPosition = OvroCalStateMPE::SKY;
            break;
        case Optics::AMBIENT:
            calPosRequest_.requestedPosition = OvroCalStateMPE::AMBIENT_LOAD;
            break;
        case Optics::PARTIAL:
            calPosRequest_.requestedPosition = OvroCalStateMPE::PARTIAL;
            break;
        case Optics::HOT:
            calPosRequest_.requestedPosition = OvroCalStateMPE::HOT_LOAD;
            break;
        default:
            throw CARMA_ERROR( "Optics::setCalPosition() - Invalid cal pos." );
    }

    calPosRequest_.requestPending = true;
    calPosRequest_.requestPendingFrames = 0;
    calPosRequest_.pendingSequenceNo = sequenceNo;
    calPosRequest_.sequenceNoFromRx = sequenceNoFromRx;

}

// -----------------------------------------------------------------------------
void Optics::moveMmRxSelect( enum MM_RX_SELECTION mmRx )
{
    canbus::Message msg = this->createMsgToNode( SET_MM_RX );

    msg << static_cast< unsigned char >( mmRx );

    io_.postMessage( msg );
}

// -----------------------------------------------------------------------------
void Optics::moveTertiary( const bool in )
{
    const unsigned char pos = ( in ? 0x01 : 0x00 );

    canbus::Message msg = this->createMsgToNode( MOVE_TERTIARY ); 
    
    msg << pos;

    io_.postMessage( msg );
}

// -----------------------------------------------------------------------------
void Optics::setMmCalPosition( const enum CAL_POS calPos )
{
    canbus::Message msg = this->createMsgToNode( SET_MM_CAL_POSITION );

    msg << static_cast< unsigned char >( calPos );

    io_.postMessage( msg );
}

// -----------------------------------------------------------------------------
void Optics::setCmCalPosition( const enum CAL_POS calPos )
{
    canbus::Message msg = this->createMsgToNode( SET_CM_CAL_POSITION );

    if ( !(calPos == SKY || calPos == AMBIENT) ) 
        throw CARMA_ERROR( "Optics::setCmCalPosition() - Invalid cal pos." );

    msg << static_cast< unsigned char >( calPos );

    io_.postMessage( msg );
}

// -----------------------------------------------------------------------------
void Optics::setMmRxSelectorAngle( const float angleInDegrees )
{
    canbus::Message msg = this->createMsgToNode( SET_MM_RX_ANGLE );

    msg << static_cast< unsigned short >( angleInDegrees * 10.0 );

    io_.postMessage( msg );
}

// -----------------------------------------------------------------------------
void 
Optics::handlePendingCalibratorPositionRequests( 
    const OvroCalStateMPE::CALSTATE instantaneousCalState )
{
    // I deliberated over whether or not to use the whole frame calibration 
    // state for this logic rather than the instantaneous state.  I decided on 
    // the latter because ultimately sequence numbers are used to coordinate
    // control tasks and thus inherently involve multiple frame latency.  Using
    // the whole frame state would just add an additional frame to this 
    // latency.  The only case where this could bite us is if somebody is using
    // the sequence number in the monitor system to signal a valid data state.
    ScopedPthreadMutexLock scopelock( calPosRequest_.mutex );

    if ( calPosRequest_.requestPending ) 
    {

        ++calPosRequest_.requestPendingFrames;

        if (calPosRequest_.requestPendingFrames >= CAL_POS_WAIT_ATLEAST_FRAMES)
        {

            if ( instantaneousCalState == calPosRequest_.requestedPosition ||
                 calPosRequest_.requestPendingFrames >= CAL_POS_MAX_WAIT_FRAMES)
            { 

                if ( calPosRequest_.sequenceNoFromRx ) {
                    common_.receivers().tuneSeqNum().setValue( 
                            calPosRequest_.pendingSequenceNo );
                } else {
                    currentSequenceNo_ = calPosRequest_.pendingSequenceNo;
                }

                calPosRequest_.requestPending = false;
                calPosRequest_.requestPendingFrames = 0;
                calPosRequest_.sequenceNoFromRx = false;
            }
        }  
    }


    // Always set the calibrator sequence number.
    common_.calibrator().calSeqNum().setValue( currentSequenceNo_ );
}

// -----------------------------------------------------------------------------
void Optics::setAntennaCommonCalibratorState(
    const OvroCalStateMPE::CALSTATE instantaneousCalState )
{
    // We intervene here and make a correction for the fact that the cal state 
    // reported by the can module is an instantaneous value rather than a 
    // value which reflects the best state for the entire frame. 
    CommonCalStateMPE::CALSTATE commonCalStateMp;

    switch ( instantaneousCalState ) {
        case OvroCalStateMPE::UNKNOWN:
            commonCalStateMp = CommonCalStateMPE::ERROR;
            break;
        case OvroCalStateMPE::MOVING:
            commonCalStateMp = CommonCalStateMPE::MOVING;
            break;
        case OvroCalStateMPE::SKY:
            commonCalStateMp = ( 
                lastInstantaneousCalState_ == instantaneousCalState ? 
                CommonCalStateMPE::SKY : CommonCalStateMPE::MOVING );
            break;
        case OvroCalStateMPE::AMBIENT_LOAD:
            commonCalStateMp = ( 
                lastInstantaneousCalState_ == instantaneousCalState ? 
                CommonCalStateMPE::AMB : CommonCalStateMPE::MOVING );
            break;
        case OvroCalStateMPE::PARTIAL:
            commonCalStateMp = ( 
                lastInstantaneousCalState_ == instantaneousCalState ? 
                CommonCalStateMPE::REFLEC : CommonCalStateMPE::MOVING );
            break;
        case OvroCalStateMPE::HOT_LOAD:
            commonCalStateMp = ( 
                lastInstantaneousCalState_ == instantaneousCalState ? 
                CommonCalStateMPE::FIXED : CommonCalStateMPE::MOVING );
            break;
        case OvroCalStateMPE::STUCK:
            commonCalStateMp = CommonCalStateMPE::ERROR;
        default:
            commonCalStateMp = CommonCalStateMPE::ERROR;
    }

    common_.calibrator().calState().setValue( commonCalStateMp );

    // Determine if the cal state is acquired.
    // Note this is false until we receive a command.
    const bool calStateAcquired = ( 
            lastInstantaneousCalState_ == instantaneousCalState &&
            instantaneousCalState == calPosRequest_.requestedPosition );

    common_.calibrator().calStateAcquired().setValue( calStateAcquired );
    
    lastInstantaneousCalState_ = instantaneousCalState;
}

