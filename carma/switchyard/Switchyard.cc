#include "carma/switchyard/Switchyard.h"

#include "carma/canbus/Utilities.h"
#include "carma/monitor/CanbusCommon.h"
#include "carma/monitor/SwitchyardCommon.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/programLogging.h"

#include <boost/foreach.hpp>
#include <sstream>

using namespace carma;
using namespace carma::canbus;
using namespace carma::switchyard;
using namespace carma::util;
using namespace std;

namespace {

    const apiType API_ID = 12;
    
    const double PACKET_LATE_THRESHOLD                   = 150.0;

    const carma::canbus::msgType BLANKING_FRAME_PACKET_1 = 0x0E0;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_2 = 0x0E1;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_3 = 0x0E2;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_4 = 0x0E3;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_5 = 0x0E4;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_6 = 0x0E5;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_7 = 0x0E6;

    const carma::canbus::msgType SET_SWITCH_POS_START    = 0x081;
    const carma::canbus::msgType SET_SWITCH_POS_END      = 0x085;

} // namespace < unnamed >


Switchyard::Switchyard( const carma::canbus::nodeType node,
                        carma::canbus::CanOutput & io,
                        carma::monitor::StateMonitorPointEnum & state,
                        carma::monitor::Switchyard & switchyard,
                        carma::monitor::Xac & xac ) : 
    XacDevice( API_ID, node, io ),
    state_( state ),
    switchyard_( switchyard ),
    xac_( xac )
{

}

Switchyard::~Switchyard( )
{

}

void
Switchyard::setSwitches( const SwitchMap & switchPosMap )
{
    {
        ostringstream info;
        info << "Switchyard::setSwitches( [ ";
        BOOST_FOREACH( const SwitchMap::value_type val, switchPosMap ) {
            info << "(" << val.first << "," << val.second << ") ";
        }
        info << " ] ).";

        programLogInfoIfPossible( info.str() );
    }

    // Form up a map of msg ids and switch positions.
    const unsigned short switchMin = 1;
    const unsigned short switchMax = 38;
    const unsigned short posMax = 4;

    typedef map< msgType, DataVector > OutMsgMap;
    OutMsgMap outMsgMap;
    DataVector unchanged( 8 );

    BOOST_FOREACH( const SwitchMap::value_type value, switchPosMap ) {
        const unsigned short switchId = value.first;
        const unsigned short switchPos = value.second;

        if ( switchId < switchMin || switchId > switchMax )
            throw CARMA_EXCEPTION( IllegalArgumentException,
                                   "Invalid switch id." );

        if ( switchPos > posMax ) 
            throw CARMA_EXCEPTION( IllegalArgumentException,
                                   "Invalid switch position." );

        const msgType msgId = SET_SWITCH_POS_START + ( switchId - 1 ) / 8; 

        if ( outMsgMap.find( msgId ) == outMsgMap.end() ) {
            outMsgMap[msgId] = unchanged;
        }

        outMsgMap[ msgId ].at( ( switchId - 1 ) % 8 ) = switchPos; 
    }

    const canbus::Message dummyMsg = createDummyMsg();

    // Now form and send the messages in the outMsgMap.
    BOOST_FOREACH( const OutMsgMap::value_type value, outMsgMap ) {
        carma::canbus::Message msg = createMsgToNode( value.first );

        BOOST_FOREACH( const carma::canbus::byteType byte, value.second ) {
            msg << byte;
        }

        io_.postMessage( msg );
        io_.postMessage( dummyMsg );
        io_.postMessage( dummyMsg );
    }
}

MsgBriefMap
Switchyard::getHalfSecMonitors( ) const
{
    static MsgBriefMap mids;

    if ( mids.empty() ) {
        mids[BLANKING_FRAME_PACKET_1] = "BFP1";
        mids[BLANKING_FRAME_PACKET_2] = "BFP2";
        mids[BLANKING_FRAME_PACKET_3] = "BFP3";
        mids[BLANKING_FRAME_PACKET_4] = "BFP4";
        mids[BLANKING_FRAME_PACKET_5] = "BFP5";
        mids[BLANKING_FRAME_PACKET_6] = "BFP6";
        mids[BLANKING_FRAME_PACKET_7] = "BFP7";
    }

    return mids;
}

MsgBriefMap
Switchyard::getSlowMonitors( ) const
{
    return XacDevice::getSlowMonitors( );
}

void
Switchyard::processMsg( const msgType mid,
                        DataVector & data,
                        const bool sim )
{
    if ( getState() == ONLINE ) {
        if ( isPacketLate( PACKET_LATE_THRESHOLD ) ) {
            incrementLatePacketCount( );
        }
    }

    switch ( mid ) {  
        case BLANKING_FRAME_PACKET_1:
        case BLANKING_FRAME_PACKET_2:
        case BLANKING_FRAME_PACKET_3:
        case BLANKING_FRAME_PACKET_4:
        case BLANKING_FRAME_PACKET_5:
            processSwitchPosPacket( mid, data );
            break;
        case BLANKING_FRAME_PACKET_6:
            processBlankingFramePacket6( data );
            break;
        case BLANKING_FRAME_PACKET_7:
            processBlankingFramePacket7( data );
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_1:
        case XacDevice::SYSTEM_MONITOR_PACKET_2:
        case XacDevice::SYSTEM_MONITOR_PACKET_3:
        case XacDevice::SYSTEM_MONITOR_PACKET_4:
        case XacDevice::SYSTEM_MONITOR_PACKET_5:
            XacDevice::processSystemMonitorPacket( mid, data, xac_ );
            break;
        default:
            break;
    }
}

canbus::Message
Switchyard::simulateMsg( msgType mid ) 
{
    switch ( mid ) {  
        case BLANKING_FRAME_PACKET_1:
        case BLANKING_FRAME_PACKET_2:
        case BLANKING_FRAME_PACKET_3:
        case BLANKING_FRAME_PACKET_4:
        case BLANKING_FRAME_PACKET_5:
            return simulateSwitchPosPacket( mid );
        case BLANKING_FRAME_PACKET_6:
            return simulateBlankingFramePacket6( );
        case BLANKING_FRAME_PACKET_7:
            return simulateBlankingFramePacket7( );
        case XacDevice::SYSTEM_MONITOR_PACKET_1:
        case XacDevice::SYSTEM_MONITOR_PACKET_2:
        case XacDevice::SYSTEM_MONITOR_PACKET_3:
        case XacDevice::SYSTEM_MONITOR_PACKET_4:
        case XacDevice::SYSTEM_MONITOR_PACKET_5:
            return simSystemMonitorPacket( mid );
        default:
            throw CARMA_EXCEPTION( IllegalArgumentException, "Invalid mid." );
    }
}

void
Switchyard::updateFrameData( )
{
    state_.setValue(
        static_cast< carma::monitor::StateMonitorPointEnum::STATE >(
            getState( ) ) );

}

apiType 
Switchyard::getApiId( ) 
{
    return API_ID;
}

void
Switchyard::processSwitchPosPacket( const msgType mid, 
                                    carma::canbus::DataVector & data )
{
    const msgType bank = mid - BLANKING_FRAME_PACKET_1;

    const short maxSwitches = ( ( bank == 4 ) ? 6 : 8 );

    for ( short i = 0; i < maxSwitches; ++i ) {
        const short switchIdx = ( bank * 8 ) + i;
        const short switchPos = dataToUbyte( data );
        switchyard_.switchPosition( switchIdx ).setValue( switchPos );
    }
}

void
Switchyard::processBlankingFramePacket6( carma::canbus::DataVector & data )
{
    const float ps24v = dataToShort( data ) / 1000.0;
    const float ps5vd = dataToShort( data ) / 1000.0;
    const float ps3vd = dataToShort( data ) / 1000.0;
    const float ps5va = dataToShort( data ) / 1000.0;

    switchyard_.ps24v().setValue( ps24v );
    switchyard_.ps5vDigital().setValue( ps5vd );
    switchyard_.ps3_3v().setValue( ps3vd );
    switchyard_.ps5vAnalog().setValue( ps5va );
}

void
Switchyard::processBlankingFramePacket7( carma::canbus::DataVector & data )
{
    const float temp = dataToShort( data ) / 100.0;
    const unsigned short fpgaMaj = dataToUbyte( data );
    const unsigned short fpgaMin = dataToUbyte( data );

    ostringstream fpgaVersion;
    fpgaVersion << fpgaMaj << "." << fpgaMin;

    switchyard_.moduleTemp().setValue( temp );
    switchyard_.fpgaVersion().setValue( fpgaVersion.str() );
}

carma::canbus::Message
Switchyard::simulateSwitchPosPacket( const msgType mid ) 
{
    carma::canbus::Message msg = createMsgToHost( mid );

    const msgType bank = mid - BLANKING_FRAME_PACKET_1;

    const short maxSwitches = ( ( bank == 4 ) ? 6 : 8 );

    for ( short i = 0; i < maxSwitches; ++i ) {
        const unsigned char pos = (bank * 8) + (i + 1);
        msg << pos;
    }

    return msg;
}

carma::canbus::Message
Switchyard::simulateBlankingFramePacket6( ) 
{
    carma::canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_6 );

    const short ps24v = 24100;
    const short ps5vd = 5200;
    const short ps3_3v = 3310;
    const short ps5va = 4900;

    msg << ps24v << ps5vd << ps3_3v << ps5va;

    return msg;
}

carma::canbus::Message
Switchyard::simulateBlankingFramePacket7( ) 
{
    carma::canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_7 );

    const short temp = 3000;
    const unsigned char maj = 0;
    const unsigned char min = 0;

    msg << temp << maj << min;

    return msg;
}
