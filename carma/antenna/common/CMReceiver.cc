//$Id: CMReceiver.cc,v 1.7 2013/01/23 19:05:04 abeard Exp $
#include "carma/antenna/common/CMReceiver.h"

#include "carma/canbus/exceptions.h"
#include "carma/canbus/Utilities.h"
#include "carma/monitor/RxBias.h"
#include "carma/monitor/CanbusCommon.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/Trace.h"

#include <iomanip>
#include <sstream>
#include <vector>

using namespace std;
using namespace carma;
using namespace carma::canbus;
using namespace carma::antenna;
using namespace carma::antenna::common;
using namespace carma::canbus::devices;
using namespace carma::util;

namespace { // Anonymous namespace for local constants and typedefs
    
  // API Id for this device.
  const carma::canbus::apiType API_ID                      = 176;

  // API verstion this class was implemented from 
  const char API_VERSION                                   = 'D';

  // Late packet timeout in ms
  const double PACKET_LATE_THRESHOLD                       = 150.0;

  // Control command message ids
  const carma::canbus::msgType SET_30GHZ_DRAIN_VOLTAGE     = 0x081;
  const carma::canbus::msgType SET_30GHZ_DRAIN_CURRENT     = 0x082;
  const carma::canbus::msgType SET_30GHZ_IF_DRAIN_CURRENT  = 0x083;
  const carma::canbus::msgType SET_90GHZ_DRAIN_VOLTAGE     = 0x084;
  const carma::canbus::msgType SET_90GHZ_GATE_VOLTAGE      = 0x085;
  const carma::canbus::msgType SET_90GHZ_IF_DRAIN_VOLTAGE  = 0x086;
  const carma::canbus::msgType SET_90GHZ_IF_GATE_VOLTAGE   = 0x087;
  
  // Blanking frame message ids.
  const carma::canbus::msgType BLANKING_FRAME_PACKET_1     = 0x0E0;
  const carma::canbus::msgType BLANKING_FRAME_PACKET_2     = 0x0E1;
  const carma::canbus::msgType BLANKING_FRAME_PACKET_3     = 0x0E2;
  const carma::canbus::msgType BLANKING_FRAME_PACKET_4     = 0x0E3;
  const carma::canbus::msgType BLANKING_FRAME_PACKET_5     = 0x0E4;
  const carma::canbus::msgType BLANKING_FRAME_PACKET_6     = 0x0E5;
  const carma::canbus::msgType BLANKING_FRAME_PACKET_7     = 0x0E6;
  const carma::canbus::msgType BLANKING_FRAME_PACKET_8     = 0x0E7;
  const carma::canbus::msgType BLANKING_FRAME_PACKET_9     = 0x0E8;
  const carma::canbus::msgType BLANKING_FRAME_PACKET_10    = 0x0E9;
  const carma::canbus::msgType BLANKING_FRAME_PACKET_11    = 0x0EA;

  // Fast sampling message ids
  const carma::canbus::msgType FAST_FRAME_CH1_SIZE         = 0x110;
  const carma::canbus::msgType FAST_FRAME_CH2_SIZE         = 0x111;
  const carma::canbus::msgType FAST_FRAME_CH1              = 0x112;
  const carma::canbus::msgType FAST_FRAME_CH2              = 0x113;
  

  // Engineering response message ids.

  // 0 no output, 1 serial, 2 canbus, 3 both
  const carma::canbus::msgType SELECT_DIAG_OUTPUT          = 0x3f8; 

  // diagnostic log output packet type
  const carma::canbus::msgType LOG_MESSAGE          = 0x3ff; 

} // End anonymous namespace


CMReceiver::CMReceiver( carma::canbus::nodeType node,
                        carma::canbus::CanOutput &io,
                        carma::monitor::RxBias & rxMon,
                        carma::monitor::OvroSubsystem::RxBiasTemps * temps ) :

  XacDevice( API_ID, node, io ),
  rxMon_( rxMon ),
  ovroRxBiasTemps_( temps ),
  bimaRxBiasTemps_( 0 )
{
    // Nothing
}

CMReceiver::CMReceiver( carma::canbus::nodeType node,
                        carma::canbus::CanOutput &io,
                        carma::monitor::RxBias & rxMon,
                        carma::monitor::BimaSubsystem::RxBiasTemps * temps ) :

  XacDevice( API_ID, node, io ),
  rxMon_( rxMon ),
  ovroRxBiasTemps_( 0 ),
  bimaRxBiasTemps_( temps )
{
    // Nothing
}
CMReceiver::~CMReceiver() 
{
    // Nothing
}

MsgBriefMap 
CMReceiver::getSlowMonitors() const
{
    return XacDevice::getSlowMonitors( );
}

void 
CMReceiver::updateFrameData( )
{
    rxMon_.state( ).setValue(
        static_cast<carma::monitor::StateMonitorPointEnum::STATE>(
                getState( ) ) );
}

void 
CMReceiver::processMsg(msgType mid, DataVector &data, bool sim)
{
  // If state is ONLINE, check if the packet is late...
  if (getState() == ONLINE) {
    if (isPacketLate(PACKET_LATE_THRESHOLD)) {
      incrementLatePacketCount();
    }
  }
  
  CPTRACE(Trace::TRACE7, "CMReceiver::processMsg() - Processing "
	  << (sim ? "simulated" : "unsimulated") << " msg 0x"
	  << hex << mid << dec << " for node " << getNode() << ".");
  
  // Dispatch the data to the appropriate message processing routine
  switch( mid ) {
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
  case BLANKING_FRAME_PACKET_6:
    processBlankingFramePacket6( data );
    break;
  case BLANKING_FRAME_PACKET_7:
    processBlankingFramePacket7( data );
    break;
  case BLANKING_FRAME_PACKET_8:
    processBlankingFramePacket8( data );
    break;
  case BLANKING_FRAME_PACKET_9:
    processBlankingFramePacket9( data );
    break;
  case BLANKING_FRAME_PACKET_10:
    processBlankingFramePacket10( data );
    break;
  case BLANKING_FRAME_PACKET_11:
    processBlankingFramePacket11( data );
    break;
  case XacDevice::SYSTEM_MONITOR_PACKET_1:
  case XacDevice::SYSTEM_MONITOR_PACKET_2:
  case XacDevice::SYSTEM_MONITOR_PACKET_3:
  case XacDevice::SYSTEM_MONITOR_PACKET_4:
  case XacDevice::SYSTEM_MONITOR_PACKET_5:
    XacDevice::processSystemMonitorPacket( mid, data, rxMon_.xac( ) );
    break;
  default:
    CARMA_CPTRACE(Trace::TRACE6, "AntennaIF::processMsg() - "
	    "Switch doesn't match any case.  mid 0x" << hex  << mid
	    << dec << " node " << getNode() << ".");
    break;
  }
}

void 
CMReceiver::processBlankingFramePacket1( DataVector & data ) 
{
    const short boardTemp = dataToShort( data );                 // 0.01 C
    rxMon_.boardTemperature().setValue( boardTemp/100.0 );       // C

    const short neg15Analog = dataToShort( data );               // mV
    rxMon_.neg15VAnalogVoltage().setValue( neg15Analog/1000.0 ); // V

    const short pos5vAnalog = dataToShort( data );               // mV
    rxMon_.pos5VAnalogVoltage().setValue( pos5vAnalog/1000.0 );  // V

    const short neg5Dig = dataToShort( data );                   // mV
    rxMon_.neg5VDigitalVoltage().setValue( neg5Dig/1000.0 );     // V
}

void 
CMReceiver::processBlankingFramePacket2( DataVector & data )
{
    const short pos15Analog = dataToShort( data );                  // mV
    rxMon_.pos15VAnalogVoltage( ).setValue( pos15Analog / 1000.0 ); // V

    const short pos5Dig = dataToShort( data );                      // mV
    rxMon_.pos5VDigitalVoltage( ).setValue( pos5Dig / 1000.0 );     // V

    const short pos24Analog = dataToShort( data );                  // mV
    rxMon_.pos24VAnalogVoltage( ).setValue( pos24Analog / 1000.0 ); // V
}

void 
CMReceiver::processBlankingFramePacket3( DataVector & data )
{
    const short drainCurrent1 = dataToShort( data );               // 0.01 mA
    rxMon_.drainCurrent30GHz(0).setValue( drainCurrent1 / 100.0 ); // mA

    const short gateVoltage = dataToShort( data );                 // mV
    rxMon_.gateVoltage30GHz(0).setValue( gateVoltage / 1000.0 );   // V

    const short gateCurrent = dataToShort( data );                 // 0.01 mA
    rxMon_.gateCurrent30GHz(0).setValue( gateCurrent / 100.0 );    // mA

    const short drainCurrent2 = dataToShort( data );               // 0.01 mA
    rxMon_.drainCurrent30GHz(1).setValue( drainCurrent2 / 100.0 ); // mA
}

void 
CMReceiver::processBlankingFramePacket4( DataVector & data )
{
    const short gateVoltage1 = dataToShort( data );               // mV
    rxMon_.gateVoltage30GHz(1).setValue( gateVoltage1 / 1000.0 ); // V

    const short gateCurrent = dataToShort( data );                // 0.01 mA
    rxMon_.gateCurrent30GHz(1).setValue( gateCurrent / 100.0 );   // mA

    const short drainCurrent = dataToShort( data );               // 0.01 mA
    rxMon_.drainCurrent30GHz(2).setValue( drainCurrent / 100.0 ); // mA

    const short gateVoltage2 = dataToShort( data );               // mV
    rxMon_.gateVoltage30GHz(2).setValue( gateVoltage2 / 1000.0 ); // V
}

void 
CMReceiver::processBlankingFramePacket5( DataVector & data )
{
    const short gateCurrent1 = dataToShort( data );               // 0.01 mA
    rxMon_.gateCurrent30GHz(2).setValue( gateCurrent1 / 100.0 );  // mA

    const short drainCurrent = dataToShort( data );               // 0.01 mA
    rxMon_.drainCurrent30GHz(3).setValue( drainCurrent / 100.0 ); // mA

    const short gateVoltage = dataToShort( data );                // mV
    rxMon_.gateVoltage30GHz(3).setValue( gateVoltage / 1000.0 );  // V

    const short gateCurrent2 = dataToShort( data );               // 0.01 mA
    rxMon_.gateCurrent30GHz(3).setValue( gateCurrent2 / 100.0 );  // mA
}

void 
CMReceiver::processBlankingFramePacket6( DataVector & data )
{
    const short ifAmpVolt = dataToShort( data );               // mV
    rxMon_.ifAmpVoltage30GHz().setValue( ifAmpVolt / 1000.0 ); // V

    const short ifAmpCurr = dataToShort( data );               // 0.01 mA
    rxMon_.ifAmpCurrent30GHz().setValue( ifAmpCurr / 100.0 );  // mA

    const short mixerCurr = dataToShort( data );               // 0.01 mA
    rxMon_.mixerCurrent30GHz().setValue( mixerCurr / 100.0 );  // mA

    const short ledCurr = dataToShort( data );                 // 0.01 mA
    rxMon_.ledCurrent30GHz().setValue( ledCurr / 100.0 );      // mA
}

void 
CMReceiver::processBlankingFramePacket7( DataVector & data )
{
    for ( int s = 0; s < 4; ++s ) {
        const short gateCurrent90GHz = dataToShort( data ); // 0.01 mA
        rxMon_.gateCurrent90GHz( s ).setValue( gateCurrent90GHz / 100.0 ); // mA
    }
}

void
CMReceiver::processBlankingFramePacket8( DataVector & data )
{
    for ( int s = 0; s < 2; ++s ) {
        const short drainCurrent90GHz = dataToShort( data ); // 0.01 mA
        rxMon_.drainCurrent90GHz( s ).setValue( drainCurrent90GHz/100.0 ); //mA
    }
    
    const short ifAmpDrainCurrent90GHz = dataToShort( data ); // 0.01 mA
    rxMon_.ifAmpDrainCurrent90GHz( ).setValue( ifAmpDrainCurrent90GHz / 100.0 );

    const short ifAmpGateCurrent90GHz = dataToShort( data ); // 0.01 mA
    rxMon_.ifAmpGateCurrent90GHz( ).setValue( ifAmpGateCurrent90GHz / 100.0 );
}

void 
CMReceiver::processBlankingFramePacket9( DataVector & data )
{
    const short temp1 = dataToShort( data );          // 0.01 K
    const short temp2 = dataToShort( data );          // 0.01 K
    const short temp3 = dataToShort( data );          // 0.01 K
    const short temp4 = dataToShort( data );          // 0.01 K

    if ( ovroRxBiasTemps_ ) {
        ovroRxBiasTemps_->tempFirstStage().setValue( temp1 / 100.0 ); // K
        ovroRxBiasTemps_->tempSecondStage().setValue( temp2 / 100.0 ); // K
    }

    if ( bimaRxBiasTemps_ ) {
        bimaRxBiasTemps_->tempHemtStage().setValue( temp1 / 100.0 ); // K
        bimaRxBiasTemps_->tempSecondStage().setValue( temp2 / 100.0 ); // K
        bimaRxBiasTemps_->shieldTemp().setValue( temp3 / 100.0 ); // K
        bimaRxBiasTemps_->hornTemp().setValue( temp4 / 100.0 ); // K
    }
}

void 
CMReceiver::processBlankingFramePacket10( DataVector & data )
{
    for( int s = 0; s < 4; ++s ) {
        const short setVoltage = dataToShort( data );                   // mV 
        rxMon_.drainSetVoltage30GHz(s).setValue( setVoltage / 1000.0 ); // V
    }
}

void
CMReceiver::processBlankingFramePacket11( DataVector & data )
{
    for( int s = 0; s < 2; ++s ) {
        const short setVoltage = dataToShort( data );                   // mV
        rxMon_.drainSetVoltage90GHz(s).setValue( setVoltage / 1000.0 ); // V
    }
}

MsgBriefMap
CMReceiver::getHalfSecMonitors() const 
{
    static MsgBriefMap tmp; 

    if ( tmp.empty() ) {
        tmp[BLANKING_FRAME_PACKET_1] = "CMReceiver::BLANKING_FRAME_PACKET_1";
        tmp[BLANKING_FRAME_PACKET_2] = "CMReceiver::BLANKING_FRAME_PACKET_2";
        tmp[BLANKING_FRAME_PACKET_3] = "CMReceiver::BLANKING_FRAME_PACKET_3";
        tmp[BLANKING_FRAME_PACKET_4] = "CMReceiver::BLANKING_FRAME_PACKET_4";
        tmp[BLANKING_FRAME_PACKET_5] = "CMReceiver::BLANKING_FRAME_PACKET_5";
        tmp[BLANKING_FRAME_PACKET_6] = "CMReceiver::BLANKING_FRAME_PACKET_6";
        tmp[BLANKING_FRAME_PACKET_7] = "CMReceiver::BLANKING_FRAME_PACKET_7";
        tmp[BLANKING_FRAME_PACKET_8] = "CMReceiver::BLANKING_FRAME_PACKET_8";
        tmp[BLANKING_FRAME_PACKET_9] = "CMReceiver::BLANKING_FRAME_PACKET_9";
        tmp[BLANKING_FRAME_PACKET_10] = "CMReceiver::BLANKING_FRAME_PACKET_10";
        tmp[BLANKING_FRAME_PACKET_11] = "CMReceiver::BLANKING_FRAME_PACKET_11";
    }

    return tmp; 
}

carma::canbus::Message CMReceiver::simulateMsg( msgType mid )
{
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
        case BLANKING_FRAME_PACKET_6:
            msg = simBlankingFramePacket6();
            break;
        case BLANKING_FRAME_PACKET_7:
            msg = simBlankingFramePacket7();
            break;
        case BLANKING_FRAME_PACKET_8:
            msg = simBlankingFramePacket8();
            break;
        case BLANKING_FRAME_PACKET_9:
            msg = simBlankingFramePacket9();
            break;
        case BLANKING_FRAME_PACKET_10:
            msg = simBlankingFramePacket10();
            break;
        case BLANKING_FRAME_PACKET_11:
            msg = simBlankingFramePacket11();
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_1:
        case XacDevice::SYSTEM_MONITOR_PACKET_2:
        case XacDevice::SYSTEM_MONITOR_PACKET_3:
        case XacDevice::SYSTEM_MONITOR_PACKET_4:
        case XacDevice::SYSTEM_MONITOR_PACKET_5:
            msg = XacDevice::simSystemMonitorPacket( mid );
            break;
        default:
            ostringstream oss;
            oss << "CMReceiver::simulateMsg() - Switch doesn't match any "
                << "case (mid=0x" << hex << mid << dec << " node " 
                << getNode() << ".";
            throw CARMA_ERROR( oss.str() );
            break;
    }
    return msg;
}

carma::canbus::Message 
CMReceiver::simBlankingFramePacket1( )
{
    canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_1 );

    DataVector data;
    sShortToData( data, 5000 );   // 50C
    sShortToData( data, -14900 ); // -14.9 V
    sShortToData( data, 0 );      // 0.0 V
    sShortToData( data, -5000 );  // -5.0 V
    msg.setData( data );
    return msg;
}

carma::canbus::Message 
CMReceiver::simBlankingFramePacket2( )
{
    canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_2 );

    static short voltage = 0;
    voltage += 1;

    DataVector data;
    sShortToData( data, voltage );  // in volts
    sShortToData( data, 5200 );     // 5.2 V
    sShortToData( data, 23900 );    // 23.9 V
    msg.setData( data );
    return msg;
}

carma::canbus::Message 
CMReceiver::simBlankingFramePacket3( )
{
    canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_3 );

    DataVector data;
    sShortToData( data,10 );    // 0.1 mA
    sShortToData( data,1200 );  // 1.2 V
    sShortToData( data,20 );    // 0.2 mA
    sShortToData( data,30 );    // 0.3 mA
    msg.setData( data );
    return msg;
}
carma::canbus::Message 
CMReceiver::simBlankingFramePacket4( )
{
    canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_4 );

    DataVector data;
    sShortToData( data,1500 );   // 1.5 V
    sShortToData( data,50 );     // 0.5 mA
    sShortToData( data,50 );     // 0.5 mA
    sShortToData( data,1900 );   // 1.9 V
    msg.setData( data );
    return msg;
}

carma::canbus::Message 
CMReceiver::simBlankingFramePacket5( )
{
    canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_5 );

    DataVector data;
    sShortToData( data,50 );      // 0.5 mA
    sShortToData( data,10 );      // 0.1 mA
    sShortToData( data,2000 );    // 2.0 V
    sShortToData( data,30 );      // 0.3 mA
    msg.setData( data );
    return msg;
}

carma::canbus::Message 
CMReceiver::simBlankingFramePacket6( )
{
    canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_6 );

    DataVector data;
    sShortToData( data,1300 );    // 1.3 V
    sShortToData( data,50 );      // 0.5 mA 
    sShortToData( data,120 );     // 1.2 mA
    sShortToData( data,190 );     // 1.9 mA
    msg.setData( data );
    return msg;
}

carma::canbus::Message 
CMReceiver::simBlankingFramePacket7( )
{
    carma::canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_7 ); 

    DataVector data;
    sShortToData( data, 110 ); // 1.1 mA
    sShortToData( data, 120 ); // 1.2 mA
    sShortToData( data, 130 ); // 1.3 mA
    sShortToData( data, 140 ); // 1.4 mA
    msg.setData( data );
    return msg;
}

carma::canbus::Message 
CMReceiver::simBlankingFramePacket8( )
{
    carma::canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_8 ); 

    DataVector data;
    sShortToData( data, 101 ); // 1.01 mA
    sShortToData( data, 102 ); // 1.02 mA
    sShortToData( data, 210 ); // 2.1 mA
    sShortToData( data, 220 ); // 2.2 mA
    msg.setData( data );
    return msg;
}

carma::canbus::Message 
CMReceiver::simBlankingFramePacket9( )
{
    carma::canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_9 ); 

    DataVector data;
    sShortToData( data,500 );      // 5 K
    sShortToData( data,1000 );     // 10 K
    sShortToData( data,4000 );     // 40 K
    sShortToData( data,700 );      // 7 K
    msg.setData( data );
    return msg;
}

carma::canbus::Message 
CMReceiver::simBlankingFramePacket10( )
{
    carma::canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_10 ); 

    DataVector data;
    uShortToData( data,1100 );    // 1.1 V
    uShortToData( data,1200 );    // 1.2 V
    uShortToData( data,1300 );    // 1.3 V
    uShortToData( data,1400 );    // 1.4 V
    msg.setData( data );
    return msg;
}

carma::canbus::Message 
CMReceiver::simBlankingFramePacket11( )
{
    carma::canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_11 ); 

    DataVector data;
    uShortToData( data, 1900 ); // 1.9 V
    uShortToData( data, 2900 ); // 2.9 V
    msg.setData( data );
    return msg;
}

// *************************************************************************
// This message is intentionally sent to a bogus nodeID so that its
// only effect is to create a temporal buffer between messages that
// are going out of the output queue
void
CMReceiver::sendGuardMessage()
{
  const int BOGUS_NODE = 125;
  canbus::Message msg = createMsgToChan(BOGUS_NODE, SET_30GHZ_IF_DRAIN_CURRENT);
  const unsigned long bogus1 = 0XDeadBeef;
  msg << bogus1;
  io_.postMessage(msg);
}

canbus::Message CMReceiver::createMsgToChan(int chanNo, canbus::msgType msgID)
{
    canbus::idType id =
        createId(false, API_ID, chanNo, msgID);
    canbus::Message msg(id, ALL_BUSSES);
    return msg;
}

void 
CMReceiver::set30GHzDrainVoltage( const unsigned short stage, 
                                  const float volts )
{
    canbus::Message msg = createMsgToNode( SET_30GHZ_DRAIN_VOLTAGE );
    msg << static_cast< unsigned char >( stage ) 
        << static_cast< unsigned short >( volts * 1000.0 );
    io_.postMessage( msg );

    const canbus::Message dummy = createDummyMsg();
    io_.postMessage( dummy );
    io_.postMessage( dummy );
}

void 
CMReceiver::set30GHzDrainCurrent( const unsigned short stage, 
                                  const float milliAmps ) 
{
    canbus::Message msg = createMsgToNode( SET_30GHZ_DRAIN_CURRENT );
    msg << static_cast< unsigned char >( stage ) 
        << static_cast< unsigned short >( milliAmps * 100.0 );
    io_.postMessage( msg );
    
    const canbus::Message dummy = createDummyMsg();
    io_.postMessage( dummy );
    io_.postMessage( dummy );
}

void 
CMReceiver::set30GHzIFDrainCurrent( const float milliAmps )
{
    canbus::Message msg = createMsgToNode( SET_30GHZ_IF_DRAIN_CURRENT );
    msg << static_cast< unsigned short >( milliAmps * 100.0 );
    io_.postMessage( msg );
    
    const canbus::Message dummy = createDummyMsg();
    io_.postMessage( dummy );
    io_.postMessage( dummy );
}

void 
CMReceiver::set90GHzDrainVoltage( const unsigned short amplifier,  
                                  const float volts )
{
    canbus::Message msg = createMsgToNode( SET_90GHZ_DRAIN_VOLTAGE );
    msg << static_cast< unsigned char >( amplifier )
        << static_cast< unsigned short >( volts * 1000.0 );
    io_.postMessage( msg );
}

void 
CMReceiver::set90GHzGateVoltage( const unsigned short amplifier, 
                                 const unsigned short stage,
                                 const float volts )
{
    canbus::Message msg = createMsgToNode( SET_90GHZ_GATE_VOLTAGE );
    msg << static_cast< unsigned char >( amplifier )
        << static_cast< unsigned char >( stage )
        << static_cast< unsigned short >( volts * 1000.0 );
    io_.postMessage( msg );
}

void 
CMReceiver::set90GHzIFDrainVoltage( const float volts )
{
    canbus::Message msg = createMsgToNode( SET_90GHZ_IF_DRAIN_VOLTAGE );
    msg << static_cast< unsigned short >( volts * 1000.0 );
    io_.postMessage( msg );
}

void 
CMReceiver::set90GHzIFGateVoltage( const float volts )
{
    canbus::Message msg = createMsgToNode( SET_90GHZ_IF_GATE_VOLTAGE );
    msg << static_cast< unsigned short >( volts * 1000.0 );
    io_.postMessage( msg );
}
