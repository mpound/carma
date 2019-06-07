/** @file
 * Implementation of Antenna IF CAN Device.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.14 $
 * $Date: 2011/01/03 18:48:04 $
 * $Id: AntennaIF.cc,v 1.14 2011/01/03 18:48:04 iws Exp $
 *
 * $CarmaCopyright$
 */

// Carma includes
#include "carma/antenna/common/AntennaIF.h"
#include "carma/canbus/exceptions.h"
#include "carma/canbus/Utilities.h"
#include "carma/monitor/AntennaIF.h"
#include "carma/monitor/CanbusCommon.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"

// Carma Tools includes
#include <log4cpp/Category.hh>
#include <log4cpp/Priority.hh>

// C++ Standard Library includes
#include <iomanip>

using namespace carma::antenna::common;
using namespace carma::canbus;
using namespace carma::canbus::devices;
using namespace carma::util;
using namespace log4cpp;
using namespace std;

namespace { // Anonymous namespace for local constants and typedefs
    
    // API Id for this device.
    const carma::canbus::apiType API_ID                      = 224;

    // API verstion this class was implemented from 
    const char API_VERSION                                   = 'D';

    // Late packet timeout in ms
    const double PACKET_LATE_THRESHOLD                       = 150.0;

    // Control command message ids
    const carma::canbus::msgType STOP_FAST_CH1               = 0x002; 
    const carma::canbus::msgType STOP_FAST_CH2               = 0x003; 
    const carma::canbus::msgType START_FAST_CH1              = 0x004; 
    const carma::canbus::msgType START_FAST_CH2              = 0x005; 

    const carma::canbus::msgType SELECT_BAND                 = 0x080; 
    const carma::canbus::msgType SET_IF_TOTAL_ATTEN          = 0x081;
    const carma::canbus::msgType SET_IF_LEVEL                = 0x082;
    const carma::canbus::msgType SET_INPUT_ATTENUATOR        = 0x083;
    const carma::canbus::msgType SET_OUTPUT_ATTENUATOR       = 0x084;
    const carma::canbus::msgType QUERY_TOTAL_POWER           = 0x085;
    const carma::canbus::msgType GOTO_PRESET_OUTPUT_POWER    = 0x086;

    // Blanking frame message ids.
    const carma::canbus::msgType BLANKING_FRAME_PACKET_1     = 0x0E0;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_2     = 0x0E1;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_3     = 0x0E2;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_4     = 0x0E3;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_5     = 0x0E4;

    // Fast sampling message ids
    const carma::canbus::msgType FAST_FRAME_CH1_SIZE         = 0x110;
    const carma::canbus::msgType FAST_FRAME_CH2_SIZE         = 0x111;
    const carma::canbus::msgType FAST_FRAME_CH1              = 0x112;
    const carma::canbus::msgType FAST_FRAME_CH2              = 0x113;


    // Engineering response message ids.
    const carma::canbus::msgType PAM_STATUS_ON_CHANGE        = 0x130;
    const carma::canbus::msgType SWITCH_STATUS_ON_CHANGE     = 0x131;
    const carma::canbus::msgType TOTAL_POWER_RESPONSE        = 0x170;
    const carma::canbus::msgType FAST_TOTAL_POWER_PACKET     = 0x171;

    // 0 no output, 1 serial, 2 canbus, 3 both
    const carma::canbus::msgType SELECT_DIAG_OUTPUT          = 0x3f8; 

    // diagnostic log output packet type
    const carma::canbus::msgType LOG_MESSAGE          = 0x3ff; 


} // End anonymous namespace

// -----------------------------------------------------------------------------
AntennaIF::AntennaIF(
    nodeType node, 
    CanOutput &io, 
    carma::monitor::StateMonitorPointEnum & state,
    carma::monitor::AntennaIF &ifMon,
    carma::monitor::Xac &xacMon)
    : 
    XacDevice(API_ID, node, io),
    log_(Program::getLogger()),
    state_(state),
    ifMon_(ifMon),
    xacMon_(xacMon)
{
    CPTRACE(Trace::TRACE5, "common::AntennaIF::AntennaIF() - Device "
        "class created for api " << API_ID << " node " << node);
}

// -----------------------------------------------------------------------------
AntennaIF::~AntennaIF()
{
    CPTRACE(Trace::TRACE5, "AntennaIF::~AntennaIF() - Device "
            "class destroyed for api " << API_ID << " node " << node_);
}

// -----------------------------------------------------------------------------
MsgBriefMap AntennaIF::getHalfSecMonitors() const
{
    static MsgBriefMap tmp;
    static bool init = false;

    if (!init) {
        tmp[BLANKING_FRAME_PACKET_1] =
            "AntennaIF::BLANKING_FRAME_PACKET_1";
        tmp[BLANKING_FRAME_PACKET_2] =
            "AntennaIF::BLANKING_FRAME_PACKET_2";
        tmp[BLANKING_FRAME_PACKET_3] =
            "AntennaIF::BLANKING_FRAME_PACKET_3";
        tmp[BLANKING_FRAME_PACKET_4] =
            "AntennaIF::BLANKING_FRAME_PACKET_4";
        tmp[BLANKING_FRAME_PACKET_5] =
            "AntennaIF::BLANKING_FRAME_PACKET_5";
        init = true;
    }
    return tmp;
}

// -----------------------------------------------------------------------------
MsgBriefMap AntennaIF::getSlowMonitors() const
{
    return XacDevice::getSlowMonitors();
}

// -----------------------------------------------------------------------------
void AntennaIF::updateFrameData()
{
    state_.setValue(
        static_cast<carma::monitor::StateMonitorPointEnum::STATE>(
                getState( ) ) );
}

// -----------------------------------------------------------------------------
void AntennaIF::processMsg(msgType mid, vector<byteType> &data, bool sim)
{
    // If state is ONLINE, check if the packet is late...
    if (getState() == ONLINE) {
        if (isPacketLate(PACKET_LATE_THRESHOLD)) {
            incrementLatePacketCount();
        }
    }

    CPTRACE(Trace::TRACE7, "common::AntennaIF::processMsg() - Processing "
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
        case XacDevice::SYSTEM_MONITOR_PACKET_1:
            XacDevice::processSystemMonitorPacket1(data, xacMon_);
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_2:
            XacDevice::processSystemMonitorPacket2(data, xacMon_);
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_3:
            XacDevice::processSystemMonitorPacket3(data, xacMon_);
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_4:
            XacDevice::processSystemMonitorPacket4(data, xacMon_);
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_5:
            XacDevice::processSystemMonitorPacket5(data, xacMon_);
            break;
        case PAM_STATUS_ON_CHANGE:
            processPAMStatusOnChange(data);
            break;
        case SWITCH_STATUS_ON_CHANGE:
            processSwitchStatusOnChange(data);
            break;
        case TOTAL_POWER_RESPONSE:
            processTotalPowerResponse(data);
            break;
	case LOG_MESSAGE:
	    processLogMsg(data);
	    break;
	case FAST_FRAME_CH1:
	    processFastChannel1Packet(data);
	    break;
	case FAST_FRAME_CH2:
	    processFastChannel2Packet(data);
	    break;
        case FAST_TOTAL_POWER_PACKET:
            processFastTotalPowerPacket(data);
            break;
        default:
            // I don't know how to process this message id!
            // Not a problem, just log it.
            log_ << Priority::DEBUG << "AntennaIF::processMsg() - "
                << "Switch does not match any case: Unknown mid "
                << mid << ". Node " << getNode();
            CPTRACE(Trace::TRACE6, "AntennaIF::processMsg() - "
                    "Switch doesn't match any case.  mid 0x" << hex  << mid
                    << dec << " node " << getNode() << ".");
            break;
    }
}

// -----------------------------------------------------------------------------
carma::canbus::Message AntennaIF::simulateMsg(msgType mid)
{
    carma::canbus::Message msg;
    CPTRACE(Trace::TRACE7, "AntennaIF::simulateMsg() - Simulating msg "
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
            log_ << Priority::DEBUG << "AntennaIF::simulateMsg - "
                << "Switch does not match any case: mid " << mid;
            CPTRACE(Trace::TRACE6, "AntennaIF::simulateMsg() - "
                    "Switch doesn't match any case.  mid 0x" << hex  << mid
                    << dec << " node " << getNode() << ".");
            break;
    }
    return msg;
}

// -----------------------------------------------------------------------------
void AntennaIF::processLogMsg(vector<byteType> &data)
{
  static char *msg = (char *)NULL;
  static int pos = 0;
  char dataChar;

  if ( msg == (char *)NULL )
  {
    msg = (char *)malloc(81);
    pos = 0;
  }

  if (data.size() != 8)
    throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
	"AntennaIF::processLogMsg - Data size != 8!");

  for ( int i = 0; i < 8; i++ )
  {
    dataChar = dataToUbyte( data );

    if ( dataChar == '\1' )
    {
      msg[pos] = '\0';
      pos = 0;
      break;
    }

    if ( pos < 78 )
    {

      if ( isprint(dataChar) )
	msg[pos++] = dataChar;

      msg[pos] = '\0';
    }
    else
    {
      msg[79] = '\0';
      pos = 0;
    }
  }

  ifMon_.ifLog().setValue( msg );

}

// -----------------------------------------------------------------------------
void AntennaIF::processBlankingFramePacket1(vector<byteType> &data)
{
    float ifoutTotPower, pamTemp;
    
    if (data.size() != 8)
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "AntennaIF::processBlankingFramePacket1 - Data size != 8!");

    // Unpack the data
    ifoutTotPower = dataToFloat(data);
    pamTemp = dataToFloat(data);
    
    // Convert and stuff into monitor stream.
    ifMon_.ifOutTotalPower().setValue(ifoutTotPower);
    ifMon_.pamTemp().setValue(pamTemp);

    blankingFrameHook1( ifoutTotPower, pamTemp );
}

// -----------------------------------------------------------------------------
void AntennaIF::processBlankingFramePacket2(vector<byteType> &data)
{
    float attenSet;
    unsigned char pamStat, ifSwitchStat, laserStat, nErrors;
    
    if (data.size() != 8)
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "AntennaIF::processBlankingFramePacket2 - Data size != 8!");

    // Unpack the data
    attenSet = dataToFloat(data);
    pamStat = dataToUbyte(data);
    ifSwitchStat = dataToUbyte(data);
    laserStat = dataToUbyte(data);
    nErrors = dataToUbyte(data);

    // Convert and stuff into monitor stream.
    ifMon_.attenSet().setValue(attenSet);
    ifMon_.pamStat().setValue(
        static_cast<carma::monitor::AntennaIF::PamStatMonitorPointEnum::PAMSTAT>(pamStat));
    ifMon_.ifSwitchStat().setValue(
        static_cast<carma::monitor::AntennaIF::IfSwitchStatMonitorPointEnum::IFSWITCHSTAT>(ifSwitchStat));
    ifMon_.laserStat().setValue(
        static_cast<carma::monitor::AntennaIF::LaserStatMonitorPointEnum::LASERSTAT>(laserStat));
    ifMon_.errorCount().setValue(nErrors);

    blankingFrameHook2( attenSet, pamStat, ifSwitchStat, laserStat, nErrors );
}

// -----------------------------------------------------------------------------
void AntennaIF::processBlankingFramePacket3(vector<byteType> &data)
{
    float laserOpticalPow, laserTemp;
    
    if (data.size() != 8)
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "AntennaIF::processBlankingFramePacket3 - Data size != 8!");

    // Unpack the data
    laserOpticalPow = dataToFloat(data);
    laserTemp = dataToFloat(data);
    
    // Convert and place into monitor stream.
    ifMon_.laserOpticalPower().setValue(laserOpticalPow);
    ifMon_.laserTemp().setValue(laserTemp);

    blankingFrameHook3( laserOpticalPow, laserTemp );
}

// -----------------------------------------------------------------------------
void AntennaIF::processBlankingFramePacket4(vector<byteType> &data)
{
    float inputAttenSet, outputAttenSet;

    if (data.size() != 8)
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "AntennaIF::processBlankingFramePacket4 - Data size != 8!");

    // Unpack the data
    inputAttenSet = dataToFloat(data);
    outputAttenSet = dataToFloat(data);

    // Convert and place into monitor stream.
    ifMon_.setInputAtten().setValue(inputAttenSet);
    ifMon_.setOutputAtten().setValue(outputAttenSet);

    blankingFrameHook4(inputAttenSet, outputAttenSet);
}

// -----------------------------------------------------------------------------
void AntennaIF::processBlankingFramePacket5(vector<byteType> &data)
{
    ostringstream topHalf;
    ostringstream bottomHalf;
    
    if (data.size() != 8)
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "AntennaIF::processBlankingFramePacket5 - Data size != 8!");
    
    topHalf << "0x" << hex;
    bottomHalf << "0x" << hex;

    for (unsigned short i = 0; i < 4; i++) {
        // Unpack the top half
        topHalf << setw(2) << setfill('0') 
            << static_cast<int>( dataToUbyte( data ) );
    }

    for (unsigned short i = 0; i < 4; i++) {
        // Unpack the bottom half
        bottomHalf << setw(2) << setfill('0')
            << static_cast<int>( dataToUbyte( data ) );
    }

    ifMon_.laserIdTop().setValue( topHalf.str() );
    ifMon_.laserIdBot().setValue( bottomHalf.str() );

    // no hook required...
}

// -----------------------------------------------------------------------------
carma::canbus::Message AntennaIF::simBlankingFramePacket1()
{
    static float totalPower = 0.000;
    totalPower += 0.001;
    carma::canbus::Message msg(
        createId(true, getApi(), getNode(), BLANKING_FRAME_PACKET_1), 
        getBusId());  // Addressed to host.
    vector<byteType> data;
    floatToData(data, totalPower); // 50mW
    floatToData(data, 50.0); // 50C
    msg.setData(data);
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message AntennaIF::simBlankingFramePacket2()
{
    carma::canbus::Message msg(
        createId(true, getApi(), getNode(), BLANKING_FRAME_PACKET_2), 
        getBusId());
    vector<byteType> data;
    floatToData(data, 1.0);
    uByteToData(data, 0x05); // PAM temp out of range, atten changed last frame.
    uByteToData(data, 0x02); // Cycle through. 
    uByteToData(data, 0x03); // Laser out of temp reg, optical power ! in range.
    uByteToData(data, 100); // Lots of errors!
    msg.setData(data);
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message AntennaIF::simBlankingFramePacket3()
{
    carma::canbus::Message msg(
        createId(true, getApi(), getNode(), BLANKING_FRAME_PACKET_3), 
        getBusId());
    vector<byteType> data;
    floatToData(data, 0.001); // 0.001 V
    floatToData(data, 5.002); // 5.002 V - Nominal should be very small.
    msg.setData(data);
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message AntennaIF::simBlankingFramePacket4()
{
    carma::canbus::Message msg(
        createId(true, getApi(), getNode(), BLANKING_FRAME_PACKET_4), 
        getBusId());
    vector<byteType> data;
    floatToData(data, 0.00); // 0 db
    floatToData(data, 0.00); // 0 db
    msg.setData(data);
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message AntennaIF::simBlankingFramePacket5()
{
    carma::canbus::Message msg(
        createId(true, getApi(), getNode(), BLANKING_FRAME_PACKET_5), 
        getBusId());
    vector<byteType> data;
    uByteToData(data, 0x00);
    uByteToData(data, 0x02);
    uByteToData(data, 0x03);
    uByteToData(data, 0xa3);
    uByteToData(data, 0xab);
    uByteToData(data, 0xcd);
    uByteToData(data, 0xef);
    uByteToData(data, 0xf0);
    msg.setData(data);
    return msg;
}

// -----------------------------------------------------------------------------
void AntennaIF::selectBand( unsigned short band )
{
    carma::canbus::Message msg = createMsgToNode( SELECT_BAND );
    msg << static_cast<unsigned char>(band); 
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void AntennaIF::setIFtotalAttenuation(float atten)
{
    carma::canbus::Message msg(createId(false, getApi(), getNode(),
                SET_IF_TOTAL_ATTEN), getBusId());
    vector<byteType> data;
    floatToData(data, atten);
    msg.setData(data);
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void AntennaIF::setIFlevel(float pow)
{
    carma::canbus::Message msg(createId(false, getApi(), getNode(),
                SET_IF_LEVEL), getBusId());
    vector<byteType> data;
    floatToData(data, pow);
    msg.setData(data);
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void AntennaIF::setInputIFattenuator(float inputAtten)
{
    carma::canbus::Message msg(createId(false, getApi(), getNode(),
                SET_INPUT_ATTENUATOR), getBusId());
    vector<byteType> data;
    floatToData(data, inputAtten);
    msg.setData(data);
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void AntennaIF::setOutputIFattenuator(float outputAtten)
{
    carma::canbus::Message msg(createId(false, getApi(), getNode(),
                SET_OUTPUT_ATTENUATOR), getBusId());
    vector<byteType> data;
    floatToData(data, outputAtten);
    msg.setData(data);
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void AntennaIF::queryTotalPower()
{
    carma::canbus::Message msg(createId(false, getApi(), getNode(),
                QUERY_TOTAL_POWER), getBusId());
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void AntennaIF::setOutputPowerToPreset()
{
    carma::canbus::Message msg(createId(false, getApi(), getNode(),
                GOTO_PRESET_OUTPUT_POWER), getBusId());
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void AntennaIF::simTotalPower( const unsigned int nsamps )
{
    // No-op
}

// -----------------------------------------------------------------------------
void AntennaIF::blankingFrameHook1( float ifoutTotPower, float pamTemp )
{
    // No-op
}

// -----------------------------------------------------------------------------
void AntennaIF::blankingFrameHook2( float attenSet, 
    unsigned char pamStat, unsigned char ifSwitchStat, unsigned char laserStat,
    unsigned char nErrors)
{
    // No-op
}

// -----------------------------------------------------------------------------
void AntennaIF::blankingFrameHook3( float laserOpticalPow, float laserTemp )
{
    // No-op
}

// -----------------------------------------------------------------------------
void AntennaIF::blankingFrameHook4( float inputAttenSet, float outputAttenSet )
{
    // No-op
}
