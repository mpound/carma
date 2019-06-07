/** @file
 * Definition of 10-m Electronics Temperature Controller class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.4 $
 * $Date: 2011/01/03 18:48:06 $
 * $Id: RxTemperatures.cc,v 1.4 2011/01/03 18:48:06 iws Exp $
 * 
 * $CarmaCopyright$
 */

// Carma includes
#include "carma/antenna/ovro/canbus/RxTemperatures.h"
#include "carma/canbus/exceptions.h"
#include "carma/canbus/Utilities.h"
#include "carma/monitor/OvroSubsystem.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"

// Carma Tools includes
#include <log4cpp/Category.hh>
#include <log4cpp/Priority.hh>

using namespace carma::antenna::ovro;
using namespace carma::canbus;
using namespace carma::canbus::devices;
using namespace carma::monitor;
using namespace carma::util;
using namespace log4cpp;
using namespace std;

const carma::canbus::apiType RxTemperatures::API_ID;
const char RxTemperatures::API_VERSION;
const double RxTemperatures::PACKET_LATE_THRESHOLD;
const msgType RxTemperatures::SET_TEMPERATURE;
const msgType RxTemperatures::REGULATE_TEMPERATURE;
const msgType RxTemperatures::SET_LOOP_GAIN;
const msgType RxTemperatures::SET_LOOP_INTEGRATION_CONSTANT;
const msgType RxTemperatures::SET_LOOP_RATE_CONSTANT;
const msgType RxTemperatures::SET_LOOP_BANDWIDTH;
const msgType RxTemperatures::LOOP2_COMMAND_OFFSET;
const msgType RxTemperatures::BLANKING_FRAME_PACKET_1;
const msgType RxTemperatures::BLANKING_FRAME_PACKET_2;
const msgType RxTemperatures::BLANKING_FRAME_PACKET_3;
const msgType RxTemperatures::BLANKING_FRAME_PACKET_4;
const msgType RxTemperatures::BLANKING_FRAME_PACKET_5;
const msgType RxTemperatures::BLANKING_FRAME_PACKET_6;
const msgType RxTemperatures::BLANKING_FRAME_PACKET_7;

// -----------------------------------------------------------------------------
RxTemperatures::RxTemperatures( nodeType node, 
                                CanOutput & io,
                                OvroSubsystem & ovroSubsys ) :
	XacDevice( API_ID, node, io ),
    log_( Program::getLogger( ) ),
    mon_( ovroSubsys.rxThermalControl( ) )
{
    CPTRACE(Trace::TRACE6, "RxTemperatures() - C'tor - Device "
            "class created for api " << API_ID << " node " << node);
}

// -----------------------------------------------------------------------------
RxTemperatures::~RxTemperatures( )
{
    CPTRACE(Trace::TRACE6, "~RxTemperatures() - D'tor Device "
            "class destroyed for api " << API_ID << " node " << getNode());
}

// -----------------------------------------------------------------------------
map<msgType, string> RxTemperatures::getHalfSecMonitors( ) const
{
	static map< msgType, string > tmp;
	static bool init = false;

	if (!init) {
		// Fill the map
        tmp[BLANKING_FRAME_PACKET_1] =
            "RxTemperatures::BLANKING_FRAME_PACKET_1";
        tmp[BLANKING_FRAME_PACKET_2] =
            "RxTemperatures::BLANKING_FRAME_PACKET_2";
        tmp[BLANKING_FRAME_PACKET_3] =
            "RxTemperatures::BLANKING_FRAME_PACKET_3";
        tmp[BLANKING_FRAME_PACKET_4] =
            "RxTemperatures::BLANKING_FRAME_PACKET_4";
        tmp[BLANKING_FRAME_PACKET_5] =
            "RxTemperatures::BLANKING_FRAME_PACKET_5";
        tmp[BLANKING_FRAME_PACKET_6] =
            "RxTemperatures::BLANKING_FRAME_PACKET_6";
        tmp[BLANKING_FRAME_PACKET_7] =
            "RxTemperatures::BLANKING_FRAME_PACKET_7";
		init = true;
	}
	return tmp;
}

// -----------------------------------------------------------------------------
map<msgType, string> RxTemperatures::getSlowMonitors( ) const
{
    return XacDevice::getSlowMonitors();
}

// -----------------------------------------------------------------------------
void RxTemperatures::updateFrameData()
{
    // Set the state of the device...
    mon_.state().setValue( 
        static_cast< StateMonitorPointEnum::STATE >( getState( ) ) );
}

// -----------------------------------------------------------------------------
void RxTemperatures::processMsg( 
    msgType mid, vector< byteType > & data, bool sim )
{
    // If state is ONLINE, check if the packet is late...
    if (getState() == ONLINE) {
        if (isPacketLate(PACKET_LATE_THRESHOLD)) {
            incrementLatePacketCount();
        }
    }
    
    CPTRACE(Trace::TRACEALL, "RxTemperatures::processMsg() - Processing "
            << (sim ? "simulated" : "unsimulated") << " msg 0x"
            << hex << mid << dec << " for node " << getNode() << ".");

    // Dispatch the data to the appropriate message processing routine
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
        case BLANKING_FRAME_PACKET_4: 
            processBlankingFramePacket4(data);
            break;
        case BLANKING_FRAME_PACKET_5: 
            processBlankingFramePacket5(data);
            break;
        case BLANKING_FRAME_PACKET_6: 
            processBlankingFramePacket6(data);
            break;
        case BLANKING_FRAME_PACKET_7: 
            processBlankingFramePacket7(data);
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
            // Unknown message id...
            log_ << Priority::DEBUG << "RxTemperatures::processMsg() - "
                << "Switch does not match any case: Unknown msg id " 
                << mid << ". Node " << getNode();
            CPTRACE(Trace::TRACE6, "RxTemperatures::processMsg() - "
                "Switch doesn't match any case. Msg id 0x" << hex << mid
                << dec << " node " << getNode() << ".");
            break;
    }
}

// -----------------------------------------------------------------------------
carma::canbus::Message RxTemperatures::simulateMsg( msgType mid )
{
    carma::canbus::Message msg;
    CPTRACE(Trace::TRACEALL, "RxTemperatures() - Simulating msg "
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
        case BLANKING_FRAME_PACKET_7:
            msg = simBlankingFramePacket7();
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
            log_ << Priority::DEBUG << "RxTemperatures::simulateMsg "
                << "- Switch does not match any case: mid " << mid;
            CPTRACE(Trace::TRACE6, "RxTemperatures::simulateMsg() - "
                    "Switch doesn't match any case.  mid 0x" << hex  << mid
                    << dec << " node " << getNode() << ".");
            break;
    }
    return msg;
}

// -----------------------------------------------------------------------------
void RxTemperatures::setTemperature( enum LoopId loop, float temp )
{
    msgType msgid = SET_TEMPERATURE + 
        (static_cast<msgType>(loop) * LOOP2_COMMAND_OFFSET);
    carma::canbus::Message msg(
        createId(false, getApi(), getNode(), msgid), 
        getBusId());
    vector<byteType> data;
    
    floatToData(data, temp); 

    msg.setData(data);
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void RxTemperatures::regulateTemperature( 
    enum LoopId loop, 
    enum OpMode mode,
    float pwr )
{
    msgType msgid = REGULATE_TEMPERATURE + 
        (static_cast<msgType>(loop) * LOOP2_COMMAND_OFFSET);
    carma::canbus::Message msg(
        createId(false, getApi(), getNode(), msgid), 
        getBusId());
    vector<byteType> data;

    uByteToData(data, static_cast<unsigned char>(mode));
    floatToData(data, pwr);

    msg.setData(data);
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void RxTemperatures::setLoopGain( enum LoopId loop, float gain )
{
    msgType msgid = SET_LOOP_GAIN + 
        (static_cast<msgType>(loop) * LOOP2_COMMAND_OFFSET);
    carma::canbus::Message msg(
        createId(false, getApi(), getNode(), msgid), 
        getBusId());
    vector<byteType> data;

    floatToData(data, gain);

    msg.setData(data);
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void RxTemperatures::setLoopIntegrationConstant( 
    enum LoopId loop, float integration )
{
    msgType msgid = SET_LOOP_INTEGRATION_CONSTANT + 
        (static_cast<msgType>(loop) * LOOP2_COMMAND_OFFSET);
    carma::canbus::Message msg(
        createId(false, getApi(), getNode(), msgid), 
        getBusId());
    vector<byteType> data;

    floatToData(data, integration);

    msg.setData(data);
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void RxTemperatures::setLoopRateConstant( 
    enum LoopId loop, float rate )
{
    msgType msgid = SET_LOOP_RATE_CONSTANT + 
        (static_cast<msgType>(loop) * LOOP2_COMMAND_OFFSET);
    carma::canbus::Message msg(
        createId(false, getApi(), getNode(), msgid), 
        getBusId());
    vector<byteType> data;

    floatToData(data, rate);

    msg.setData(data);
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void RxTemperatures::setLoopBandwidth(
    enum LoopId loop, float bandwidth )
{
    msgType msgid = SET_LOOP_BANDWIDTH + 
        (static_cast<msgType>(loop) * LOOP2_COMMAND_OFFSET);
    carma::canbus::Message msg(
        createId(false, getApi(), getNode(), msgid), 
        getBusId());
    vector<byteType> data;

    floatToData(data, bandwidth);

    msg.setData(data);
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void RxTemperatures::writeParametersToEEPROM( )
{
    carma::canbus::Message msg(
        createId(false, getApi(), getNode(), WRITE_PARAMS_TO_EEPROM), 
        getBusId());
    
    // No Data in message.
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void RxTemperatures::processBlankingFramePacket1(
    vector< byteType > & data)
{

    short loop1Temp, loop2Temp, enclAirTemp, modTemp;

    if (data.size() < 8) 
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "RxTemperatures::processBlankingFramePacket1 - Size != 8!");

    // Extract monitor information from raw data.
    loop1Temp = dataToShort(data);
    loop2Temp = dataToShort(data);
    enclAirTemp = dataToShort(data);
    modTemp = dataToShort(data);

    // Scale and write to monitor stream.
    mon_.loopTemp(0).setValue(loop1Temp * 0.01); // 0.01C
    mon_.loopTemp(1).setValue(loop2Temp * 0.01); // 0.01C
    mon_.airTemp().setValue(enclAirTemp * 0.01); // 0.01C
    mon_.moduleTemp().setValue(modTemp * 0.01); // 0.01C
}

// -----------------------------------------------------------------------------
void RxTemperatures::processBlankingFramePacket2(
    vector< byteType > & data)
{
    short heat1V, heat1I, heat2V, heat2I;

    if (data.size() < 8) 
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "RxTemperatures::processBlankingFramePacket2 - Size != 8!");

    // Extract monitor points from raw data.
    heat1V = dataToShort(data);
    heat1I = dataToShort(data);
    heat2V = dataToShort(data);
    heat2I = dataToShort(data);

    // Scale and write to monitor stream.
    mon_.heaterVoltage(0).setValue(heat1V * 0.001); // mV
    mon_.heaterCurrent(0).setValue(heat1I * 0.001); // mA
    mon_.heaterVoltage(1).setValue(heat2V * 0.001); // mV
    mon_.heaterCurrent(1).setValue(heat2I * 0.001); // mA
}

// -----------------------------------------------------------------------------
void RxTemperatures::processBlankingFramePacket3(
    vector< byteType > & data)
{
    unsigned char loop1State;
    short pwr1Fract, temp1Diff, integTemp1Diff;

    if (data.size() < 7) 
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "RxTemperatures::processBlankingFramePacket3 - Size != 7!");

    // Extract monitor points from raw data
    loop1State = dataToUbyte(data);
    pwr1Fract = dataToShort(data);
    temp1Diff = dataToShort(data);
    integTemp1Diff = dataToShort(data);

    // Scale and write to monitor stream.
    mon_.heaterLoopState(0).setValue(
        static_cast
        <CM::OvroSubsystem::HeaterLoopStateMonitorPointEnum::HEATERLOOPSTATE>
        (loop1State)
        );
    mon_.powerFraction(0).setValue(pwr1Fract * 0.01); // 0.01%
    mon_.tempDiff(0).setValue(temp1Diff * 0.01); // 0.01K
    mon_.integratedDiff(0).setValue(integTemp1Diff * 0.01); // 0.01 K s
}

// -----------------------------------------------------------------------------
void RxTemperatures::processBlankingFramePacket4(
    vector< byteType > & data)
{
    short loop1Gain, loop1IntConst, loop1RateConst, loop1Bw;

    if (data.size() < 8) 
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "RxTemperatures::processBlankingFramePacket4 - Size != 8!");

    // Extract monitor points from raw data
    loop1Gain = dataToShort(data);
    loop1IntConst = dataToShort(data);
    loop1RateConst = dataToShort(data);
    loop1Bw = dataToShort(data);

    // Scale and write to monitor stream.
    mon_.heaterLoopGain(0).setValue(loop1Gain *0.1 ); // 0.1 (%max pwr)/K
    mon_.integConstant(0).setValue(loop1IntConst * 0.1); // 0.1 (%max pwr) K/s
    mon_.rateConstant(0).setValue(loop1RateConst * 0.1); // 0.1 *%max pwr) s/K
    mon_.bandwidth(0).setValue(loop1Bw * 0.01); // 0.01 Hz
}

// -----------------------------------------------------------------------------
void RxTemperatures::processBlankingFramePacket5(
    vector< byteType > & data)
{
    unsigned char loop2State;
    short pwr2Fract, temp2Diff, integTemp2Diff;
    
    if (data.size() < 7) 
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "RxTemperatures::processBlankingFramePacket5 - Size != 7!");

    // Extract monitor points from raw data
    loop2State = dataToUbyte(data);
    pwr2Fract = dataToShort(data);
    temp2Diff = dataToShort(data);
    integTemp2Diff = dataToShort(data);

    // Scale and write to monitor stream.
    mon_.heaterLoopState(1).setValue(
        static_cast
        <CM::OvroSubsystem::HeaterLoopStateMonitorPointEnum::HEATERLOOPSTATE>
        (loop2State)
        );
    mon_.powerFraction(1).setValue(pwr2Fract * 0.01); // 0.01%
    mon_.tempDiff(1).setValue(temp2Diff * 0.01); // 0.01K
    mon_.integratedDiff(1).setValue(integTemp2Diff * 0.01); // 0.01 K s
}


// -----------------------------------------------------------------------------
void RxTemperatures::processBlankingFramePacket6(
    vector< byteType > & data)
{
    short loop2Gain, loop2IntConst, loop2RateConst, loop2Bw;

    if (data.size() < 8) 
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "RxTemperatures::processBlankingFramePacket6 - Size != 8!");

    // Extract monitor points from raw data
    loop2Gain = dataToShort(data);
    loop2IntConst = dataToShort(data);
    loop2RateConst = dataToShort(data);
    loop2Bw = dataToShort(data);

    // Scale and write to monitor stream.
    mon_.heaterLoopGain(1).setValue(loop2Gain *0.1 );    // 0.1 (%max pwr)/K
    mon_.integConstant(1).setValue(loop2IntConst * 0.1); // 0.1 (%max pwr) K/s
    mon_.rateConstant(1).setValue(loop2RateConst * 0.1); // 0.1 *%max pwr) s/K
    mon_.bandwidth(1).setValue(loop2Bw * 0.01);          // 0.01 Hz
}

// -----------------------------------------------------------------------------
void RxTemperatures::processBlankingFramePacket7(
    vector< byteType > & data)
{
    short ps24v, psPlus12v, psMinus12v, psPlus5v;
    
    if (data.size() < 8) 
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "RxTemperatures::processBlankingFramePacket7 - Size != 8!");
    
    // Extract monitor points from raw data
    ps24v = dataToShort(data);
    psPlus12v = dataToShort(data);
    psMinus12v = dataToShort(data);
    psPlus5v = dataToShort(data);

    // Scale and write to monitor stream.
    mon_.ps24v().setValue(ps24v * 0.001); // mV
    mon_.psPos12v().setValue(psPlus12v * 0.001); // mV
    mon_.psNeg12v().setValue(psMinus12v * 0.001); // mV
    mon_.ps5v().setValue(psPlus5v * 0.001); // mV
    
}

// -----------------------------------------------------------------------------
carma::canbus::Message RxTemperatures::simBlankingFramePacket1( )
{
    carma::canbus::Message msg(
        createId(true, getApi(), getNode(), BLANKING_FRAME_PACKET_1),
        getBusId());
    vector<byteType> data;
    sShortToData(data, 500); // 5C
    sShortToData(data, 501); // 5.01C
    sShortToData(data, 2500); // 25 C
    sShortToData(data, 4500); // 45 C
    msg.setData(data);
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message RxTemperatures::simBlankingFramePacket2( )
{
    carma::canbus::Message msg(
        createId(true, getApi(), getNode(), BLANKING_FRAME_PACKET_2),
        getBusId());
    vector<byteType> data;
    sShortToData(data, 1100); // 1.1V
    sShortToData(data, 500); // 500mA
    sShortToData(data, 1200); // 1.2V
    sShortToData(data, 501); // 501mA
    msg.setData(data);
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message RxTemperatures::simBlankingFramePacket3( )
{
    carma::canbus::Message msg(
        createId(true, getApi(), getNode(), BLANKING_FRAME_PACKET_3),
        getBusId());
    vector<byteType> data;
    uByteToData(data, 0); // HEATER_OFF
    sShortToData(data, 5000); // 50%
    sShortToData(data, 200); // 2 K s
    sShortToData(data, 201); // 2.01 K s
    msg.setData(data);
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message RxTemperatures::simBlankingFramePacket4( )
{
    carma::canbus::Message msg(
        createId(true, getApi(), getNode(), BLANKING_FRAME_PACKET_4),
        getBusId());
    vector<byteType> data;
    sShortToData(data, 10); // 1 (%max pwr/K
    sShortToData(data, 20); // 2 (%max pwr) K/s
    sShortToData(data, 30); // 3 (%max pwr) s/K
    sShortToData(data, 400); // 4 Hz
    msg.setData(data);
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message RxTemperatures::simBlankingFramePacket5( )
{
    carma::canbus::Message msg(
        createId(true, getApi(), getNode(), BLANKING_FRAME_PACKET_5),
        getBusId());
    vector<byteType> data;
    uByteToData(data, 0); // HEATER_OFF
    sShortToData(data, 5002); // 50.02%
    sShortToData(data, 202); // 202 K s
    sShortToData(data, 202); // 2.02 K s
    msg.setData(data);
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message RxTemperatures::simBlankingFramePacket6( )
{
    carma::canbus::Message msg(
        createId(true, getApi(), getNode(), BLANKING_FRAME_PACKET_6),
        getBusId());
    vector<byteType> data;
    sShortToData(data, 11); // 1.1 (%max pwr/K
    sShortToData(data, 21); // 2.1 (%max pwr) K/s
    sShortToData(data, 31); // 3.1 (%max pwr) s/K
    sShortToData(data, 410); // 4.1 Hz
    msg.setData(data);
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message RxTemperatures::simBlankingFramePacket7( )
{
    carma::canbus::Message msg(
        createId(true, getApi(), getNode(), BLANKING_FRAME_PACKET_7),
        getBusId());
    vector<byteType> data;
    sShortToData(data, 24000); // 24 V
    sShortToData(data, 12000); // 12 V
    sShortToData(data, -12000); // -12 V
    sShortToData(data, 5000); // 5 V
    msg.setData(data);
    return msg;
}
