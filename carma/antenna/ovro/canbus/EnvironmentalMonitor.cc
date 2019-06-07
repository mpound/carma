/** @file
 * Implementation of 10-m Sidecab Environmental Monitor CAN Device.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.29 $
 * $Date: 2011/09/21 21:57:31 $
 * $Id: EnvironmentalMonitor.cc,v 1.29 2011/09/21 21:57:31 abeard Exp $
 *
 * $CarmaCopyright$
 */

#include "carma/antenna/ovro/canbus/EnvironmentalMonitor.h"

// Carma includes
#include "carma/canbus/exceptions.h"
#include "carma/canbus/Utilities.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedPthreadMutexLock.h"
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

namespace { // Anonymous namespace for local constants and riffraff.

    // API Id for this device.
    const carma::canbus::apiType API_ID                      = 88;

    // API version this class was implemented from
    const char API_VERSION                                   = 'C';

    // Late packe timeout in ms
    const double PACKET_LATE_THRESHOLD                       = 150.0;

    // Control command message ids.
    const carma::canbus::msgType SET_CAMERA_STATE            = 0x080;
    const carma::canbus::msgType TURN_SIDECAB_POWER_OFF      = 0x081;
    const carma::canbus::msgType SWITCH_24V_POWER_SUPPLY     = 0x082;

    // Blanking frame message ids.
    const carma::canbus::msgType BLANKING_FRAME_PACKET_1     = 0x0E0;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_2     = 0x0E1;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_3     = 0x0E2;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_4     = 0x0E3;

    const unsigned int           POSITION_CHANGE_WAIT_FRAMES = 19; // 9 seconds

} // End namespace <unnamed>

// -----------------------------------------------------------------------------
EnvironmentalMonitor::EnvironmentalMonitor( 
        nodeType node, 
        CanOutput & io,
        OvroSubsystem & mon ) :
    XacDevice(API_ID, node, io),
    log_( Program::getLogger( ) ),
    mon_( mon.environmentalMonitor( ) ),
    trackMon_( mon.drive().track( ) ),
    comMon_( mon.antennaCommon( ) )
{
    CPTRACE(Trace::TRACE6, "EnvironmentalMonitor::EnvironmentalMonitor() - "
        "Device class created for api " << API_ID << " node " << node);
}

// -----------------------------------------------------------------------------
EnvironmentalMonitor::~EnvironmentalMonitor()
{
    CPTRACE(Trace::TRACE6, "EnvironmentalMonitor::~EnvironmentalMonitor() - "
        "Device class destroyed for api " << API_ID << " node " << getNode());
}

// -----------------------------------------------------------------------------
map<msgType, string> EnvironmentalMonitor::getHalfSecMonitors() const
{
    static map<msgType, string> tmp;
    static bool init = false;

    if (!init) {
        tmp[BLANKING_FRAME_PACKET_1] = 
            "EnvironmentalMonitor::BLANKING_FRAME_PACKET_1";
        tmp[BLANKING_FRAME_PACKET_2] = 
            "EnvironmentalMonitor::BLANKING_FRAME_PACKET_2";
        tmp[BLANKING_FRAME_PACKET_3] = 
            "EnvironmentalMonitor::BLANKING_FRAME_PACKET_3";
        tmp[BLANKING_FRAME_PACKET_4] = 
            "EnvironmentalMonitor::BLANKING_FRAME_PACKET_4";
        init = true;
    }
    return tmp;
}

// -----------------------------------------------------------------------------
map<msgType, string> EnvironmentalMonitor::getSlowMonitors() const
{
    return XacDevice::getSlowMonitors();
}

// -----------------------------------------------------------------------------
void EnvironmentalMonitor::updateFrameData()
{
    // This method gets called once every half second FOR EACH INSTANCE OF
    // THIS DEVICE REGISTERED WITH MASTER! Use it to update information 
    // pertinent to the frame time but not updated every message.
    
    // Set the state of the device...
    mon_.state().setValue(static_cast<StateMonitorPointEnum::STATE>(getState()));
}

// -----------------------------------------------------------------------------
void EnvironmentalMonitor::processMsg(msgType mid, vector<byteType> &data, bool sim)
{
    // If state is ONLINE, check if the packet is late...
    if (getState() == ONLINE) {
        if (isPacketLate(PACKET_LATE_THRESHOLD)) {
            incrementLatePacketCount();
        }
    }

    CPTRACE(Trace::TRACEALL, "EnvironmentalMonitor::processMsg() - Processing "
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
            // I don't know how to process this message id!
            // Not a problem, just log it.
            log_ << Priority::DEBUG << "EnvironmentalMonitor::processMsg() - "
                << "Switch does not match any case: Unknown mid "
                << mid << ". Node " << getNode();
            CPTRACE(Trace::TRACE6, "EnvironmentalMonitor::processMsg() - "
                "Switch doesn't match any case.  mid 0x" << hex  << mid
                << dec << " node " << getNode() << ".");
            break;
    }
}

// -----------------------------------------------------------------------------
carma::canbus::Message EnvironmentalMonitor::simulateMsg(msgType mid)
{
    carma::canbus::Message msg;
    CPTRACE(Trace::TRACEALL, "EnvironmentalMonitor::simulateMsg() - "
        "Simulating msg 0x" << hex << mid << dec 
        << " for node " << getNode() << ".");
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
            // I don't know how to simulate this message!
            // Not a problem, just log it.
            log_ << Priority::DEBUG << "EnvironmentalMonitor::simulateMsg - "
                << "Switch does not match any case: mid " << mid;
            CPTRACE(Trace::TRACE6, "EnvironmentalMonitor::simulateMsg() - "
                "Switch doesn't match any case.  mid 0x" << hex  << mid
                << dec << " node " << getNode() << ".");
            break;
    }
    return msg;
}

// -----------------------------------------------------------------------------
void EnvironmentalMonitor::processBlankingFramePacket1(vector<byteType> &data)
{
    short sidecabTemp, outsideTemp, modTemp;
    unsigned char lensCapState, emergencySwitch;
    
    if (data.size() < 8)
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "EnvironmentalMonitor::processBlankingFramePacket1 - Size != 8.");

    // Unpack the data
    sidecabTemp = dataToShort(data);
    outsideTemp = dataToShort(data);
    modTemp = dataToShort(data);
    lensCapState = dataToUbyte(data);
    emergencySwitch = dataToUbyte(data);

    // Convert and place into monitor system...
    mon_.sidecabTemp().setValue(sidecabTemp * 0.01); // C
    mon_.outsideTemp().setValue(outsideTemp * 0.01); // C
    mon_.modTemp().setValue(modTemp * 0.01); // C
    mon_.lensCapState().setValue(
        static_cast<OvroSubsystem::LensCapStateMonitorPointEnum::LENSCAPSTATE>
            (lensCapState));

    // Set the emergency off switch monitor point enums...
    OvroSubsystem::EmergencyOffMonitorPointEnum::EMERGENCYOFF eEmergOff = 
        OvroSubsystem::EmergencyOffMonitorPointEnum::OK;
    AntennaCommon::EmergencyOffMonitorPointEnum::EMERGENCYOFF comEmergOff =
        AntennaCommon::EmergencyOffMonitorPointEnum::OK;

    if ( emergencySwitch == 0 ) {
        eEmergOff = OvroSubsystem::EmergencyOffMonitorPointEnum::SIDECAB;
        comEmergOff = AntennaCommon::EmergencyOffMonitorPointEnum::OFF;
    }

    mon_.emergencySwitch().setValue( 
        static_cast<
            OvroSubsystem::EmergencySwitchMonitorPointEnum::EMERGENCYSWITCH>(
                emergencySwitch ) );
    trackMon_.emergencyOff().setValue( eEmergOff );
    comMon_.drive( ).track( ).emergencyOff( ).setValue( comEmergOff );

    if ( lensCapState == OvroSubsystem::LensCapStateMonitorPointEnum::OPEN ) {
        comMon_.opticalTel().cover().setValue( 
            AntennaCommon::CoverMonitorPointEnum::OPEN );
    } else {
        comMon_.opticalTel().cover().setValue(
            AntennaCommon::CoverMonitorPointEnum::CLOSED );
    }
}

// -----------------------------------------------------------------------------
void EnvironmentalMonitor::processBlankingFramePacket2(vector<byteType> &data)
{
   unsigned char sidecabDoor, teepeeDoor;
   unsigned short timeSincePPM;
   short pos5vDig, pos3_3vDig;

    if (data.size() < 8)
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "EnvironmentalMonitor::processBlankingFramePacket2 - Size != 8.");

    // Unpack the data
    sidecabDoor = dataToUbyte(data);
    teepeeDoor = dataToUbyte(data);
    timeSincePPM = dataToUshort(data);
    pos5vDig = dataToShort(data);
    pos3_3vDig = dataToShort(data);

    // Convert and place into monitor stream...
    mon_.sidecabDoorOpen().setValue(sidecabDoor == 0x01);
    mon_.teepeeDoorOpen().setValue(teepeeDoor == 0x01);
    mon_.timeSincePPM().setValue(static_cast<int>(timeSincePPM));
    mon_.ps5vDigital().setValue(pos5vDig * 0.001); // Units V
    mon_.ps3_3vDigital().setValue(pos3_3vDig * 0.001); // Units V
}

// -----------------------------------------------------------------------------
void EnvironmentalMonitor::processBlankingFramePacket3(vector<byteType> &data)
{
    short mod24v, ant24v, therm24v;
    unsigned char psu1, psu2;

    if (data.size() != 8)
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "EnvironmentalMonitor::processBlankingFramePacket3 - Size != 8.");

    // Unpack the data
    mod24v = dataToShort(data);
    ant24v = dataToShort(data);
    therm24v = dataToShort(data);
    psu1 = dataToUbyte(data);
    psu2 = dataToUbyte(data);

    // Convert and place into monitor system...
    mon_.ps24vModule().setValue(mod24v * 0.001); // Units V
    mon_.ps24vAntenna().setValue(ant24v * 0.001); // Units V
    mon_.ps24vThermalShutdown().setValue(therm24v * 0.001); // Units V
    mon_.ps1State().setValue(static_cast<
        OvroSubsystem::Ps1StateMonitorPointEnum::PS1STATE>(psu1));
    mon_.ps2State().setValue(static_cast<
        OvroSubsystem::Ps2StateMonitorPointEnum::PS2STATE>(psu2));
}

// -----------------------------------------------------------------------------
void EnvironmentalMonitor::processBlankingFramePacket4(vector<byteType> &data)
{
    unsigned char psu3, capClosed, capOpen, cameraControl;
    
    if (data.size() < 4)
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "EnvironmentalMonitor::processBlankingFramePacket4 - Size < 5.");
    
    // Unpack the data
    psu3 = dataToUbyte(data);
    capClosed = dataToUbyte(data);
    capOpen = dataToUbyte(data);
    cameraControl = dataToUbyte(data);

    // Convert and place into monitor stream...
    mon_.ps3State().setValue(static_cast<
        OvroSubsystem::Ps3StateMonitorPointEnum::PS3STATE>(psu3));
    mon_.lensCapClosed().setValue(capClosed == 0x01);
    mon_.lensCapOpen().setValue(capOpen == 0x01);
    mon_.cameraState().setValue(static_cast<
        OvroSubsystem::CameraStateMonitorPointEnum::CAMERASTATE>(cameraControl));
}

// -----------------------------------------------------------------------------
carma::canbus::Message EnvironmentalMonitor::simBlankingFramePacket1()
{
    canbus::Message msg(
        createId(true, getApi(), getNode(), BLANKING_FRAME_PACKET_1),
        getBusId());
    vector<byteType> data;
    sShortToData(data, 2000); // 20 C
    sShortToData(data, 2500); // 25 C
    sShortToData(data, 3500); // 35 C
    uByteToData(data, 0x08); // ERROR
    uByteToData(data, 0x01); // Off - drives enabled.
    msg.setData(data);
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message EnvironmentalMonitor::simBlankingFramePacket2()
{
    canbus::Message msg(
        createId(true, getApi(), getNode(), BLANKING_FRAME_PACKET_2),
        getBusId());
    vector<byteType> data;
    uByteToData(data, 0x01); // sidecab door open
    uByteToData(data, 0x01); // Teepee door open 
    uShortToData(data, 30); // 30 s since last PPM
    sShortToData(data, 5001); // 5.001 V
    sShortToData(data, 3301); // 3.301 V
    msg.setData(data);
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message EnvironmentalMonitor::simBlankingFramePacket3()
{
    canbus::Message msg(
        createId(true, getApi(), getNode(), BLANKING_FRAME_PACKET_3),
        getBusId());
    vector<byteType> data;
    sShortToData(data, 24001); // 24.001 V
    sShortToData(data, 24002); // 24.002 V
    sShortToData(data, 24003); // 24.003 V
    uByteToData(data, 0x00); // Turned OFF
    uByteToData(data, 0x00); // Turned OFF
    msg.setData(data);
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message EnvironmentalMonitor::simBlankingFramePacket4()
{
    carma::canbus::Message msg(
        createId(true, getApi(), getNode(), BLANKING_FRAME_PACKET_4),
            getBusId());
    vector<byteType> data;
    uByteToData(data, 0x00); // Turned OFF
    uByteToData(data, 0x00); // Not fully closed
    uByteToData(data, 0x00); // Not fully open!
    uByteToData(data, 0x00); // Camera off / lens cap on
    msg.setData(data);
    return msg;
}

// -----------------------------------------------------------------------------
void EnvironmentalMonitor::enableCamera(bool on) 
{
    CARMA_CPTRACE(Trace::TRACE3, "EnvironmentalMonitor::enableCamera("
        << boolalpha << on << noboolalpha << ").");
    carma::canbus::Message msg(createId(false, getApi(), getNode(),
        SET_CAMERA_STATE), getBusId());
    vector<byteType> data;
    uByteToData(data, (on ? 0x01 : 0x00));
    msg.setData(data);
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void EnvironmentalMonitor::turnSidecabPowerOff()
{
    carma::canbus::Message msg(createId(false, getApi(), getNode(),
        TURN_SIDECAB_POWER_OFF), getBusId());
    // This message contains no data.
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void EnvironmentalMonitor::enable24vPs(unsigned short supplyNo, bool on)
{
    carma::canbus::Message msg(createId(false, getApi(), getNode(),
        SWITCH_24V_POWER_SUPPLY), getBusId());
    vector<byteType> data;
    uByteToData(data, static_cast<unsigned short>(supplyNo));
    uByteToData(data, (on ? 0x01 : 0x00));
    msg.setData(data);
    io_.postMessage(msg);
}
