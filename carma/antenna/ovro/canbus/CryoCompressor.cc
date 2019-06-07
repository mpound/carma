/**
 * $Id: CryoCompressor.cc,v 1.15 2011/01/03 18:48:06 iws Exp $
 * 
 * Implementation for CryoCompressor Device.
 *
 * Author: Andy Beard
 * $Revision: 1.15 $
 * $Date: 2011/01/03 18:48:06 $
 */

// Carma includes
#include "carma/antenna/ovro/canbus/CryoCompressor.h"
#include "carma/canbus/Utilities.h"
#include "carma/monitor/OvroSubsystem.h"
#include "carma/util/Program.h"

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

// Anonymous namespace for constants which are only used here...
namespace {
    
    // API Id for this device.
    const carma::canbus::apiType API_ID                    = 32;

    // API version this class was implemented from 
    const char API_VERSION                                 = 'K';
    
    // Late packet timeout in ms
    const double PACKET_LATE_THRESHOLD                     = 150.0;

    // Control command message ids.
    const carma::canbus::msgType COMPRESSOR_ON_OFF         = 0x080;
    const carma::canbus::msgType COMPRESSOR_RESET          = 0x081;
    const carma::canbus::msgType HELIUM_FILL               = 0x082;
    const carma::canbus::msgType HELIUM_PURGE              = 0x083;
    const carma::canbus::msgType TEMP_SERVO_ON_OFF         = 0x084;
    const carma::canbus::msgType INLET_LOUVER_POS          = 0x085;
    const carma::canbus::msgType OUTLET_LOUVER_POS         = 0x086;

    // Blanking frame message ids.
    const carma::canbus::msgType BLANKING_FRAME_PACKET_1   = 0x0E0;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_2   = 0x0E1;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_3   = 0x0E2;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_4   = 0x0E3;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_5   = 0x0E4;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_6   = 0x0E5;

    typedef OvroSubsystem OS;
    typedef OvroSubsystem::Compressor Comp;
    
} // End anonymous namespace 

// -----------------------------------------------------------------------------
CryoCompressor::CryoCompressor( nodeType node, 
                                CanOutput & io,
                                OvroSubsystem & ovroSubsys ) : 
    XacDevice (API_ID, node, io), 
    log_(Program::getProgram().getLogger()),
    mon_( ovroSubsys.cryo().compressor())
{
    // Nothing here yet.
}

// -----------------------------------------------------------------------------
CryoCompressor::~CryoCompressor() 
{
    // Nothing here yet.  
}

// -----------------------------------------------------------------------------
map<msgType, string> CryoCompressor::getControls() const
{
    static bool init = false;
    static map<msgType, string> tmp;

    if (!init) {
        tmp[COMPRESSOR_ON_OFF] = "Turn helium compressor on or off.";
        tmp[COMPRESSOR_RESET]  = "Reset the helium compressor.";
        tmp[HELIUM_FILL]       = "Fill comp with 1s charge of helium.";
        tmp[HELIUM_PURGE]      = "Purge 0.5s charge of helium from comp.";
        tmp[TEMP_SERVO_ON_OFF] = "Enable or disable the temperature servo.";
        tmp[INLET_LOUVER_POS]  = "Set the inlet louver position.";
        tmp[OUTLET_LOUVER_POS] = "Set the outlet louver position.";
        init = true;
    }
    
    return tmp;
}

// -----------------------------------------------------------------------------
map<msgType, string> CryoCompressor::getHalfSecMonitors() const
{
    static bool init = false;
    static map<msgType, string> tmp;

    if (!init) {
        tmp[BLANKING_FRAME_PACKET_1] = "BLANKING_FRAME_PACKET_1";
        tmp[BLANKING_FRAME_PACKET_2] = "BLANKING_FRAME_PACKET_2";
        tmp[BLANKING_FRAME_PACKET_3] = "BLANKING_FRAME_PACKET_3";
        tmp[BLANKING_FRAME_PACKET_4] = "BLANKING_FRAME_PACKET_4";
        tmp[BLANKING_FRAME_PACKET_5] = "BLANKING_FRAME_PACKET_5";
        tmp[BLANKING_FRAME_PACKET_6] = "BLANKING_FRAME_PACKET_6";
        init = true;
    }

    return tmp;
}

// -----------------------------------------------------------------------------
map<msgType, string> CryoCompressor::getSlowMonitors() const
{
    return XacDevice::getSlowMonitors();
}
    
// -----------------------------------------------------------------------------
void CryoCompressor::updateFrameData()
{
    // This method gets called once every half second FOR EACH INSTANCE OF
    // THIS DEVICE REGISTERED WITH MASTER!  Use it to update
    // information pertinent to the frame time but info that does'nt need
    // updated every time we receive a CAN message.

    // Set the state...
    mon_.state().setValue(static_cast<StateMonitorPointEnum::STATE>(getState()));
}

// -----------------------------------------------------------------------------
void CryoCompressor::processMsg(msgType mid, vector<byteType> &data, bool sim)
{
    // If state is ONLINE, check if the packet is late.
    if (getState() == ONLINE) {
        if (isPacketLate(PACKET_LATE_THRESHOLD)) {
            incrementLatePacketCount();
        }
    }
        
    // Dispatch the data to the appropriate message processing routine.
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
            // I don't know how to process this msg id!
            // Not a problem, just log it.
            log_ << Priority::DEBUG << "CryoCompressor::processMsg() - "
                << "Switch does not match any case: Unknown mid " << mid;
            break;
    } 
}

// -----------------------------------------------------------------------------
carma::canbus::Message CryoCompressor::simulateMsg(msgType mid)
{
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
        case BLANKING_FRAME_PACKET_5:
            msg = simBlankingFramePacket5();
            break;
        case BLANKING_FRAME_PACKET_6:
            msg = simBlankingFramePacket6();
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_1:
            msg = simSystemMonitorPacket1();
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_2:
            msg = simSystemMonitorPacket2();
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_3:
            msg = simSystemMonitorPacket3();
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_4:
            msg = simSystemMonitorPacket4();
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_5:
            msg = simSystemMonitorPacket5();
            break;
        default:
            break;
    }
    return msg;
}

// -----------------------------------------------------------------------------
void CryoCompressor::enableCompressor(bool on)
{
    carma::canbus::Message msg(
        createId(false, getApi(), getNode(), COMPRESSOR_ON_OFF), 
        getBusId());
    vector<byteType> data;
    uByteToData(data, (on ? 0x01 : 0x00));
    msg.setData(data);
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void CryoCompressor::resetCompressor() 
{
    canbus::Message msg(createId(false, getApi(), getNode(), COMPRESSOR_RESET), 
        getBusId());
    // This is a no data msg.
    io_.postMessage(msg);
}
    
// -----------------------------------------------------------------------------
void CryoCompressor::fillCompressor() 
{
    canbus::Message msg(createId(false, getApi(), getNode(), HELIUM_FILL),
        getBusId());
    // This is a no data msg.
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void CryoCompressor::purgeCompressor() 
{
    canbus::Message msg(createId(false, getApi(), getNode(), HELIUM_PURGE),
        getBusId());
    // This is a no data msg.
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void CryoCompressor::enableTemperatureServo(bool on) 
{
    canbus::Message msg(createId(false, getApi(), getNode(), TEMP_SERVO_ON_OFF),
        getBusId());
    vector<byteType> data;
    uByteToData(data, (on ? 0x01 : 0x00));
    msg.setData(data);
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void CryoCompressor::setInletLouver(float volts)
{
    canbus::Message msg(createId(false, getApi(), getNode(), INLET_LOUVER_POS),
        getBusId());
    vector<byteType> data;
    uShortToData(data, static_cast<unsigned short>(volts / 0.001));
    msg.setData(data);
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void CryoCompressor::setOutletLouver(float volts)
{
    canbus::Message msg(createId(false, getApi(), getNode(), OUTLET_LOUVER_POS),
        getBusId());
    vector<byteType> data;
    uShortToData(data, static_cast<unsigned short>(volts / 0.001));
    msg.setData(data);
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void CryoCompressor::processBlankingFramePacket1(vector<byteType> &data)
{
    const short ambTemp = dataToShort(data);
    const short cabTemp = dataToShort(data);
    const short compTemp = dataToShort(data);
    const short returnTemp = dataToShort(data);

    // Stuff data into monitor the packet
    mon_.ambientTemp().setValue(ambTemp / 100.0 );     // Units 0.01 C
    mon_.cabinetTemp().setValue(cabTemp / 100.0 );     // Units 0.01 C
    mon_.compressorTemp().setValue(compTemp / 100.0 ); // Units 0.01 C
    mon_.returnTemp().setValue(returnTemp / 100.0 );   // Units 0.01 C
}

// -----------------------------------------------------------------------------
void CryoCompressor::processBlankingFramePacket2(vector<byteType> &data)
{
    const short highSideTemp = dataToShort(data);
    const short supplyPress = dataToShort(data);
    const short returnPress = dataToShort(data);
    const unsigned short outLouvCmdPercentage = dataToUshort(data);

    // Convert and stuff data into monitor stream
    mon_.highSidetemp().setValue(highSideTemp / 100.0 );     // Units 0.01 C 
    mon_.supplyPressure().setValue(supplyPress / 10.0 );     // Units 0.1 PSI
    mon_.returnPressure().setValue(returnPress / 10.0 );     // Units 0.1 PSI
    mon_.outLouverCmd().setValue(outLouvCmdPercentage);      // Units %
}

// -----------------------------------------------------------------------------
void CryoCompressor::processBlankingFramePacket3(vector<byteType> &data)
{
    const unsigned short inLouvCmdPercentage = dataToUshort(data);
    const unsigned short outLouvPosPercentage = dataToUshort(data);
    const unsigned short inLouvPosPercentage = dataToUshort(data);
    const short tempError = dataToShort(data);

    // Convert and stuff data into monitor stream
    mon_.inLouverCmd().setValue(inLouvCmdPercentage);   // Units % 
    mon_.outLouverPos().setValue(outLouvPosPercentage); // Units % 
    mon_.inLouverPos().setValue(inLouvPosPercentage);   // Units % 
    mon_.tempError().setValue(tempError / 100.0);       // Units C
}

// -----------------------------------------------------------------------------
void CryoCompressor::processBlankingFramePacket4(vector<byteType> &data)
{
    const short heCylinderPress = dataToShort(data);
    const unsigned char hiPressAlarm = dataToUbyte(data);
    const unsigned char hiTempAlarm = dataToUbyte(data);
    const unsigned char roomTempAlarm = dataToUbyte(data);
    const unsigned char driveOn = dataToUbyte(data);
    const short modTemp = dataToShort(data);

    // Convert and stuff the data into the monitor packet
    mon_.heCylinderPressure().setValue(heCylinderPress / 10.0); // Units 0.1 PSI
    mon_.highPressureAlarm().setValue(static_cast<bool>(hiPressAlarm));
    mon_.highTemperatureAlarm().setValue(static_cast<bool>(hiTempAlarm));
    mon_.roomTemperatureAlarm().setValue(static_cast<bool>(roomTempAlarm));
    mon_.fridgeDriveState().setValue(
        static_cast<OS::FridgeDriveStateMonitorPointEnum::FRIDGEDRIVESTATE>(
            driveOn) );
    mon_.moduleTemperature().setValue(modTemp / 100.0); // Units 0.01 C
}

// -----------------------------------------------------------------------------
void CryoCompressor::processBlankingFramePacket5(vector<byteType> &data)
{
    const short ps24v = dataToShort(data);
    const short ps12v = dataToShort(data);
    const short ps5vAnalog = dataToShort(data);
    const short ps5vDigital = dataToShort(data);
    
    // Convert and stuff the data into the monitor packet
    mon_.powerSupply24v().setValue(ps24v / 1000.0); // Units mV
    mon_.powerSupply12v().setValue(ps12v / 1000.0); // Units mV
    mon_.powerSupply5vAnalog().setValue(ps5vAnalog / 1000.0); // Units mV
    mon_.powerSupply5vDigital().setValue(ps5vDigital / 1000.0); // Units mV
}

// -----------------------------------------------------------------------------
void CryoCompressor::processBlankingFramePacket6(vector<byteType> &data)
{
    unsigned char loopState;
    
    loopState = dataToUbyte(data);

    // Convert and stuff the data into the monitor stream
    mon_.tempLoopState().setValue(static_cast<
        OS::TempLoopStateMonitorPointEnum::TEMPLOOPSTATE>(
            loopState) );
}

// -----------------------------------------------------------------------------
carma::canbus::Message CryoCompressor::simBlankingFramePacket1()
{
    canbus::Message msg(
        createId(true, getApi(), getNode(), BLANKING_FRAME_PACKET_1), 
        getBusId());
    vector<byteType> data;
    sShortToData(data, 10000);  // 100 C 
    sShortToData(data, 9500);   // 95 C
    sShortToData(data, 9000);   // 90 C
    sShortToData(data, 8500);   // 85 C
    msg.setData(data);
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message CryoCompressor::simBlankingFramePacket2()
{
    canbus::Message msg(
        createId(true, getApi(), getNode(), BLANKING_FRAME_PACKET_2), 
        getBusId());
    vector<byteType> data;
    sShortToData(data, 0);
    sShortToData(data, 1500);  // 150 psi
    sShortToData(data, 3000);  // 300 psi
    uShortToData(data, 5000);  // 5 volts
    msg.setData(data);
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message CryoCompressor::simBlankingFramePacket3()
{
    canbus::Message msg(
        createId(true, getApi(), getNode(), BLANKING_FRAME_PACKET_3), 
        getBusId());
    vector<byteType> data;
    uShortToData(data, 5000); // 5 volts
    uShortToData(data, 5100); // 5.1 volts
    uShortToData(data, 5100); // 5.1 volts
    uShortToData(data, 21);   // 2.1 volts
    msg.setData(data);
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message CryoCompressor::simBlankingFramePacket4()
{
    canbus::Message msg(
        createId(true, getApi(), getNode(), BLANKING_FRAME_PACKET_4), 
        getBusId());
    vector<byteType> data;
    sShortToData(data, 10000);   // 10000 psi
    uByteToData(data, 0x00);     // No alarm
    uByteToData(data, 0x00);     // No alarm
    uByteToData(data, 0x00);     // No alarm
    uByteToData(data, 0x00);     // Drive is ON
    sShortToData(data, 6000);    // 60 c
    msg.setData(data);
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message CryoCompressor::simBlankingFramePacket5()
{
    canbus::Message msg(
        createId(true, getApi(), getNode(), BLANKING_FRAME_PACKET_5), 
        getBusId());
    vector<byteType> data;
    sShortToData(data, 24000);   // 24 Volts
    sShortToData(data, 12000);   // 12 Volts
    sShortToData(data, 5000);    //  5 Volts
    sShortToData(data, 5000);    //  5 Volts
    msg.setData(data);
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message CryoCompressor::simBlankingFramePacket6()
{
    canbus::Message msg(
        createId(true, getApi(), getNode(), BLANKING_FRAME_PACKET_6), 
        getBusId());
    vector<byteType> data;
    sShortToData(data, 5000); // 5 Volts
    uByteToData(data, 0x0);  // Loop disabled
    msg.setData(data);     
    return msg;
}

