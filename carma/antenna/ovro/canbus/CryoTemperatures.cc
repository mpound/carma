/** @file
 * $Id: CryoTemperatures.cc,v 1.14 2011/01/03 18:48:06 iws Exp $
 *
 * CAN Device implementation for CARMA CANbus API No. 160 - Cryo Temperatures.
 *
 * Author: Andy Beard
 * $Revision: 1.14 $
 * $Date: 2011/01/03 18:48:06 $
 */
        
// Carma includes
#include "carma/antenna/ovro/canbus/CryoTemperatures.h"
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

// Must define constants declared in header file...
const carma::canbus::apiType CryoTemperatures::API_ID;
const char CryoTemperatures::API_VERSION;
const double CryoTemperatures::PACKET_LATE_THRESHOLD;
const carma::canbus::msgType CryoTemperatures::BLANKING_FRAME_PACKET_1;
const carma::canbus::msgType CryoTemperatures::BLANKING_FRAME_PACKET_2;
const carma::canbus::msgType CryoTemperatures::BLANKING_FRAME_PACKET_3;

// -----------------------------------------------------------------------------
CryoTemperatures::CryoTemperatures( nodeType node, 
                                    CanOutput & io,
                                    OvroSubsystem & ovroSubsys ) :
    XacDevice(API_ID, node, io),  
    log_(Program::getLogger()),
    mon_(ovroSubsys.cryo().dewar()),
    comMon_( ovroSubsys.antennaCommon() )
{
    CPTRACE(Trace::TRACE6, "CryoTemperatures::CryoTemperatures() - "
        "Device class created for api " << API_ID << " node " << node);
}

// -----------------------------------------------------------------------------
CryoTemperatures::~CryoTemperatures()
{
    CPTRACE(Trace::TRACE6, "CryoTemperatures::~CryoTemperatures() - "
        "Device class destroyed for api " << API_ID << " node " << getNode());
}

// -----------------------------------------------------------------------------
map<msgType, string> CryoTemperatures::getHalfSecMonitors() const
{
    map<msgType, string> tmp;
    tmp[BLANKING_FRAME_PACKET_1] = "CryoTemperatures::BLANKING_FRAME_PACKET_1";
    tmp[BLANKING_FRAME_PACKET_2] = "CryoTemperatures::BLANKING_FRAME_PACKET_2";
    tmp[BLANKING_FRAME_PACKET_3] = "CryoTemperatures::BLANKING_FRAME_PACKET_3";
    return tmp;
}

// -----------------------------------------------------------------------------
map<msgType, string> CryoTemperatures::getSlowMonitors() const
{
    return XacDevice::getSlowMonitors();
}

// -----------------------------------------------------------------------------
void CryoTemperatures::updateFrameData()
{
    // This method gets called once every half second FOR EACH INSTANCE OF
    // THIS DEVICE REGISTERED WITH MASTER!  Use it to update
    // information pertinent to the frame time but not updated every message.

    // Set the state...
    mon_.state().setValue(static_cast<StateMonitorPointEnum::STATE>(getState()));
}

// -----------------------------------------------------------------------------
void CryoTemperatures::processMsg(msgType mid, vector<byteType> &data, bool sim)
{
    // If state is ONLINE, check if the packet is late.
    if (getState() == ONLINE) {
        if (isPacketLate(PACKET_LATE_THRESHOLD)) {
            incrementLatePacketCount();
        }
    }

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
            log_ << Priority::DEBUG << "CryoTemperatures::processMsg() - "
                << "Switch does not match any case: Unknown mid "
                << mid << ". Node " << getNode();
            break;
    }
}

// -----------------------------------------------------------------------------
carma::canbus::Message CryoTemperatures::simulateMsg(msgType mid)
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
            log_ << Priority::DEBUG << "CryoTemperatures::simulateMsg - "
                << "Switch does not match any case: mid " << mid;
            break;
    }
    return msg;
}

// -----------------------------------------------------------------------------
void CryoTemperatures::processBlankingFramePacket1(vector<byteType> &data)
{
    unsigned int t3temp, t4temp;
    
    if (data.size() < 8) 
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "CryoTemperatures::processBlankingFramePacket1 - Size != 8.");
    
    // Unpack data
    t3temp = dataToUlong(data);
    t4temp = dataToUlong(data);

    // Convert and place into monitor stream.
    const float plateTemp = t3temp * 1.0e-3; // Units mK
    mon_.plate4kTemp().setValue(plateTemp); // Units mK
    mon_.heliumPotTemp().setValue(t4temp * 0.001); // Units mK

    // Set corresponding AntennaCommon monitor points
    comMon_.receivers().dewarTemp().setValue(plateTemp);
}

// -----------------------------------------------------------------------------
void CryoTemperatures::processBlankingFramePacket2(vector<byteType> &data)
{
    unsigned short t1temp, t2temp, vacuum1, vacuum2;

    if (data.size() < 8)
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "CryoTemperatures::processBlankingFramePacket2 - Size != 8.");

    // Unpack data
    t1temp = dataToUshort(data);
    t2temp = dataToUshort(data);
    vacuum1 = dataToUshort(data);
    vacuum2 = dataToUshort(data);

    // Convert and place into monitor stream.
    mon_.firstStageTemp().setValue(t1temp * 0.01); // Units 10mK
    mon_.shieldTemp().setValue(t2temp * 0.01);  // Units 10mK
    mon_.hastingsVac().setValue(vacuum1 * 0.1);    // Units 0.1 mTorr
    mon_.modionVac().setValue(vacuum2 * 0.01);     // Units 0.01 uTorr
}

// -----------------------------------------------------------------------------
void CryoTemperatures::processBlankingFramePacket3(vector<byteType> &data)
{
    short ps24v, ps12v, ps5vAnalog, ps5vDigital;
    
    if (data.size() < 8)
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "CryoTemperatures::processBlankingFramePacket3 - Size != 8.");

    // Unpack data
    ps24v = dataToShort(data);
    ps12v = dataToShort(data);
    ps5vAnalog = dataToShort(data);
    ps5vDigital = dataToShort(data);
    
    // Convert and place into monitor stream.
    mon_.ps24v().setValue(ps24v * 0.001);             // Units mV
    mon_.ps12v().setValue(ps12v * 0.001);             // Units mV
    mon_.ps5vAnalog().setValue(ps5vAnalog * 0.001);   // Units mV
    mon_.ps5vDigital().setValue(ps5vDigital * 0.001); // Units mV
}

// -----------------------------------------------------------------------------
carma::canbus::Message CryoTemperatures::simBlankingFramePacket1()
{
    canbus::Message msg(
            createId(true, getApi(), getNode(), BLANKING_FRAME_PACKET_1),
            getBusId());
    vector<byteType> data;
    uLongToData(data, 4000);   // 4 K 
    uLongToData(data, 80000);  // 80 K
    msg.setData(data);
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message CryoTemperatures::simBlankingFramePacket2()
{
    canbus::Message msg(
            createId(true, getApi(), getNode(), BLANKING_FRAME_PACKET_2),
            getBusId());
    vector<byteType> data;
    uShortToData(data, 2000);    // 20k
    uShortToData(data,10000);    // 100k
    uShortToData(data, 2000); // 200 mTorr
    uShortToData(data, 200);     // 200 E-8 Torr
    msg.setData(data);
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message CryoTemperatures::simBlankingFramePacket3()
{
    canbus::Message msg(
            createId(true, getApi(), getNode(), BLANKING_FRAME_PACKET_3),
            getBusId());
    vector<byteType> data;
    sShortToData(data, 24000);  // 24 volts
    sShortToData(data, 12000);  // 12 volts
    sShortToData(data, 5000);   //  5 volts
    sShortToData(data, 5000);   //  5 volts
    msg.setData(data);
    return msg;
}
