/** @file
 * carma::antenna::ovro::LOReferenceMonitor class definition.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.7 $
 * $Date: 2011/01/03 18:48:04 $
 * $Id: LOReferenceMonitor.cc,v 1.7 2011/01/03 18:48:04 iws Exp $
 */

// Carma includes
#include "carma/antenna/common/LOReferenceMonitor.h"
#include "carma/canbus/exceptions.h"
#include "carma/canbus/Utilities.h"
#include "carma/monitor/CanbusCommon.h"
#include "carma/monitor/LoReferenceMonitor.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"

// Carma Tools includes
#include <log4cpp/Category.hh>
#include <log4cpp/Priority.hh>

using namespace carma::antenna::common;
using namespace carma::canbus;
using namespace carma::monitor;
using namespace carma::util;
using namespace log4cpp;
using namespace std;

namespace { // Anonymous namespace for local constants, typedefs and riffraff.
    
    // API Id for this device.
    const carma::canbus::apiType API_ID                       = 184;

    // API version this class was implemented from
    const char API_VERSION                                    = 'E';

    // Late packet timeout in ms
    const double PACKET_LATE_THRESHOLD                        = 150.0;

    // Control command message ids.
    const carma::canbus::msgType SET_LO_TERMINATOR_ATTEN      = 0x080;
    const carma::canbus::msgType SET_PRESET_POWER_LEVEL       = 0x081;
    const carma::canbus::msgType SET_REQUESTED_POWER_LEVEL    = 0x082;

    // Blanking frame message ids.
    const carma::canbus::msgType BLANKING_FRAME_PACKET_1      = 0x0E0;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_2      = 0x0E1;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_3      = 0x0E2;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_4      = 0x0E3;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_5      = 0x0E4;

} // End namespace <unnamed>

// -----------------------------------------------------------------------------
LOReferenceMonitor::LOReferenceMonitor(
    nodeType node, 
    CanOutput & io,
    carma::monitor::StateMonitorPointEnum & state,
    carma::monitor::LoReference& lorefMon,
    carma::monitor::Xac & xac) 
    : 
    XacDevice(API_ID, node, io),
    log_(Program::getLogger()),
    state_( state ),
    mon_( lorefMon ),
    xacMon_( xac )
{
    CPTRACE(Trace::TRACE6, "LOReferenceMonitor::LOReferenceMonitor() - Device "
        "class created for api " << API_ID << " node " << node);
}
    
// -----------------------------------------------------------------------------
LOReferenceMonitor::~LOReferenceMonitor()
{
    CPTRACE(Trace::TRACE6, "LOReferenceMonitor::~LOReferenceMonitor() - Device "
        "class destroyed for api " << API_ID << " node " << getNode());
}

// -----------------------------------------------------------------------------
MsgBriefMap LOReferenceMonitor::getHalfSecMonitors() const
{
    static map<msgType, string> tmp;
    static bool init = false;

    if (!init) {
        tmp[BLANKING_FRAME_PACKET_1] =
            "LOReferenceMonitor::BLANKING_FRAME_PACKET_1";
        tmp[BLANKING_FRAME_PACKET_2] =
            "LOReferenceMonitor::BLANKING_FRAME_PACKET_2";
        tmp[BLANKING_FRAME_PACKET_3] =
            "LOReferenceMonitor::BLANKING_FRAME_PACKET_3";
        tmp[BLANKING_FRAME_PACKET_4] =
            "LOReferenceMonitor::BLANKING_FRAME_PACKET_4";
        tmp[BLANKING_FRAME_PACKET_5] =
            "LOReferenceMonitor::BLANKING_FRAME_PACKET_5";
        init = true;
    }
    return tmp;
}

// -----------------------------------------------------------------------------
MsgBriefMap LOReferenceMonitor::getSlowMonitors() const
{
    return XacDevice::getSlowMonitors();
}

// -----------------------------------------------------------------------------
void LOReferenceMonitor::updateFrameData()
{
    // This method gets called once every half second FOR EACH INSTANCE OF
    // THIS DEVICE REGISTERED WITH MASTER! Use it to update information
    // every frame but not every message.

    // Set the state of the device...
    state_.setValue(static_cast<CM::StateMonitorPointEnum::STATE>(getState()));
}

// -----------------------------------------------------------------------------
void LOReferenceMonitor::processMsg(msgType mid, DataVector &data, 
    bool sim)
{
    // If state is ONLINE, check if the packet is late...
    if (getState() == ONLINE) {
        if (isPacketLate(PACKET_LATE_THRESHOLD)) {
            incrementLatePacketCount();
        }
    }

    CPTRACE(Trace::TRACEALL, "LOReferenceMonitor::processMsg() - Processing "
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
        default:
            // I don't know how to process this message id!
            // Not a problem, just log it.
            log_ << Priority::DEBUG << "LOReferenceMonitor::processMsg() - "
                << "Switch does not match any case: Unknown mid "
                << mid << ". Node " << getNode();
            CPTRACE(Trace::TRACE6, "LOReferenceMonitor::processMsg() - "
                    "Switch doesn't match any case.  mid 0x" << hex  << mid
                    << dec << " node " << getNode() << ".");
            break;
    }
}

// -----------------------------------------------------------------------------
carma::canbus::Message LOReferenceMonitor::simulateMsg(msgType mid)
{
    carma::canbus::Message msg;
    CPTRACE(Trace::TRACEALL, "LOReferenceMonitor::simulateMsg() - Simulating "
            "msg 0x" << hex << mid << dec << " for node " << getNode() << ".");
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
            log_ << Priority::DEBUG << "LOReferenceMonitor::simulateMsg - "
                << "Switch does not match any case: mid " << mid;
            CPTRACE(Trace::TRACE6, "LOReferenceMonitor::simulateMsg() - "
                    "Switch doesn't match any case.  mid 0x" << hex  << mid
                    << dec << " node " << getNode() << ".");
            break;
    }
    return msg;
}

// -----------------------------------------------------------------------------
void LOReferenceMonitor::processBlankingFramePacket1(DataVector &data)
{
    short photoLev10, photoLev50, photoLevLo, loRfInLev;
    
    if (data.size() < 8)
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "LOReferenceMonitor::processBlankingFramePacket1 - Size != 8.");

    // Unpack the data
    photoLev10 = dataToShort(data);
    photoLev50 = dataToShort(data);
    photoLevLo = dataToShort(data);
    loRfInLev = dataToShort(data);
    
    // Convert and place into monitor system...
    mon_.photoLevel10Mhz().setValue(photoLev10 / 100.0 ); // 0.01 dBm 
    mon_.photoLevel50Mhz().setValue(photoLev50 / 100.0 ); // 0.01 dBm
    mon_.photoLevelLO().setValue(photoLevLo / 100.0 ); // 0.01 dBm 
    mon_.rfinLevelLO().setValue( loRfInLev / 100.0 ); // 0.01 dBm
}
    
// -----------------------------------------------------------------------------
void LOReferenceMonitor::processBlankingFramePacket2(DataVector &data)
{
    short loRfOutLev, loTempV, modTemp; 
    unsigned short refAtt;

    if (data.size() < 7)
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "LOReferenceMonitor::processBlankingFramePacket2 - Size != 7.");

    // Unpack the data
    loRfOutLev = dataToShort(data);
    loTempV = dataToShort(data);
    refAtt = dataToUbyte(data);
    modTemp = dataToShort(data);

    // Convert and place into monitor system...
    mon_.rfoutLevelLO().setValue( loRfOutLev / 100.0 ); // 0.01 dBm
    mon_.loTempVolts().setValue( loTempV / 10.0 ); // 0.1 C 
    mon_.refAttReq().setValue( refAtt );
    mon_.modTemp().setValue( modTemp / 100.0 );
}
    
// -----------------------------------------------------------------------------
void LOReferenceMonitor::processBlankingFramePacket3(DataVector &data)
{
    short pos24v, pos5vDig, pos15v, neg9v;
    
    if (data.size() < 8)
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "LOReferenceMonitor::processBlankingFramePacket3 - Size != 8.");

    // Unpack the data
    pos24v = dataToShort(data);
    pos5vDig = dataToShort(data);
    pos15v = dataToShort(data);
    neg9v = dataToShort(data);

    // Convert and place into monitor system...
    mon_.psPos24v().setValue(pos24v / 1000.0 ); // mV
    mon_.psPos5vDig().setValue(pos5vDig / 1000.0 ); // mV
    mon_.psPos15v().setValue(pos15v / 1000.0 ); // mV
    mon_.psNeg9v().setValue(neg9v / 1000.0 ); // mV
}
    
// -----------------------------------------------------------------------------
void LOReferenceMonitor::processBlankingFramePacket4(DataVector &data)
{
    short neg5v, neg15v, pos5v, pos9v;

    if (data.size() < 8)
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "LOReferenceMonitor::processBlankingFramePacket4 - Size != 8.");

    // Unpack the data
    neg5v = dataToShort(data);
    neg15v = dataToShort(data);
    pos5v = dataToShort(data);
    pos9v = dataToShort(data);

    // Convert and place into monitor system...
    mon_.psNeg5v().setValue( neg5v / 1000.0 ); // mV
    mon_.psNeg15v().setValue( neg15v / 1000.0 ); // mV
    mon_.psPos5v().setValue( pos5v / 1000.0 ); // mV
    mon_.psPos9v().setValue( pos9v / 1000.0 ); // mV
}

// -----------------------------------------------------------------------------
void LOReferenceMonitor::processBlankingFramePacket5(DataVector &data)
{
    unsigned char lock10Mhz, photo10Mhz, photo50Mhz, loTermStat;
    unsigned char sn10, sn50, snLo;

    if (data.size() < 7)
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "LOReferenceMonitor::processBlankingFramePacket5 - Size != 7.");

    lock10Mhz = dataToUbyte( data );
    photo10Mhz = dataToUbyte( data );
    photo50Mhz = dataToUbyte( data );
    loTermStat = dataToUbyte( data );
    sn10 = dataToUbyte( data );
    sn50 = dataToUbyte( data );
    snLo = dataToUbyte( data );

    typedef 
    LoReference::LockStatus10MhzMonitorPointEnum::LOCKSTATUS10MHZ Lock10Mhz;
    mon_.lockStatus10Mhz( ).setValue( static_cast<Lock10Mhz>( lock10Mhz ) );
    typedef 
    LoReference::PhotoStatus10MhzMonitorPointEnum::PHOTOSTATUS10MHZ Photo10Mhz;
    mon_.photoStatus10Mhz( ).setValue( static_cast<Photo10Mhz>( photo10Mhz ) );
    typedef
    LoReference::PhotoStatus50MhzMonitorPointEnum::PHOTOSTATUS50MHZ Photo50Mhz;
    mon_.photoStatus50Mhz( ).setValue( static_cast<Photo50Mhz>( photo50Mhz ) );
    typedef
    LoReference::LoTermPowerStateMonitorPointEnum::LOTERMPOWERSTATE LoTermState;
    mon_.loTermPowerState( ).setValue( static_cast<LoTermState>( loTermStat ) );
    mon_.sn10MHz( ).setValue( sn10 );
    mon_.sn50MHz( ).setValue( sn50 );
    mon_.snLoTerm( ).setValue( snLo );

}

// -----------------------------------------------------------------------------
carma::canbus::Message LOReferenceMonitor::simBlankingFramePacket1()
{
    canbus::Message msg(
        createId(true, getApi(), getNode(), BLANKING_FRAME_PACKET_1),
        getBusId());
    DataVector data;
    sShortToData(data, 0xff);
    sShortToData(data, 0xff);
    sShortToData(data, 0xff);
    sShortToData(data, 0xff);
    msg.setData(data);
    return msg;
}
                            
// -----------------------------------------------------------------------------
carma::canbus::Message LOReferenceMonitor::simBlankingFramePacket2()
{
    canbus::Message msg(
        createId(true, getApi(), getNode(), BLANKING_FRAME_PACKET_2),
        getBusId());
    DataVector data;
    sShortToData(data, 0xff);
    sShortToData(data, 0xff);
    uByteToData(data, 0xf);
    sShortToData(data, 5000); // 50 C
    msg.setData(data);
    return msg;
}
                            
// -----------------------------------------------------------------------------
carma::canbus::Message LOReferenceMonitor::simBlankingFramePacket3()
{
    canbus::Message msg(
        createId(true, getApi(), getNode(), BLANKING_FRAME_PACKET_3),
        getBusId());
    DataVector data;
    sShortToData(data, 24001); // 24.001 volts
    sShortToData(data, 5001);  //  5.001 volts
    sShortToData(data, 15001); // 15.001 volts
    sShortToData(data, -9001); // -9.001 volts
    msg.setData(data);
    return msg;
}
                            
// -----------------------------------------------------------------------------
carma::canbus::Message LOReferenceMonitor::simBlankingFramePacket4()
{
    canbus::Message msg(
        createId(true, getApi(), getNode(), BLANKING_FRAME_PACKET_4),
        getBusId());
    DataVector data;
    sShortToData(data, -5001); // -5.001 volts
    sShortToData(data, -15001); // -15.001 volts
    sShortToData(data, 5001); // +5.001 volts
    sShortToData(data, 9001); // +9.001 volts
    msg.setData(data);
    return msg;
}
                            
// -----------------------------------------------------------------------------
carma::canbus::Message LOReferenceMonitor::simBlankingFramePacket5( )
{
    canbus::Message msg(
        createId( true, getApi(), getNode(), BLANKING_FRAME_PACKET_5 ),
        getBusId() );

    unsigned char lock10Mhz = 0, photo10Mhz = 1, photo50Mhz = 0, loTermStat = 2;
    unsigned char sn10 = 1, sn50 = 2, snLo = 3;

    msg << lock10Mhz << photo10Mhz << photo50Mhz << loTermStat 
        << sn10 << sn50 << snLo;

    return msg;

}

// -----------------------------------------------------------------------------
void LOReferenceMonitor::setLOTerminatorAttenuation(unsigned char atten)
{
    canbus::Message msg = createMsgToNode( SET_LO_TERMINATOR_ATTEN );
    DataVector data;
    uByteToData(data, atten);
    msg.setData(data);
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void LOReferenceMonitor::setPowerLevelToPreset( )
{
    canbus::Message msg = createMsgToNode( SET_PRESET_POWER_LEVEL );
    io_.postMessage( msg );
}

// -----------------------------------------------------------------------------
void LOReferenceMonitor::setPowerLevel( const double power ) {
    canbus::Message msg = createMsgToNode( SET_REQUESTED_POWER_LEVEL );
    const long int rawpower = static_cast<int>( power * 100.0 );
    msg << rawpower;
    io_.postMessage( msg );
}

