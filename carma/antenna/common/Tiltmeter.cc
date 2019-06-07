/** @file
 * Tiltmeter class definition.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.2 $
 * $Date: 2011/01/03 18:48:05 $
 * $Id: Tiltmeter.cc,v 1.2 2011/01/03 18:48:05 iws Exp $
 *
 * $CarmaCopyright$
 */

// Carma includes
#include "carma/antenna/common/Tiltmeter.h"

#include "carma/canbus/Utilities.h"
#include "carma/services/Angle.h"
#include "carma/util/Program.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/ScopedPthreadMutexLock.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"

// Carma Tools includes
#include <log4cpp/Category.hh>
#include <log4cpp/Priority.hh>

// Other includes
#include <cmath>

using namespace carma::antenna::ovro;
using namespace carma::canbus;
using namespace carma::canbus::devices;
using namespace carma::monitor;
using namespace carma::services;
using namespace carma::util;
using namespace log4cpp;
using namespace std;

namespace {

    // API Id for this device.
    const carma::canbus::apiType API_ID                    = 40;

    // API version this class was implemented from 
    const char API_VERSION                                 = 'E';

    // Late packet timeout in ms
    const double PACKET_LATE_THRESHOLD                     = 150.0;

    // Control command message ids.
    const carma::canbus::msgType SET_TEMPERATURE           = 0x080;
    const carma::canbus::msgType REGULATE_TEMPERATURE      = 0x081;
    const carma::canbus::msgType SET_LOOP_GAIN             = 0x082;
    const carma::canbus::msgType SET_LOOP_INT_CONSTANT     = 0x083; 
    const carma::canbus::msgType SET_LOOP_RATE_CONSTANT    = 0x084;
    const carma::canbus::msgType SET_LOOP_BANDWIDTH        = 0x085;
    const carma::canbus::msgType WRITE_LOOP_PARAMS         = 0x086;

    // Blanking frame message ids.
    const carma::canbus::msgType BLANKING_FRAME_PACKET_1   = 0x0E0;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_2   = 0x0E1;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_3   = 0x0E2;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_4   = 0x0E3;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_5   = 0x0E4;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_6   = 0x0E5;

}  // namespace < unnamed >

struct Tiltmeter::Shared {

    mutable PthreadMutex mutex;
    Angle aftForwardTilt;
    Angle leftRightTilt;
    
    Shared( ) : 
        aftForwardTilt( 0.0, "radians" ),
        leftRightTilt( 0.0, "radians" )
    { };

};

// -----------------------------------------------------------------------------
Tiltmeter::Tiltmeter( nodeType node, 
                      CanOutput & io,
                      OvroSubsystem & mon,
                      unsigned short antNo) :
    XacDevice( API_ID, node, io ),
    antennaNo_( antNo ),
    log_( Program::getLogger() ),
    mon_( mon.tiltmeter() ),
    shared_( new Tiltmeter::Shared( ) )
{
    CPTRACE(Trace::TRACE6, "Tiltmeter() - C'tor - Device "
            "class created for api " << API_ID << " node " << node);
}

// -----------------------------------------------------------------------------
Tiltmeter::~Tiltmeter()
{
    CPTRACE(Trace::TRACE6, "~Tiltmeter() - D'tor - Device "
            "class destroyed for api " << API_ID << " node " << getNode());
}

// -----------------------------------------------------------------------------
carma::services::Angle Tiltmeter::getMostRecentAftForwardTilt( ) const
{
    ScopedPthreadMutexLock scopelock( shared_->mutex );

    return shared_->aftForwardTilt;
}

// -----------------------------------------------------------------------------
carma::services::Angle Tiltmeter::getMostRecentLeftRightTilt( ) const
{
    ScopedPthreadMutexLock scopelock( shared_->mutex );

    return shared_->leftRightTilt;
}

// -----------------------------------------------------------------------------
map<msgType, string> Tiltmeter::getHalfSecMonitors() const 
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
map<msgType, string> Tiltmeter::getSlowMonitors() const
{
    return XacDevice::getSlowMonitors();
}

// -----------------------------------------------------------------------------
void Tiltmeter::updateFrameData()
{
    // This method gets called once every half second FOR EACH INSTANCE OF
    // THIS DEVICE REGISTERED WITH MASTER!  Use it to update
    // information pertinent to the frame time but info that doesn't need
    // updated every time we receive a CAN message.

    // Set the state...
    mon_.state().setValue(static_cast<StateMonitorPointEnum::STATE>(getState()));
}

// -----------------------------------------------------------------------------
void Tiltmeter::processMsg(msgType mid, vector<byteType> &data, bool sim)
{
    // If state is ONLINE, check if the packet is late.
    if (getState() == ONLINE) {
        if (isPacketLate(PACKET_LATE_THRESHOLD)) {
            incrementLatePacketCount();
        }
    }

    CPTRACE(Trace::TRACEALL, "Tiltmeter::processMsg() - Processing "
            << (sim ? "simulated" : "unsimulated") << " msg 0x"
            << hex << mid << dec << " for node " << getNode() << ".");
        
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
            log_ << Priority::DEBUG << "Tiltmeter::processMsg() - "
                << "Switch does not match any case: Unknown mid " << mid;
            break;
    } 
}
     
// -----------------------------------------------------------------------------
carma::canbus::Message Tiltmeter::simulateMsg(msgType mid)
{
    carma::canbus::Message msg;
    
    CPTRACE(Trace::TRACEALL, "Tiltmeter::simulateMsg() - Simulating msg "
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
            // I don't know how to simulate this msg - log it.
            log_ << Priority::DEBUG << "Tiltmeter::simulateMsg() - "
                << "Switch does not match any case: Invalid mid " << mid << ".";
            break;
    }
    return msg;
}

// Monitor packet processing routines....
// -----------------------------------------------------------------------------
void Tiltmeter::processBlankingFramePacket1(vector<byteType> &data)
{
    const short int lrTilt = dataToShort(data); 
    const short int afTilt = dataToShort(data);
    const short int modTemp = dataToShort(data);

    const double LSBS_PER_ARCMIN = 1000.0;
    const double lrTiltInArcmin = lrTilt / LSBS_PER_ARCMIN; 
    const double afTiltInArcmin = afTilt / LSBS_PER_ARCMIN;

    {
        ScopedPthreadMutexLock scopelock( shared_->mutex );
        
        shared_->aftForwardTilt.reset( afTiltInArcmin, "arcminutes" );
        shared_->leftRightTilt.reset( lrTiltInArcmin, "arcminutes" );
    }

    // Convert and place into monitor system
    mon_.lrTilt().setValue(lrTiltInArcmin); // Units 0.01 arcmin
    mon_.afTilt().setValue(afTiltInArcmin); // Units 0.01 arcmin
    mon_.modTemp().setValue(modTemp * 0.01); // Units 0.01 C
}

// -----------------------------------------------------------------------------
void Tiltmeter::processBlankingFramePacket2(vector<byteType> &data)
{
    short int tiltTemp, structTemp, heaterV, heaterI;

    // Unpack the data
    tiltTemp = dataToShort(data);
    structTemp = dataToShort(data);
    heaterV = dataToShort(data);
    heaterI = dataToShort(data);

    // Convert and place into monitor system
    mon_.tiltTemp().setValue(tiltTemp * 0.01); // Units 0.01 C
    mon_.structTemp().setValue(structTemp * 0.01); // Units 0.01 C
    mon_.heaterVoltage().setValue(heaterV * 0.001); // Units mV
    mon_.heaterCurrent().setValue(heaterI * 0.001); // Units mA
}

// -----------------------------------------------------------------------------
void Tiltmeter::processBlankingFramePacket3(vector<byteType> &data)
{
    unsigned char loopState;
    short int pwrFrac, tempDiff, integDiff;

    // Unpack the data
    loopState = dataToUbyte(data);
    pwrFrac = dataToShort(data);
    tempDiff = dataToShort(data);
    integDiff = dataToShort(data);
    
    // Convert and place into monitor system
    mon_.loopState().setValue(static_cast<
        TiltmeterModule::LoopStateMonitorPointEnum::LOOPSTATE>(loopState));
    mon_.maxPowerFrac().setValue(pwrFrac * 0.01); // Units 0.01%
    mon_.tempDiff().setValue(tempDiff * 0.01); // Units 0.01K
    mon_.integDiff().setValue(integDiff * 0.01); // Units 0.01 K s
}

// -----------------------------------------------------------------------------
void Tiltmeter::processBlankingFramePacket4(vector<byteType> &data)
{
    short int loopGain, loopInteg, loopDiffGain, loopBandwidth;

    // Unpack the data
    loopGain = dataToShort(data);
    loopInteg = dataToShort(data);
    loopDiffGain = dataToShort(data);
    loopBandwidth = dataToShort(data);

    // Convert and place into monitor system
    mon_.loopGain().setValue(loopGain * 0.1); // Units 0.1 % max pwr/K
    mon_.loopIntegGain().setValue(loopInteg * 0.1); // Units 0.1 % max pwr K/s
    mon_.loopDiffGain().setValue(loopDiffGain * 0.1); // Units 0.1 % max pwr s/K
    mon_.loopBandwidth().setValue(loopBandwidth * 0.01); // Units 0.01 Hz
}

// -----------------------------------------------------------------------------
void Tiltmeter::processBlankingFramePacket5(vector<byteType> &data)
{
    short int ps24v, psPos12v, psNeg12v, ps5v;

    // Unpack the data
    ps24v = dataToShort(data);
    psPos12v = dataToShort(data);
    psNeg12v = dataToShort(data);
    ps5v = dataToShort(data);

    // Convert and place into monitor system
    mon_.ps24v().setValue(ps24v * 0.001); // Units mV
    mon_.psTiltPos12v().setValue(psPos12v * 0.001); // Units mV
    mon_.psTiltNeg12v().setValue(psNeg12v * 0.001); // Units mV
    mon_.psTilt5v().setValue(ps5v * 0.001); // Units mV
}

// -----------------------------------------------------------------------------
void Tiltmeter::processBlankingFramePacket6(vector<byteType> &data)
{
    short int psPos12v, psNeg12v, ps5v, teepeeTemp;

    // Unpack the data
    psPos12v = dataToShort(data);
    psNeg12v = dataToShort(data);
    ps5v = dataToShort(data);
    teepeeTemp = dataToShort(data);

    // Convert and place into monitor system.
    mon_.psThermPos12v().setValue(psPos12v * 0.001); // Units mV
    mon_.psThermNeg12v().setValue(psNeg12v * 0.001); // Units mV
    mon_.psTherm5v().setValue(ps5v * 0.001); // Units mV
    mon_.teepeeTemp().setValue(teepeeTemp * 0.01); // 0.01C LSB 
}

// Simulated msg creation routines...
// -----------------------------------------------------------------------------
carma::canbus::Message Tiltmeter::simBlankingFramePacket1()
{
    // Add some slow varying drift to our tilts.
    const short nominalLrTilt = -12;
    const short nominalAfTilt = 55;
    const short drift = ( ( Time::computeCurrentFrame( ) >> 4 ) % 20 );
    
    carma::canbus::Message msg(
        createId(true, getApi(), getNode(), BLANKING_FRAME_PACKET_1),
        getBusId());
    vector<byteType> data;
    
    sShortToData(data, nominalLrTilt + drift ); 
    sShortToData(data, nominalAfTilt - drift );  
    sShortToData(data, 5000); // 50 Degrees C
    
    msg.setData(data);
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message Tiltmeter::simBlankingFramePacket2()
{
    static bool aSwitch = false;
    aSwitch = !aSwitch; // Switch it every time we're called (every frame).
    
    carma::canbus::Message msg(
        createId(true, getApi(), getNode(), BLANKING_FRAME_PACKET_2),
        getBusId());
    vector<byteType> data;

    sShortToData(data, (aSwitch ? 4000 : 4100)); // 40.0 or 41.0 C
    sShortToData(data, (aSwitch ? 2000 : 2001)); // 20.0 or 20.01 C
    sShortToData(data, (aSwitch ? 1000 : 2000)); // 1V or 2V
    sShortToData(data, (aSwitch ? 10   : 20)); // 10mA or 20 mA
    
    msg.setData(data);
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message Tiltmeter::simBlankingFramePacket3()
{
    static bool aSwitch = false;
    aSwitch = !aSwitch; // Switch it every time we're called (every frame).
    
    carma::canbus::Message msg(
        createId(true, getApi(), getNode(), BLANKING_FRAME_PACKET_3),
        getBusId());
    vector<byteType> data;
    
    uByteToData(data, static_cast<unsigned char>(aSwitch)); // Off or Nominal
    sShortToData(data, (aSwitch ? 2500 : 10000)); // 25% or 100% power
    sShortToData(data, (aSwitch ? 10000 : 20000)); // 100 K or 200 K
    sShortToData(data, (aSwitch ? 1000 : 2000));  // 10 K s or 20 K s

    msg.setData(data);
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message Tiltmeter::simBlankingFramePacket4()
{
    static bool aSwitch = false;
    aSwitch = !aSwitch; // Switch it every time we're called (every frame).
    
    carma::canbus::Message msg(
        createId(true, getApi(), getNode(), BLANKING_FRAME_PACKET_4),
        getBusId());
    vector<byteType> data;
    
    sShortToData(data, (aSwitch ? 650 : 700)); // 65%/K or 70%/K
    sShortToData(data, (aSwitch ? 550 : 560)); // 55%K/s or 56%K/s
    sShortToData(data, (aSwitch ? 540 : 530)); // 54%s/K or 53%s/K
    sShortToData(data, (aSwitch ? 32000 : 26000)); // 320Hz or 260Hz

    msg.setData(data);
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message Tiltmeter::simBlankingFramePacket5()
{
    carma::canbus::Message msg(
        createId(true, getApi(), getNode(), BLANKING_FRAME_PACKET_5),
        getBusId());
    vector<byteType> data;
    
    sShortToData(data, 24000); // 24 V
    sShortToData(data, 12000); // 12 V
    sShortToData(data, -12000); // -12 V
    sShortToData(data, 5000); // 5 V

    msg.setData(data);
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message Tiltmeter::simBlankingFramePacket6()
{
    carma::canbus::Message msg(
        createId(true, getApi(), getNode(), BLANKING_FRAME_PACKET_6),
        getBusId());
    vector<byteType> data;
    
    sShortToData(data, 12000); // 12 V
    sShortToData(data, -12000); // -12 V
    sShortToData(data, 5000);  // 5 V
    sShortToData(data, 2000);  // 20 C
    msg.setData(data);
    return msg;
}

// -----------------------------------------------------------------------------
void Tiltmeter::setTemperature(float temp)
{
    carma::canbus::Message msg(
        createId(false, getApi(), getNode(), SET_TEMPERATURE), getBusId());
    vector<byteType> data;
    floatToData(data, temp);
    msg.setData(data);
    io_.postMessage(msg);
}


// -----------------------------------------------------------------------------
void Tiltmeter::regulateTemperature(OpMode opMode, float pwrFract)
{
    carma::canbus::Message msg(
        createId(false, getApi(), getNode(), REGULATE_TEMPERATURE), 
        getBusId());
    vector<byteType> data;
    uByteToData(data, static_cast<byteType>(opMode));
    floatToData(data, pwrFract);
    msg.setData(data);
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void Tiltmeter::setLoopGain(float gain)
{
    carma::canbus::Message msg(
        createId(false, getApi(), getNode(), SET_LOOP_GAIN), getBusId());
    vector<byteType> data;
    floatToData(data, gain);
    msg.setData(data);
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void Tiltmeter::setLoopIntegrationConstant(float loopInteg)
{
    carma::canbus::Message msg(
        createId(false, getApi(), getNode(), SET_LOOP_INT_CONSTANT), getBusId());
    vector<byteType> data;
    floatToData(data, loopInteg);
    msg.setData(data);
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void Tiltmeter::setLoopRateConstant(float rateConst)
{
    carma::canbus::Message msg(
        createId(false, getApi(), getNode(), SET_LOOP_RATE_CONSTANT), 
        getBusId());
    vector<byteType> data;
    floatToData(data, rateConst);
    msg.setData(data);
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void Tiltmeter::setLoopBandwidth(float bw)
{
    carma::canbus::Message msg(
        createId(false, getApi(), getNode(), SET_LOOP_BANDWIDTH), getBusId());
    vector<byteType> data;
    floatToData(data, bw);
    msg.setData(data);
    io_.postMessage(msg);
}
 
// -----------------------------------------------------------------------------
void Tiltmeter::writeLoopParametersToEEPROM()
{
    carma::canbus::Message msg(
        createId(false, getApi(), getNode(), WRITE_LOOP_PARAMS), getBusId());
    io_.postMessage(msg);
}
