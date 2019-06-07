/** @file
 * Tiltmeter class definition.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.3 $
 * $Date: 2011/01/03 18:48:03 $
 * $Id: Tiltmeter.cc,v 1.3 2011/01/03 18:48:03 iws Exp $
 *
 * $CarmaCopyright$
 */

// Carma includes
#include "carma/antenna/bima/Tiltmeter.h"
#include "carma/canbus/Utilities.h"
#include "carma/monitor/BimaSubsystem.h"
#include "carma/services/Angle.h"
#include "carma/util/Program.h"
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

const carma::canbus::apiType Tiltmeter::API_ID;
const char Tiltmeter::API_VERSION;
const double Tiltmeter::PACKET_LATE_THRESHOLD;
const carma::canbus::msgType Tiltmeter::SET_TEMPERATURE;
const carma::canbus::msgType Tiltmeter::REGULATE_TEMPERATURE;
const carma::canbus::msgType Tiltmeter::SET_LOOP_GAIN;
const carma::canbus::msgType Tiltmeter::SET_LOOP_INT_CONSTANT;
const carma::canbus::msgType Tiltmeter::SET_LOOP_RATE_CONSTANT;
const carma::canbus::msgType Tiltmeter::SET_LOOP_BANDWIDTH;
const carma::canbus::msgType Tiltmeter::WRITE_LOOP_PARAMS;
const carma::canbus::msgType Tiltmeter::BLANKING_FRAME_PACKET_1;
const carma::canbus::msgType Tiltmeter::BLANKING_FRAME_PACKET_2;
const carma::canbus::msgType Tiltmeter::BLANKING_FRAME_PACKET_3;
const carma::canbus::msgType Tiltmeter::BLANKING_FRAME_PACKET_4;
const carma::canbus::msgType Tiltmeter::BLANKING_FRAME_PACKET_5;
const carma::canbus::msgType Tiltmeter::BLANKING_FRAME_PACKET_6;

// -----------------------------------------------------------------------------
Tiltmeter::Tiltmeter(nodeType node, CanOutput &io, BimaSubsystem &bmon ) :
  XacDevice( API_ID, node, io ),
  log_( Program::getLogger() ),
  mon_( bmon.tiltmeter() ),
  inputMon_( bmon ) 
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
    short int lrTilt, afTilt, modTemp; 
    float lrTiltFloat, afTiltFloat;

    // Unpack the data
    lrTilt = dataToShort(data); 
    afTilt = dataToShort(data);
    modTemp = dataToShort(data);

    lrTiltFloat = lrTilt * 0.01; // Left Right Tilt in arcmin.
    afTiltFloat = afTilt * 0.01; // Forward Aft Tilt in arcmin.

    updateTiltMagAndDir(lrTiltFloat, afTiltFloat);

    // Convert and place into monitor system
    mon_.lrTilt().setValue(lrTiltFloat); // Units 0.01 arcmin
    mon_.afTilt().setValue(afTiltFloat); // Units 0.01 arcmin
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
    mon_.loopState().setValue(static_cast<TiltmeterModule::LoopStateMonitorPointEnum::LOOPSTATE>(loopState));
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
    static bool aSwitch = false;
    aSwitch = !aSwitch; // Switch it every time we're called (every frame).
    
    carma::canbus::Message msg(
        createId(true, getApi(), getNode(), BLANKING_FRAME_PACKET_1),
        getBusId());
    vector<byteType> data;
    
    sShortToData(data, (aSwitch ? 0 : -1000)); // 0.00 arcmin or -10.0 arcmin
    sShortToData(data, (aSwitch ? 100 : -999)); // 1.00 armin or -9.99 arcmin
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

// -----------------------------------------------------------------------------
void Tiltmeter::updateTiltMagAndDir(double lrTilt, double afTilt)
{
    // NOTE: I cannot vouch for the correctness of these calculations. They 
    // have been copied from legacy code which has been used at OVRO for many
    // years.  However, I was unable to rederive them from first principals, 
    // despite several attempts.  The main problem as I see it is that the
    // calculations treat tilts, which are angles, as vector components
    // which by definition need to be linear.  In the small angle case, this 
    // is probably correct, but as I said before, I was unable to verify this.
    // The original code is located in 
    // /ovmm/sat_sources/10m/mmtex/work/ippacket_sender.c
    
    static const double THRESHOLD = 0.0001; // 1/1000 arcminute.

    // Before we can do much of anything, we need to retrieve the newest frame
    // of monitor data from the drives.  If this fails, mark data as invalid
    // and get the hell out of dodge.
    if ( ! inputMon_.readNewest() ) {
        CPTRACE(Trace::TRACE5, "Tiltmeter::updateTiltMagAndDir() - "
            "Unable to read newest values from monitor system.");
        mon_.tiltDirection().setValidity( MonitorPoint::INVALID_NO_DATA );
        mon_.tiltMag().setValidity( MonitorPoint::INVALID_NO_DATA );
        return;
    }

    MonitorPointDouble& actualAz
      = inputMon_.antennaCommon().drive().track().actualAzimuth();
//    MonitorPointDouble& af0 = inputMon_.drive().point().constants().af0();
 //   MonitorPointDouble& lr0 = inputMon_.drive().point().constants().lr0();
    
    Angle lrTiltAngle(lrTilt, "arcminutes");
    Angle afTiltAngle(afTilt, "arcminutes");
    Angle azActualAngle(actualAz.getValue(), "degrees");
    Angle tiltAngle(0.0, "radians");
    Angle tiltMagnitude(0.0, "arcminutes");
//    Angle lr0Angle(lr0.getValue(), "arcminutes");
//    Angle af0Angle(af0.getValue(), "arcminutes");
    Angle lr0Angle(0.0, "arcminutes"); // BIMA doesn't have af0/lr0 yet
    Angle af0Angle(0.0, "arcminutes");

    CPTRACE(Trace::TRACE5, "Tiltmeter::updateTiltMagAndDir() -------------");

    CPTRACE(Trace::TRACE5, "Tiltmeter::updateTiltMagAndDir() - "
            << "lrTilt: " << lrTiltAngle.arcMinutes() 
            << ", afTilt: " << afTiltAngle.arcMinutes()
            << ", lr0: " << lr0Angle.arcMinutes()
            << ", af0: " << af0Angle.arcMinutes()
            << ", az: " << azActualAngle.degrees());
    
    // If either offsets are bad, then we can calculate neither the 
    // magnitude or direction of the tilt accurately - mark tilts as invalid
    // and get out of dodge.
    /*
    if ( af0.getValidity() < MonitorPoint::VALID ||
         lr0.getValidity() < MonitorPoint::VALID ) {

        CPTRACE(Trace::TRACE5, "Tiltmeter::UpdateTiltMagAndDir() - "
            << "Offset data (af0 & lr0) invalid.");
        
        mon_.tiltDirection().setValidity( MonitorPoint::INVALID_NO_DATA );
        mon_.tiltMag().setValidity( MonitorPoint::INVALID_NO_DATA );
        return;
    }
    */

    // Compensate for tiltmeter zeros.
    lrTiltAngle.reset( 
        lrTiltAngle.arcMinutes() - lr0Angle.arcMinutes(),
        "arcminutes");
    afTiltAngle.reset(
        afTiltAngle.arcMinutes() - af0Angle.arcMinutes(),
        "arcminutes");

    // If Actual Azimuth is valid, calculate tilt direction, otherwise
    // mark as invalid.
    if ( actualAz.getValidity() >= MonitorPoint::VALID ) {

        // If arctan doesn't diverge, calculate tiltangle, otherwise set
        // to sensible defaults.
        if ( fabs( afTiltAngle.arcMinutes() ) > THRESHOLD ) {
            tiltAngle.reset( 
                ::std::atan2( lrTiltAngle.arcMinutes(), 
                              afTiltAngle.arcMinutes() ),
                "radians" );
        } else {
            if ( lrTiltAngle.radians() < 0.0 ) {
                tiltAngle.reset( 3.0 * M_PI_2l, "radians" );
            } else {
                tiltAngle.reset( M_PI_2l, "radians" );
            }
        }
        
        CPTRACE(Trace::TRACE5, "Tiltmeter::UpdateTiltMagAndDir() - "
            << "tiltAngle in tiltmeter reference frame = "
            << tiltAngle.degrees() << " degrees.");

        // Put in telescope reference frame.
        tiltAngle += azActualAngle;
        mon_.tiltDirection().setValue(tiltAngle.degrees( true ));

        CPTRACE(Trace::TRACE5, "Tiltmeter::updateTiltMagAndDir() - "
            << "tiltAngle: " << tiltAngle.degrees());

    } else {
        
        // Actual azimuth is invalid, mark the tilt direction as INVALID_NO_DATA
        CPTRACE(Trace::TRACE5, "Tiltmeter::UpdateTiltMagAndDir() - "
            << "Azimuth is invalid.");
        mon_.tiltDirection().setValidity( MonitorPoint::INVALID_NO_DATA );
    }
        
    tiltMagnitude.reset( 
        hypot( afTiltAngle.arcMinutes(), lrTiltAngle.arcMinutes() ),
        "arcminutes" );  
    mon_.tiltMag().setValue( tiltMagnitude.arcMinutes() );
    
    CPTRACE(Trace::TRACE5, "Tiltmeter::UpdateTiltMagAndDir() - "
          << "Tilt magnitude: " << tiltMagnitude.arcMinutes()
          << ", from adjusted afTilt: " << afTiltAngle.arcMinutes() 
          << " rads, lrTilt: " << lrTiltAngle.arcMinutes() << ".");

    // Ciao!
}

