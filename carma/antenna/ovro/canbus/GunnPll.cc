/** @file
 * CAN Device class definition for the Bias-Tuned Gunn PLL (API 16).
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.19 $
 * $Date: 2012/08/28 17:46:08 $
 * $Id: GunnPll.cc,v 1.19 2012/08/28 17:46:08 abeard Exp $
 *
 * $CarmaCopyright$
 */

// Carma includes
#include "carma/antenna/ovro/canbus/GunnPll.h"
#include "carma/canbus/Utilities.h"
#include "carma/monitor/OvroSubsystem.h"
#include "carma/util/ErrorException.h"
#include "carma/util/Program.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"

// Carma Tools includes
#include <log4cpp/Category.hh>
#include <log4cpp/Priority.hh>

// STL includes
#include <iostream>
#include <iomanip>

using namespace carma::antenna::ovro;
using namespace carma::canbus;
using namespace carma::util;
using namespace log4cpp;
using namespace std;

namespace CM = carma::monitor;

namespace { // Anonymous namespace for local constants, typedefs, etc.

    // API version this class was implemented from 
    const char API_VERSION                                           = 'B';

    // API Id for this device
    const carma::canbus::apiType API_ID                              = 16;

    // Controls
    const carma::canbus::msgType SET_LO_FREQUENCY                    = 0x040;
    const carma::canbus::msgType SET_GUNN_VOLTAGE                    = 0x080;
    const carma::canbus::msgType SET_LOOP_GAIN                       = 0x081;
    const carma::canbus::msgType ENABLE_GUNN                         = 0x082;
    const carma::canbus::msgType ENABLE_SWEEP                        = 0x083;
    const carma::canbus::msgType ENABLE_IF_MONITOR                   = 0x084;
    const carma::canbus::msgType SET_TUNER                           = 0x085;
    const carma::canbus::msgType SET_BACKSHORT                       = 0x086;
    const carma::canbus::msgType SET_ATTENUATOR                      = 0x087; 
    const carma::canbus::msgType JOG_TUNER                           = 0x088;
    const carma::canbus::msgType JOG_BACKSHORT                       = 0x089;
    const carma::canbus::msgType JOG_ATTENUATOR                      = 0x08A;

    // Monitors
    const carma::canbus::msgType BLANKING_FRAME_PACKET_1             = 0x0E0;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_2             = 0x0E1;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_3             = 0x0E2;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_4             = 0x0E3;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_5             = 0x0E4;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_6             = 0x0E5;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_7             = 0x0E6;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_8             = 0x0E7;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_9             = 0x0E8;

    // Other constants
    const carma::canbus::nodeType SZA_3MM_GUNN_NODE                  = 1;
    const carma::canbus::nodeType OVRO_3MM_GUNN_NODE                 = 2;
    const carma::canbus::nodeType OVRO_1MM_GUNN_NODE                 = 3;

    typedef carma::monitor::GunnPll CMGP;

} // End anonymous namespace

// -----------------------------------------------------------------------------
GunnPll::GunnPll( nodeType node, 
                  CanOutput & co, 
                  carma::monitor::OvroSubsystem & subsys ) : 
    XacDevice(API_ID, node, co), 
    log_( Program::getLogger() )
{
    // Initialize monitor system based on node id.
    switch (node) {
        case OVRO_3MM_GUNN_NODE:
            state_ = &( subsys.gunn3mm().state() );
            mon_ = &( subsys.gunn3mm().gunnPll() );
            xacMon_ = &( subsys.gunn3mm().xac() );
            break;
        case OVRO_1MM_GUNN_NODE:
            state_ = &( subsys.gunn1mm().state() );
            mon_ = &( subsys.gunn1mm().gunnPll() );
            xacMon_ = &( subsys.gunn1mm().xac() );
            break;
        default:
            throw CARMA_EXCEPTION(ErrorException, "GunnPll::GunnPll() - "
                "Invalid node id!  Valid node IDs for the Bias Tuned Gunn "
                "are 2 (OVRO 3MM Gunn) and 3 (OVRO 1MM Gunn) only.");
            break;
    }

    CPTRACE(Trace::TRACE6, "GunnPll::GunnPll() - Device class created for "
        "api " << API_ID << " node " << getNode());
}

// -----------------------------------------------------------------------------
GunnPll::~GunnPll()
{
    CPTRACE(Trace::TRACE6, "GunnPll::~GunnPll() - Device class destroyed for "
        "api " << API_ID << " node " << getNode());
}

// -----------------------------------------------------------------------------
MsgIdInfoMap GunnPll::getHalfSecMonitors() const
{
    static MsgIdInfoMap tmp;
    static bool init = false;

    if (!init) {
        tmp[BLANKING_FRAME_PACKET_1] = "GunnPll::BLANKING_FRAME_PACKET_1"; 
        tmp[BLANKING_FRAME_PACKET_2] = "GunnPll::BLANKING_FRAME_PACKET_2"; 
        tmp[BLANKING_FRAME_PACKET_3] = "GunnPll::BLANKING_FRAME_PACKET_3"; 
        tmp[BLANKING_FRAME_PACKET_4] = "GunnPll::BLANKING_FRAME_PACKET_4"; 
        tmp[BLANKING_FRAME_PACKET_5] = "GunnPll::BLANKING_FRAME_PACKET_5"; 
        tmp[BLANKING_FRAME_PACKET_6] = "GunnPll::BLANKING_FRAME_PACKET_6"; 
        tmp[BLANKING_FRAME_PACKET_7] = "GunnPll::BLANKING_FRAME_PACKET_7"; 
        tmp[BLANKING_FRAME_PACKET_8] = "GunnPll::BLANKING_FRAME_PACKET_8"; 
        tmp[BLANKING_FRAME_PACKET_9] = "GunnPll::BLANKING_FRAME_PACKET_9"; 
        init = true;
    }
    return tmp;
}

// -----------------------------------------------------------------------------
MsgIdInfoMap GunnPll::getSlowMonitors() const
{
    return XacDevice::getSlowMonitors();
}

// -----------------------------------------------------------------------------
void GunnPll::updateFrameData()
{
    // Set the state of the device...
    state_->setValue(
        static_cast<CM::StateMonitorPointEnum::STATE>(getState()));
}

// Control commands
// -----------------------------------------------------------------------------
void GunnPll::setLoFrequency( double freq ) const
{
    unsigned short raw = static_cast<unsigned short>( round( freq * 100.0) );
    ::carma::canbus::Message msg = createMsgToNode(SET_LO_FREQUENCY);
    msg << raw;
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void GunnPll::setGunnOperatingVoltage(float voltage) const
{
    unsigned short raw = static_cast<unsigned short>(round( voltage * 100.0 )); 
    ::carma::canbus::Message msg = createMsgToNode(SET_GUNN_VOLTAGE);
    msg << raw;
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void GunnPll::setLoopGain(float gain) const
{
    unsigned short raw = static_cast<unsigned short>(round( gain * 10.0 ));
    ::carma::canbus::Message msg = createMsgToNode(SET_LOOP_GAIN);
    msg << raw;
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void GunnPll::enableGunn(bool on) const
{
    unsigned char enable = ( on ? 0x01 : 0x00); 
    ::carma::canbus::Message msg = createMsgToNode(ENABLE_GUNN);
    msg << enable;
    io_.postMessage(msg);
}
    
// -----------------------------------------------------------------------------
void GunnPll::disableAllGunns() const
{
    unsigned char disable = 0x00; 
    ::carma::canbus::Message msg = createMsgToAllNodes(ENABLE_GUNN);
    msg << disable;
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void GunnPll::toggleSweep(bool on) const
{
    ::carma::canbus::Message msg = createMsgToNode(ENABLE_SWEEP);
    unsigned char enable = ( on ? 0x01 : 0x00); 
    msg << enable;
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void GunnPll::enableIfMonitorOutput(bool on) const
{
    ::carma::canbus::Message msg = createMsgToNode(ENABLE_IF_MONITOR);
    unsigned char enable = ( on ? 0x01 : 0x00); 
    msg << enable;
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void GunnPll::setTuner(unsigned long pos) const
{
    ::carma::canbus::Message msg = createMsgToNode(SET_TUNER);
    msg << pos;
    io_.postMessage(msg);
}
    
// -----------------------------------------------------------------------------
void GunnPll::setBackshort(unsigned long pos) const
{
    ::carma::canbus::Message msg = createMsgToNode(SET_BACKSHORT);
    msg << pos;
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void GunnPll::setAttenuator(unsigned long pos) const
{
    ::carma::canbus::Message msg = createMsgToNode(SET_ATTENUATOR);
    msg << pos;
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void GunnPll::jogTuner(short microsteps) const
{
    ::carma::canbus::Message msg = createMsgToNode(JOG_TUNER);
    msg << microsteps;
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void GunnPll::jogBackshort(short microsteps) const
{
    ::carma::canbus::Message msg = createMsgToNode(JOG_BACKSHORT); 
    msg << microsteps;
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void GunnPll::jogAttenuator(short microsteps) const
{
    ::carma::canbus::Message msg = createMsgToNode(JOG_ATTENUATOR);
    msg << microsteps;
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void GunnPll::processMsg(msgType mid, DataVector & data, bool sim)
{
    
    CPTRACE(Trace::TRACEALL, "GunnPll::processMsg() - Processing "
        << (sim ? "simulated" : "unsimulated") << " msg 0x"
        << hex << mid << dec << " for node " << getNode() << ".");

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
        case BLANKING_FRAME_PACKET_8:
            processBlankingFramePacket8(data);
            break;
        case BLANKING_FRAME_PACKET_9:
            processBlankingFramePacket9(data);
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_1:
            XacDevice::processSystemMonitorPacket1(data, *xacMon_);
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_2:
            XacDevice::processSystemMonitorPacket2(data, *xacMon_);
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_3:
            XacDevice::processSystemMonitorPacket3(data, *xacMon_);
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_4:
            XacDevice::processSystemMonitorPacket4(data, *xacMon_);
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_5:
            XacDevice::processSystemMonitorPacket5(data, *xacMon_); 
            break;
        default:
            // Unexpected messae id... log it
            log_ << Priority::DEBUG << "GunnPll::processMsg() - "
                << "Switch does not match any case: Unknown mid "
                << mid << ". Node " << getNode();
            CPTRACE(Trace::TRACE6, "GunnPll::processMsg() - "
                "Switch doesn't match any case. mid 0x" << hex << mid
                << dec << " node " << getNode() << ".");
            break;
    }
}

// -----------------------------------------------------------------------------
carma::canbus::Message GunnPll::simulateMsg(msgType mid)
{
    carma::canbus::Message msg;
    CPTRACE(Trace::TRACEALL, "GunnPll::simulateMsg() - Simulating msg "
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
        case BLANKING_FRAME_PACKET_8:
            msg = simBlankingFramePacket8();
            break;
        case BLANKING_FRAME_PACKET_9:
            msg = simBlankingFramePacket9();
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
            // Log messages I don't know how to log.
            log_ << Priority::DEBUG << "GunnPll::simulateMsg - "
                << "Switch does not match any case: mid " << mid;
            CPTRACE(Trace::TRACE6, "GunnPll::simulateMsg() - "
                    "Switch doesn't match any case.  mid 0x" << hex  << mid
                    << dec << " node " << getNode() << ".");
            break;
    }
    return msg;
}

// -----------------------------------------------------------------------------
void GunnPll::processBlankingFramePacket1(DataVector & data)
{
    const unsigned char lockStat = dataToUbyte(data);
    const unsigned char lockBit = dataToUbyte(data);
    const unsigned char refStat = dataToUbyte(data);
    const unsigned char sweepStat = dataToUbyte(data);
    const unsigned char gunnStat = dataToUbyte(data);
    const unsigned char dataStat = dataToUbyte(data);
    const unsigned char relockStat = dataToUbyte(data);
    const unsigned char relockCount = dataToUbyte(data);

    // Place into monitor system.
    mon_->lockState().setValue( 
        static_cast<CMGP::LockStateMonitorPointEnum::LOCKSTATE>(lockStat));
    mon_->lockBit().setValue(
        static_cast<CMGP::LockBitMonitorPointEnum::LOCKBIT>(lockBit));
    mon_->refStat().setValue(
        static_cast<CMGP::RefStatMonitorPointEnum::REFSTAT>(refStat));
    mon_->sweepStat().setValue(
        static_cast<CMGP::SweepStatMonitorPointEnum::SWEEPSTAT>(sweepStat));
    mon_->gunnStat().setValue(
        static_cast<CMGP::GunnStatMonitorPointEnum::GUNNSTAT>(gunnStat));
    mon_->dataStat().setValue(
        static_cast<CMGP::DataStatMonitorPointEnum::DATASTAT>(dataStat));
    mon_->autoRelockStat().setValue(static_cast<
        CMGP::AutoRelockStatMonitorPointEnum::AUTORELOCKSTAT>(relockStat));
    mon_->relockCount().setValue(relockCount);
}

// -----------------------------------------------------------------------------
void GunnPll::processBlankingFramePacket2(DataVector & data)
{
    const unsigned short gunnId = dataToUshort(data);
    const unsigned short gunnVoltage = dataToUshort(data);
    const unsigned short multiplier = dataToUshort(data);
    const unsigned char freqStat = dataToUbyte(data);
    const unsigned char ifMonStat = dataToUbyte( data );

    mon_->gunnId().setValue( static_cast<int>(gunnId) );
    mon_->gunnVoltage().setValue( gunnVoltage / 100.0 );
    mon_->multiplier().setValue( static_cast<int>(multiplier) );
    mon_->freqRange().setValue( 
        static_cast<CMGP::FreqRangeMonitorPointEnum::FREQRANGE>(freqStat) );
    mon_->ifMonStat().setValue(
        static_cast<CMGP::IfMonStatMonitorPointEnum::IFMONSTAT>( ifMonStat ) );
}

// -----------------------------------------------------------------------------
void GunnPll::processBlankingFramePacket3(DataVector & data)
{
    const unsigned char calStat = dataToUbyte(data);
    const unsigned char month = dataToUbyte(data);
    const unsigned char day = dataToUbyte(data);
    const unsigned char year = dataToUbyte(data);
    const unsigned char nZabers = dataToUbyte( data );
    const unsigned char allZabers = dataToUbyte( data );

    string dateString;
    {
        ostringstream calDate;

        calDate << static_cast<int>(month) << "/"
            << static_cast<int>(day) << "/"
            << setw(2) << setfill('0') << static_cast<int>(year);
        
        dateString = calDate.str( );
    }

    mon_->calTblState().setValue(
        static_cast<CMGP::CalTblStateMonitorPointEnum::CALTBLSTATE>(calStat) );
    mon_->calDate().setValue( dateString );
    mon_->nZabers().setValue( nZabers );
    mon_->allZabers().setValue( static_cast<bool>( allZabers ) );
}

// -----------------------------------------------------------------------------
void GunnPll::processBlankingFramePacket4(DataVector & data)
{
    const unsigned short freq = dataToUshort(data);
    const unsigned short gain = dataToUshort(data);
    const unsigned int pos = dataToUlong(data);
    
    mon_->gunnFreq().setValue( freq / 100.0 );
    mon_->loopGain().setValue( gain / 100.0 );
    mon_->tunerPos().setValue( static_cast<int>(pos) );
}

// -----------------------------------------------------------------------------
void GunnPll::processBlankingFramePacket5(DataVector & data)
{
    const unsigned int bsPos = dataToUlong(data);
    const unsigned int attenPos = dataToUlong(data);

    mon_->backshortPos().setValue( static_cast<int>(bsPos) );
    mon_->attenPos().setValue( static_cast<int>(attenPos) );
}

// -----------------------------------------------------------------------------
void GunnPll::processBlankingFramePacket6(DataVector & data)
{
    const short ifLev = dataToShort(data);
    const short vErr = dataToShort(data);
    const unsigned short current = dataToUshort(data);
    const short noiseMeter = dataToShort(data);

    mon_->ifLevel().setValue(ifLev);
    mon_->vError().setValue(vErr);
    mon_->gunnCurrent().setValue( static_cast<int>(current) );
    mon_->noiseMeterV().setValue( static_cast<int>(noiseMeter) );
}

// -----------------------------------------------------------------------------
void GunnPll::processBlankingFramePacket7(DataVector & data)
{
    const short temp = dataToShort(data);
    const short ps24v = dataToShort(data);
    const short ps5v = dataToShort(data);
    const short ps15v = dataToShort(data);

    mon_->temp().setValue( temp / 100.0 );
    mon_->ps24v().setValue( ps24v / 1000.0 );
    mon_->ps5vDigital().setValue( ps5v / 1000.0 );
    mon_->ps15vAnalog().setValue( ps15v / 1000.0 );

}

// -----------------------------------------------------------------------------
void GunnPll::processBlankingFramePacket8(DataVector & data)
{
    const short ps12v = dataToShort(data);
    const short ps5v = dataToShort(data);
    const short psNeg12v = dataToShort(data);
    const short ps6v = dataToShort(data);

    mon_->ps12vAnalog().setValue( ps12v / 1000.0 );
    mon_->ps5vAnalog().setValue( ps5v / 1000.0 );
    mon_->psNeg12vAnalog().setValue( psNeg12v / 1000.0 );
    mon_->ps6vAnalog().setValue( ps6v / 1000.0 );
}

// -----------------------------------------------------------------------------
void GunnPll::processBlankingFramePacket9(DataVector & data)
{
    const unsigned char crowbarState = dataToUbyte( data );
    const unsigned short crowbarCount = dataToUshort( data );

    mon_->crowbarState().setValue( 
        static_cast<CMGP::CrowbarStateMonitorPointEnum::CROWBARSTATE>( 
            crowbarState ) );
    mon_->crowbarCount().setValue( static_cast<int>( crowbarCount ) );
}

// -----------------------------------------------------------------------------
::carma::canbus::Message GunnPll::simBlankingFramePacket1()
{
    unsigned char lockStat = 0x07 * ( ( Time::computeCurrentFrame( ) / 10 ) % 2 );
    unsigned char lockBit = 0x00, refStat = 0x00;
    unsigned char sweepStat = 0x00, gunnStat = 0x00, dataStat = 0x00;
    unsigned char relockStat = 0x00, relockCount = 0xff;
    ::carma::canbus::Message msg = createMsgToHost(BLANKING_FRAME_PACKET_1);

    msg << lockStat << lockBit << refStat << sweepStat << gunnStat
        << dataStat << relockStat << relockCount;
    return msg;
}

// -----------------------------------------------------------------------------
::carma::canbus::Message GunnPll::simBlankingFramePacket2()
{
    const unsigned short gunnId = 0xDEAD, gunnV = 100, multiplier = 3;
    const unsigned char freqRange = 0x01; // Too Low
    const unsigned char ifMonStat = 0x00; // OFF
    ::carma::canbus::Message msg = createMsgToHost(BLANKING_FRAME_PACKET_2);
    msg << gunnId << gunnV << multiplier << freqRange << ifMonStat;
    return msg;
}

// -----------------------------------------------------------------------------
::carma::canbus::Message GunnPll::simBlankingFramePacket3()
{
    const unsigned char state = 0x01, month = 1, day = 1, year = 0;
    const unsigned char nZabers = 4, allZabers = 1;
    ::carma::canbus::Message msg = createMsgToHost(BLANKING_FRAME_PACKET_3);
    msg << state << month << day << year << nZabers << allZabers;
    return msg;
}

// -----------------------------------------------------------------------------
::carma::canbus::Message GunnPll::simBlankingFramePacket4()
{
    const unsigned short freq = 1221, gain = 5001;
    const unsigned long int tunerPos = 1;
    ::carma::canbus::Message msg = createMsgToHost(BLANKING_FRAME_PACKET_4);
    msg << freq << gain << tunerPos;
    return msg;
}

// -----------------------------------------------------------------------------
::carma::canbus::Message GunnPll::simBlankingFramePacket5()
{
    unsigned long int backshortPos = 2, attenuationPos = 2;
    ::carma::canbus::Message msg = createMsgToHost(BLANKING_FRAME_PACKET_5);

    msg << backshortPos << attenuationPos;
    return msg;
}

// -----------------------------------------------------------------------------
::carma::canbus::Message GunnPll::simBlankingFramePacket6()
{
    short ifLevel = 500, vError = 5, noiseMeter = 600;
    unsigned short current = 700;
    ::carma::canbus::Message msg = createMsgToHost(BLANKING_FRAME_PACKET_6);

    msg << ifLevel << vError << current << noiseMeter;
    return msg;
}

// -----------------------------------------------------------------------------
::carma::canbus::Message GunnPll::simBlankingFramePacket7()
{
    short temp = 400, ps24v = 24000, ps5v = 5000, ps15v = 15000;
    ::carma::canbus::Message msg = createMsgToHost(BLANKING_FRAME_PACKET_7);

    msg << temp << ps24v << ps5v << ps15v;
    return msg;
}

// -----------------------------------------------------------------------------
::carma::canbus::Message GunnPll::simBlankingFramePacket8()
{
    short ps12v = 12000, ps5v = 5000, psNeg12v = -12000, ps6v = 6000;
    ::carma::canbus::Message msg = createMsgToHost(BLANKING_FRAME_PACKET_8);

    msg << ps12v << ps5v << psNeg12v << ps6v;
    return msg;
}

// -----------------------------------------------------------------------------
::carma::canbus::Message GunnPll::simBlankingFramePacket9()
{
    const unsigned char crowbarState = 0x00;
    const unsigned short crowbarCount = 111;
    
    ::carma::canbus::Message msg = createMsgToHost(BLANKING_FRAME_PACKET_9);

    msg << crowbarState << crowbarCount;

    return msg;
}
