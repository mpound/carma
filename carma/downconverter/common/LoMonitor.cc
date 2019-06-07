/**@file
 * Class definition for Lo Monitor device API# 192.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.34 $
 * $Date: 2013/04/19 18:04:52 $
 * $Id: LoMonitor.cc,v 1.34 2013/04/19 18:04:52 abeard Exp $
 */

// STL includes
#include <iostream>
#include <sstream>

// Carma Tools includes
#include <log4cpp/Category.hh>
#include <log4cpp/Priority.hh>

// Carma includes
#include "carma/canbus/exceptions.h"
#include "carma/canbus/Utilities.h"
#include "carma/downconverter/common/LoMonitor.h"
#include "carma/monitor/CanbusCommon.h"
#include "carma/monitor/LoMonitor.h"
#include "carma/monitor/MonitorPoint.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/Program.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"

using namespace std;
using namespace carma;
using namespace carma::downconverter;
using namespace carma::canbus;
using namespace carma::canbus::devices;
using namespace carma::util;
using namespace log4cpp;

namespace { // Anonymous namespace for local constants and typedefs

    // API Id
    const carma::canbus::apiType API_ID                  = 192;

    // The CANbus API document version id.
    const char API_VERSION                               = 'G';

    // Controls commands message ids.
    const carma::canbus::msgType INITIALIZE_POWER_METER  = 0x040;

    // Fast Monitor frame message ids and ranges
    const carma::canbus::msgType BLANKING_FRAME_PACKET_1 = 0x0E0;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_2 = 0x0E1;
    const carma::canbus::msgType LO_STATUS_PACKET_BEGIN  = 0x0E2;
    const carma::canbus::msgType LO_STATUS_PACKET_END    = 0x0F2;

    namespace CM = carma::monitor;

} // End anonymous namespace

// -----------------------------------------------------------------------------
LoMonitor::LoMonitor(
        nodeType node,
        CanOutput &io,
        carma::monitor::StateMonitorPointEnum & state,
        carma::monitor::LoMonitor & lomon,
        carma::monitor::Xac & xac,
        const bool wideband ) :
    XacDevice(API_ID, node, io),
    state_( 0 ),
    mon_( 0 ),
    xacMon_( 0 ),
    log_(Program::getProgram().getLogger()),
    wideband_( wideband )
{
    ostringstream os;
    string objectId;

    // I don't like this, but I haven't yet solved the problem of making
    // const device instances equivalent to node 0 control.
    if (node != 0) {
        state_ = &state;
        mon_ = &lomon;
        xacMon_ = &xac;
    }

    // Initialize board parameters
    setBoardType(0);
    setSerialNumber(0);

    CPTRACE(Trace::TRACE6, "LoMonitor::LoMonitor() - Device class created "
        "for node " << getNode());
}

// -----------------------------------------------------------------------------
LoMonitor::~LoMonitor()
{
    CPTRACE(Trace::TRACE6, "LoMonitor::~LoMonitor() - Device class destroyed "
        "for node " << getNode());
}

// -----------------------------------------------------------------------------
apiType LoMonitor::getApiId()
{
    return API_ID;
}

// -----------------------------------------------------------------------------
void LoMonitor::updateFrameData()
{
    state_->setValue(static_cast< CM::StateMonitorPointEnum::STATE>(getState()));
}

// -----------------------------------------------------------------------------
map<msgType, string> LoMonitor::getHalfSecMonitors() const
{
    static bool init = false;
    static map<msgType, string> mons;

    if (!init) {
        ostringstream os;
        mons[BLANKING_FRAME_PACKET_1] = "BLANKING_FRAME_PACKET_1";
        mons[BLANKING_FRAME_PACKET_2] = "BLANKING_FRAME_PACKET_2";
        for (int i = LO_STATUS_PACKET_BEGIN; i < LO_STATUS_PACKET_END; i++) {
            int packetId = i - BLANKING_FRAME_PACKET_1;
            os << "BLANKING_FRAME_PACKET_" << packetId << ends;
            mons[i] = os.str();
            os.str("");
        }
        init = true;
    }
    return mons;
}

// -----------------------------------------------------------------------------
map<msgType, string> LoMonitor::getSlowMonitors() const
{
    return XacDevice::getSlowMonitors();
}

// -----------------------------------------------------------------------------
void LoMonitor::processMsg(msgType mid, vector<byteType> &data, bool sim)
{
    // If state is ONLINE, check to see if packet is late.
    if (getState() == ONLINE) {
        if (isPacketLate(200.0))
            incrementLatePacketCount();
    }

    // Process the message based on which mid we've received
    if (mid >= LO_STATUS_PACKET_BEGIN && mid <= LO_STATUS_PACKET_END) {
        // calculate LoId and type of LoStatus message
        int loId = (mid - LO_STATUS_PACKET_BEGIN)/2;
        if ((mid - LO_STATUS_PACKET_BEGIN)%2 == 0) {
            processLoFrequencyPacket(data, loId);
        } else {
            processLoStatusPacket(data, loId);
        }
    } else {
        switch (mid) {
            case BLANKING_FRAME_PACKET_1:
                processBlankingFramePacket1(data);
                break;
            case BLANKING_FRAME_PACKET_2:
                processBlankingFramePacket2(data);
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
                // I don't know how to process this message id!
                // Not a problem, just log it.
                log_ << Priority::DEBUG << "LoMonitor::processMsg() - "
                    << "Switch does not match any case: Unknown mid "
                     << mid << ". Node " << getNode();
                break; // Good habit.
        }
    }
}

// -----------------------------------------------------------------------------
carma::canbus::Message LoMonitor::simulateMsg(msgType mid)
{
    carma::canbus::Message msg;

    if (mid >= LO_STATUS_PACKET_BEGIN && mid <= LO_STATUS_PACKET_END) {
        // Calculate LOId and type of status message and
        // call appropriate simLoStatusMessage routine.
        // calculate LoId and type of LoStatus message
        int loId = (mid - LO_STATUS_PACKET_BEGIN)/2;
        if ((mid - LO_STATUS_PACKET_BEGIN)%2 == 0) {
            msg = simLoFrequencyPacket(loId);
        } else {
            msg = simLoStatusPacket(loId);
        }
    } else {
        switch (mid) {
            case BLANKING_FRAME_PACKET_1:
                msg = simBlankingFramePacket1();
                break;
            case BLANKING_FRAME_PACKET_2:
                msg = simBlankingFramePacket2();
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
                // I don't know how to process this message!
                // Not a problem, just log it.
                log_ << Priority::DEBUG << "LoMonitor::simulateMsg() - "
                    << "Switch does not match any case: mid "
                    << mid;
                break;
        }
    }
    return msg;
}

// -----------------------------------------------------------------------------
void LoMonitor::processBlankingFramePacket1(vector<byteType> &data)
{
    unsigned char pmStat, pmChan;

    // This check exists to provide a higher level of error resolution to a
    // developer.  If it didn't exist here, the below dataToXXX routines would
    // still throw BadDataSizeExceptions, however the user would have no way
    // of knowing which processXXX routine it came from.
    if (data.size() < 2) {
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "LoMonitor::processBlankingFramePacket1 - Invalid data size.");
    }

    // Unpack data
    pmStat = dataToUbyte(data);
    pmChan = dataToUbyte(data);

    // Convert and stuff into monitor system
    mon_->pmStat().setValue(static_cast<
        carma::monitor::LoMonitor::PmStatMonitorPointEnum::PMSTAT>(pmStat));
    mon_->pmChannel().setValue(
        static_cast<short>(pmChan));
}

// -----------------------------------------------------------------------------
void LoMonitor::processBlankingFramePacket2(vector<byteType> &data)
{
    short ps5vAnalog, ps5vDigital, ps24v, temp;

    if (data.size() < 8) {
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "LoMonitor::processBlankingFramePacket2 - Invalid data size.");
    }

    // Unpack data
    ps5vAnalog = dataToShort(data);
    ps5vDigital = dataToShort(data);
    ps24v = dataToShort(data);
    temp = dataToShort(data);

    // Convert and stuff into monitor system
    mon_->ps5vAnalog().setValue(ps5vAnalog * 0.001);   // Units mV
    mon_->ps5vDigital().setValue(ps5vDigital * 0.001); // Units mV
    mon_->ps24v().setValue(ps24v * 0.001);             // Units mV
    mon_->temp().setValue(temp * 0.01);                // Units 0.01 C
}

// -----------------------------------------------------------------------------
void LoMonitor::processLoFrequencyPacket(vector<byteType> &data, int loId)
{
    double freq;

    if (data.size() < 8) {
        ostringstream os;
        os << "LoMonitor::processLoFrequencyPacket" << loId << " - "
           << "Invalid data size.";
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException, os);
    }

    // There is no frequency information on the wideband system.
    if ( wideband_ ) 
        mon_->loChan(loId).loFreq().setValidity( 
            carma::monitor::MonitorPoint::INVALID_NO_HW );
    else {
        // Unpack data
        freq = dataToDouble(data);

        // Stuff into monitor system
        mon_->loChan(loId).loFreq().setValue(freq); // Units Hz
    }
}

// -----------------------------------------------------------------------------
void LoMonitor::processLoStatusPacket(vector<byteType> &data, int loId)
{
    short phaselockVolts, phaselockPow, plo2Volts;
    unsigned char phaselockStat, measureStat;

    if (data.size() < 6) {
        ostringstream os;
        os << "LoMonitor::processLoStatusPacket" << loId << " - "
           << "Invalid data size.";
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException, os);
    }

    // Unpack data
    phaselockVolts = dataToShort(data);
    phaselockStat = dataToUbyte(data);
    phaselockPow = dataToShort(data);
    measureStat = dataToUbyte(data);
    // On the wideband system, chans 3,4,5 & 8 only contain a 2nd Lo Voltage.
    const int chan( loId + 1 ); 
    if ( !wideband_ || chan == 3 || chan == 4 || chan == 5 ) {
        plo2Volts = dataToShort(data);
        mon_->loChan(loId).lo2Voltage().setValue(plo2Volts * 0.001); // mV
    } else { 
        mon_->loChan(loId).lo2Voltage().setValidity( 
            carma::monitor::MonitorPoint::INVALID_NO_HW );
    }

    // Convert and stuff into monitor system
    if ( wideband_ && ( chan > 5 ) ) {
        mon_->loChan(loId).loVoltage().setValidity( 
                carma::monitor::MonitorPoint::INVALID_NO_HW );
        mon_->loChan(loId).lockStat().setValidity( 
                carma::monitor::MonitorPoint::INVALID_NO_HW );
        mon_->loChan(loId).loPower().setValidity( 
                carma::monitor::MonitorPoint::INVALID_NO_HW );
        mon_->loChan(loId).measureStat().setValidity(
                carma::monitor::MonitorPoint::INVALID_NO_HW );
    } else { 
        mon_->loChan(loId).loVoltage().setValue(phaselockVolts * 0.001); // mV
        mon_->loChan(loId).lockStat().setValue(static_cast<
            monitor::LoMonitor::LockStatMonitorPointEnum::LOCKSTAT>(
                phaselockStat));
        mon_->loChan(loId).loPower().setValue(phaselockPow * 0.01); // 0.01 dBm
        mon_->loChan(loId).measureStat().setValue(static_cast<
            monitor::LoMonitor::MeasureStatMonitorPointEnum::MEASURESTAT>(
                    measureStat));
    }
}

// -----------------------------------------------------------------------------
carma::canbus::Message LoMonitor::simBlankingFramePacket1()
{
    const unsigned char state( 1 ); // setting up
    const unsigned char loChan( 5 ); 
    carma::canbus::Message msg = createMsgToHost(BLANKING_FRAME_PACKET_1);
    msg << state << loChan;
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message LoMonitor::simBlankingFramePacket2()
{
    short ps5v = 5000, ps24v = 24000, temp = 4000;
    carma::canbus::Message msg = createMsgToHost(BLANKING_FRAME_PACKET_2);
    msg << ps5v << ps5v << ps24v << temp;
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message LoMonitor::simLoFrequencyPacket(int loId)
{
    double loFreq = 2.00 + (0.5 * loId);
    carma::canbus::Message msg = createMsgToHost(LO_STATUS_PACKET_BEGIN +
        (loId * 2));
    msg << loFreq;
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message LoMonitor::simLoStatusPacket(int loId)
{
    const short phaseLockVolts( 5000 + loId * 100 );
    const unsigned char phaseLockStat( 1 ); // locked
    const short power( 100 * loId );
    const unsigned char loStatus( loId % 2 );
    const short phaseLock2Volts( 4900 + loId * 100 );
    
    carma::canbus::Message msg = createMsgToHost(LO_STATUS_PACKET_BEGIN +
        (loId * 2) + 1);
    msg << phaseLockVolts << phaseLockStat << power << loStatus 
        << phaseLock2Volts;
    return msg;
}

// -----------------------------------------------------------------------------
void LoMonitor::initializePowerMeter()
{
    try {
        static const unsigned char magic[8] = {
            0x1E,
            0xA5,
            0x69,
            0xC3,
            0x3C,
            0x96,
            0x5A,
            0xE1 };
        carma::canbus::Message msg = createMsgToNode(INITIALIZE_POWER_METER);
        for (unsigned i = 0; i < 8; ++i) msg << magic[i];
        io_.postMessage(msg);
        log_ << Priority::INFO << "LoMonitor::initializePowerMeter() - "
            << "Initializing power meter.";
    } catch (std::exception &ex) {
        throw CARMA_EXCEPTION(carma::util::UserException, ex.what());
    }
}

