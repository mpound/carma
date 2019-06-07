/**@file
 * Class definition for Cobra Wide Downconverter device API# 130.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.53 $
 * $Date: 2012/01/31 00:22:03 $
 * $Id: Downconverter.cc,v 1.53 2012/01/31 00:22:03 abeard Exp $
 */

#include "carma/downconverter/common/Downconverter.h"

#include "carma/canbus/exceptions.h"
#include "carma/canbus/Utilities.h"
#include "carma/canbus/devices/XacDevice.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"

using namespace carma::downconverter;
using namespace carma::canbus;
using namespace carma::canbus::devices;
using namespace carma::util;
using namespace carma::monitor;
using namespace log4cpp;
using namespace std;

namespace { // Anonymous namespace for local constants.

    // The controls.
    const carma::canbus::msgType SET_PSYS_POWER_TO_PRESET    = 0x040;
    const carma::canbus::msgType SET_PSYS_POWER_TO_REQSTD    = 0x041;
    const carma::canbus::msgType SET_PSYS_ATTEN_TO_REQSTD    = 0x042;
    const carma::canbus::msgType SET_IFOUT_POWER_TO_PRESET	 = 0x043;
    const carma::canbus::msgType SET_IFOUT_POWER_TO_REQSTD   = 0x044;
    const carma::canbus::msgType SET_IFOUT_ATTEN_TO_REQSTD   = 0x045;
    const carma::canbus::msgType SET_AMP_POWER_SWITCH        = 0X046;
    const carma::canbus::msgType SET_IFOUT_ALC_LOOP_SWITCH   = 0x047;

    // Fast monitors.
    const carma::canbus::msgType BLANKING_FRAME_PACKET_1     = 0x0E0;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_2     = 0x0E1;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_3     = 0x0E2;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_4     = 0x0E3;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_5     = 0x0E4;

    // Slow monitors.  Most of these are handled by the XacDevice
    // base class.  Any system monitors handled here, overload routines
    // in that class.
    const carma::canbus::msgType SYSTEM_MONITOR_PACKET_4     = 0x123;

#define USE_API_131 1

#if USE_API_131
    const carma::canbus::apiType API_ID                      = 131;
    const char API_VERSION                                   = 'A';
#else
    const carma::canbus::apiType API_ID                      = 130;
    const char API_VERSION                                   = 'B';
#endif

    string makeStringId( const unsigned inputNo, const unsigned bandNo )
    {
        ostringstream oss;
        oss << "Downconverter( inputNo = " << inputNo << ", "
            << " bandNo = " << bandNo << " )";
        return oss.str();
    }

} // End anonymous namespace

// -----------------------------------------------------------------------------
Downconverter::Downconverter( nodeType node, 
                              CanOutput &io, 
                              WbdcSubsystem & wbdcSubsys ) :
    XacDevice( API_ID, node, io ),
    inputNo_((node/0x10)), bandNo_( node == 0 ? 0 : (node%0x10) + 1 ),
    stringId_( makeStringId( inputNo_, bandNo_ ) ),
    mon_( node == 0 ? 0 : 
          &( wbdcSubsys.band( bandNo_ - 1 ).input( inputNo_ - 1 ) ) ),
    ifOutputPower_( 0.0 ),
    commandLoggingEnabled_( true )
{
    setBoardType(0);
    setSerialNumber(api_ + node_);
    setApiVersion(API_VERSION);
}

// -----------------------------------------------------------------------------
Downconverter::~Downconverter()
{
	// Nothing
}

// -----------------------------------------------------------------------------
nodeType Downconverter::calculateNodeId(short input, short band)
{
   nodeType node = (input * 0x10) + (band - 1);
   return node;
}

// -----------------------------------------------------------------------------
apiType Downconverter::getApiId()
{
    return API_ID;
}

// -----------------------------------------------------------------------------
void Downconverter::updateFrameData()
{
    // This guarantees that this is done every half second...
    mon_->state().setValue(static_cast<StateMonitorPointEnum::STATE>(getState()));
    mon_->bandNo().setValue( bandNo_ );
    mon_->inputNo().setValue( inputNo_ );
}

// -----------------------------------------------------------------------------
map<msgType, string> Downconverter::getHalfSecMonitors() const
{
    static bool init = false;
    static map<msgType, string> mons;

    if (!init) {
        mons[BLANKING_FRAME_PACKET_1] = "BLANKING_FRAME_PACKET_1";
        mons[BLANKING_FRAME_PACKET_2] = "BLANKING_FRAME_PACKET_2";
        mons[BLANKING_FRAME_PACKET_3] = "BLANKING_FRAME_PACKET_3";
        mons[BLANKING_FRAME_PACKET_4] = "BLANKING_FRAME_PACKET_4";
        mons[BLANKING_FRAME_PACKET_5] = "BLANKING_FRAME_PACKET_5";
        init = true;
    }
    return mons;
}

// -----------------------------------------------------------------------------
map<msgType, string> Downconverter::getSlowMonitors() const
{
    return XacDevice::getSlowMonitors();
}

// -----------------------------------------------------------------------------
void Downconverter::processMsg( msgType mid, DataVector & data, bool sim )
{
    // If state is ONLINE, check if the packet is late.
    if (getState() == ONLINE) {
        if (isPacketLate(200.0)) {
            incrementLatePacketCount();
        }
    }

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
        case XacDevice::SYSTEM_MONITOR_PACKET_1:
            XacDevice::processSystemMonitorPacket1(data, mon_->xac());
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_2:
            XacDevice::processSystemMonitorPacket2(data, mon_->xac());
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_3:
            XacDevice::processSystemMonitorPacket3(data, mon_->xac());
            break;
        case SYSTEM_MONITOR_PACKET_4:
#if USE_API_131
            XacDevice::processSystemMonitorPacket4(data, mon_->xac());
#else
            processSystemMonitorPacket4(data);
#endif
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_5:
            XacDevice::processSystemMonitorPacket5(data, mon_->xac());
            break;
        default:
            break;
    }
}

// -----------------------------------------------------------------------------
carma::canbus::Message Downconverter::simulateMsg(msgType mid)
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
   	case XacDevice::SYSTEM_MONITOR_PACKET_1:
		msg = XacDevice::simSystemMonitorPacket1();
		break;
	case XacDevice::SYSTEM_MONITOR_PACKET_2:
		msg = XacDevice::simSystemMonitorPacket2();
		break;
    case XacDevice::SYSTEM_MONITOR_PACKET_3:
        msg = XacDevice::simSystemMonitorPacket3();
        break;
    case SYSTEM_MONITOR_PACKET_4:
#if USE_API_131
        msg = XacDevice::simSystemMonitorPacket4();
#else
        msg = simSystemMonitorPacket4();
#endif
        break;
    case XacDevice::SYSTEM_MONITOR_PACKET_5:
        msg = XacDevice::simSystemMonitorPacket5();
        break;
   	default:
        break;
	}

    return msg;
}

// -----------------------------------------------------------------------------
void Downconverter::processBlankingFramePacket1( DataVector & data )
{
    short ps5vAnalog, psMinus5vAnalog, ps5vDigital;
    unsigned char psysStat, ifOutStat;

    // The below condition is checked and an exception is thrown in order
    // to provide a slightly higher level of error information...  If this
    // condition were to be true and the below exception wasn't thrown here
    // it would still be thrown from the dataToType calls below.
	if (data.size() < 8) {
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "Downconverter::processBlankingFrame - "
            "Packet1() - Invalid data size.");
	}

    // Unpack the raw data
	ps5vAnalog = dataToShort(data);
    psMinus5vAnalog = dataToShort(data);
    ps5vDigital = dataToShort(data);
    psysStat = dataToUbyte(data);
    ifOutStat = dataToUbyte(data);

    // Convert and place into monitor stream
	ifOutStat = ifOutStat&0x03;
    mon_->ps5vAnalog().setValue(ps5vAnalog * 0.001);
    mon_->psNeg5vAnalog().setValue(psMinus5vAnalog * 0.001);
    mon_->ps5vDigital().setValue(ps5vDigital * 0.001);
    mon_->psysStat().setValue(static_cast
        <WbdcSubsystem::PsysStatMonitorPointEnum::PSYSSTAT>(psysStat));
    mon_->ifOutStat().setValue(static_cast
        <WbdcSubsystem::IfOutStatMonitorPointEnum::IFOUTSTAT>(ifOutStat));

}

// -----------------------------------------------------------------------------
void Downconverter::processBlankingFramePacket2( DataVector & data )
{
    short psys[4];

    if (data.size() < 8) {
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "Downconverter::processBlankingFrame"
            "Packet2() - Invalid data size.");
    }

    // Unpack the raw data
    for (unsigned int i = 0; i < 4; i++) {
        psys[i] = dataToShort(data);
    }

    // Convert and place into monitor stream
    for (unsigned int s = 0; s < 4; s++) {
        mon_->psys().setValue(psys[s] * 0.01, s);
    }
}

// -----------------------------------------------------------------------------
void Downconverter::processBlankingFramePacket3( DataVector & data )
{
    short psys, psysAtten, ifOut, ifOutAtten;

	if (data.size() < 8) {
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "Downconverter::processBlankingFrame"
            "Packet3() - Invalid data size.");
	}

    // Unpack data
    psys = dataToShort(data);
    psysAtten = dataToShort(data);
    ifOut = dataToShort(data);
    ifOutAtten = dataToShort(data);

    ifOutputPower_ = ifOut * 0.01;

    // Convert and place into monitor stream
    mon_->psys().setValue(psys * 0.01, 4);
    mon_->psysAtten().setValue(psysAtten * 0.01);
    mon_->ifOutPower().setValue(ifOut * 0.01);
    mon_->ifOutAtten().setValue(ifOutAtten * 0.01);
}

// -----------------------------------------------------------------------------
void Downconverter::processBlankingFramePacket4( DataVector & data )
{
    short ps24v, ps7v1, ps7v2, ps7v3;

	if (data.size() < 8) {
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "Downconverter::processBlankingFrame"
            "Packet4() - Invalid data size.");
	}

    // Unpack data
    ps24v = dataToShort(data);
    ps7v1 = dataToShort(data);
    ps7v2 = dataToShort(data);
    ps7v3 = dataToShort(data);

    // Convert and place into monitor stream
    mon_->ps24v().setValue(ps24v * 0.001);
    mon_->ps7v1().setValue(ps7v1 * 0.001);
    mon_->ps7v2().setValue(ps7v2 * 0.001);
    mon_->ps7v3().setValue(ps7v3 * 0.001);
}

// -----------------------------------------------------------------------------
void Downconverter::processBlankingFramePacket5( DataVector & data )
{
	if (data.size() < 3) {
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "Downconverter::processBlankingFrame"
            "Packet5() - Invalid data size.");
	}

    // Unpack data
    const short temp = dataToShort( data );
    mon_->temp().setValue(temp * 0.01);

#if USE_API_131
    typedef WbdcSubsystem::RfAmpMonitorPointEnum RfAmpType;
    const unsigned char rfAmp = dataToUbyte( data ) & 0x1; // zero 3 high bits

    if ( rfAmp == 0x00 )
        mon_->rfAmp().setValue( RfAmpType::OFF );
    else
        mon_->rfAmp().setValue( RfAmpType::ON );
#else
    const unsigned char timeStat = dataToUbyte(data) & 0x3; // zero 2 high bits
    mon_->timeStat().setValue(
        static_cast<TimeStatMonitorPointEnum::TIMESTAT>(timeStat) );
#endif

}


// -----------------------------------------------------------------------------
void Downconverter::processSystemMonitorPacket4(
    std::vector<carma::canbus::byteType> &data)
{
    short timeOffset, tsPeriod, tsDelta;
    byteType apiVersion;

    // Make sure data is atleast 7 bytes long
    if (data.size() < 7) {
        throw CARMA_EXCEPTION(BadDataSizeException,
                "Downconverter::processSystemMonitorPacket4() - Data is "
                "less than minimum 7 bytes!");
    }

    // Unpack the data
    timeOffset = dataToShort(data);
    tsPeriod = dataToShort(data);
    tsDelta = dataToShort(data);
    apiVersion = dataToUbyte(data);

    // Convert and place into monitor stream
    // Ignore timeOffset and tsPeriod.
    mon_->xac().tsDelta().setValue(tsDelta);
    mon_->xac().apiVer().setValue(apiVersion);

}

// -----------------------------------------------------------------------------
carma::canbus::Message Downconverter::simBlankingFramePacket1()
{
    short ps5v = 5000, psNeg5v = -5000;
    unsigned char stat = 0x01;
    carma::canbus::Message msg = createMsgToHost(BLANKING_FRAME_PACKET_1);
    msg << ps5v << psNeg5v << ps5v << stat << stat;
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message Downconverter::simBlankingFramePacket2()
{
    short val = bandNo_ * 100 + inputNo_;
    carma::canbus::Message msg = createMsgToHost(BLANKING_FRAME_PACKET_2);
    msg << val << val << val << val;
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message Downconverter::simBlankingFramePacket3()
{
    short val = bandNo_ * 100 + inputNo_;
    carma::canbus::Message msg = createMsgToHost(BLANKING_FRAME_PACKET_3);
    for (unsigned i = 0; i < 4; ++i) msg << static_cast<short>(val + (i * val));
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message Downconverter::simBlankingFramePacket4()
{
    short ps28v = 28000, ps7v = 7000;
    carma::canbus::Message msg = createMsgToHost(BLANKING_FRAME_PACKET_4);
    msg << ps28v << ps7v << ps7v << ps7v;
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message Downconverter::simBlankingFramePacket5()
{
    short temp = 5000;
    unsigned char stat = 0x03;
    carma::canbus::Message msg = createMsgToHost(BLANKING_FRAME_PACKET_5);
    msg << temp << stat;
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message Downconverter::simSystemMonitorPacket4()
{
    short stat = 0;
    unsigned char api = 'B';
    carma::canbus::Message msg = createMsgToHost(SYSTEM_MONITOR_PACKET_4);
    msg << stat << stat << stat << api;
    return msg;
}

// -----------------------------------------------------------------------------
void Downconverter::setPsysPreset()
try {
    if ( commandLoggingEnabled_ )
        programLogInfoIfPossible( stringId_ + " setPsysPreset()" );
    carma::canbus::Message msg = createMsgToNode(SET_PSYS_POWER_TO_PRESET);
    io_.postMessage(msg);
} catch (std::exception &ex) {
    throw CARMA_EXCEPTION(carma::util::UserException, ex.what());
}

// -----------------------------------------------------------------------------
void Downconverter::setPsys(::CORBA::Float psys)
{
    try {
        if ( commandLoggingEnabled_ ) {
            ostringstream os;
            os << stringId_ << " setPsys( psys=" << psys << " )";
            programLogInfoIfPossible( os.str() );
        }
        short ps = static_cast<short>( round( psys/0.01 ) );
        carma::canbus::Message msg = createMsgToNode(SET_PSYS_POWER_TO_REQSTD);
        msg << ps;
        io_.postMessage(msg);
    } catch (std::exception &ex) {
        throw CARMA_EXCEPTION(carma::util::UserException, ex.what());
    }
}

// -----------------------------------------------------------------------------
void Downconverter::setPsysAtten(::CORBA::Float atten)
{
    try {
        if ( commandLoggingEnabled_ ) {
            ostringstream os;
            os << stringId_ << " setPsysAtten( atten=" << atten << " )";
            programLogInfoIfPossible( os.str() );
        }
        short at = static_cast<short>( round( atten/0.01 ) );
        carma::canbus::Message msg = createMsgToNode(SET_PSYS_ATTEN_TO_REQSTD);
        msg << at;
        io_.postMessage(msg);
    } catch (std::exception &ex) {
        throw CARMA_EXCEPTION(carma::util::UserException, ex.what());
    }
}

// -----------------------------------------------------------------------------
void Downconverter::setIfOutPreset()
{
    try {
        if ( commandLoggingEnabled_ )
            programLogInfoIfPossible( stringId_ + "setIfOutPreset()" );
        carma::canbus::Message msg = createMsgToNode(SET_IFOUT_POWER_TO_PRESET);
        io_.postMessage(msg);
    } catch (std::exception &ex) {
        throw CARMA_EXCEPTION(carma::util::UserException, ex.what());
    }
}

// -----------------------------------------------------------------------------
void Downconverter::setIfOut(::CORBA::Float ifout)
{
    try {
        if ( commandLoggingEnabled_ ) {
            ostringstream os;
            os << stringId_ << " setIfOut( ifout=" << ifout << " )";
            programLogInfoIfPossible( os.str() );
        }
        carma::canbus::Message msg = createMsgToNode(SET_IFOUT_POWER_TO_REQSTD);
        msg << ( static_cast<short>( round( ifout/0.01 ) ) );
        io_.postMessage(msg);
    } catch (std::exception &ex) {
        throw CARMA_EXCEPTION(carma::util::UserException, ex.what());
    }
}

// -----------------------------------------------------------------------------
void Downconverter::setIfOutAtten(::CORBA::Float atten)
{
    try {
        if ( commandLoggingEnabled_ ) {
            ostringstream os;
            os << stringId_ << " setIfOutAtten( atten=" << atten << " )";
            programLogInfoIfPossible( os.str() );
        }
        carma::canbus::Message msg = createMsgToNode(SET_IFOUT_ATTEN_TO_REQSTD);
        msg << ( static_cast<short>( round( atten / 0.01 ) ) );
        io_.postMessage(msg);
    } catch (std::exception &ex) {
        throw CARMA_EXCEPTION(carma::util::UserException, ex.what());
    }
}

// -----------------------------------------------------------------------------
void Downconverter::enableRfInputAmp(::CORBA::Boolean enable)
{
    try {
        if ( commandLoggingEnabled_ ) {
            ostringstream os;
            os << stringId_ << " enableRfInputAmp( enable=" << enable << " )";
            programLogInfoIfPossible( os.str() );
        }
        unsigned char answer = ( enable ? 0x01 : 0x00 );
        carma::canbus::Message msg = createMsgToNode(SET_AMP_POWER_SWITCH);
        msg << answer;
        io_.postMessage(msg);
    } catch (std::exception &ex) {
        throw CARMA_EXCEPTION(carma::util::UserException, ex.what());
    }
}

// -----------------------------------------------------------------------------
void Downconverter::enableIfOutAlc(::CORBA::Boolean enable)
{
    try {
        if ( commandLoggingEnabled_ ) {
            ostringstream os;
            os << stringId_ << " enableIfOutAlc( enable=" << enable << " )";
            programLogInfoIfPossible( os.str() );
        }
        unsigned char answer = ( enable ? 0x01 : 0x00 );
        carma::canbus::Message msg = createMsgToNode(SET_IFOUT_ALC_LOOP_SWITCH);
        msg << answer;
        io_.postMessage(msg);
    } catch (std::exception &ex) {
        throw CARMA_EXCEPTION(carma::util::UserException, ex.what());
    }
}

// -----------------------------------------------------------------------------
::CORBA::Boolean
Downconverter::checkIfOutputPower(
    ::CORBA::Float power,
    ::CORBA::Float delta)
{
    ::CORBA::Boolean answer;
    if (getState() != ONLINE) {
        throw CARMA_EXCEPTION(carma::util::UserException,
                "Downconverter::checkIfOutputPower() - The Downconverter "
                "is not ONLINE!  Two things could be wrong a) The device is "
                "truly not online or b) You have issued this command on a node "
                "0 or global control device (a no no).");
    }

    answer = ((ifOutputPower_ >= (power - delta))
            && (ifOutputPower_ <= (power + delta)));
    return answer;
}

// -----------------------------------------------------------------------------
void
Downconverter::enableCommandLogging( )
{
    commandLoggingEnabled_ = true;
}

// -----------------------------------------------------------------------------
void
Downconverter::disableCommandLogging( )
{
    commandLoggingEnabled_ = false;
}
