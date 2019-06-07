/**@file
 * Definition for Carma QuadratureModulator class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.48 $
 * $Date: 2012/01/31 00:22:03 $
 * $Id: QuadratureModulator.cc,v 1.48 2012/01/31 00:22:03 abeard Exp $
 */

// Carma includes
#include "carma/canbus/exceptions.h"
#include "carma/canbus/Utilities.h"
#include "carma/monitor/DownconverterCommon.h"
#include "carma/monitor/QuadratureModulator.h"
#include "carma/downconverter/common/QuadratureModulator.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedPthreadMutexLock.h"
#include "carma/util/Time.h"

// Carma Tools includes
#include <log4cpp/Priority.hh>
#include <log4cpp/Category.hh>

using namespace std;
using namespace carma;
using namespace carma::canbus;
using namespace carma::util;
using namespace carma::downconverter;
using namespace carma::monitor;
using namespace log4cpp;

namespace { // Anonymous namespace for local constants and typedefs

    // API version this class was implemented from
    const char API_VERSION                                = 'A';

    // API Id for this device.
    const carma::canbus::apiType API_ID                   = 66;

    // Controls
    const carma::canbus::msgType SET_POUT_TO_PRESET            = 0x040;
    const carma::canbus::msgType SET_POUT_TO_REQUESTED         = 0x041;
    const carma::canbus::msgType SET_NOISE_ATTENUATION         = 0x042;
    const carma::canbus::msgType ENABLE_QUAD_MOD_NOISE         = 0x043;
    const carma::canbus::msgType ENABLE_QUAD_MOD_ONLY          = 0x044;
    const carma::canbus::msgType WALSH_TABLE_COLUMN_IMMINENT   = 0x045;
    const carma::canbus::msgType LOAD_WALSH_TABLE_COLUMN       = 0x046;

    // Monitors
    const carma::canbus::msgType BLANKING_FRAME_PACKET_1  = 0x0E0;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_2  = 0x0E1;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_3  = 0x0E2;

} // End anonymous namespace

// -----------------------------------------------------------------------------
QuadratureModulator::QuadratureModulator(
        nodeType node,
        CanOutput &io,
        QuadModContainer & quadModMon ) :
    XacDevice( API_ID, node, io ), 
    inputNo_( node ),
    state_( node == 0 ? 0 : &( quadModMon.state() ) ),
    mon_( node == 0 ? 0 : &( quadModMon.quadMod() ) ),
    xacMon_( node == 0 ? 0 : &( quadModMon.xac() ) ),
    ifInPower_( 0.0 ),
    ifOutPower_( 0.0 ),
    enabled_( false )
{
    setBoardType(0);
    setSerialNumber(0);
}

// -----------------------------------------------------------------------------
QuadratureModulator::~QuadratureModulator()
{
	// Nothing
}

// -----------------------------------------------------------------------------
apiType QuadratureModulator::getApiId()
{
    return API_ID;
}

// -----------------------------------------------------------------------------
void QuadratureModulator::updateFrameData()
{
    mon_->inputNo().setValue(inputNo_);
    state_->setValue( static_cast<CM::StateMonitorPointEnum::STATE>(getState()));
}

// -----------------------------------------------------------------------------
map<msgType, string> QuadratureModulator::getHalfSecMonitors() const
{
    static bool init = false;
    static map<msgType, string> mons;

    if (!init) {
        mons[BLANKING_FRAME_PACKET_1] = "BLANKING_FRAME_PACKET_1";
        mons[BLANKING_FRAME_PACKET_2] = "BLANKING_FRAME_PACKET_2";
        mons[BLANKING_FRAME_PACKET_3] = "BLANKING_FRAME_PACKET_3";
        init = true;
    }
    return mons;

}

// -----------------------------------------------------------------------------
map<msgType, string> QuadratureModulator::getSlowMonitors() const
{
    return XacDevice::getSlowMonitors();
}

// -----------------------------------------------------------------------------
void QuadratureModulator::processMsg( msgType mid, DataVector & data, bool sim )
{
    // If state is ONLINE, check if the packet is late.
    if (getState() == ONLINE) {
        if (isPacketLate(200.0))
            incrementLatePacketCount();
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
        case XacDevice::SYSTEM_MONITOR_PACKET_1:
        case XacDevice::SYSTEM_MONITOR_PACKET_2:
        case XacDevice::SYSTEM_MONITOR_PACKET_3:
        case XacDevice::SYSTEM_MONITOR_PACKET_4:
        case XacDevice::SYSTEM_MONITOR_PACKET_5:
            XacDevice::processSystemMonitorPacket( mid, data, *(xacMon_) );
            break;
        default:
            break;
    }
}

// -----------------------------------------------------------------------------
carma::canbus::Message QuadratureModulator::simulateMsg(msgType mid)
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
        case XacDevice::SYSTEM_MONITOR_PACKET_2:
        case XacDevice::SYSTEM_MONITOR_PACKET_3:
        case XacDevice::SYSTEM_MONITOR_PACKET_4:
        case XacDevice::SYSTEM_MONITOR_PACKET_5:
            msg = XacDevice::simSystemMonitorPacket( mid );
            break;
        default:
            break;
    }

    return msg;
}

// -----------------------------------------------------------------------------
void QuadratureModulator::processBlankingFramePacket1( DataVector & data )
{
    if (data.size() < 8) {
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "QuadratureModulator::processBlankingFramePacket1() - "
            "Invalid data size.");
    }

    const float LSBS_PER_VOLT = 1000.0;

    const short ps5v = dataToShort(data);
    const short psMinus5v = dataToShort(data);
    const short ps5vDigital = dataToShort(data);
    const unsigned char pOutState = dataToUbyte(data);
    const unsigned char modEnabled = dataToUbyte(data);

    {
        ScopedPthreadMutexLock scopelock( mutex_ );

        enabled_ = (modEnabled != 0);
    }

    // Convert and place into monitor stream
    mon_->ps5v1().setValue( ps5v / LSBS_PER_VOLT );
    mon_->ps5v2().setValue( psMinus5v / LSBS_PER_VOLT );
    mon_->ps5v3().setValue( ps5vDigital / LSBS_PER_VOLT );

    mon_->powerOutEnabled().setValue( static_cast<bool>( pOutState ) );

    mon_->modulationEnabled().setValue( static_cast<bool>( modEnabled ) );
}

// -----------------------------------------------------------------------------
void QuadratureModulator::processBlankingFramePacket2( DataVector & data )
{
    if (data.size() < 8) {
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "QuadratureModulator::processBlankingFramePacket2() - "
            "Invalid data size.");
    }

    const float LSBS_PER_VOLT = 1000.0;
    const float LSBS_PER_DBM = 100.0;

    const short ps24v = dataToShort(data);
    const short logIn = dataToShort(data);
    const short logOut = dataToShort(data);
    const unsigned char attenuation = dataToUbyte(data);
    const unsigned char poutStat = dataToUbyte(data);

    {
        ScopedPthreadMutexLock scopelock( mutex_ );

        ifInPower_ = logIn / LSBS_PER_DBM;
        ifOutPower_ = logOut / LSBS_PER_DBM;

        mon_->inputPower().setValue( ifInPower_ );
        mon_->outputPower().setValue( ifOutPower_ );
    }

    mon_->ps24v().setValue(ps24v / LSBS_PER_VOLT);
    mon_->atten().setValue(attenuation);

    typedef carma::monitor::QuadMod::POutStatMonitorPointEnum PoutStat;
    mon_->pOutStat().setValue( static_cast<PoutStat::POUTSTAT>( poutStat ) );
}

// -----------------------------------------------------------------------------
void QuadratureModulator::processBlankingFramePacket3( DataVector & data )
{
    if (data.size() < 5) {
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "QuadratureModulator::processBlankingFramePacket3() - "
            "Invalid data size.");
    }

    const short temp = dataToShort(data);
    const unsigned char ppsPresent = dataToUbyte( data ) & 0x03;
    const unsigned char heartbeatPresent = dataToUbyte( data ) & 0x03;
    const unsigned char pstcState = dataToUbyte(data);

    const float LSBS_PER_DEG_C = 100.0;

    mon_->temp().setValue( temp / LSBS_PER_DEG_C );
    mon_->ppsPresent().setValue( static_cast<bool>( ppsPresent ) );
    mon_->heartbeatPresent().setValue( static_cast<bool>( heartbeatPresent ) );
    mon_->pstcState().setValue(static_cast
        <QuadMod::PstcStateMonitorPointEnum::PSTCSTATE>(pstcState));
}

// -----------------------------------------------------------------------------
carma::canbus::Message QuadratureModulator::simBlankingFramePacket1()
{
    const short ps5v = 5000;
    const short psNeg5v = -5000;
    const unsigned char poutEnabled = 0x01;
    const unsigned char modEnabled = 0x01;
    carma::canbus::Message msg = createMsgToHost(BLANKING_FRAME_PACKET_1);
    msg << ps5v << psNeg5v << ps5v << poutEnabled << modEnabled;
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message QuadratureModulator::simBlankingFramePacket2()
{
    short ps28v = 28000, logIn = -4000, logOut = -3000;
    unsigned char atten = 1;
    const unsigned char levelState = 0x02;
    carma::canbus::Message msg = createMsgToHost(BLANKING_FRAME_PACKET_2);
    msg << ps28v << logIn << logOut << atten << levelState;
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message QuadratureModulator::simBlankingFramePacket3()
{
    const short temp = 5000;
    const unsigned char ppsPresent = 0u;
    const unsigned char heartbeatPresent = 1u;
    const unsigned char walshValid = 0u;
    carma::canbus::Message msg = createMsgToHost(BLANKING_FRAME_PACKET_3);
    msg << temp << ppsPresent << heartbeatPresent << walshValid;

    return msg;
}

// -----------------------------------------------------------------------------
void QuadratureModulator::walshTableColumnImminent( const unsigned short length,
                                                    const unsigned char crc )
{
    carma::canbus::Message msg = createMsgToNode(WALSH_TABLE_COLUMN_IMMINENT);
    msg << length << crc;
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void QuadratureModulator::loadWalshTableColumn(
    const unsigned char segmentIndex,
    const vector<unsigned char> segment )
{
    if ( segment.size( ) > 7 ) {
        throw CARMA_EXCEPTION( IllegalArgumentException,  "QuadratureModulator"
            "::loadWalshTableColumn( ) - Walsh sequence segment size > 7." );
    }
    carma::canbus::Message msg = createMsgToNode(LOAD_WALSH_TABLE_COLUMN);
    msg << segmentIndex;
    const vector<unsigned char>::const_iterator iBegin = segment.begin( );
    const vector<unsigned char>::const_iterator iEnd = segment.end( );
    vector<unsigned char>::const_iterator i;
    for ( i = iBegin; i != iEnd; ++i ) {
        msg << (*i);
    }

    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void QuadratureModulator::setPoutPreset()
try {
    programLogInfoIfPossible( "setPoutPreset()" );
    carma::canbus::Message msg = createMsgToNode(SET_POUT_TO_PRESET);
    io_.postMessage(msg);
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

// -----------------------------------------------------------------------------
void QuadratureModulator::setPout(double pout)
try {
    ostringstream os;
    os << "QuadratureModulator::setPout( pout=" << pout << " )";
    programLogInfoIfPossible( os.str() );
    carma::canbus::Message msg = createMsgToNode(SET_POUT_TO_REQUESTED);
    msg << ( static_cast<unsigned short>(pout / 0.01) );
    io_.postMessage(msg);
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

// -----------------------------------------------------------------------------
void QuadratureModulator::setPoutAtten(unsigned short atten)
try {
    ostringstream os;
    os << "QuadratureModulator::setPoutAtten( atten=" << atten << " )";
    programLogInfoIfPossible( os.str() );
    carma::canbus::Message msg = createMsgToNode(SET_NOISE_ATTENUATION);
    msg << ( static_cast<unsigned char>(atten) );
    io_.postMessage(msg);
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

// -----------------------------------------------------------------------------
void QuadratureModulator::enableQuadMod(bool enable)
try {
    ostringstream os;
    os << "QuadratureModulator::enableQuadMod( enable=" << enable << " )";
    programLogInfoIfPossible( os.str() );
    unsigned char cbool = (enable ? 0x01 : 0x00);
    carma::canbus::Message msg = createMsgToNode(ENABLE_QUAD_MOD_NOISE);
    msg << cbool;
    io_.postMessage(msg);
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

// -----------------------------------------------------------------------------
void QuadratureModulator::enableModulationOnly(const bool enable)
try {
    ostringstream os;
    os << "QuadratureModulator::enableModulationOnly( enable="
       << enable << " )";
    programLogInfoIfPossible( os.str() );
    const unsigned char cbool = (enable ? 0x01 : 0x00);
    carma::canbus::Message msg = createMsgToNode(ENABLE_QUAD_MOD_ONLY);
    msg << cbool;
    io_.postMessage(msg);
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
}

// -----------------------------------------------------------------------------
void QuadratureModulator::loadWalshSequence(
    const WalshSequence & walshSequence )
{
    programLogInfoIfPossible( "QuadratureModulator::loadWalshSequence( )" );
    throw CARMA_EXCEPTION(carma::util::UserException, "Not implemented.");
}

// -----------------------------------------------------------------------------
bool QuadratureModulator::isEnabled()
{
    ScopedPthreadMutexLock scopelock( mutex_ );
    return enabled_;
}

// -----------------------------------------------------------------------------
bool QuadratureModulator::checkIfOutPower(double power, double delta)
{
    if (getState() != ONLINE) {
        throw CARMA_EXCEPTION(carma::util::UserException,
            "QuadratureModulator::checkIfOutPower() - The QuadratureModulator "
            "is not ONLINE!  Two things could be wrong a) The device is truly "
            "not online or b) You have issued this command on a node 0 or "
            "global control device (a no no).");
    }

    ScopedPthreadMutexLock scopelock( mutex_ );

    const bool answer = ((ifOutPower_ >= (power - delta))
                      && (ifOutPower_ <= (power + delta)));
    return answer;
}

// -----------------------------------------------------------------------------
bool QuadratureModulator::checkIfInPower(double power, double delta)
{
    bool answer;
    if (getState() != ONLINE) {
        throw CARMA_EXCEPTION(carma::util::UserException,
            "QuadratureModulator::checkIfInPower() - The QuadratureModulator "
            "is not ONLINE!  Two things could be wrong a) The device is truly "
            "not online or b) You have issued this command on a node 0 or "
            "global control device (a no no).");
    }

    answer = ((ifInPower_ >= (power - delta))
             && (ifInPower_ <= (power + delta)));
    return answer;
}

