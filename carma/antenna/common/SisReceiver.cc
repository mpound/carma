/** @file
 * CAN Device definition for 10-m SIS Receiver Control.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.3 $
 * $Date: 2011/01/03 18:48:05 $
 * $Id: SisReceiver.cc,v 1.3 2011/01/03 18:48:05 iws Exp $
 *
 * $CarmaCopyright$
 */

// Carma includes
#include "carma/antenna/common/SisReceiver.h"

#include "carma/antenna/common/AntennaIF.h"
#include "carma/canbus/Utilities.h"
#include "carma/monitor/AntennaCommon.h"
#include "carma/monitor/CanbusCommon.h"
#include "carma/monitor/SisReceiver.h"
#include "carma/util/ErrorException.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedPthreadMutexLock.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"

// Carma Tools includes
#include <log4cpp/Category.hh>
#include <log4cpp/Priority.hh>

// System includes
#include <cmath>

// C++ Standard Lib includes
#include <iomanip>

using namespace carma::antenna::common;
using namespace carma::canbus;
using namespace carma::util;
using namespace log4cpp;
using namespace std;

namespace CM = carma::monitor;

namespace { // Anonymous namespace for local constants and typedefs.

    // API document version this class was implemented from
    const char API_VERSION                                           = 'B';

    // API Id for this device
    const carma::canbus::apiType API_ID                              = 209;

    // Monitors
    const carma::canbus::msgType BLANKING_FRAME_PACKET_1             = 0x0E0;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_2             = 0x0E1;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_3             = 0x0E2;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_4             = 0x0E3;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_5             = 0x0E4;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_6             = 0x0E5;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_7             = 0x0E6;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_8             = 0x0E7;

    // Controls
    const carma::canbus::msgType TUNE_MIXER                          = 0x040;
    const carma::canbus::msgType SET_VJ                              = 0x080;
    const carma::canbus::msgType SET_IJ                              = 0x081;
    const carma::canbus::msgType SET_LO_ATTEN                        = 0x082;
    const carma::canbus::msgType SET_VD                              = 0x083;
    const carma::canbus::msgType SET_ID                              = 0x084;
    const carma::canbus::msgType SET_VG                              = 0x085;
    const carma::canbus::msgType DO_IV_CURVE                         = 0x086;
    const carma::canbus::msgType GET_VGAP                            = 0x087;
    const carma::canbus::msgType SET_IGAP                            = 0x088;
    const carma::canbus::msgType SET_VJ_LOOP_MODE                    = 0x089;
    const carma::canbus::msgType SET_IJ_LOOP_MODE                    = 0x08A;

    const carma::canbus::msgType IV_CURVE_POINT                      = 0x170;

    // Wrapper function to create a map of fast monitor packets.
    MsgBriefMap getFastMonitors() {
        MsgBriefMap mons;
        mons[BLANKING_FRAME_PACKET_1] = "BLANKING_FRAME_PACKET_1";
        mons[BLANKING_FRAME_PACKET_2] = "BLANKING_FRAME_PACKET_2";
        mons[BLANKING_FRAME_PACKET_3] = "BLANKING_FRAME_PACKET_3";
        mons[BLANKING_FRAME_PACKET_4] = "BLANKING_FRAME_PACKET_4";
        mons[BLANKING_FRAME_PACKET_5] = "BLANKING_FRAME_PACKET_5";
        mons[BLANKING_FRAME_PACKET_6] = "BLANKING_FRAME_PACKET_6";
        mons[BLANKING_FRAME_PACKET_7] = "BLANKING_FRAME_PACKET_7";
        mons[BLANKING_FRAME_PACKET_8] = "BLANKING_FRAME_PACKET_8";
        return mons;
    } // End local function getFastMonitors

    typedef carma::monitor::SisReceiver CMSR;

} // End namespace <unnamed>

const carma::canbus::msgType SisReceiver::RX_3MM_LEFT_POL_NODE_ID;
const carma::canbus::msgType SisReceiver::RX_3MM_RIGHT_POL_NODE_ID;
const carma::canbus::msgType SisReceiver::RX_1MM_LEFT_POL_NODE_ID;
const carma::canbus::msgType SisReceiver::RX_1MM_RIGHT_POL_NODE_ID;

// -----------------------------------------------------------------------------
SisReceiver::SisReceiver (
    nodeType node,
    CanOutput & io,
    carma::monitor::AntennaCommon & antCommon,
    carma::monitor::StateMonitorPointEnum & state,
    carma::monitor::SisReceiver & sis,
    carma::monitor::Xac & xac,
    carma::antenna::common::AntennaIF & antIF ) :
        carma::canbus::devices::XacDevice ( API_ID, node, io ),
        log_ ( Program::getLogger() ),
        common_( antCommon ),
        state_( state ),
        mon_ ( sis ),
        xacMon_ ( xac ),
        antIF_( antIF )
{
    ivCurveCollationInfo_.nPointsExpected = 0;
    ivCurveCollationInfo_.nPointsReceived = 0;
}
       
// -----------------------------------------------------------------------------
SisReceiver::~SisReceiver ( )
{
    // Nothing
}

// -----------------------------------------------------------------------------
MsgBriefMap SisReceiver::getHalfSecMonitors() const
{
    static MsgBriefMap monitors = getFastMonitors();
    return monitors; 
}

// -----------------------------------------------------------------------------
MsgBriefMap SisReceiver::getSlowMonitors() const 
{
    static MsgBriefMap monitors = XacDevice::getSlowMonitors();
    return monitors;
}

// -----------------------------------------------------------------------------
void SisReceiver::processMsg ( msgType mid, DataVector &data, bool sim )
{
    if (getState() == ONLINE) {
        if ( isPacketLate() ) {
            incrementLatePacketCount();
        }
    }

    CPTRACE(Trace::TRACEALL, "SisReceiver::processMsg() - Processing "
            << (sim ? "simulated" : "real") << " msg 0x" << hex << mid << dec 
            << " for node " << getNode() << ".");

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
        case IV_CURVE_POINT:
            processIVCurvePoint( data );
            break;
        default:
            // I don't know how to process this message id!
            // Not a problem, just log it.
            log_ << Priority::DEBUG << "SisReceiver::processMsg() - "
                << "Switch does not match any case: Unknown mid "
                << mid << ". Node " << getNode();
            CPTRACE(Trace::TRACE6, "SisReceiver::processMsg() - "
                    "Switch doesn't match any case.  mid 0x" << hex  << mid
                    << dec << " node " << getNode() << ".");
            break;
    } // End switch on msgid
}

// -----------------------------------------------------------------------------
carma::canbus::Message SisReceiver::simulateMsg( msgType mid )
{
    carma::canbus::Message msg;
    CPTRACE(Trace::TRACEALL, "SisReceiver::simulateMsg() - "
            "Simulating msg 0x" << hex << mid << dec << " for node "
            << getNode() << ".");

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
            // Not a problem, just log it - but should I throw???
            log_ << Priority::DEBUG << "SisReceiver::simulateMsg - "
                << "Switch does not match any case: mid " << mid;
            CPTRACE(Trace::TRACE6, "SisReceiver::simulateMsg() - "
                    "Switch doesn't match any case.  mid 0x" << hex  << mid
                    << dec << " node " << getNode() << ".");
            break;
    }
    return msg;
}

// -----------------------------------------------------------------------------
void SisReceiver::updateFrameData()
{
    state_.setValue( static_cast<CM::StateMonitorPointEnum::STATE>( 
            getState() ) );
}

// -----------------------------------------------------------------------------
void SisReceiver::tuneMixer( const float frequencyInGhz ) const
{
    carma::canbus::Message msg = createMsgToNode(TUNE_MIXER); 
    const short freq = static_cast<short>(::roundf( frequencyInGhz * 100.0f )); 
    msg << freq; // converted to .01Ghz
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void SisReceiver::setVj( const float VjInMilliVolts ) const
{
    carma::canbus::Message msg = createMsgToNode(SET_VJ);
    short vj = static_cast<short>( ::roundf( VjInMilliVolts * 1000.0f ) );
    msg << vj; // In microVolts
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void SisReceiver::setIj( const float IjInMicroAmps ) const
{
    carma::canbus::Message msg = createMsgToNode(SET_IJ);
    const short ij = static_cast<short>( ::roundf( IjInMicroAmps * 10.0f ) );
    msg << ij; // In 0.1 uA
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void SisReceiver::setLoAttenuation( const float loAttenPercent ) const
{
    carma::canbus::Message msg = createMsgToNode(SET_LO_ATTEN);
    const unsigned short loAtten = 
        static_cast<unsigned short>( ::roundf( loAttenPercent * 100.0 ) ); 
    msg << loAtten; // In 0.01% units.
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void SisReceiver::setVd( short stageNo, const float VdInVolts ) const
{
    carma::canbus::Message msg = createMsgToNode(SET_VD);
    const unsigned short vd = 
        static_cast<unsigned short>( ::roundf( VdInVolts * 1000.0f ) );
    msg << static_cast<byteType>(stageNo) << vd;
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void SisReceiver::setId( short stageNo, const float IdInMilliAmps ) const
{
    carma::canbus::Message msg = createMsgToNode(SET_ID);
    const unsigned short id = 
        static_cast< unsigned short >( ::roundf( IdInMilliAmps * 1000.0f ) );
    msg << static_cast<byteType>(stageNo) << id; 
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void SisReceiver::setVg( short stageNo, float Vg ) const
{
    carma::canbus::Message msg = createMsgToNode(SET_VG);
    Vg *= 1000.0; // Convert to mV
    Vg = roundf( Vg );
    msg << static_cast<byteType>(stageNo) << static_cast<short>(Vg);
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void SisReceiver::doIVCurve( const float startInMilliVolts, 
                             const float stopInMilliVolts, 
                             const float stepInMilliVolts, 
                             const int timeDeltaMilliSeconds, 
                             const int seqNo,
                             const bool doPower ) 
{
    carma::canbus::Message msg = createMsgToNode(DO_IV_CURVE);

    ScopedPthreadMutexLock collateScopelock( ivCurveCollationInfo_.mutex );

    // Calculate and set the number of IV curve points expected, clear curve.
    ivCurveCollationInfo_.nPointsExpected = static_cast<unsigned int>(
        truncf((stopInMilliVolts - startInMilliVolts) / stepInMilliVolts ) + 1);
    ivCurveCollationInfo_.nPointsReceived = 0;
    ivCurveCollationInfo_.tempIVCurve.data.clear();
    ivCurveCollationInfo_.tempIVCurve.doPowerRequested = doPower;
    ivCurveCollationInfo_.pendingSequenceNumber = seqNo;
    
    const short startInUv = 
        static_cast<short>( ::roundf( startInMilliVolts * 1000 ) ); 
    const short stopInUv = 
        static_cast<short>( ::roundf( stopInMilliVolts * 1000 ) ); 
    const short stepInUv = 
        static_cast<short>( ::roundf( stepInMilliVolts * 1000 ) ); 
    const unsigned char deltaIn100Ms = timeDeltaMilliSeconds / 100; 
    const unsigned char getPower = static_cast< unsigned char >( doPower );

    msg << startInUv << stopInUv << stepInUv << deltaIn100Ms << getPower;

    io_.postMessage(msg);

    if ( getState() == SIMULATED ) { // If simulating, form up a fake IV curve

        // WARNING: LOCK ORDER DEPENDENCY
        ScopedPthreadMutexLock curveScopelock( ivCurveMutex_ );

        // Form up a fake IV curve if simulated.
        const double dfjd = timeDeltaMilliSeconds/Time::MILLISECONDS_PER_DAY;
        const double now = Time::MJD();
        double fjd = now - ::trunc( now );
        float vj = startInMilliVolts; 
        ivCurve_.data.clear();
        for ( unsigned int idx = 0; 
              idx < ivCurveCollationInfo_.nPointsExpected; ++idx ) {
            IVPoint ivp;
            ivp.fjd = fjd;
            ivp.Vj = vj;
            ivp.Ij = 50 * vj + 15; // Linear across expected Ij range
            ivCurve_.data.push_back( ivp );
            fjd += dfjd;
            vj += stepInMilliVolts;
        }

        ivCurve_.doPowerRequested = doPower;

        if ( doPower ) { // Tell the IF module so it can return sim total powers
            antIF_.simTotalPower( ivCurveCollationInfo_.nPointsExpected );
        }
            
        common_.receivers().tuneSeqNum().setValue( seqNo );
    } // If simulated
}

// -----------------------------------------------------------------------------
SisReceiver::IVCurve SisReceiver::getIVCurve() 
{
    carma::util::ScopedPthreadMutexLock scopelock( ivCurveMutex_ );
    return ivCurve_;
}

// -----------------------------------------------------------------------------
void SisReceiver::getVgap( CurrentModeType mode, float Igap ) const
{
    carma::canbus::Message msg = createMsgToNode(GET_VGAP);
    Igap *= 10.0; // Convert to 0.1 uA
    Igap = roundf( Igap );
    msg << static_cast<byteType>(mode) << static_cast<short>(Igap);
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void SisReceiver::setIgap( float Igap ) const
{
    carma::canbus::Message msg = createMsgToNode(SET_IGAP);
    Igap *= 10.0; // Convert to 0.01 uA
    Igap = roundf( Igap );
    msg << static_cast<short>(Igap);
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void SisReceiver::setVjLoopMode( VjLoopModeType mode ) const
{
    carma::canbus::Message msg = createMsgToNode(SET_VJ_LOOP_MODE);
    msg << static_cast<byteType>(mode);
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void SisReceiver::setIjLoopMode( IjLoopModeType mode ) const
{
    carma::canbus::Message msg = createMsgToNode(SET_IJ_LOOP_MODE);
    msg << static_cast<byteType>(mode);
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void SisReceiver::processBlankingFramePacket1( DataVector &data ) 
{
    const short setVj = dataToShort( data );
    const short actualVj = dataToShort( data );
    const short setIj = dataToShort( data );
    const short actualIj = dataToShort( data );

    mon_.requestedVj().setValue( setVj / 1000.0 ); // Set Vj in mV
    mon_.actualVj().setValue( actualVj / 1000.0 ); // Actual Vj in mV
    mon_.requestedIj().setValue( setIj / 10.0 ); // Set Ij in uA
    mon_.actualIj().setValue( actualIj / 10.0 ); // Actual Ij in uA
}

// -----------------------------------------------------------------------------
void SisReceiver::processBlankingFramePacket2( DataVector &data ) 
{
    const byteType sourceVj = dataToUbyte( data );
    const byteType sourceIj = dataToUbyte( data );
    const byteType modeVj = dataToUbyte( data );
    const byteType modeIj = dataToUbyte( data );
    const short gapCurrent = dataToShort( data );
    const short junctionGapVoltage = dataToShort( data );

    mon_.vjSource().setValue( 
        static_cast<CMSR::VjSourceMonitorPointEnum::VJSOURCE>(sourceVj) );
    mon_.ijSource().setValue(
        static_cast<CMSR::IjSourceMonitorPointEnum::IJSOURCE>(sourceIj) );
    mon_.vjLoopMode().setValue(
        static_cast<CMSR::VjLoopModeMonitorPointEnum::VJLOOPMODE>(modeVj) );
    mon_.ijLoopMode().setValue(
        static_cast<CMSR::IjLoopModeMonitorPointEnum::IJLOOPMODE>(modeIj) );
    mon_.gapCurrent().setValue( gapCurrent / 10.0 ); // Ig in uA
    mon_.calGapVoltage().setValue( junctionGapVoltage / 1000.0 ); // Vg in mV
}

// -----------------------------------------------------------------------------
void SisReceiver::processBlankingFramePacket3( DataVector &data ) 
{
    const short lastVg = dataToShort( data );
    const short ttId = dataToShort( data );
    const byteType calMonth = dataToUbyte( data );
    const byteType calDay = dataToUbyte( data );
    const byteType calYear = dataToUbyte( data );
    const byteType tuneState = dataToUbyte( data );

    string calDateString;
    {
        ostringstream calDate;
        calDate << static_cast<int>(calMonth) << "/" 
            << static_cast<int>(calDay) << "/"
            <<  setw(2) << setfill('0') << static_cast<int>(calYear);
        calDateString = calDate.str( );
    }

    mon_.gapVoltage().setValue( lastVg / 1000.0 ); // Vg in mV
    mon_.tuneTableId().setValue( ttId );
    mon_.calDate().setValue( calDateString );
    mon_.tuneState().setValue(
        static_cast<CMSR::TuneStateMonitorPointEnum::TUNESTATE>(tuneState) );
}

// -----------------------------------------------------------------------------
void SisReceiver::processBlankingFramePacket4( DataVector &data ) 
{
    const unsigned short atten = dataToUshort( data );
    const short modTemp = dataToShort( data );
    const byteType ivCurve = dataToUbyte( data );
    const unsigned short rSeries = dataToUshort( data );
    const byteType rxType = dataToUbyte( data );

    mon_.attenuation().setValue( atten / 100.0 ); // In %
    mon_.temp().setValue( modTemp / 100.0 ); // In C
    mon_.ivCurveState().setValue( 
        static_cast<CMSR::IvCurveStateMonitorPointEnum::IVCURVESTATE>(ivCurve) );
    mon_.rSeries().setValue( rSeries / 1000.0 );
    mon_.rxType().setValue( 
        static_cast<CMSR::RxTypeMonitorPointEnum::RXTYPE>( rxType ) );
}

// -----------------------------------------------------------------------------
void SisReceiver::processBlankingFramePacket5( DataVector &data ) 
{
    const unsigned short Vd1 = dataToUshort( data );
    const unsigned short Id1 = dataToUshort( data );
    const short Vg1 = dataToShort( data );
    const unsigned short rSenseInMilliOhms = dataToUshort( data );

    mon_.drainVoltage1().setValue( Vd1 / 1000.0 ); // In V
    mon_.drainCurrent1().setValue( Id1 / 1000.0 ); // In mA
    mon_.gateVoltage1().setValue( Vg1 / 1000.0 ); // In V
    mon_.rSense().setValue( rSenseInMilliOhms / 1000.0 );
}

// -----------------------------------------------------------------------------
void SisReceiver::processBlankingFramePacket6( DataVector &data ) 
{
    const unsigned short Vd2 = dataToUshort( data );
    const unsigned short Id2 = dataToUshort( data );
    const short Vg2 = dataToShort( data );
    const unsigned short vjDacMultiplier = dataToUshort( data );

    mon_.drainVoltage2().setValue( Vd2 / 1000.0 ); // In V
    mon_.drainCurrent2().setValue( Id2 / 1000.0 ); // In mA
    mon_.gateVoltage2().setValue( Vg2 / 1000.0 ); // In V
    mon_.vjDacMultiplier().setValue( vjDacMultiplier / 10.0 ); // In %
}

// -----------------------------------------------------------------------------
void SisReceiver::processBlankingFramePacket7( DataVector &data ) 
{
    const short ps24v = dataToShort( data );
    const short ps5vDig = dataToShort( data );
    const short ps12v = dataToShort( data );
    const short psNeg12v = dataToShort( data );

    mon_.ps24v().setValue( ps24v / 1000.0 ); // In V
    mon_.ps5vDigital().setValue( ps5vDig / 1000.0 ); // In V
    mon_.ps12v().setValue( ps12v / 1000.0 ); // In V
    mon_.ps12vNeg().setValue( psNeg12v / 1000.0 ); // V
}

// -----------------------------------------------------------------------------
void SisReceiver::processBlankingFramePacket8( DataVector &data ) 
{
    const short vjDacOut = dataToShort( data );

    mon_.vjDacOut().setValue( vjDacOut );
}

// -----------------------------------------------------------------------------
carma::canbus::Message SisReceiver::simBlankingFramePacket1()
{
    const nodeType node = getNode();
    const short setVj = 5000 * node;
    const short actVj = 4000 * node;
    const short setIj = 5100 * node;
    const short actIj = 2010 * node;
    carma::canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_1 );
    msg << setVj << actVj << setIj << actIj;
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message SisReceiver::simBlankingFramePacket2()
{
    byteType sourceVj = 0, sourceIj = 1, modeVj = 2, modeIj = 0;
    short Ig = 2000, calVg = 20001;
    carma::canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_2 );
    msg << sourceVj << sourceIj << modeVj << modeIj << Ig << calVg;
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message SisReceiver::simBlankingFramePacket3()
{
    short lastVg = 25000, tuneTableId = 0xbeef;
    byteType month = 10, day = 31, year = 5, tuneState = 6;
    carma::canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_3 );
    msg << lastVg << tuneTableId << month << day << year << tuneState;
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message SisReceiver::simBlankingFramePacket4()
{
    const unsigned short atten = 5000; // 50 %
    const short temp = 40; // 40 C
    const byteType ivState = 0;
    const unsigned short rSeries = 34020;
    const byteType rxType = 2; // WBA 
    carma::canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_4 );
    msg << atten << temp << ivState << rSeries << rxType;
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message SisReceiver::simBlankingFramePacket5()
{
    const unsigned short Vd1 = 200, Id1 = 22000, Vg1 = 202, rSense = 15032;
    carma::canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_5 );
    msg << Vd1 << Id1 << Vg1 << rSense;
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message SisReceiver::simBlankingFramePacket6()
{
    unsigned short Vd2 = 200, Id2 = 20000, Vg2 = 202, vjDacMult = 998;
    carma::canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_6 );
    msg << Vd2 << Id2 << Vg2 << vjDacMult;
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message SisReceiver::simBlankingFramePacket7()
{
    short ps24v = 24000, ps5vD = 5000, ps12v = 12000, ps12vNeg = -12000;
    carma::canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_7 );
    msg << ps24v << ps5vD << ps12v << ps12vNeg;
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message SisReceiver::simBlankingFramePacket8()
{
    const short vjDacOut = 1234;
    carma::canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_8 );
    msg << vjDacOut;
    return msg;
}

// -----------------------------------------------------------------------------
void SisReceiver::processIVCurvePoint( DataVector &data )
{
    const unsigned int jd = dataToUlong( data );
    const short Vj = dataToShort( data );
    const short Ij = dataToShort( data );

    ScopedPthreadMutexLock scopelock( ivCurveCollationInfo_.mutex );

    const bool endMarkerReceived = ( jd == 0 );
    const bool nPointsReceived = ( ivCurveCollationInfo_.nPointsReceived == 
                                   ivCurveCollationInfo_.nPointsExpected );

    if ( endMarkerReceived || nPointsReceived ) {
        {
            // Note lock order dependency
            ScopedPthreadMutexLock ivScopelock( ivCurveMutex_ ); 
            ivCurve_ = ivCurveCollationInfo_.tempIVCurve;
        }
            
        ostringstream msg;
        msg << "SisReceiver::processIVCurvePoint() - IVCurve completed ";

        if ( endMarkerReceived && !nPointsReceived ) {
            msg << "prematurely with "
                << ivCurveCollationInfo_.nPointsReceived << " received out of " 
                << ivCurveCollationInfo_.nPointsExpected << " expected.";
            programLogErrorIfPossible( msg.str( ) ); 
        } else if ( nPointsReceived && !endMarkerReceived ) {
            msg << "with expected number of points but no end marker.";
            programLogErrorIfPossible( msg.str( ) );
        } else {
            msg << "successfully.";
            programLogInfoIfPossible( msg.str( ) );
        }
        
        if ( ivCurveCollationInfo_.pendingSequenceNumber ) {
            common_.receivers().tuneSeqNum().setValue( 
                ivCurveCollationInfo_.pendingSequenceNumber );
            ivCurveCollationInfo_.pendingSequenceNumber = 0;
        }

        ivCurveCollationInfo_.nPointsReceived = 0;
        ivCurveCollationInfo_.tempIVCurve.data.clear();

    } else { // This is just another point on the IV curve.

        const double LSBS_PER_DAY = 1000000000.0;
        IVPoint point;
        point.fjd = jd / LSBS_PER_DAY;

        const double LSBS_PER_MICROAMP = 10.0;
        point.Ij = Ij / LSBS_PER_MICROAMP;

        const double LSBS_PER_MILLIVOLT = 1000.0;
        point.Vj = Vj / LSBS_PER_MILLIVOLT;

        ++ivCurveCollationInfo_.nPointsReceived;
        ivCurveCollationInfo_.tempIVCurve.data.push_back( point ); 
    }
}
