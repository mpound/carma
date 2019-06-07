/** @file
 * YigPll class definition.
 *
 * <dl><dt><b>Author</b></dt><dd>Andy Beard</dl>
 * $Revision: 1.26 $
 * $Date: 2011/01/03 18:48:06 $
 * $Id: YigPll.cc,v 1.26 2011/01/03 18:48:06 iws Exp $
 *
 * $CarmaCopyright$
 */

// Carma includes
#include "carma/antenna/ovro/canbus/YigPll.h"
#include "carma/canbus/exceptions.h"
#include "carma/canbus/Utilities.h"
#include "carma/util/Program.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/ScopedPthreadMutexLock.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"

// Carma Tools includes
#include <log4cpp/Category.hh>
#include <log4cpp/Priority.hh>

// System includes
#include <time.h>

// C++ Standard Library includes
#include <iostream>
#include <iomanip>

using namespace carma::antenna::ovro;
using namespace carma::canbus;
using namespace carma::monitor;
using namespace carma::util;
using namespace log4cpp;
using namespace std;

namespace { // Anonymous namespace for local constants and riffraff 

    // API Id for this device.
    const carma::canbus::apiType API_ID                  = 80;

    // API version this class was implemented from 
    const char API_VERSION                               = 'B';

    // Late packet timeout in ms
    const double PACKET_LATE_THRESHOLD                   = 150.0;

    // Control command message ids
    const carma::canbus::msgType SET_YIG_LOCK_FREQ       = 0x040;
    const carma::canbus::msgType EXTRACT_TUNE_TABLE      = 0x041;

    // Engineering control command message ids
    const carma::canbus::msgType SET_YIG_OUTPUT_FREQ     = 0x300;
    const carma::canbus::msgType SET_DAMPING_RESISTANCE  = 0x306;
    const carma::canbus::msgType SWEEP_ON_OFF            = 0x307;

    // Blanking frame message ids.
    const carma::canbus::msgType BLANKING_FRAME_PACKET_1 = 0x0E0;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_2 = 0x0E1;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_3 = 0x0E2;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_4 = 0x0E3;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_5 = 0x0E4;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_6 = 0x0E5;

    //  Broadcast message ids.
    const carma::canbus::msgType LOCK_STATE_CHANGE       = 0x130;

    // Other constants
    const time_t YIG_LOCK_TIMEOUT                        = 3; // 3 seconds.

    const Trace::TraceLevel TRACE_THREADING = Trace::TRACE7;
    
    typedef enum { // Broadcasted state changes coming from YigPll module.
        RAW_UNLOCKED,
        RAW_SEARCHING,
        RAW_REFINING,
        RAW_LOCKED
    } YigLockStateType;
        
    // Shorter aliases for wordy enums.
    typedef OvroSubsystem::LockStateMonitorPointEnum   LockStateEnum;
    typedef AntennaCommon::YigStateMonitorPointEnum  YigStateEnum;
    typedef OvroSubsystem::SweepStatusMonitorPointEnum SweepStatusEnum;
    typedef AntennaCommon::YigSweepMonitorPointEnum  YigSweepEnum;


    YigStateEnum::YIGSTATE 
    convertLockStateToYigState( LockStateEnum::LOCKSTATE lockState )
    {
        YigStateEnum::YIGSTATE answer = YigStateEnum::FAILED;

        switch ( lockState ) {
            case LockStateEnum::SEARCHING:
                answer = YigStateEnum::SEARCH;
                break;
            case LockStateEnum::REFINING:
                answer = YigStateEnum::OPTIMIZING;
                break;
            case LockStateEnum::LOCKED:
                answer = YigStateEnum::LOCK;
                break;
            case LockStateEnum::UNLOCKED:
            default:
                break;
        }
        return answer;
    } // End convertLockStateToYigState

    YigSweepEnum::YIGSWEEP
    convertSweepStatusToYigSweep( SweepStatusEnum::SWEEPSTATUS sweepState )
    {
        switch ( sweepState ) {
            case SweepStatusEnum::OFF: return YigSweepEnum::OFF;
            case SweepStatusEnum::ON: return YigSweepEnum::ON;
            default:
                throw CARMA_ERROR( "Invalid SweepStatus enumeration" );
        }
        throw CARMA_ERROR( "Invalid SweepStatus enumeration" );
    } // End converteSweepStatusToYigSweep

} // End namespace <unnamed>

// -----------------------------------------------------------------------------
YigPll::YigPll( nodeType node, CanOutput & io, OvroSubsystem & mon ) :
    XacDevice(API_ID, node, io),
    log_(Program::getLogger()),
    common_( mon.antennaCommon( ) ),
    mon_( mon.yig( ) )
{
    CPTRACE(Trace::TRACE6, "YigPll::YigPll() - Device "
        "class created for api " << API_ID << " node " << node);
    lockSequence_.state = IDLE;
}

// -----------------------------------------------------------------------------
YigPll::~YigPll()
{
    CPTRACE(Trace::TRACE6, "YigPll::~YigPll() - Device "
        "instance destroyed for api " << API_ID << " node " << getNode());
}

// -----------------------------------------------------------------------------
map<msgType, string> YigPll::getHalfSecMonitors() const
{
    static map<msgType, string> tmp;
    static bool init = false;

    if (!init) {
        tmp[BLANKING_FRAME_PACKET_1] = 
            "YigPll::BLANKING_FRAME_PACKET_1";
        tmp[BLANKING_FRAME_PACKET_2] = 
            "YigPll::BLANKING_FRAME_PACKET_2";
        tmp[BLANKING_FRAME_PACKET_3] = 
            "YigPll::BLANKING_FRAME_PACKET_3";
        tmp[BLANKING_FRAME_PACKET_4] = 
            "YigPll::BLANKING_FRAME_PACKET_4";
        tmp[BLANKING_FRAME_PACKET_5] = 
            "YigPll::BLANKING_FRAME_PACKET_5";
        tmp[BLANKING_FRAME_PACKET_6] = 
            "YigPll::BLANKING_FRAME_PACKET_6";
        init = true;
    }
    return tmp;
}

// -----------------------------------------------------------------------------
map<msgType, string> YigPll::getSlowMonitors() const
{
    return XacDevice::getSlowMonitors();
}

// -----------------------------------------------------------------------------
void YigPll::updateFrameData()
{
    // Set the state of the device...
    mon_.state().setValue(static_cast<StateMonitorPointEnum::STATE>(getState()));
}

// -----------------------------------------------------------------------------
void YigPll::processMsg(msgType mid, vector<byteType> &data, bool sim)
{
    // If state is ONLINE, check if the packet is late...
    if (getState() == ONLINE) {
        if (isPacketLate(PACKET_LATE_THRESHOLD)) {
            incrementLatePacketCount();
        }
    }

    CPTRACE(Trace::TRACEALL, "YigPll::processMsg() - Processing "
        << (sim ? "simulated" : "real") << " msg 0x"
        << hex << mid << dec << " for node " << getNode() << ".");

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
        case LOCK_STATE_CHANGE:
            processLockStateChangePacket(data);
            break;
        default:
            // I don't know how to process this message id!
            // Not a problem, just log it.
            log_ << Priority::DEBUG << "YigPll::processMsg() - "
                << "Switch does not match any case: Unknown mid "
                << mid << ". Node " << getNode();
            CPTRACE(Trace::TRACE6, "YigPll::processMsg() - "
                "Switch doesn't match any case.  mid 0x" << hex  << mid
                << dec << " node " << getNode() << ".");
            break;
    }
}

// -----------------------------------------------------------------------------
carma::canbus::Message YigPll::simulateMsg(msgType mid)
{
    carma::canbus::Message msg;
    CPTRACE(Trace::TRACEALL, "YigPll::simulateMsg() - Simulating msg "
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
            log_ << Priority::DEBUG << "YigPll::simulateMsg - "
                << "Switch does not match any case: mid " << mid;
            CPTRACE(Trace::TRACE6, "YigPll::simulateMsg() - "
                "Switch doesn't match any case.  mid 0x" << hex  << mid
                << dec << " node " << getNode() << ".");
            break;
    }
    return msg;
}

// -----------------------------------------------------------------------------
void YigPll::processBlankingFramePacket1(DataVector &data)
{
    LockStateEnum::LOCKSTATE lockState;
    unsigned char dataValid;
    unsigned short yigFreq, loopGainRes, dampingRes;

    if (data.size() < 8) {
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "YigPll::processBlankingFramePacket1 - Data size < 8!");
    }

    // Unpack the data
    lockState = static_cast< LockStateEnum::LOCKSTATE >( dataToUbyte(data) );
    dataValid = dataToUbyte(data);
    yigFreq = dataToUshort(data); // In MHz
    loopGainRes = dataToUshort(data);
    dampingRes = dataToUshort(data);

    // Convert and stuff into monitor stream
    mon_.lockState().setValue( lockState ); 
    mon_.dataValid().setValue(dataValid == 0x01);
    mon_.yigFreq().setValue( yigFreq / 1000.0 ); // In GHz
    mon_.loopGainRes().setValue(loopGainRes);
    mon_.dampingRes().setValue(dampingRes);
    
    // Set common monitor points
    common_.lO().yigFreq().setValue( yigFreq / 1000.0 ); // In GHz
    common_.lO().yigState().setValue( convertLockStateToYigState( lockState ) );
}

// -----------------------------------------------------------------------------
void YigPll::processBlankingFramePacket2(DataVector &data)
{
    short noiseMeterV;
    unsigned short yigCurrent;
    double vError, ifLevel;
    
    if (data.size() < 8) {
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "YigPll::processBlankingFramePacket2 - Data size < 8!");
    }

    // Unpack the data
    ifLevel = ( dataToShort(data) / 1000.0 ); // mV to Volts
    vError = ( dataToShort(data) / 1000.0 ); // mV to Volts
    yigCurrent = dataToUshort(data);
    noiseMeterV = dataToShort(data);

    // Convert and stuff into monitor stream
    mon_.ifLevel().setValue( ifLevel ); // mV to Volts
    mon_.errorVoltage().setValue( vError );
    mon_.yigCurrent().setValue(static_cast<float>(yigCurrent));
    mon_.noiseMeterVoltage().setValue(noiseMeterV * 0.001);

    // Set common monitor points...
    common_.lO().yigError().setValue( vError );
    common_.lO().yigIFLevel().setValue( ifLevel );
}

// -----------------------------------------------------------------------------
void YigPll::processBlankingFramePacket3(DataVector &data)
{
    short temp, ps24v, ps5vDigital, ps5vAnalog;
    
    if (data.size() < 8) {
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "YigPll::processBlankingFramePacket3 - Data size < 8!");
    }

    // Unpack the data
    temp = dataToShort(data);
    ps24v = dataToShort(data);
    ps5vDigital = dataToShort(data);
    ps5vAnalog = dataToShort(data);

    // Convert and stuff into monitor stream
    mon_.modTemp().setValue(temp * 0.01); // 0.1C to C
    mon_.psPos24vAnalog().setValue(ps24v * 0.001);
    mon_.psPos5vDigital().setValue(ps5vDigital * 0.001);
    mon_.psPos5vAnalog().setValue(ps5vAnalog * 0.001);
}

// -----------------------------------------------------------------------------
void YigPll::processBlankingFramePacket4(DataVector &data)
{
    short ps9vAnalog, ps15vAnalog, psNeg5vAnalog, psNeg15vAnalog;
    
    if (data.size() < 8) {
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "YigPll::processBlankingFramePacket4 - Data size < 8!");
    }

    // Unpack the data
    ps9vAnalog = dataToShort(data);
    ps15vAnalog = dataToShort(data);
    psNeg5vAnalog = dataToShort(data);
    psNeg15vAnalog = dataToShort(data);

    // Convert and stuff into monitor stream
    mon_.psPos9v().setValue(ps9vAnalog * 0.001); // mV to V
    mon_.psPos15v().setValue(ps15vAnalog * 0.001); // mV to V
    mon_.psNeg5vAnalog().setValue(psNeg5vAnalog * 0.001); // mV to V
    mon_.psNeg15v().setValue(psNeg15vAnalog * 0.001); // mV to V
}

// -----------------------------------------------------------------------------
void YigPll::processBlankingFramePacket5(DataVector &data)
{
    unsigned char id, month, day, year;
    unsigned char refStat, autoRelock, relockCount;
    SweepStatusEnum::SWEEPSTATUS sweepStat;
    ostringstream calDate;
    
    if (data.size() < 4) {
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "YigPll::processBlankingFramePacket5 - Data size < 4!");
    }

    // Unpack the data
    id = dataToUbyte(data);
    month = dataToUbyte(data);
    day = dataToUbyte(data);
    year = dataToUbyte(data);
    sweepStat = static_cast<SweepStatusEnum::SWEEPSTATUS>( dataToUbyte(data) );
    refStat = dataToUbyte(data);
    autoRelock = dataToUbyte(data);
    relockCount = dataToUbyte(data);

    calDate << static_cast<int>(month) << "/"
            << static_cast<int>(day) << "/"
            << setw(2) << setfill('0') << static_cast<int>(year);
    
    mon_.yigId().setValue(id);
    mon_.calDate().setValue( calDate.str() );
    mon_.sweepStatus().setValue( sweepStat );
    mon_.refStatus().setValue(static_cast<
        OvroSubsystem::RefStatusMonitorPointEnum::REFSTATUS>(refStat));
    mon_.autoRelockStatus().setValue(static_cast<
        OvroSubsystem::AutoRelockStatusMonitorPointEnum::AUTORELOCKSTATUS>(
            autoRelock));
    mon_.relockCount().setValue(relockCount);

    // Set common monitor points.
    common_.lO().yigSweep().setValue( convertSweepStatusToYigSweep( sweepStat ) );
}

// -----------------------------------------------------------------------------
void YigPll::processBlankingFramePacket6( DataVector & data )
{
    if (data.size() < 1) {
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "YigPll::processBlankingFramePacket6 - Data size < 1!");
    }

    const unsigned char lockBit = dataToUbyte( data );
    
    typedef OvroSubsystem::LockBitStatusMonitorPointEnum LockBitStatusSPE;
    mon_.lockBitStatus().setValue( 
        static_cast<LockBitStatusSPE::LOCKBITSTATUS>( lockBit ) );
}

// -----------------------------------------------------------------------------
void YigPll::processLockStateChangePacket(DataVector &data)
{
    ScopedPthreadMutexLock scopelock(lockSequence_.mutex);
    YigLockStateType state = static_cast<YigLockStateType>(dataToUbyte(data));
                    
    CPTRACE( Trace::TRACE4, "YigPll::processLockStateChangePacket() - "
                        "Received LOCK_STATE_CHANGED message " 
                        << static_cast<int>(state) << ", lock sequence "
                        << static_cast<int>(lockSequence_.state) << "." );
    
    // If set frequency command hasn't initiated a lock sequence, return.
    if ( lockSequence_.state == UNLOCKED || lockSequence_.state == LOCKED )
        return;

    switch ( lockSequence_.state ) {
        case IDLE:
            switch ( state ) {
                case RAW_UNLOCKED:
                case RAW_SEARCHING:
                case RAW_REFINING:
                    lockSequence_.state = WAITING;
                    CPTRACE(Trace::TRACE4, 
                        "YigPll::processLockStateChangePacket() - "
                        "Transitioning to WAITING.");
                    break;
                default:
                    break;
            }
            break;
        case WAITING:
            switch ( state ) {
                case RAW_UNLOCKED:
                    lockSequence_.state = UNLOCKED;
                    CPTRACE(Trace::TRACE4, 
                        "YigPll::processLockStateChangePacket() - "
                        "Transitioning to UNLOCKED.");
                    break;
                case RAW_LOCKED:
                    lockSequence_.state = LOCKED;
                    CPTRACE(Trace::TRACE4, 
                        "YigPll::processLockStateChangePacket() - "
                        "Transitioning to LOCKED.");
                    break;
                case RAW_SEARCHING:
                case RAW_REFINING:
                default:
                    // Keep on waiting on
                    break;
            }
            break;
        case UNLOCKED:
        case LOCKED:
        default:
            // Invalid transitions
            break;
    }

    // Signal to waiters if we're LOCKED or UNLOCKED.
    if ( lockSequence_.state == UNLOCKED || lockSequence_.state == LOCKED )
        lockSequence_.cond.Broadcast();
}

// -----------------------------------------------------------------------------
carma::canbus::Message YigPll::simBlankingFramePacket1()
{
    carma::canbus::Message msg(
        createId(true, getApi(), getNode(), BLANKING_FRAME_PACKET_1),
        getBusId());
    vector<byteType> data;
    uByteToData(data, ( Time::computeCurrentFrame( ) % 2 ) * 0x03);
    uByteToData(data, 0x00); // Not valid
    uShortToData(data, 100); // 100MHz
    uShortToData(data, 50); // 50 Ohms
    uShortToData(data, 50); // 50 Ohms
    msg.setData(data);
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message YigPll::simBlankingFramePacket2()
{
    carma::canbus::Message msg(
        createId(true, getApi(), getNode(), BLANKING_FRAME_PACKET_2),
        getBusId());
    vector<byteType> data;
    sShortToData(data, 2500); // 2.5 Volts.
    sShortToData(data, 1);    // 0.001 Volts.
    uShortToData(data, 150); // 150 mA
    sShortToData(data, 500); // 0.5 Volts 
    msg.setData(data);
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message YigPll::simBlankingFramePacket3()
{
    carma::canbus::Message msg(
        createId(true, getApi(), getNode(), BLANKING_FRAME_PACKET_3),
        getBusId());
    vector<byteType> data;
    sShortToData(data, 500); // 50 C
    sShortToData(data, 24000); // 24 V
    sShortToData(data, 5000); // 5 V
    sShortToData(data, 5111); // 5.111V
    msg.setData(data);
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message YigPll::simBlankingFramePacket4()
{
    carma::canbus::Message msg(
        createId(true, getApi(), getNode(), BLANKING_FRAME_PACKET_4),
        getBusId());
    vector<byteType> data;
    sShortToData(data, 9000); // 9 V
    sShortToData(data, 15000); // 15 V
    sShortToData(data, -5000); // -5 V
    sShortToData(data, -15000); // -15 V
    msg.setData(data);
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message YigPll::simBlankingFramePacket5()
{
    carma::canbus::Message msg(
        createId(true, getApi(), getNode(), BLANKING_FRAME_PACKET_5),
        getBusId());
    vector<byteType> data;
    uByteToData(data, 1);
    uByteToData(data, 12); // Dec
    uByteToData(data, 02); // 2nd
    uByteToData(data, 50); // Equally unbelievable 2050 or 1950!
    uByteToData(data, 0x00); // OFF
    uByteToData(data, 0x00); // BAD
    uByteToData(data, 0x00); // OFF
    uByteToData(data, 255);
    msg.setData(data);
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message YigPll::simBlankingFramePacket6()
{
    carma::canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_6 );
    const unsigned char lockBit = 0x00; // UNLOCKED
    msg << lockBit;
    return msg;
}
// -----------------------------------------------------------------------------
void YigPll::setYigFrequencyAndLockNoBlock( const double freqInGhz )
{
    const unsigned short freqVal = 
        static_cast<unsigned short>( freqInGhz * 1000 ); 
    carma::canbus::Message msg = createMsgToNode( SET_YIG_LOCK_FREQ );
    msg << freqVal;
    io_.postMessage( msg );  // Initiate the lock sequence
}
 
// -----------------------------------------------------------------------------
YigPll::LockResultType YigPll::setYigFrequencyAndLock( const double freqInGhz )
{
    const ScopedLogNdc ndc( "YigPll::setYigFrequencyAndLock()" );
    bool timeout = false;
    struct timespec ts;
    LockResultType result;

    clock_gettime(CLOCK_REALTIME, &ts);
    ts.tv_sec = ts.tv_sec + YIG_LOCK_TIMEOUT;

    // Lock mutex, test predicate, wait, test again, unlock, proceed.  The Mutex
    // Lock and unlock (implicit) come from the scopelock guard below.
    ScopedPthreadMutexLock scopelock( lockSequence_.mutex );

    lockSequence_.state = IDLE;

    CPTRACE(Trace::TRACE4, "Transitioning to IDLE.");

    setYigFrequencyAndLockNoBlock( freqInGhz );

    // Wait until either we're locked, unlocked or timedout.
    // TimedWait call below unlocks mutex and returns with it locked.
    while ( lockSequence_.state != LOCKED && 
            lockSequence_.state != UNLOCKED && 
            !timeout ) { 
        CARMA_CPTRACE( TRACE_THREADING, "Blocking on condition variable." );
        timeout = !( lockSequence_.cond.TimedWait( lockSequence_.mutex, ts ) );
        CARMA_CPTRACE( TRACE_THREADING, "Wake on condition variable." );
    }
       
    if ( timeout ) {
        result = YigPll::YIG_TIMEDOUT;
        lockSequence_.state = UNLOCKED;
        CPTRACE( Trace::TRACE4, "TIMEDOUT while waiting for lock." );
    } else if ( lockSequence_.state == LOCKED ) {
        result = YigPll::YIG_LOCKED;
    } else if ( lockSequence_.state == UNLOCKED ) {
        result = YigPll::YIG_UNLOCKED;
    } else { 
        // Assume an error and mark as unlocked
        result = YigPll::YIG_UNLOCKED;
    }

    return result;
}

// -----------------------------------------------------------------------------
void YigPll::extractTuneTable()
{
    carma::canbus::Message msg(
        createId(false, getApi(), getNode(), EXTRACT_TUNE_TABLE), getBusId());
    
    // Message has no data! Post to bus.
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void YigPll::setYigFrequencyWithoutLock(double freqInGhz) 
{
    carma::canbus::Message msg(
        createId(false, getApi(), getNode(), SET_YIG_OUTPUT_FREQ), getBusId());
    vector<byteType> data;  // Raw CAN bytes.
    unsigned short freqVal; // Raw frequency value.
    
    // Convert input frequency to integer values.
    freqVal = static_cast<unsigned short>(freqInGhz * 1000);
    
    // Convert to raw data, add to message and post to bus.
    uShortToData(data, freqVal);
    msg.setData(data);
    io_.postMessage(msg);
   
}
   
// -----------------------------------------------------------------------------
void YigPll::toggleSweep(bool on)
{
    carma::canbus::Message msg(
        createId(false, getApi(), getNode(), SWEEP_ON_OFF), getBusId());
    vector<byteType> data;

    // Convert to raw data, add data to message and post msg to bus.
    uByteToData(data, (on ? 0x01 : 0x00));
    msg.setData(data);
    io_.postMessage(msg);
}

// -----------------------------------------------------------------------------
void YigPll::setDampingResistance(unsigned short damping)
{
    carma::canbus::Message msg(
        createId(false, getApi(), getNode(), SET_DAMPING_RESISTANCE), 
        getBusId());
    vector<byteType> data;

    // Convert to raw data, add data to message and post message to bus.
    uShortToData(data, damping);
    msg.setData(data);
    io_.postMessage(msg);
}
