/** @file
 * CAN Device definition for 10-m Antenna Drive Module.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.52 $
 * $Date: 2014/10/20 17:56:17 $
 * $Id: Drive.cc,v 1.52 2014/10/20 17:56:17 scott Exp $
 */

#include "carma/antenna/ovro/canbus/Drive.h"

#include "carma/antenna/ovro/canbus/Encoder.h"
#include "carma/canbus/Message.h"
#include "carma/canbus/Utilities.h"
#include "carma/canbus/Types.h"
#include "carma/monitor/OvroSubsystem.h"
#include "carma/services/Angle.h"
#include "carma/services/Global.h"
#include "carma/services/Table.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/IllegalStateException.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/ScopedPthreadMutexLock.h"
#include "carma/util/ScopedPthreadMutexLockManager.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"

#include <iostream>

using namespace carma::antenna::ovro;
using namespace carma::canbus;
using namespace carma::monitor;
using namespace carma::services;
using namespace carma::util;
using namespace std;

namespace {

    const carma::util::Trace::TraceLevel TRACE_CTOR_DTOR = Trace::TRACE3;
    const carma::util::Trace::TraceLevel TRACE_TIMING = Trace::TRACE3;

    const double PACKET_LATE_THRESHOLD                   = 150.0;
    const carma::canbus::apiType API_NO                  = 248;
    const carma::canbus::nodeType NODE_NO                = 1;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_1 = 0x0E0;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_2 = 0x0E1;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_3 = 0x0E2;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_4 = 0x0E3;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_5 = 0x0E4;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_6 = 0x0E5;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_7 = 0x0E6;
    const carma::canbus::msgType BLANKING_FRAME_START = BLANKING_FRAME_PACKET_1;
    const carma::canbus::msgType BLANKING_FRAME_END = BLANKING_FRAME_PACKET_7;
    const carma::canbus::msgType SET_DRIVE_STATE         = 0x040;
    const carma::canbus::msgType SET_AZ_AND_EL_RATE      = 0x041;
    const carma::canbus::msgType SET_AZ_OR_EL_RATE       = 0x042;
    const carma::canbus::msgType SET_AZ_OR_EL_RATE_LIMIT = 0x043;
    const carma::canbus::msgType SET_ENGINEERING_MODE    = 0x300;

    const double DEG_PER_RAD = ( 180.0 ) / ( M_PI );
    const double SEC_PER_MIN = 60.0;

    const double AZ_HW_LIMIT_PLUS_IN_DEG  = 354.0;
    const double AZ_HW_LIMIT_MINUS_IN_DEG = -89.0;
    const double EL_HW_LIMIT_PLUS_IN_DEG  = 88.5;
    const double EL_HW_LIMIT_MINUS_IN_DEG = 5.0; 

    const double DEFAULT_SIM_AZ_IN_DEG = 0.0;
    const double DEFAULT_SIM_EL_IN_DEG = 20.0;

    /**
     * Max slew rate. This is the maximum rate we will drive the antennas at.
     * Each antenna may have the ability to drive slightly faster or slower
     * but we limit slew rates to the below values. Ditto for voltage.
     */
    const double MAX_AZ_SLEW_RATE_IN_DEG_PER_MIN = 60.0;

    const double MAX_EL_SLEW_RATE_IN_DEG_PER_MIN = 30.0;

    const double MAX_VOLTAGE = 5.0;

    const double AZ_GAIN_IN_DEG_PER_MIN_PER_VOLT = 
        MAX_AZ_SLEW_RATE_IN_DEG_PER_MIN / MAX_VOLTAGE;

    // Negative elevation voltages correspond to positive rates.
    const double EL_GAIN_IN_DEG_PER_MIN_PER_VOLT = 
        -1.0 * MAX_EL_SLEW_RATE_IN_DEG_PER_MIN / MAX_VOLTAGE;
    
    // I can't tell you how much I hate this - lower the bus rate I say again.
    const ::timespec delayBetweenMsgs = { 0, 1000000 };
    ::timespec gIgnoredRemainder;
    
    double
    getLeverarmRateFactor( const Angle & elevation );

} // namespace <unnamed>


    struct Drive::SimInfo {
    
        SimInfo( unsigned short antNo );

        void updateSimInfo( double azGainInDegPerMinPerVolt,
                            double elGainInDegPerMinPerVolt );
        
        double azPositionInDeg;       // degrees 
        double elPositionInDeg;       // degrees 
        double azVoltage;
        double elVoltage;
        bool driveEnabled;
        bool overlap;
        bool azPosLimit;
        bool azNegLimit;
        bool elPosLimit;
        bool elNegLimit;
        bool engineeringMode;
        double lastPositionUpdateMJD;
        const unsigned short antNumber;
    };

    Drive::SimInfo::SimInfo( const unsigned short antNo ) :
        azPositionInDeg( DEFAULT_SIM_AZ_IN_DEG ),
        elPositionInDeg( DEFAULT_SIM_EL_IN_DEG ),
        azVoltage( 0.0 ),
        elVoltage( 0.0 ),
        driveEnabled( true ),
        overlap( false ),
        azPosLimit( false ),
        azNegLimit( false ),
        elPosLimit( false ),
        elNegLimit( false ),
        engineeringMode( false ),
        lastPositionUpdateMJD( Time::MJD( ) ),
        antNumber( antNo )
    { 
        // Nothing
    }

    void 
    Drive::SimInfo::updateSimInfo( const double azGainInDegPerMinPerVolt,
                            const double elGainInDegPerMinPerVolt )
    {
        const double now = Time::MJD( );

        const double diffInMinutes = 
            ( now - lastPositionUpdateMJD ) * Time::MINUTES_PER_DAY;

        const double azRateInDegPerMin = azVoltage * azGainInDegPerMinPerVolt;
        azPositionInDeg += ( azRateInDegPerMin * diffInMinutes );

        const double elRateInDegPerMin = elVoltage * elGainInDegPerMinPerVolt;
        elPositionInDeg += ( elRateInDegPerMin * getLeverarmRateFactor(
            Angle( elPositionInDeg, Angle::DEGREES_STR ) ) * diffInMinutes );

        // Use the antenna number to provide some variability in where the
        // overlap switch gets triggered.  Note that the overlap switch cams
        // are quite variable and not consistently set.
        overlap = ( azPositionInDeg < 4 * antNumber * 2.15423 );

        azNegLimit = ( azPositionInDeg <= AZ_HW_LIMIT_MINUS_IN_DEG );
        azPosLimit = ( azPositionInDeg >= AZ_HW_LIMIT_PLUS_IN_DEG );
        elNegLimit = ( elPositionInDeg <= EL_HW_LIMIT_MINUS_IN_DEG );
        elPosLimit = ( elPositionInDeg >= EL_HW_LIMIT_PLUS_IN_DEG );
        lastPositionUpdateMJD = now;
    }

namespace {
    
    double
    getLeverarmRateFactor( const Angle & elevation )
    {
        // Due to the non-linear ball screw elevation leverarm,
        // elevation rates are directly dependent upon the elevation itself.
        // The relationship is well modeled by a quadratic function fit from
        // the following values:
        //  EL          Leverarm
        //  90            1.00
        //  60            0.93
        //  45            0.85
        //  30            0.80
        //  15            0.70
        //   0            0.60
        //
        // The legacy VAX code uses a quadratic with a=-8.493/74.0,
        // b=32.726/74.0 and c=43.8/74.0.  These values are in good agreement
        // with matlab/mathcad fits from the above data set.
        const double el = elevation.radians( );
        return ( ( ( ( (-8.493 * el ) + 32.726 ) * el) + 43.8 ) / 74.0 );

    } // getLeverarmRateFactor

    void
    verifyConfTableConsistency( const Table & table, unsigned short antNo ) 
    {
        // About all we can do here is make sure we've got the necessary
        // columns and that they each contain the proper number of rows.
        vector<int> antNos = table.getIntColumn( "no" );
        vector<double> azOffsets = table.getDoubleColumn( "az_dc_offset" );
        vector<double> elOffsets = table.getDoubleColumn( "el_dc_offset" );
        vector<double> azMaxRates = table.getDoubleColumn( "az_max" );
        vector<double> elMaxRates = table.getDoubleColumn( "el_max" );
        vector<double> azGains = table.getDoubleColumn( "az_gain" );
        vector<double> elGains= table.getDoubleColumn( "el_gain" );

        vector<double>::size_type nAnts = Global::nOvroAntennas( );

        if ( antNos.at( antNo - 1 ) != antNo || // Verify ant entry exists
             antNos.size( ) != nAnts ||
             azOffsets.size( ) != nAnts ||
             elOffsets.size( ) != nAnts ||
             azMaxRates.size( ) != nAnts ||
             elMaxRates.size( ) != nAnts ||
             azGains.size( ) != nAnts ||
             elGains.size( ) != nAnts ) {

            programLogErrorIfPossible( "verifyConfTableConsistency( ) - "
                "Conf data does not contain the proper number of elements." );
            throw CARMA_EXCEPTION( IllegalStateException, "Conf data does "
                "not contain the proper number of elements." );
        }

    } // verifyConfTableConsistency

} // namespace <unnamed>

Drive::TimestampedAzOverlapState::TimestampedAzOverlapState() :
    mjd( 0.0 ),
    azOverlapDetected( false )
{ }

struct Drive::Shared {

    explicit Shared( unsigned short antNo );

    mutable PthreadMutex mutex_; // Below info is shared by 2 or more threads
    
    bool driveEnabled_;
    bool engineeringMode_;
    bool azPosLimit_;
    bool azNegLimit_;
    bool elPosLimit_;
    bool elNegLimit_;
    double requestedAzInDegPerMin_;
    double requestedElInDegPerMin_;
    double commandedAzInDegPerMin_;
    double commandedElInDegPerMin_;
    double azDcOffsetVoltage_;
    double elDcOffsetVoltage_;
    double azVoltage_;
    double elVoltage_;
    double azMaxVoltage_;
    double elMaxVoltage_;
    double azMaxRateInDegPerMin_;
    double elMaxRateInDegPerMin_;
    double azGainInDegPerMinPerVolt_;
    double elGainInDegPerMinPerVolt_;
    
    BaseSwitchState baseSwitchState_;
    TimestampedAzOverlapState azOverlapState_;
    bool controllerOvertemp_; // Any of the three controllers...
    SimInfo sim_;
};

Drive::Shared::Shared( const unsigned short antNo ) :
    engineeringMode_( false ),
    azPosLimit_( false ),
    azNegLimit_( false ),
    elPosLimit_( false ),
    elNegLimit_( false ),
    requestedAzInDegPerMin_( 0.0 ),
    requestedElInDegPerMin_( 0.0 ),
    commandedAzInDegPerMin_( 0.0 ),
    commandedElInDegPerMin_( 0.0 ),
    azDcOffsetVoltage_( 0.0 ),
    elDcOffsetVoltage_( 0.0 ),
    azVoltage_( 0.0 ),
    elVoltage_( 0.0 ),
    azMaxVoltage_( MAX_VOLTAGE ),
    elMaxVoltage_( MAX_VOLTAGE ),
    azMaxRateInDegPerMin_( MAX_AZ_SLEW_RATE_IN_DEG_PER_MIN ), 
    elMaxRateInDegPerMin_( MAX_EL_SLEW_RATE_IN_DEG_PER_MIN ),
    azGainInDegPerMinPerVolt_( AZ_GAIN_IN_DEG_PER_MIN_PER_VOLT ),
    elGainInDegPerMinPerVolt_( EL_GAIN_IN_DEG_PER_MIN_PER_VOLT ),
    azOverlapState_( ),
    controllerOvertemp_(false),
    sim_( antNo )
{

}
    
Drive::Drive( carma::canbus::CanOutput & canOutput,
              carma::monitor::OvroSubsystem & monitorSubsys,
              const unsigned short antNo,
              const string confFilename ) :
    XacDevice( API_NO, NODE_NO, canOutput ),
    mon_( monitorSubsys ),
    driveMon_( monitorSubsys.drive().driveModule( ) ),
    limitMon_( monitorSubsys.drive().limit( ) ),
    trackMon_( monitorSubsys.drive().track( ) ),
    comMon_( monitorSubsys.antennaCommon( ) ),
    antNo_( antNo ),
    configFilename_( ProgramBase::getConfFile( confFilename ) ),
    shared_( new Drive::Shared( antNo ) )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "ovro::Drive( ) - Constructor." );

    try {
        table_ = auto_ptr<Table>( 
            new Table( configFilename_ ) );

        updateConfDataFromFile( );
    } catch (...) {
        logCaughtAsError( );
        programLogErrorIfPossible( "Unable to open " + configFilename_
            + ", default configuration parameters will be used instead." );
    }
}
        
Drive::~Drive( )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "ovro::~Drive( ) - Destructor." );
}

void
Drive::enableDrives( const bool enable ) 
{
    const unsigned char reqDrvState = ( enable ? 0x01 : 0x00 );

    carma::canbus::Message msg = createMsgToNode( SET_DRIVE_STATE );

    msg << reqDrvState;

    io_.postMessage( msg );

    {
        ScopedPthreadMutexLock scopelock( shared_->mutex_ );
    
        shared_->sim_.updateSimInfo( shared_->azGainInDegPerMinPerVolt_, 
                                     shared_->elGainInDegPerMinPerVolt_ );
    
        shared_->sim_.driveEnabled = enable;
    }
}

void
Drive::setAzElDriveRates( const double azRateInRadPerS, 
                          const double elRateInRadPerS,
                          const Angle & elevation )
{
    // Apply leverarm correction - based on the elevation we drive the motors
    // harder to achieve the requested elevation rate.
    const double leverarm = getLeverarmRateFactor( elevation );

    const double azRateDegPerMin = azRateInRadPerS * DEG_PER_RAD * SEC_PER_MIN;
    const double elRateDegPerMin = 
        elRateInRadPerS * DEG_PER_RAD * SEC_PER_MIN / leverarm;

    double commandedAzRateDegPerMin = azRateDegPerMin;
    double commandedElRateDegPerMin = elRateDegPerMin;

    ScopedPthreadMutexLockManager managedLock( shared_->mutex_ );

    managedLock.LockMutex( );

    double azVoltage = azRateDegPerMin / shared_->azGainInDegPerMinPerVolt_;
    double elVoltage = elRateDegPerMin / shared_->elGainInDegPerMinPerVolt_;
    
    // Apply dc offsets.  This could also be done after limiting to the 
    // max voltage, however, this makes us vulnerable to incorrect changes
    // to the conf file (e.g. very large offsets).  Besides, offsets are so 
    // small that they may not even be seen at large slew rates.
    azVoltage += shared_->azDcOffsetVoltage_;
    elVoltage += shared_->elDcOffsetVoltage_;

    // Limit to current max voltage 
    if ( fabs( azVoltage ) > shared_->azMaxVoltage_ ) {
        const float azSign = ( azVoltage < 0.0 ? -1.0 : 1.0 );
        azVoltage = azSign * shared_->azMaxVoltage_;
        commandedAzRateDegPerMin  = 
            azVoltage * shared_->azGainInDegPerMinPerVolt_;
    }
         
    if ( fabs( elVoltage ) > shared_->elMaxVoltage_ ) {
        const float elSign = ( elVoltage < 0.0 ? -1.0 : 1.0 );
        elVoltage = elSign * shared_->elMaxVoltage_;
        commandedElRateDegPerMin  = 
            elVoltage * shared_->elGainInDegPerMinPerVolt_;
    }
    
    managedLock.UnlockMutex( );

    if ( !isDriveEnabled( ) ) {
        enableDrives( true );
        nanosleep( &delayBetweenMsgs, &gIgnoredRemainder ); 
    }

    setDriveVoltages( azVoltage, elVoltage );

    // Now update shared and sim info...
    managedLock.LockMutex( );

    shared_->sim_.updateSimInfo( shared_->azGainInDegPerMinPerVolt_, 
                                 shared_->elGainInDegPerMinPerVolt_ );

    // Set drive rate info
    shared_->requestedAzInDegPerMin_ = 
        azRateInRadPerS * DEG_PER_RAD * SEC_PER_MIN;
    shared_->requestedElInDegPerMin_ = 
        elRateInRadPerS * DEG_PER_RAD * SEC_PER_MIN;
    shared_->commandedAzInDegPerMin_ = commandedAzRateDegPerMin;
    shared_->commandedElInDegPerMin_ = commandedElRateDegPerMin;

    shared_->sim_.azVoltage = azVoltage;
    shared_->sim_.elVoltage = elVoltage;
    
    managedLock.UnlockMutex( );
}
        
void
Drive::stop( )
{
    if ( isDriveEnabled( ) ) {
        setAzElDriveRates( 0.0, 0.0, Angle( 0.0, Angle::RADIANS_STR ) );
        nanosleep( &delayBetweenMsgs, &gIgnoredRemainder ); 
    }
    enableDrives( false );
}

Drive::TimestampedAzOverlapState
Drive::getAzOverlapState( ) const
{
    ScopedPthreadMutexLock scopelock( shared_->mutex_ );
    return shared_->azOverlapState_;
}

bool 
Drive::getControllerOvertemp() const
{
    return shared_->controllerOvertemp_;
}

bool
Drive::isDriveEnabled( ) const
{
    ScopedPthreadMutexLock scopelock( shared_->mutex_ );
    return shared_->driveEnabled_;
}

bool
Drive::isAtHwLimit( ) const
{
    ScopedPthreadMutexLock scopelock( shared_->mutex_ );
    
    if ( !shared_->driveEnabled_ && 
         shared_->azPosLimit_ && shared_->azNegLimit_ &&
         shared_->elPosLimit_ && shared_->elNegLimit_ ) {
        return false;
    } else if ( shared_->driveEnabled_ && 
                ( shared_->azPosLimit_ || shared_->azNegLimit_ ||
                  shared_->elPosLimit_ || shared_->elNegLimit_ ) ) {
        return true;
    } else {
        return false;
    }
}

bool
Drive::isInEngineeringMode( ) const
{
    ScopedPthreadMutexLock scopelock( shared_->mutex_ );
    return shared_->engineeringMode_;
}

Drive::BaseSwitchState
Drive::getBaseSwitchState( ) const
{
    ScopedPthreadMutexLock scopelock( shared_->mutex_ );
    return shared_->baseSwitchState_;
}

carma::canbus::MsgIdInfoMap 
Drive::getHalfSecMonitors( ) const
{
    static MsgIdInfoMap midMap;
    static bool init = false;
    if ( !init ) {

        for ( msgType bf = BLANKING_FRAME_START; 
              bf <= BLANKING_FRAME_END;
              ++bf ) {
            ostringstream des;
            des << "BLANKING_FRAME_PACKET_" 
                << ( bf - BLANKING_FRAME_START );
            midMap[ bf ] = des.str( );
        }
    }

    return midMap;
}

carma::canbus::MsgIdInfoMap 
Drive::getSlowMonitors( ) const
{
    return XacDevice::getSlowMonitors( );
}

void
Drive::processMsg( msgType messageId,
                   vector< byteType > & data,
                   bool sim )
{
    // If state is ONLINE, check if the packet is late...
    if ( getState() == ONLINE ) {
        if ( isPacketLate( PACKET_LATE_THRESHOLD ) ) {
            incrementLatePacketCount( );
        }
    }

    CPTRACE(Trace::TRACEALL, "Drive::processMsg() - Processing "
        << (sim ? "simulated" : "real") << " msg 0x"
        << hex << messageId << dec << " for node " << getNode() << ".");

    switch ( messageId ) {  
        case BLANKING_FRAME_PACKET_1:
            processBlankingFramePacket1( data );
            break;
        case BLANKING_FRAME_PACKET_2:
            processBlankingFramePacket2( data );
            break;
        case BLANKING_FRAME_PACKET_3:
            processBlankingFramePacket3( data );
            break;
        case BLANKING_FRAME_PACKET_4:
            processBlankingFramePacket4( data );
            break;
        case BLANKING_FRAME_PACKET_5:
            processBlankingFramePacket5( data );
            break;
        case BLANKING_FRAME_PACKET_6:
            processBlankingFramePacket6( data );
            break;
        case BLANKING_FRAME_PACKET_7:
            processBlankingFramePacket7( data );
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_1:
        case XacDevice::SYSTEM_MONITOR_PACKET_2:
        case XacDevice::SYSTEM_MONITOR_PACKET_3:
        case XacDevice::SYSTEM_MONITOR_PACKET_4:
        case XacDevice::SYSTEM_MONITOR_PACKET_5:
            XacDevice::processSystemMonitorPacket( 
                messageId, 
                data, 
                driveMon_.xac( ) );
            break;
        default:
            { 
                ostringstream msg;
                msg << "Drive::processMsg() - Switch does not match any case: "
                    << "Unknown mid " << messageId << ". Node " 
                    << getNode() << ".";
                programLogInfoIfPossible( msg.str( ) );
                CPTRACE( Trace::TRACE6, msg.str( ) );
            }
            break;
    }
}

carma::canbus::Message
Drive::simulateMsg( msgType messageId )
{
    switch ( messageId ) {
        case BLANKING_FRAME_PACKET_1:
            return simulateBlankingFramePacket1( );
        case BLANKING_FRAME_PACKET_2:
            return simulateBlankingFramePacket2( );
        case BLANKING_FRAME_PACKET_3:
            return simulateBlankingFramePacket3( );
        case BLANKING_FRAME_PACKET_4:
            return simulateBlankingFramePacket4( );
        case BLANKING_FRAME_PACKET_5:
            return simulateBlankingFramePacket5( );
        case BLANKING_FRAME_PACKET_6:
            return simulateBlankingFramePacket6( );
        case BLANKING_FRAME_PACKET_7:
            return simulateBlankingFramePacket7( );
        case Encoder::FAKE_AZ_ENCODER_PACKET:
            {
                ScopedPthreadMutexLock scopelock( shared_->mutex_ );

                shared_->sim_.updateSimInfo( 
                    shared_->azGainInDegPerMinPerVolt_, 
                    shared_->elGainInDegPerMinPerVolt_ );

                double azEncReading = shared_->sim_.azPositionInDeg;
                if ( azEncReading < 0 && shared_->sim_.overlap ) { 
                    azEncReading += 360.0;
                } 
                
                CARMA_CPTRACE( TRACE_TIMING, "Drive::simulateMsg( ) - "
                    "Simulating encoder azimuth position of " << azEncReading
                    << " @ " << Time::getTimeString( 4 ) );

                carma::canbus::Message azEncMsg = 
                    Encoder::simulateBlankingFramePacket1( Encoder::AZIMUTH, 
                                                           azEncReading );
                return azEncMsg;
            }
        case Encoder::FAKE_EL_ENCODER_PACKET:
            {
                ScopedPthreadMutexLock scopelock( shared_->mutex_ );

                shared_->sim_.updateSimInfo( 
                    shared_->azGainInDegPerMinPerVolt_, 
                    shared_->elGainInDegPerMinPerVolt_ );

                CARMA_CPTRACE( TRACE_TIMING, "Drive::simulateMsg( ) - "
                    "Simulating encoder elevation position of " << 
                    shared_->sim_.elPositionInDeg << " @ " 
                    << Time::getTimeString( 4 ) );

                carma::canbus::Message elEncMsg = 
                    Encoder::simulateBlankingFramePacket1( 
                            Encoder::ELEVATION, 
                            shared_->sim_.elPositionInDeg );

                return elEncMsg;
            }
        case XacDevice::SYSTEM_MONITOR_PACKET_1:
            return XacDevice::simSystemMonitorPacket1( );
        case XacDevice::SYSTEM_MONITOR_PACKET_2:
            return XacDevice::simSystemMonitorPacket2( );
        case XacDevice::SYSTEM_MONITOR_PACKET_3:
            return XacDevice::simSystemMonitorPacket3( );
        case XacDevice::SYSTEM_MONITOR_PACKET_4:
            return XacDevice::simSystemMonitorPacket4( );
        case XacDevice::SYSTEM_MONITOR_PACKET_5:
            return XacDevice::simSystemMonitorPacket5( );
        default:
            break;
    }
    
    return createMsgToHost( DUMMY_PKT_MID );
} // simulateMsg

void
Drive::setMaxVoltage( const float azMaxVolts,
                      const float elMaxVolts )
{
    if ( azMaxVolts < 0.0 || elMaxVolts < 0.0 ) {
        throw CARMA_EXCEPTION( IllegalArgumentException,
                               "Invalid negative max voltage." );
    }

    const unsigned char AZIMUTH = 0;
    const unsigned char ELEVATION = 1;
    
    carma::canbus::Message azMsg = createMsgToNode( SET_AZ_OR_EL_RATE_LIMIT ); 
    carma::canbus::Message elMsg = createMsgToNode( SET_AZ_OR_EL_RATE_LIMIT ); 

    const float MV_PER_VOLT = 1000.0;

    azMsg << AZIMUTH << static_cast< short >( azMaxVolts * MV_PER_VOLT );
    elMsg << ELEVATION << static_cast< short >( elMaxVolts * MV_PER_VOLT );
    
    io_.postMessage( azMsg );
    nanosleep( &delayBetweenMsgs, &gIgnoredRemainder ); 
    io_.postMessage( elMsg ); 
}

void 
Drive::setRawDriveVoltages( const float azRawVolts, 
                            const float elRawVolts )
{
    if ( !isDriveEnabled( ) ) {
        enableDrives( true );
        nanosleep( &delayBetweenMsgs, &gIgnoredRemainder ); 
    }

    setDriveVoltages( azRawVolts, elRawVolts );

    // Make it clear that this was called in an open loop mode by indicating
    // that no requested or commanded rates are associated with the value.
    // Note also that no voltage offsets are applied in raw mode.
    { 
        ScopedPthreadMutexLock scopelock( shared_->mutex_ );

        shared_->sim_.updateSimInfo( shared_->azGainInDegPerMinPerVolt_, 
                                     shared_->elGainInDegPerMinPerVolt_ );

        shared_->requestedAzInDegPerMin_ = 0.0;
        shared_->requestedElInDegPerMin_ = 0.0;
        shared_->commandedAzInDegPerMin_ = 0.0;
        shared_->commandedElInDegPerMin_ = 0.0;

        shared_->sim_.azVoltage = azRawVolts;
        shared_->sim_.elVoltage = elRawVolts;
    }
}

void
Drive::setEngineeringMode( const bool enable ) 
{
    carma::canbus::Message msg = createMsgToNode( SET_ENGINEERING_MODE ); 

    const unsigned char mode = ( enable ? 0x01 : 0x00 );

    msg << mode;

    io_.postMessage( msg );
    
    { 
        ScopedPthreadMutexLock scopelock( shared_->mutex_ );
        shared_->sim_.engineeringMode = enable;
    }
}

void
Drive::setDriveVoltages( const float azVolts,
                         const float elVolts )
{
    carma::canbus::Message msg = createMsgToNode( SET_AZ_AND_EL_RATE ); 

    const float MV_PER_VOLT = 1000.0;

    msg << static_cast<short>( ::roundf( azVolts * MV_PER_VOLT ) ) 
        << static_cast<short>( ::roundf( elVolts * MV_PER_VOLT ) );
    
    io_.postMessage( msg );
    
    {
        ScopedPthreadMutexLock scopelock( shared_->mutex_ );
        shared_->azVoltage_ = azVolts;
        shared_->elVoltage_ = elVolts;
    }
}

void
Drive::initialize( )
{
    try {
        updateConfDataFromFile( );
    } catch (...) {
        logCaughtAsError( );
        programLogErrorIfPossible( "Drive::initialize( ) - Unable to update "
            "configuration data from " + configFilename_ + 
            ", default/existing parameters will be used instead." );
    }
}
        
void
Drive::updateFrameData( ) 
{
    driveMon_.state().setValue( 
        static_cast<StateMonitorPointEnum::STATE>( getState( ) ) );
}

void
Drive::writeMonitorData( )
{
    ScopedPthreadMutexLock scopelock( shared_->mutex_ );
    shared_->sim_.updateSimInfo( shared_->azGainInDegPerMinPerVolt_, 
                                 shared_->elGainInDegPerMinPerVolt_ );
    driveMon_.cmdAzimuthRate( ).setValue( shared_->commandedAzInDegPerMin_ );
    driveMon_.cmdElevationRate( ).setValue( shared_->commandedElInDegPerMin_ );
    driveMon_.reqAzimuthRate( ).setValue( shared_->requestedAzInDegPerMin_ );
    driveMon_.reqElevationRate( ).setValue( shared_->requestedElInDegPerMin_ ); 
    driveMon_.maxAzRate( ).setValue( shared_->azMaxRateInDegPerMin_ );
    driveMon_.maxElRate( ).setValue( shared_->elMaxRateInDegPerMin_ );
    driveMon_.reqAzimuthVoltage( ).setValue( shared_->azVoltage_ );
    driveMon_.reqElevationVoltage( ).setValue( shared_->elVoltage_ );
    driveMon_.maxAzVoltage( ).setValue( shared_->azMaxVoltage_ );
    driveMon_.maxElVoltage( ).setValue( shared_->elMaxVoltage_ );
}

void
Drive::updateConfDataFromFile( )
{
    table_->open( configFilename_ );

    verifyConfTableConsistency( *table_, antNo_ );

    vector<double> azOffsets = table_->getDoubleColumn( "az_dc_offset" );
    vector<double> elOffsets = table_->getDoubleColumn( "el_dc_offset" );
    vector<double> azMaxRates = table_->getDoubleColumn( "az_max" );
    vector<double> elMaxRates = table_->getDoubleColumn( "el_max" );
    vector<double> azGains = table_->getDoubleColumn( "az_gain" );
    vector<double> elGains = table_->getDoubleColumn( "el_gain" );

    const vector<double>::size_type antIdx = antNo_ - 1;

    ScopedPthreadMutexLockManager managedScopelock( shared_->mutex_ );

    managedScopelock.LockMutex( );

    shared_->azDcOffsetVoltage_ = azOffsets.at( antIdx );
    shared_->elDcOffsetVoltage_ = elOffsets.at( antIdx );
    shared_->azGainInDegPerMinPerVolt_ = azGains.at( antIdx );
    shared_->elGainInDegPerMinPerVolt_ = elGains.at( antIdx );
    shared_->azMaxRateInDegPerMin_ = azMaxRates.at( antIdx );
    shared_->elMaxRateInDegPerMin_ = elMaxRates.at( antIdx );

    if ( shared_->azGainInDegPerMinPerVolt_ == 0.0 ||
         shared_->elGainInDegPerMinPerVolt_ == 0.0 ) {
        throw CARMA_EXCEPTION( IllegalStateException, "Gain can not be 0." );
    }

    double azMaxVoltage =  shared_->azMaxRateInDegPerMin_ / 
        fabs( shared_->azGainInDegPerMinPerVolt_ );
    double elMaxVoltage = shared_->elMaxRateInDegPerMin_ / 
        fabs( shared_->elGainInDegPerMinPerVolt_ );

    if ( azMaxVoltage > MAX_VOLTAGE ) {
        azMaxVoltage = MAX_VOLTAGE;
    }

    if ( elMaxVoltage > MAX_VOLTAGE ) {
        elMaxVoltage = MAX_VOLTAGE;
    }

    shared_->azMaxVoltage_ = azMaxVoltage;
    shared_->elMaxVoltage_ = elMaxVoltage;

    managedScopelock.UnlockMutex( );

    setMaxVoltage( azMaxVoltage, elMaxVoltage ); 
}

void 
Drive::processBlankingFramePacket1( carma::canbus::DataVector & data )
{
    const bool driveOn = static_cast< bool >( dataToUbyte( data ) );
    const bool azEnable = static_cast< bool >( dataToUbyte( data ) );
    const bool elEnable = static_cast< bool >( dataToUbyte( data ) );
    typedef OvroSubsystem::BaseSwitchMonitorPointEnum::BASESWITCH BaseSwitchEnum;
    const BaseSwitchEnum baseSwitch = 
        static_cast< BaseSwitchEnum >( dataToUbyte( data ) );
    const bool timedOut = static_cast< bool >( dataToUbyte( data ) );
    typedef OvroSubsystem::EngineeringModeMonitorPointEnum EngModeEnum;
    const EngModeEnum::ENGINEERINGMODE engMode =     
        static_cast< EngModeEnum::ENGINEERINGMODE >( dataToUbyte( data ) );
    
    driveMon_.driveOn( ).setValue( driveOn );
    driveMon_.azEnable( ).setValue( azEnable );
    driveMon_.elEnable( ).setValue( elEnable );
    driveMon_.baseSwitch( ).setValue( baseSwitch );
    driveMon_.deadmanTimeout( ).setValue( timedOut );
    driveMon_.engineeringMode( ).setValue( engMode );
    
    {
        ScopedPthreadMutexLock scopelock( shared_->mutex_ );
        shared_->driveEnabled_ = driveOn;
        shared_->engineeringMode_ = ( engMode == EngModeEnum::ENGINEERING );
        shared_->baseSwitchState_ = 
            static_cast< BaseSwitchState >( baseSwitch );
    }


    typedef OvroSubsystem::DisabledMonitorPointEnum DisabledEnum;
    typedef AntennaCommon::ManualSwitchMonitorPointEnum ManualSwitchEnum;

    if ( baseSwitch == OvroSubsystem::BaseSwitchMonitorPointEnum::REMOTE ) {
        trackMon_.disabled( ).setValue( DisabledEnum::OK );
        comMon_.drive( ).track( ).manualSwitch( ).setValue( 
            ManualSwitchEnum::OK );
    } else {
        trackMon_.disabled( ).setValue( DisabledEnum::TEEPEE );
        comMon_.drive( ).track( ).manualSwitch( ).setValue( 
            ManualSwitchEnum::MANUAL );
    }

} 

void 
Drive::processBlankingFramePacket2( carma::canbus::DataVector & data )
{
    const bool azPosLimit = static_cast< bool >( dataToUbyte( data ) );
    const bool azNegLimit = static_cast< bool >( dataToUbyte( data ) );
    const bool elPosLimit = static_cast< bool >( dataToUbyte( data ) );
    const bool elNegLimit = static_cast< bool >( dataToUbyte( data ) );

    driveMon_.azPosLimit( ).setValue( azPosLimit );
    driveMon_.azNegLimit( ).setValue( azNegLimit );
    driveMon_.elPosLimit( ).setValue( elPosLimit );
    driveMon_.elNegLimit( ).setValue( elNegLimit );

    { 
        ScopedPthreadMutexLock scopelock( shared_->mutex_ );

        shared_->azPosLimit_ = azPosLimit;
        shared_->azNegLimit_ = azNegLimit;
        shared_->elPosLimit_ = elPosLimit;
        shared_->elNegLimit_ = elNegLimit;
    }

    typedef OvroSubsystem::AzHwLimitMonitorPointEnum AzLimitEnum;
    typedef AntennaCommon::AzHwLimitMonitorPointEnum ComAzLimitEnum;

    AntennaCommon::Limit & comLimit = comMon_.drive( ).limit( );

    if ( azPosLimit && azNegLimit ) {
        limitMon_.azHwLimit( ).setValue( AzLimitEnum::ERROR );
    } else if ( azPosLimit ) {
        limitMon_.azHwLimit( ).setValue( AzLimitEnum::PLUSLIM );
        comLimit.azHwLimit( ).setValue( ComAzLimitEnum::PLUSLIM );
    } else if ( azNegLimit ) {
        limitMon_.azHwLimit( ).setValue( AzLimitEnum::MINUSLIM );
        comLimit.azHwLimit( ).setValue( ComAzLimitEnum::MINUSLIM );
    } else {
        limitMon_.azHwLimit( ).setValue( AzLimitEnum::OK );
        comLimit.azHwLimit( ).setValue( ComAzLimitEnum::OK );
    }

    typedef OvroSubsystem::ElHwLimitMonitorPointEnum ElLimitEnum;
    typedef AntennaCommon::ElHwLimitMonitorPointEnum ComElLimitEnum;

    if ( elPosLimit && elNegLimit ) {
        limitMon_.elHwLimit( ).setValue( ElLimitEnum::ERROR );
    } else if ( elPosLimit ) {
        limitMon_.elHwLimit( ).setValue( ElLimitEnum::PLUSLIM );
        comLimit.elHwLimit( ).setValue( ComElLimitEnum::PLUSLIM );
    } else if ( elNegLimit ) {
        limitMon_.elHwLimit( ).setValue( ElLimitEnum::MINUSLIM );
        comLimit.elHwLimit( ).setValue( ComElLimitEnum::MINUSLIM );
    } else {
        limitMon_.elHwLimit( ).setValue( ElLimitEnum::OK );
        comLimit.elHwLimit( ).setValue( ComElLimitEnum::OK );
    }
}

void 
Drive::processBlankingFramePacket3( carma::canbus::DataVector & data )
{
    const bool wAzOverTemp = static_cast< bool >( dataToUbyte( data ) );
    const bool wAzFault = static_cast< bool >( dataToUbyte( data ) );
    const bool eAzOverTemp = static_cast< bool >( dataToUbyte( data ) );
    const bool eAzFault = static_cast< bool >( dataToUbyte( data ) );
    const bool elOverTemp = static_cast< bool >( dataToUbyte( data ) );
    const bool elFault = static_cast< bool >( dataToUbyte( data ) );
    const bool azOverlap = static_cast< bool >( dataToUbyte( data ) );

    driveMon_.westAzOverTemp( ).setValue( wAzOverTemp );
    driveMon_.westAzFault( ).setValue( wAzFault );
    driveMon_.eastAzOverTemp( ).setValue( eAzOverTemp );
    driveMon_.eastAzFault( ).setValue( eAzFault );
    driveMon_.elOverTemp( ).setValue( elOverTemp );
    driveMon_.elFault( ).setValue( elFault );
    driveMon_.azOverlap( ).setValue( azOverlap );

    
    { 
        ScopedPthreadMutexLock scopelock( shared_->mutex_ );
        shared_->azOverlapState_.mjd = Time::MJD();
        shared_->azOverlapState_.azOverlapDetected = azOverlap;
        shared_->controllerOvertemp_ =
                eAzOverTemp || wAzOverTemp || elOverTemp;
    }
        
    typedef OvroSubsystem::ControllerFaultMonitorPointEnum FaultEnum;

    if ( ( wAzFault || eAzFault ) && elFault ) {
        trackMon_.controllerFault( ).setValue( FaultEnum::AZEL );
    } else if ( wAzFault || eAzFault ) {
        trackMon_.controllerFault( ).setValue( FaultEnum::AZ );
    } else if ( elFault ) {
        trackMon_.controllerFault( ).setValue( FaultEnum::EL );
    } else {
        trackMon_.controllerFault( ).setValue( FaultEnum::OK );
    }
}

void 
Drive::processBlankingFramePacket4( carma::canbus::DataVector & data )
{
    const float VOLTS_PER_LSB = 0.001; 
    const float AMPS_PER_LSB = 0.010;

    const float azDacOutput = dataToShort( data ) * VOLTS_PER_LSB; 
    const float westAzCurrent = dataToShort( data ) * AMPS_PER_LSB;
    const float eastAzCurrent = dataToShort( data ) * AMPS_PER_LSB;

    driveMon_.cmdAzDac( ).setValue( azDacOutput );
    driveMon_.westAzCurrent( ).setValue( westAzCurrent );
    driveMon_.eastAzCurrent( ).setValue( eastAzCurrent );

    trackMon_.azWestCurrent( ).setValue( westAzCurrent );
    trackMon_.azEastCurrent( ).setValue( eastAzCurrent );
    trackMon_.azPreloadCurrent( ).setValue( westAzCurrent - eastAzCurrent );
}

void 
Drive::processBlankingFramePacket5( carma::canbus::DataVector & data )
{
    const float VOLTS_PER_LSB = 0.001; 
    const float AMPS_PER_LSB = 0.010;

    const float elDacOutput = dataToShort( data ) * VOLTS_PER_LSB;
    const float elCurrent = dataToShort( data ) * AMPS_PER_LSB;

    driveMon_.cmdElDac( ).setValue( elDacOutput );
    driveMon_.elCurrent( ).setValue( elCurrent );
    
    trackMon_.elCurrent( ).setValue( elCurrent ); 
}

void
Drive::processBlankingFramePacket6( carma::canbus::DataVector & data )
{
    const double VOLTS_PER_LSB = 0.001;

    const short ps24v = dataToShort( data );
    const short ps5vDigital = dataToShort( data );
    const short ps5_3vAnalog = dataToShort( data );
    const short psNeg5_3vAnalog= dataToShort( data );

    driveMon_.ps24v( ).setValue( ps24v * VOLTS_PER_LSB );
    driveMon_.ps5vDigital( ).setValue( ps5vDigital * VOLTS_PER_LSB );
    driveMon_.ps5_3vAnalog( ).setValue( ps5_3vAnalog * VOLTS_PER_LSB );
    driveMon_.psNeg5_3vAnalog( ).setValue( psNeg5_3vAnalog * VOLTS_PER_LSB );
}

void 
Drive::processBlankingFramePacket7( carma::canbus::DataVector & data )
{
    const float VOLTS_PER_LSB = 0.001;
    const float DEGREES_PER_LSB = 0.01;

    const float psTeepee24v = dataToShort( data ) * VOLTS_PER_LSB;
    const unsigned short major = dataToUbyte( data );
    const unsigned short minor = dataToUbyte( data );
    const float temperature = dataToShort( data ) *  DEGREES_PER_LSB;
    
    ostringstream ver;
    ver << major << "." << minor;
    
    driveMon_.ps24vTeepee( ).setValue( psTeepee24v );
    driveMon_.fpgaVersion( ).setValue( ver.str( ) );
    driveMon_.moduleTemp( ).setValue( temperature );
}

carma::canbus::Message 
Drive::simulateBlankingFramePacket1( )
{
    ScopedPthreadMutexLock scopelock( shared_->mutex_ );
    shared_->sim_.updateSimInfo( shared_->azGainInDegPerMinPerVolt_, 
                                 shared_->elGainInDegPerMinPerVolt_ );
    const unsigned char driveOn = shared_->sim_.driveEnabled;
    const unsigned char azEnabled = 0x01;
    const unsigned char elDisabled = 0x00;
    const unsigned char remoteControl = 0x01;
    const unsigned char timedOut = 0x01;
    const unsigned char engMode = shared_->sim_.engineeringMode;

    carma::canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_1 );

    msg << driveOn << azEnabled << elDisabled << remoteControl
        << timedOut << engMode;

    return msg; 
}

carma::canbus::Message 
Drive::simulateBlankingFramePacket2( )
{
    ScopedPthreadMutexLock scopelock( shared_->mutex_ );
    shared_->sim_.updateSimInfo( shared_->azGainInDegPerMinPerVolt_, 
                                 shared_->elGainInDegPerMinPerVolt_ );
    
    typedef unsigned char Byte;
    const Byte azPlusLimit = static_cast< Byte >( shared_->sim_.azPosLimit );
    const Byte azMinusLimit = static_cast< Byte >( shared_->sim_.azNegLimit );
    const Byte elPlusLimit = static_cast< Byte >( shared_->sim_.elPosLimit );
    const Byte elMinusLimit = static_cast< Byte >( shared_->sim_.elNegLimit );

    carma::canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_2 );

    msg << azPlusLimit << azMinusLimit << elPlusLimit << elMinusLimit;

    return msg; 
}
 
carma::canbus::Message 
Drive::simulateBlankingFramePacket3( )
{
    const unsigned char wAzTempOk = 0x00;
    const unsigned char wAzFault = 0x01;
    const unsigned char eAzOverTemp = 0x00;
    const unsigned char eAzNoFault = 0x00;
    const unsigned char elTempOk = 0x00;
    const unsigned char elNoFault = 0x00;

    ScopedPthreadMutexLock scopelock( shared_->mutex_ );
    shared_->sim_.updateSimInfo( shared_->azGainInDegPerMinPerVolt_, 
                                 shared_->elGainInDegPerMinPerVolt_ );

    const unsigned char azOverlap = static_cast< unsigned char >( 
        shared_->sim_.overlap );

    carma::canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_3 );

    msg << wAzTempOk << wAzFault << eAzOverTemp << eAzNoFault << elTempOk
        << elNoFault << azOverlap;

    return msg; 
}

carma::canbus::Message 
Drive::simulateBlankingFramePacket4( )
{
    ScopedPthreadMutexLock scopelock( shared_->mutex_ );
    shared_->sim_.updateSimInfo( shared_->azGainInDegPerMinPerVolt_, 
                                 shared_->elGainInDegPerMinPerVolt_ );

    const short azVoltage = static_cast< short >( 
        shared_->sim_.azVoltage * 1000.0 ); 
    const short wAzCurrent = 6000;
    const short eAzCurrent = 6500;

    carma::canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_4 );

    msg << azVoltage << wAzCurrent << eAzCurrent;

    return msg; 
}

carma::canbus::Message 
Drive::simulateBlankingFramePacket5( )
{
    ScopedPthreadMutexLock scopelock( shared_->mutex_ );
    shared_->sim_.updateSimInfo( shared_->azGainInDegPerMinPerVolt_, 
                                 shared_->elGainInDegPerMinPerVolt_ );

    const short elVoltage = static_cast< short >( 
        shared_->sim_.elVoltage * 1000.0 ); 
    const short elCurrent = 6100;

    carma::canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_5 );

    msg << elVoltage << elCurrent;

    return msg; 
}

carma::canbus::Message 
Drive::simulateBlankingFramePacket6( )
{
    const short ps24v = 24100;
    const short ps5vd = 5500;
    const short ps5ve = 5200;
    const short ps5vas = 4900;

    carma::canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_6 );

    msg << ps24v << ps5vd << ps5ve << ps5vas;

    return msg; 
}

carma::canbus::Message 
Drive::simulateBlankingFramePacket7( )
{
    const short ps24vTeepee = 18200;
    const unsigned char fpgaMajor = 0;
    const unsigned char fpgaMinor = 1;
    const short temperature = 4100; 

    carma::canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_7 );

    msg << ps24vTeepee << fpgaMajor << fpgaMinor << temperature;

    return msg; 
}
