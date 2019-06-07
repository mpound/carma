/** @file
 * Definition file for 10-m Antenna Encoder Module.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.23 $
 * $Date: 2011/09/26 17:48:16 $
 * $Id: Encoder.cc,v 1.23 2011/09/26 17:48:16 iws Exp $
 */

#include "carma/antenna/ovro/canbus/Encoder.h"

#include "carma/canbus/Types.h"
#include "carma/canbus/Utilities.h"
#include "carma/monitor/CanbusCommon.h"
#include "carma/monitor/Encoder.h"
#include "carma/monitor/OvroSubsystem.h"
#include "carma/services/Angle.h"
#include "carma/util/ErrorException.h"
#include "carma/util/IllegalStateException.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedPthreadMutexLock.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"

#include <iostream>

using namespace carma;
using namespace carma::antenna::ovro;
using namespace carma::canbus;
using namespace carma::services;
using namespace carma::util;
using namespace std;

namespace {
    
    const carma::util::Trace::TraceLevel TRACE_CTOR_DTOR = Trace::TRACE3;
    const carma::util::Trace::TraceLevel TRACE_SIM       = Trace::TRACE4;
    
    const carma::canbus::apiType API_NO = 232;

    const double PACKET_LATE_THRESHOLD                   = 150.0;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_1 = 0x0E0;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_2 = 0x0E1;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_3 = 0x0E2;
    const carma::canbus::msgType INVALID_BLANKING_FRAME_PACKET_4 = 0x0E3;

    const double DEGREES_PER_LSB = 360.0 / pow( 2.0, 29 );
    const double VOLTS_PER_LSB = 0.001;
    const float DEG_C_PER_LSB = 0.01;

    carma::monitor::StateMonitorPointEnum &
    retrieveEncoderStateMonitorPointEnum( Encoder::Axis axis,
                                     carma::monitor::OvroSubsystem & mon )
    {
        switch ( axis ) {
            case Encoder::AZIMUTH:
                return mon.drive( ).azEncoder( ).state( );
            case Encoder::ELEVATION:
                return mon.drive( ).elEncoder( ).state( );
            default:
                throw CARMA_ERROR( "Invalid/impossible axis." );
        }
    }
                                        
    carma::monitor::Encoder &
    retrieveEncoderMonitorContainer( Encoder::Axis axis, 
                                     carma::monitor::OvroSubsystem & mon )
    {
        switch ( axis ) {
            case Encoder::AZIMUTH:
                return mon.drive( ).azEncoder( ).encoder( );
            case Encoder::ELEVATION:
                return mon.drive( ).elEncoder( ).encoder( );
            default:
                throw CARMA_ERROR( "Invalid/impossible axis." );
        }
    } // retrieveEncoderMonitorContainer

    carma::monitor::Xac &
    retrieveXacMonitorContainer( Encoder::Axis axis,
                                 carma::monitor::OvroSubsystem & mon )
    {
        switch ( axis ) {
            case Encoder::AZIMUTH:
                return mon.drive( ).azEncoder( ).xac( );
            case Encoder::ELEVATION:
                return mon.drive( ).elEncoder( ).xac( );
            default:
                throw CARMA_ERROR( "Invalid/impossible axis." );
        }
    } // retrieveXacMonitorContainer

    string
    axisAsString( const Encoder::Axis axis )
    {
        switch ( axis ) {
            case Encoder::AZIMUTH: return "AZIMUTH";
            case Encoder::ELEVATION: return "ELEVATION";
            default: return "< error >";
        }
    }
    
    MsgIdInfoMap 
    createMidMap( const Encoder::Axis axis ) 
    {
        MsgIdInfoMap midMap;

        /* Note BLANKING_FRAME_PACKET_1 is simulated from the drive module,
           using the FAKE_AZ and EL encoder packets below. */
        midMap[ BLANKING_FRAME_PACKET_2 ] = "BFP2";
        midMap[ BLANKING_FRAME_PACKET_3 ] = "BFP3";
        switch ( axis ) {
            case Encoder::AZIMUTH: 
                midMap[ Encoder::FAKE_AZ_ENCODER_PACKET ] = 
                    "FAKE_AZ_ENCODER_PACKET";
                break;
            case Encoder::ELEVATION: 
                midMap[ Encoder::FAKE_EL_ENCODER_PACKET ] = 
                    "FAKE_EL_ENCODER_PACKET";
                break;
            default:
                break;
        }
    
        return midMap;
    }

} // namespace < unnamed >

const carma::canbus::msgType Encoder::FAKE_AZ_ENCODER_PACKET;
const carma::canbus::msgType Encoder::FAKE_EL_ENCODER_PACKET;

Encoder::Encoder( Axis axis, 
                  carma::canbus::CanOutput & canOutput,
                  carma::monitor::OvroSubsystem & monitorSubsys, 
                  carma::antenna::ovro::Drive & drive ) :
    XacDevice( API_NO, static_cast< nodeType >( axis ), canOutput ),
    axis_( axis ),
    halfSecMIDs_( createMidMap( axis ) ),
    state_( retrieveEncoderStateMonitorPointEnum( axis, monitorSubsys ) ),
    encoderMon_( retrieveEncoderMonitorContainer( axis, monitorSubsys ) ),
    xacMon_( retrieveXacMonitorContainer( axis, monitorSubsys ) ),
    drive_( drive )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "Encoder::Encoder( ) - Constructor." );
}

Encoder::~Encoder( ) 
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "Encoder::~Encoder( ) - Destructor." );
}

Encoder::TimestampedPosition::TimestampedPosition( ) : 
    mjd( 0.0 )
{ }

Encoder::Shared::Shared( ) 
{ }

Encoder::TimestampedPosition
Encoder::getMostRecentPosition( ) const
{
    ScopedPthreadMutexLock scopelock( shared_.mutex );

    return shared_.position;
}

carma::canbus::MsgIdInfoMap
Encoder::getHalfSecMonitors( ) const
{
    return halfSecMIDs_;
}

carma::canbus::MsgIdInfoMap
Encoder::getSlowMonitors( ) const
{
    return XacDevice::getSlowMonitors( );
}

void
Encoder::processMsg( carma::canbus::msgType messageId,
                     ::std::vector< carma::canbus::byteType > & data,
                     bool sim )
{
    // If state is ONLINE, check if the packet is late...
    if ( getState() == ONLINE ) {
        if ( isPacketLate( PACKET_LATE_THRESHOLD ) ) {
            incrementLatePacketCount( );
        }
    }

    CPTRACE(Trace::TRACEALL, "Encoder::processMsg() - Processing "
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
        case INVALID_BLANKING_FRAME_PACKET_4:
            {
                const unsigned long REPORT_THRESHOLD = 1000;
                static unsigned long packetCount = 0; 
                if ( packetCount++ % REPORT_THRESHOLD == 0 ) {
                    ostringstream msg;
                    msg << "Encoder::processMsg - Received a total of "
                        << packetCount << " messages with an invalid mid=0x"
                        << hex << INVALID_BLANKING_FRAME_PACKET_4 << ".";
                    programLogWarnIfPossible( msg.str( ) );
                }
            }
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_1:
        case XacDevice::SYSTEM_MONITOR_PACKET_2:
        case XacDevice::SYSTEM_MONITOR_PACKET_3:
        case XacDevice::SYSTEM_MONITOR_PACKET_4:
        case XacDevice::SYSTEM_MONITOR_PACKET_5:
            XacDevice::processSystemMonitorPacket( 
                messageId, 
                data, 
                xacMon_ );
            break;
        default:
            { 
                ostringstream msg;
                msg << "Encoder::processMsg() - Switch does not match any case:"
                    << " Unknown mid " << messageId << ". Node " 
                    << getNode( ) << ".";
                programLogInfoIfPossible( msg.str( ) );
                CPTRACE( Trace::TRACE6, msg.str( ) );
            }
            break;
    }
}

carma::canbus::Message
Encoder::simulateMsg( carma::canbus::msgType messageId )
{
    // BLANKING_FRAME_PACKET_1 is simulated in the Drive module which is
    // called back into when FAKE_AZ and FAKE_EL encoder packets are received.
    switch ( messageId ) {
        case BLANKING_FRAME_PACKET_2:
            return simulateBlankingFramePacket2( );
        case BLANKING_FRAME_PACKET_3:
            return simulateBlankingFramePacket3( );
        case FAKE_AZ_ENCODER_PACKET:
             CARMA_CPTRACE( TRACE_SIM, "Encoder::simulateMsg( ) - "
                "Simulating az encoder packet." );
            // Callback into drive module (which contains sim info).
            return drive_.simulateMsg( FAKE_AZ_ENCODER_PACKET );
        case FAKE_EL_ENCODER_PACKET:
             CARMA_CPTRACE( TRACE_SIM, "Encoder::simulateMsg( ) - "
                "Simulating el encoder packet." );
            // Callback into drive module (which contains sim info).
            return drive_.simulateMsg( FAKE_EL_ENCODER_PACKET );
        case XacDevice::SYSTEM_MONITOR_PACKET_1:
        case XacDevice::SYSTEM_MONITOR_PACKET_2:
        case XacDevice::SYSTEM_MONITOR_PACKET_3:
        case XacDevice::SYSTEM_MONITOR_PACKET_4:
        case XacDevice::SYSTEM_MONITOR_PACKET_5:
            return simSystemMonitorPacket( messageId );
        default:
            throw CARMA_EXCEPTION( IllegalStateException, "Invalid mid." );
    }
} // simulateMsg

void
Encoder::updateFrameData( )
{
    state_.setValue( 
        static_cast<carma::monitor::StateMonitorPointEnum::STATE>( 
            getState( ) ) );
}

void
Encoder::processBlankingFramePacket1( carma::canbus::DataVector & data )
{
    const unsigned long rawPosition = dataToUlong( data );
    const unsigned char alarmFlag = dataToUbyte( data );
    const unsigned char noStartFlag = dataToUbyte( data );
    const unsigned char encoderNotPresent = dataToUbyte( data );

    const Angle position( rawPosition * DEGREES_PER_LSB, Angle::DEGREES_STR );

    {
        ScopedPthreadMutexLock scopelock( shared_.mutex );
        shared_.position.position = position;
        shared_.position.mjd = Time::MJD( );
    }

    encoderMon_.encoderPosition( ).setValue( position.degrees( ) );
    encoderMon_.encoderAlarm( ).setValue( static_cast< bool >( alarmFlag ) );
    encoderMon_.noStartAlarm( ).setValue( static_cast< bool >( noStartFlag ) );
    encoderMon_.encoderNotPresent( ).setValue( 
        static_cast< bool >( encoderNotPresent ) );
}

void
Encoder::processBlankingFramePacket2( carma::canbus::DataVector & data )
{
    const short ps24v = dataToShort( data );
    const short ps5vDigital = dataToShort( data );
    const short ps5vEncoder = dataToShort( data );
    const short ps5vSwitch = dataToShort( data );

    encoderMon_.ps24v( ).setValue( ps24v * VOLTS_PER_LSB );
    encoderMon_.ps5vDigital( ).setValue( ps5vDigital * VOLTS_PER_LSB );
    encoderMon_.ps5vEncoder( ).setValue( ps5vEncoder * VOLTS_PER_LSB );
    encoderMon_.ps5vEncoderAfterSwitch( ).setValue( 
        ps5vSwitch * VOLTS_PER_LSB );
}

void
Encoder::processBlankingFramePacket3( carma::canbus::DataVector & data )
{
    const unsigned short major = dataToUbyte( data );
    const unsigned short minor = dataToUbyte( data );
    const short modTemp = dataToShort( data );

    ostringstream ver;
    ver << major << "." << minor;

    encoderMon_.fpgaVersion( ).setValue( ver.str( ) );
    encoderMon_.moduleTemp( ).setValue( modTemp * DEG_C_PER_LSB );
}

carma::canbus::Message
Encoder::simulateBlankingFramePacket1( Axis axis, const double positionInDeg )
{
    const unsigned long pos = static_cast< unsigned long >( 
        ( positionInDeg / DEGREES_PER_LSB ) ); 

    const unsigned char noAlarm = 0x00;
    const unsigned char alarm = 0x01;

    idType id = createId( TO_HOST, 
                          API_NO, 
                          static_cast< nodeType >( axis ),
                          BLANKING_FRAME_PACKET_1 );
                          
    canbus::Message msg( id, ALL_BUSSES ); // Bus Id ignored on sim packets.

    msg << pos << noAlarm << alarm << noAlarm;

    return msg;
}

carma::canbus::Message
Encoder::simulateBlankingFramePacket2( )
{
    const short ps24v = 24050;
    const short ps5v = 5100;
    const short ps5vEncoder = 4950;
    const short ps5vAfterSwitch = 4850;
    
    canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_2 );

    msg << ps24v << ps5v << ps5vEncoder << ps5vAfterSwitch;

    return msg;
}

carma::canbus::Message
Encoder::simulateBlankingFramePacket3( )
{
    const unsigned char major = 0;
    const unsigned char minor = 0;
    const short temp = static_cast< short >( 35.0 / DEG_C_PER_LSB );

    canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_3 );

    msg << major << minor << temp;

    return msg;
}
