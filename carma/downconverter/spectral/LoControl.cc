/**
 * @file
 * Spectral Line Downconverter LO Control implementation (API #200).
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.11 $
 * $Date: 2011/01/03 18:48:21 $
 * $Id: LoControl.cc,v 1.11 2011/01/03 18:48:21 iws Exp $
 */

#include "carma/downconverter/spectral/LoControl.h"

#include "carma/canbus/Utilities.h"
#include "carma/monitor/CanbusCommon.h"
#include "carma/monitor/LoControl.h"
#include "carma/services/Global.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"

#include <log4cpp/Category.hh>

#include <cmath>
#include <iomanip>
#include <iostream>

using namespace carma::canbus;
using namespace carma::downconverter;
using namespace carma::services;
using namespace carma::util;
using namespace log4cpp;
using namespace std;

namespace { 

    namespace CM = carma::monitor;

    const Trace::TraceLevel TRACE_CTOR_DTOR = Trace::TRACE5;
    const Trace::TraceLevel TRACE_PROCESS_MSG = Trace::TRACE5;

    const apiType       API_ID                                    = 200; 
    const nodeType      NODE_ID                                   = 1;
    const char          API_VERSION                               = 'A';
    const double        PACKET_LATE_THRESHOLD                     = 150.0;

    // Control commands
    const msgType       SET_LO_FREQUENCY                          = 0x040;

    // Blanking frame monitor packets for individual LO output frequencies 
    // and stats are contiguous from 1 to 8.
    const msgType       LO1_BLANKING_FRAME_PACKET                 = 0x0E0;
    const msgType       LO8_BLANKING_FRAME_PACKET                 = 0x0E7;
    const msgType       BLANKING_FRAME_PACKET_9                   = 0x0E8;
    const msgType       BLANKING_FRAME_PACKET_10                  = 0x0E9;

    MsgIdInfoMap 
    getHalfSecondMonitors() {
        MsgIdInfoMap mids;
        msgType mid = LO1_BLANKING_FRAME_PACKET;
        for ( ; mid <= LO8_BLANKING_FRAME_PACKET; ++mid ) {
            ostringstream os;
            unsigned short band = (mid - LO1_BLANKING_FRAME_PACKET) + 1;
            os << "LO " << band << " Blanking Frame Packet";
            mids[mid] = os.str(); 
        }
        mids[BLANKING_FRAME_PACKET_9] = "BLANKING_FRAME_PACKET_9";
        mids[BLANKING_FRAME_PACKET_10] = "BLANKING_FRAME_PACKET_10";
        return mids;
    } // End getHalfSecondMonitors

    bool 
    loFreqOutOfRange( const double freq ) 
    {
        static const double LO_MIN = 1.750; // GHz exactly representable
        static const double LO_MAX = 4.250; // GHz exactly representable
        return ( freq > LO_MAX || freq < LO_MIN );
    }

    bool
    bandNoInvalid( const unsigned short bandNo )
    {
        return ( bandNo < 1 || bandNo > Global::nSpectralLineBands( ) );
    } 

} // End namespace < unnamed >

LoControl::LoControl( carma::canbus::CanOutput & co,
                      carma::monitor::StateMonitorPointEnum & state,
                      carma::monitor::LoControl & mon,
                      carma::monitor::Xac & xacmon ) 
    : XacDevice( API_ID, NODE_ID, co ),
      log_( Program::getLogger( ) ),
      state_( state ),
      mon_( mon ),
      xacMon_( xacmon )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "LoControl::LoControl() - C'tor." );    
}
    
LoControl::~LoControl( )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "LoControl::~LoControl() - D'tor." );    
}
 
MsgIdInfoMap
LoControl::getHalfSecMonitors( ) const 
{
    static MsgIdInfoMap halfSecMsgDescriptions = getHalfSecondMonitors( );
    return halfSecMsgDescriptions;
}

MsgIdInfoMap
LoControl::getSlowMonitors( ) const
{
    return XacDevice::getSlowMonitors( );
}

void
LoControl::processMsg( const carma::canbus::msgType mid,
                       DataVector & data,
                       const bool sim ) 
{
    if ( getState( ) == ONLINE && isPacketLate( PACKET_LATE_THRESHOLD ) )
        incrementLatePacketCount( );

    CPTRACE( TRACE_PROCESS_MSG, "LoControl::processMsg() - Processing "
                    << (sim ? "simulated" : "real") << " msg 0x" << hex 
                    << mid << dec << " for node " << getNode() << "." );

    if ( mid >= LO1_BLANKING_FRAME_PACKET && mid <= LO8_BLANKING_FRAME_PACKET ){
        processLoFreqAndStatMessage( mid, data );
    } else if ( mid == BLANKING_FRAME_PACKET_9 ) {
        processBlankingFramePacket9( data );
    } else if ( mid == BLANKING_FRAME_PACKET_10 ) {
        processBlankingFramePacket10( data );
    } else if ( isSystemMonitorPacket( mid ) ) {
        processSystemMonitorPacket( mid, data, xacMon_ );
    } else {
        log_ << Priority::DEBUG << "LoControl::processMsg( ) - "
             << "Unknown mid " << mid << ", node " << getNode( ) << ".";
        CARMA_CPTRACE( TRACE_PROCESS_MSG, "LoControl::processMsg( ) - "
             << "Unknown mid " << mid << ", node " << getNode( ) << "." );
    }
}

carma::canbus::Message
LoControl::simulateMsg( const msgType mid )
{
    carma::canbus::Message msg;

    CARMA_CPTRACE( TRACE_PROCESS_MSG, "LoControl::simulateMsg( ) - "
                    "Simulating msg 0x" << hex << mid << dec << 
                    " for node " << getNode() << "." );

    if ( mid >= LO1_BLANKING_FRAME_PACKET && mid <= LO8_BLANKING_FRAME_PACKET ){
        msg = simLoFreqAndStatMessage( mid );
    } else if ( mid == BLANKING_FRAME_PACKET_9 ) {
        msg = simBlankingFramePacket9( );
    } else if ( mid == BLANKING_FRAME_PACKET_10 ) {
        msg = simBlankingFramePacket10( );
    } else if ( isSystemMonitorPacket( mid ) ) {
        msg = simSystemMonitorPacket( mid );
    } else {
        log_ << Priority::DEBUG << "LoControl::processMsg( ) - "
             << "Unknown mid " << mid << ", node " << getNode( ) << ".";
        CARMA_CPTRACE( TRACE_PROCESS_MSG, "LoControl::processMsg( ) - "
             << "Unknown mid " << mid << ", node " << getNode( ) << "." );
    }
    
    return msg;

}
        
carma::canbus::apiType
LoControl::getApiId( ) 
{
    return API_ID;
}

void
LoControl::updateFrameData( ) 
{
    state_.setValue( 
        static_cast<CM::StateMonitorPointEnum::STATE>( getState( ) ) );
}

void
LoControl::setLoFrequency( const unsigned short bandNo,
                           const double loFrequency ) const
try {
    static const double HZ_PER_GHZ = 1e9; 

    if ( bandNoInvalid( bandNo ) )
        throw CARMA_EXCEPTION( IllegalArgumentException, "Invalid Band." );

    if ( loFreqOutOfRange( loFrequency ) )
        throw CARMA_EXCEPTION( IllegalArgumentException, "Invalid frequency." );

    carma::canbus::Message msg = createMsgToNode( SET_LO_FREQUENCY );

    const unsigned long freqInHz = 
        static_cast<unsigned long>( ( loFrequency * HZ_PER_GHZ ) + 0.5 );
    
    ostringstream os;
    os << "LoControl::setLoFrequency( bandNo=" << bandNo
       << " , loFrequency=" << loFrequency << " ).";
    log_ << Priority::INFO << os.str( );

    msg << static_cast<byteType>( bandNo ) 
        << freqInHz; 

    io_.postMessage( msg );
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
}

void 
LoControl::processLoFreqAndStatMessage( const msgType mid, DataVector & data ) 
{
    unsigned short loIdx = ( mid - LO1_BLANKING_FRAME_PACKET );
    double freq = static_cast<double>( dataToUlong( data ) );
    bool lock = static_cast<bool>( dataToUbyte( data ) );
    unsigned char rawVco = dataToUbyte( data );
    enum CM::LoControl::VcoMonitorPointEnum::VCO vco =
        CM::LoControl::VcoMonitorPointEnum::VCO1;

    switch ( rawVco ) {
        case 0:
            vco = CM::LoControl::VcoMonitorPointEnum::VCO1;
            break;
        case 1:
            vco = CM::LoControl::VcoMonitorPointEnum::VCO2;
            break;
        default:
            break;
    }
           
    mon_.lo( loIdx ).loFrequency().setValue( freq ); 
    mon_.lo( loIdx ).lockFlag().setValue( lock );
    mon_.lo( loIdx ).vco().setValue( vco );
}

void 
LoControl::processBlankingFramePacket9( DataVector & data )
{
    const int ps24v = dataToShort( data );
    const int ps5vDigital = dataToShort( data );
    const int ps5_2v = dataToShort( data );
    const int ps3_3v = dataToShort( data );

    mon_.ps24v( ).setValue( ps24v / 1000.0 );
    mon_.ps5vDigital( ).setValue( ps5vDigital / 1000.0 ); 
    mon_.ps5_2vLinear( ).setValue( ps5_2v / 1000.0 );
    mon_.ps3_3v( ).setValue( ps3_3v / 1000.0 );
}

void
LoControl::processBlankingFramePacket10( DataVector & data )
{
    const int modTemp = dataToShort( data );

    mon_.temp( ).setValue( modTemp / 100.0 );
}

carma::canbus::Message
LoControl::simLoFreqAndStatMessage( const msgType mid )
{
    // Simulated lo frequencies range from 1751 MHz to 3935 MHz in 
    // 312 MHz increments per bend.  They alternately are locked and unlocked.
    const unsigned long int bandIdx = mid - LO1_BLANKING_FRAME_PACKET;
    const unsigned long int loFreq = 1751000000UL + bandIdx * 312000000UL; 
    const byteType lock =  bandIdx % 2;
    const byteType vco = ( loFreq  <= 2500000000UL ? 0 : 1 );
   
    carma::canbus::Message msg = createMsgToHost( mid );
    msg << loFreq << lock << vco;
    return msg;
}

carma::canbus::Message
LoControl::simBlankingFramePacket9( )
{
    const short ps24v = 24100;
    const short ps5v = 5100;
    const short ps5_2v = 5210;
    const short ps3_3v = 3310;
    carma::canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_9 );

    msg << ps24v << ps5v << ps5_2v << ps3_3v;
    return msg;
}

carma::canbus::Message
LoControl::simBlankingFramePacket10( )
{
    const short temp = 410; // 41C
    carma::canbus::Message msg = createMsgToHost( BLANKING_FRAME_PACKET_10 );

    msg << temp;
    return msg;
}

