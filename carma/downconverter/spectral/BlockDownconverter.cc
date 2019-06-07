/** @file
 * CAN Device class definition for the Block Downconverter (API 4).
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.10 $
 * $Date: 2011/09/26 17:50:59 $
 * $Id: BlockDownconverter.cc,v 1.10 2011/09/26 17:50:59 iws Exp $
 *
 * $CarmaCopyright$
 */

#include "carma/downconverter/spectral/BlockDownconverter.h"

#include "carma/canbus/Utilities.h"
#include "carma/monitor/SldcSubsystem.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/programLogging.h"
#include "carma/util/Trace.h"

#include <iostream>

using namespace carma;
using namespace carma::canbus;
using namespace carma::downconverter;
using namespace carma::util;
using namespace std;


namespace {

    const canbus::apiType API_ID = 4;

    const canbus::msgType BLANKING_FRAME_PACKET_1 = 0x0E0;
    const canbus::msgType BLANKING_FRAME_PACKET_2 = 0x0E1;
    const canbus::msgType BLANKING_FRAME_PACKET_3 = 0x0E2;
    const canbus::msgType BLANKING_FRAME_PACKET_4 = 0x0E3;
    const canbus::msgType BLANKING_FRAME_PACKET_5 = 0x0E4;
    const canbus::msgType BLANKING_FRAME_PACKET_6 = 0x0E5;

    const canbus::msgType SET_BLOCK_AND_POLARIZATION = 0x080;
    const canbus::msgType SET_BLOCK                  = 0x081;
    const canbus::msgType SET_POLARIZATION           = 0x082;

    const Trace::TraceLevel TRACE_CTOR_DTOR = Trace::TRACE3;

    namespace CM = carma::monitor;

} // namespace < unnamed >

BlockDownconverter::BlockDownconverter( carma::canbus::nodeType node,
                                        carma::canbus::CanOutput & co,
                                        monitor::SldcSubsystem & subsys ) :
    XacDevice( API_ID, node, co ),
    state_( 0 ),
    devMon_( 0 ),
    xacMon_( 0 )
{
    ostringstream trace;
    trace << "BlockDownconverter - Ctor for node " << node << ".";
    CARMA_CPTRACE( TRACE_CTOR_DTOR, trace.str() );

    if ( node != 0 ) {
        state_ = &( subsys.blockDownconverterContainer(node-1).state() );
        devMon_ =
            &(subsys.blockDownconverterContainer(node-1).blockDownconverter());
        xacMon_ = &( subsys.blockDownconverterContainer(node-1).xac() );
    }
}

BlockDownconverter::~BlockDownconverter( )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "BlockDownconverter - Dtor." );
}

carma::canbus::MsgIdInfoMap
BlockDownconverter::getHalfSecMonitors( ) const
{
    static carma::canbus::MsgIdInfoMap halfSecMons;
    static bool init = false;

    if ( !init ) {
        halfSecMons[BLANKING_FRAME_PACKET_1] = "BlockDownconverter::BFP1";
        halfSecMons[BLANKING_FRAME_PACKET_2] = "BlockDownconverter::BFP2";
        halfSecMons[BLANKING_FRAME_PACKET_3] = "BlockDownconverter::BFP3";
        halfSecMons[BLANKING_FRAME_PACKET_4] = "BlockDownconverter::BFP4";
        halfSecMons[BLANKING_FRAME_PACKET_5] = "BlockDownconverter::BFP5";
        halfSecMons[BLANKING_FRAME_PACKET_6] = "BlockDownconverter::BFP6";
        init = true;
    }
    return halfSecMons;
}

carma::canbus::MsgIdInfoMap
BlockDownconverter::getSlowMonitors( ) const
{
    return XacDevice::getSlowMonitors();
}

void
BlockDownconverter::processMsg( const ::carma::canbus::msgType mid,
                                ::carma::canbus::DataVector & data,
                                bool sim )
{
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
            break;
    }
}

carma::canbus::Message
BlockDownconverter::simulateMsg( carma::canbus::msgType mid )
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
            throw CARMA_ERROR( "Simulate called for invalid mid." );
            break;
    }
    return msg;
}

void
BlockDownconverter::updateFrameData( )
{
    if ( state_ == 0 ) return;

    state_->setValue(
        static_cast<CM::StateMonitorPointEnum::STATE>(getState( )) );
}

void
BlockDownconverter::setBlockAndPolarization(
    carma::downconverter::BlockDownconverterControl::Block block,
    carma::downconverter::BlockDownconverterControl::Polarization polarization,
    CORBA::UShort bandNo ) const
{
    if ( bandNo <= 0 || bandNo > 8 )
        throw CARMA_EXCEPTION( UserException, "Invalid band." );

    ostringstream log;
    log << "BlockDownconverter::setBlockAndPolarization( block=";

    canbus::Message msg = createMsgToNode( SET_BLOCK_AND_POLARIZATION );
    unsigned char selection = 0;
    if ( polarization == BlockDownconverterControl::POLARIZATION_1 ) {

        if ( block == BlockDownconverterControl::LOWER ) {
            selection = 1;
            log << "LOWER, polarization=POLARIZATION_1";
        } else if ( block == BlockDownconverterControl::UPPER ) {
            selection = 2;
            log << "UPPER, polarization=POLARIZATION_1";
        } else {
            // Nothing
        }

    } else if ( polarization == BlockDownconverterControl::POLARIZATION_2 ) {

        if ( block == BlockDownconverterControl::LOWER ) {
            log << "LOWER, polarization=POLARIZATION_2";
            selection = 3;
        } else if ( block == BlockDownconverterControl::UPPER ) {
            log << "UPPER, polarization=POLARIZATION_2";
            selection = 4;
        } else {
            // Nothing
        }

    } else {
        log << "NO CHANGE, polarization=NO CHANGE";
    }

    msg << static_cast< unsigned char >( bandNo ) << selection;
    io_.postMessage( msg );

    log << ", bandNo=" << bandNo << " ).";
    programLogInfoIfPossible( log.str( ) );
}

void
BlockDownconverter::setBlock(
    carma::downconverter::BlockDownconverterControl::Block block,
    CORBA::UShort bandNo ) const
{
    if ( bandNo <= 0 || bandNo > 8 )
        throw CARMA_EXCEPTION( UserException, "Invalid band." );

    ostringstream log;
    log << "BlockDownconverter::setBlock( block=";

    canbus::Message msg = createMsgToNode( SET_BLOCK );

    unsigned char selection = 0;
    if ( block == BlockDownconverterControl::LOWER ) {
        selection = 1;
        log  << "LOWER";
    } else if ( block == BlockDownconverterControl::UPPER ) {
        selection = 2;
        log << "UPPER";
    } else {
        log << "NO CHANGE";
    }


    msg << static_cast< unsigned char >( bandNo ) << selection;
    io_.postMessage( msg );

    log << ", bandNo=" << bandNo << " ).";
    programLogInfoIfPossible( log.str( ) );
}

void
BlockDownconverter::setPolarization(
    carma::downconverter::BlockDownconverterControl::Polarization polarization,
    CORBA::UShort bandNo ) const
{
    if ( bandNo <= 0 || bandNo > 8 )
        throw CARMA_EXCEPTION( UserException, "Invalid band." );

    ostringstream log;
    log << "BlockDownconverter::setPolarization( polarization=";

    canbus::Message msg = createMsgToNode( SET_POLARIZATION );

    unsigned char selection = 0;
    if ( polarization == BlockDownconverterControl::POLARIZATION_1 ) {
        selection = 1;
        log << "POLARIZATION_1";
    } else if ( polarization == BlockDownconverterControl::POLARIZATION_2 ) {
        selection = 2;
        log << "POLARIZATION_2";
    } else {
        log << "NO CHANGE";
    }

    msg << static_cast< unsigned char >( bandNo ) << selection;
    io_.postMessage( msg );

    log << ", bandNo=" << bandNo << " ).";
    programLogInfoIfPossible( log.str( ) );
}

void
BlockDownconverter::reset( )
try {
    programLogInfoIfPossible( "BlockDownconverter::reset() - Software reset." );
    Device::reset();
} catch (...) {
    rethrowCaughtAsUser( );
}

void
BlockDownconverter::processBlankingFramePacket1(
    ::carma::canbus::DataVector & data )
{
    typedef monitor::BlockDownconverter::BlockMonitorPointEnum::BLOCK BlockEnum;

    unsigned char block;
    for ( unsigned i = 0; i < 8; ++i ) {
        block = dataToUbyte( data );

        devMon_->block( i ).setValue( static_cast< BlockEnum >( block ) );
    }
}

void
BlockDownconverter::processBlankingFramePacket2(
    ::carma::canbus::DataVector & data )
{
    typedef
    CM::BlockDownconverter::PolarizationMonitorPointEnum::POLARIZATION PolEnum;

    unsigned char polarization;
    for ( unsigned i = 0; i < 8; ++i ) {
        polarization = dataToUbyte( data );

        devMon_->polarization( i ).setValue(
            static_cast< PolEnum >( polarization ) );
    }
}

void
BlockDownconverter::processBlankingFramePacket3(
    ::carma::canbus::DataVector & data )
{
    const short ps24v = dataToShort( data );
    const short ps5v = dataToShort( data );
    const short ps6v = dataToShort( data );
    const short ps3_3v = dataToShort( data );

    devMon_->ps24V().setValue( ps24v * 1.0e-3 );
    devMon_->ps5vDigital().setValue( ps5v * 1.0e-3 );
    devMon_->ps6vAnalog().setValue( ps6v * 1.0e-3 );
    devMon_->ps3_3vDigital().setValue( ps3_3v * 1.0e-3 );
}

void
BlockDownconverter::processBlankingFramePacket4(
    ::carma::canbus::DataVector & data )
{
    const short ps5v1 = dataToShort( data );
    const short ps5v2 = dataToShort( data );
    const short ps5vPort1 = dataToShort( data );
    const short ps5vPort2 = dataToShort( data );

    devMon_->ps5v1().setValue( ps5v1 * 1.0e-3 );
    devMon_->ps5v2().setValue( ps5v2 * 1.0e-3 );
    devMon_->ps5vPort(0).setValue( ps5vPort1 * 1.0e-3 );
    devMon_->ps5vPort(1).setValue( ps5vPort2 * 1.0e-3 );
}

void
BlockDownconverter::processBlankingFramePacket5(
    ::carma::canbus::DataVector & data )
{
    short ps5vPort;
    for ( unsigned i = 2; i < 6; ++i ) {
        ps5vPort = dataToShort( data );
        devMon_->ps5vPort( i ).setValue( ps5vPort * 1.0e-3 );
    }
}

void
BlockDownconverter::processBlankingFramePacket6(
    ::carma::canbus::DataVector & data )
{
    const short ps5vPort7 = dataToShort( data );
    const short ps5vPort8 = dataToShort( data );
    const short temp = dataToShort( data );
    const unsigned char fpgaMaj = dataToUbyte( data );
    const unsigned char fpgaMin = dataToUbyte( data );

    devMon_->ps5vPort( 6 ).setValue( ps5vPort7 * 1.0e-3 );
    devMon_->ps5vPort( 7 ).setValue( ps5vPort8 * 1.0e-3 );
    devMon_->temperature().setValue( temp * 1.0e-2 );
    ostringstream fpgaVersion;
    fpgaVersion << static_cast<short>( fpgaMaj ) << "."
                << static_cast<short>( fpgaMin );
    devMon_->fpgaVersion().setValue( fpgaVersion.str() );
}

::carma::canbus::Message
BlockDownconverter::simBlankingFramePacket1( )
{
    ::carma::canbus::Message msg = createMsgToHost(BLANKING_FRAME_PACKET_1);

    unsigned char block;

    for ( unsigned i = 0; i < 8; ++i ) {
        block = i % 3;
        msg << block;
    }

    return msg;
}

::carma::canbus::Message
BlockDownconverter::simBlankingFramePacket2( )
{
    ::carma::canbus::Message msg = createMsgToHost(BLANKING_FRAME_PACKET_2);

    unsigned char polarization;

    for ( unsigned i = 0; i < 8; ++i ) {
        polarization = i % 3;
        msg << polarization;
    }

    return msg;
}

::carma::canbus::Message
BlockDownconverter::simBlankingFramePacket3( )
{
    ::carma::canbus::Message msg = createMsgToHost(BLANKING_FRAME_PACKET_3);

    const short ps24v = 24100;
    const short ps5v = 5200;
    const short ps6v = 6300;
    const short ps3_3v = 3400;

    msg << ps24v << ps5v << ps6v << ps3_3v;

    return msg;
}

::carma::canbus::Message
BlockDownconverter::simBlankingFramePacket4( )
{
    ::carma::canbus::Message msg = createMsgToHost(BLANKING_FRAME_PACKET_4);

    const short ps5v1 = 5100;
    const short ps5v2 = 5200;
    const short ps5vPort1 = 5100;
    const short ps5vPort2 = 5200;

    msg << ps5v1 << ps5v2 << ps5vPort1 << ps5vPort2;

    return msg;
}

::carma::canbus::Message
BlockDownconverter::simBlankingFramePacket5( )
{
    ::carma::canbus::Message msg = createMsgToHost(BLANKING_FRAME_PACKET_5);

    for ( unsigned i = 3 ; i < 7; ++i ) {
        const short ps5vPort = 5000 + ( 100 * i );
        msg << ps5vPort;
    }

    return msg;
}

::carma::canbus::Message
BlockDownconverter::simBlankingFramePacket6( )
{
    ::carma::canbus::Message msg = createMsgToHost(BLANKING_FRAME_PACKET_6);

    const short ps5vPort7 = 5700;
    const short ps5vPort8 = 5800;
    const short temperature = 4800;
    const unsigned char fpgaMaj = '6';
    const unsigned char fpgaMin = '6';

    msg << ps5vPort7 << ps5vPort8 << temperature << fpgaMaj << fpgaMin;

    return msg;
}
