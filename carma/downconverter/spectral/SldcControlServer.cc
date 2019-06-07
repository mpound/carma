/** @file
 * carma::downconverter::SldcControlServer class declaration.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.39 $
 * $Id: SldcControlServer.cc,v 1.39 2012/08/28 21:43:09 abeard Exp $
 */

// Carma includes
#include "carma/corba/corba.h"
#include "carma/corba/Server.h"
#include "carma/canbus/Types.h"
#include "carma/downconverter/common/LoMonitor.h"
#include "carma/downconverter/common/NoiseSource.h"
#include "carma/downconverter/common/downconverterSystem_skel.h"
#include "carma/downconverter/common/LoMonitorControl.h"
#include "carma/downconverter/common/LoMonitorControl_skel.h"
#include "carma/downconverter/common/LoMonitorControl_skel_tie.h"
#include "carma/downconverter/common/QuadratureModulator.h"
#include "carma/downconverter/common/SldcLoControl.h"
#include "carma/downconverter/common/SldcLoControl_skel.h"
#include "carma/downconverter/common/SldcLoControl_skel_tie.h"
#include "carma/downconverter/spectral/BlockDownconverter.h"
#include "carma/downconverter/spectral/BlockDownconverterControl_skel.h"
#include "carma/downconverter/spectral/BlockDownconverterControl_skel_tie.h"
#include "carma/downconverter/spectral/LoControl.h"
#include "carma/downconverter/spectral/SpectralDownconverter.h"
#include "carma/downconverter/spectral/SpectralDownconverterControl.h"
#include "carma/downconverter/spectral/SpectralDownconverterControl_skel.h"
#include "carma/downconverter/spectral/SpectralDownconverterControl_skel_tie.h"
#include "carma/downconverter/spectral/SldcControlServer.h"
#include "carma/downconverter/spectral/SldcMaster.h"
#include "carma/monitor/SldcSubsystem.h"
#include "carma/services/Global.h"
#include "carma/switchyard/SwitchyardControl.h"
#include "carma/switchyard/SwitchyardControlImpl.h"
#include "carma/switchyard/SwitchyardControl_skel.h"
#include "carma/switchyard/SwitchyardControl_skel_tie.h"
#include "carma/util/UserException.h"
#include "carma/util/programLogging.h"
#include "carma/util/Program.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/ScopedPthreadMutexLock.h"
#include "carma/util/Trace.h"
#include "carma/util/ExceptionUtils.h"

#include <log4cpp/Priority.hh>
#include <ostream>
#include <string>

using namespace carma;
using namespace carma::canbus;
using namespace carma::downconverter;
using namespace carma::services;
using namespace carma::switchyard;
using namespace carma::util;
using namespace log4cpp;
using namespace std;

namespace { // Anonymous namespace for local constants & typedefs.

    namespace POA_cdc = POA_carma::downconverter;
    namespace POA_csy = POA_carma::switchyard;

    typedef ::std::map<
        carma::canbus::nodeType,
        carma::downconverter::SpectralDownconverterControl_ptr > SldcServerMap;

    typedef ::std::map<
        ::carma::canbus::nodeType,
        QuadModControl_ptr > QuadModServerMap;

    typedef ::std::map< ::carma::canbus::nodeType,
                        carma::downconverter::BlockDownconverterControl_ptr > 
        BlockDownconverterServerMap;

} // End anonymous namespace

namespace carma {
namespace downconverter {

    class SldcControlServerPimpl {
    public:

        explicit SldcControlServerPimpl( carma::corba::Server & server,
                                         SldcMaster & master,
                                         monitor::SldcSubsystem & sldcMon );

        virtual ~SldcControlServerPimpl();

        bool isDone();

        carma::downconverter::DownconverterControl_ptr GlobalDownconverter();
        carma::downconverter::DownconverterControl_ptr Downconverter(
                ::CORBA::Short input, ::CORBA::Short band);
        carma::downconverter::SpectralDownconverterControl_ptr
        SpectralDownconverter( ::CORBA::Short input, ::CORBA::Short band );
        carma::downconverter::QuadModControl_ptr GlobalQuadMod();
        carma::downconverter::QuadModControl_ptr QuadMod(
                ::CORBA::Short input);
        carma::downconverter::NoiseSourceControl_ptr GlobalNoiseSource();
        carma::downconverter::NoiseSourceControl_ptr NoiseSource();
        carma::downconverter::LoMonitorControl_ptr GlobalLoMonitor();
        carma::downconverter::LoMonitorControl_ptr LoMonitor();
        carma::downconverter::SldcLoControl_ptr LoControl( );
        carma::downconverter::BlockDownconverterControl_ptr
        BlockDownconverter( CORBA::Short inputNo );
        carma::downconverter::BlockDownconverterControl_ptr
        GlobalBlockDownconverter( );
        void selectSidebandFrequency(
            carma::downconverter::DownconverterControl::SidebandType sideband,
            CORBA::Double lofreq,
            CORBA::UShort bandNo);
        void selectFilter(
            carma::downconverter::DownconverterControl::FilterType filter,
            CORBA::UShort bandNo );
        void setPsysPreset( CORBA::UShort inputNo,
                            CORBA::UShort bandNo );
        void setPsys(
            CORBA::Float psys,
            CORBA::UShort inputNo,
            CORBA::UShort bandNo );
        void reset();
        void softReset();
        void quit();
    
    private:

        carma::downconverter::SldcLoControl_ptr loControlServer_;
        carma::downconverter::LoMonitorControl_ptr loMonServer_; 
        carma::downconverter::NoiseSourceControl_ptr nsServer_;  
        QuadModServerMap quadModServers_;  
        SldcServerMap sldcServers_;  
        BlockDownconverterServerMap blockDcServers_; 

        SwitchyardControlImpl switchyardImpl_;
        SwitchyardControlImpl dcLoSwitchyardImpl_;

        // Global servers
        carma::downconverter::LoMonitorControl_ptr gLoMonServer_;
        carma::downconverter::QuadModControl_ptr gQmServer_;
        carma::downconverter::SpectralDownconverterControl_ptr gSldcServer_;
        carma::downconverter::BlockDownconverterControl_ptr gBlockDcServer_;

        SldcMaster & master_;
        monitor::SldcSubsystem & sldcMon_;

        bool done_;
        PthreadMutex doneMutex_;

    }; // End class SldcControlServerPimpl
}} // End namespace carma::downconverter

// -----------------------------------------------------------------------------
SldcControlServerPimpl::SldcControlServerPimpl(
        carma::corba::Server & server,
        carma::downconverter::SldcMaster & master,
        carma::monitor::SldcSubsystem & sldcMon ) :
    switchyardImpl_( master.getSwitchyard() ),
    dcLoSwitchyardImpl_( master.getDcLoSwitchyard() ),
    master_(master),
    sldcMon_( sldcMon ),
    done_(false)
{
    // Create servers.
    // This is a really good example of why you want to avoid IDL interfaces
    // which return other interfaces (DOs which return DOs).  It looks good
    // in principal, but in practice you are then committed to implement 
    // functionality which is normally delegated to the nameserver (e.g.
    // keeping track of all your references).  

    CPTRACE(Trace::TRACE6, "SldcControlServerPimpl() - Adding Noise Source and "
        "LO Monitor CAN devices.");

    server.addServant< POA_cdc::SldcLoControl_tie >(
        master.getLoControl(), 
        loControlServer_ );
    server.addServant< POA_cdc::LoMonitorControl_tie >(
        master.getLoMonitor(),
        loMonServer_ );
    server.addServant< POA_cdc::NoiseSourceControl_tie >(
        master.getNoiseSource(),
        nsServer_ );

    // Populate the sldcServers_ and quadModServers_ map
    for (unsigned input = 1; input <= Global::nSpectralStations(); input++) {

        // There is one quadrature modulator for each antenna input.
        nodeType node = static_cast<nodeType>(input);

        CPTRACE(Trace::TRACE6, "SldcControlServerPimpl() - Adding Quad Mod "
            "node " << node);

        QuadModControl_ptr qmPtr = 0;
        server.addServant< POA_cdc::QuadModControl_tie >(
            master_.getQuadMod( input ), qmPtr );
        quadModServers_.insert( std::make_pair( node, qmPtr ) );

        CPTRACE( Trace::TRACE6, "SldcControlServerPimpl() - Adding block dc "
            << "node " << node );

        BlockDownconverterControl_ptr bdcPtr = 0;
        server.addServant< POA_cdc::BlockDownconverterControl_tie >(
            master_.getBlockDownconverter( input ), bdcPtr );
        blockDcServers_.insert( std::make_pair( node, bdcPtr ) );

        for (unsigned band = 1; band <= Global::nSpectralLineBands(); band++) {

            node = SpectralDownconverter::calculateNodeId(input, band);

            // There is one spectral line downconverter for each antenna input
            // and each band.
            SpectralDownconverterControl_ptr sldcPtr = 0;
            server.addServant< POA_cdc::SpectralDownconverterControl_tie >(
                    master_.getSldc( input, band ), sldcPtr );
            sldcServers_.insert( std::make_pair( node, sldcPtr ) );

        } // End loop over bands
    } // End loop over correlator stations

    CPTRACE(Trace::TRACE6, "SldcControlServerPimpl() - Adding global device "
        "servers.");

    server.addServant< POA_cdc::LoMonitorControl_tie >(
        master.getLoMonitor( ), gLoMonServer_ );
    server.addServant< POA_cdc::QuadModControl_tie >(
        master.getGlobalQuadMod( ), gQmServer_ );
    server.addServant< POA_cdc::SpectralDownconverterControl_tie >(
        master.getGlobalSldc(), gSldcServer_ );
    server.addServant< POA_cdc::BlockDownconverterControl_tie >(
        master.getGlobalBlockDownconverter( ), gBlockDcServer_ );
    server.addServant< POA_cdc::System_tie >( *this, SLDCCONTROL_NAME );
    server.addServant< POA_csy::SwitchyardControl_tie >(
        switchyardImpl_, carma::switchyard::IFSWITCHYARDCONTROL_NAME );
    server.addServant< POA_csy::SwitchyardControl_tie >(
        dcLoSwitchyardImpl_, carma::switchyard::DCLOSWITCHYARDCONTROL_NAME );

    CPTRACE(Trace::TRACE6, "SldcControlServerPimpl() - Finished.");
}

// -----------------------------------------------------------------------------
SldcControlServerPimpl::~SldcControlServerPimpl()
{
    // Nothing
}

// -----------------------------------------------------------------------------
DownconverterControl_ptr SldcControlServerPimpl::GlobalDownconverter()
try {
    return DownconverterControl::_duplicate( gSldcServer_ );
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( );
    return DownconverterControl::_nil();
}

// -----------------------------------------------------------------------------
SpectralDownconverterControl_ptr SldcControlServerPimpl::SpectralDownconverter(
    const ::CORBA::Short input,
    const ::CORBA::Short band )
try {
    const nodeType node = SpectralDownconverter::calculateNodeId(input, band);
    SldcServerMap::iterator i = sldcServers_.find(node);

    if ( i == sldcServers_.end() )
        throw CARMA_EXCEPTION(UserException, "Invalid input and/or band "
                "Input and band ranges are [1..15] and [1..8] respectively." );

    return SpectralDownconverterControl::_duplicate( i->second );
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( );
    return SpectralDownconverterControl::_nil();
}

// -----------------------------------------------------------------------------
DownconverterControl_ptr 
SldcControlServerPimpl::Downconverter( ::CORBA::Short input, 
                                       ::CORBA::Short band )
try {
    nodeType node = SpectralDownconverter::calculateNodeId(input, band);
    SldcServerMap::iterator i = sldcServers_.find(node);

    if ( i == sldcServers_.end() )
        throw CARMA_EXCEPTION(UserException, "Invalid input and/or "
                "band.  Input and band ranges are [1..15] and [1..8] "
                "respectively.");

    return DownconverterControl::_duplicate( i->second );
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( );
    return DownconverterControl::_nil();
}

// -----------------------------------------------------------------------------
QuadModControl_ptr SldcControlServerPimpl::GlobalQuadMod()
try {
    return QuadModControl::_duplicate( gQmServer_ );
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( );
    return QuadModControl::_nil();
}

// -----------------------------------------------------------------------------
QuadModControl_ptr SldcControlServerPimpl::QuadMod(::CORBA::Short input)
try {
    const nodeType node = static_cast<nodeType>(input);
    QuadModServerMap::iterator i = quadModServers_.find(node);

    if ( i == quadModServers_.end() )
        throw CARMA_EXCEPTION(UserException, "Invalid input.  Input should "
                "be in the range [1..15].");

    return QuadModControl::_duplicate( i->second );
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( );
    return QuadModControl::_nil();
}

// -----------------------------------------------------------------------------
NoiseSourceControl_ptr SldcControlServerPimpl::GlobalNoiseSource()
try {
    return NoiseSourceControl::_duplicate( nsServer_ );
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( );
    return NoiseSourceControl::_nil();
}

// -----------------------------------------------------------------------------
NoiseSourceControl_ptr SldcControlServerPimpl::NoiseSource()
try {
    return NoiseSourceControl::_duplicate( nsServer_ );
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( );
    return NoiseSourceControl::_nil();
}

// -----------------------------------------------------------------------------
LoMonitorControl_ptr SldcControlServerPimpl::GlobalLoMonitor()
try {
    return LoMonitorControl::_duplicate( gLoMonServer_ );
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( );
    return LoMonitorControl::_nil();
}

// -----------------------------------------------------------------------------
LoMonitorControl_ptr SldcControlServerPimpl::LoMonitor()
try {
    return LoMonitorControl::_duplicate( loMonServer_ );
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( );
    return LoMonitorControl::_nil();
}

// -----------------------------------------------------------------------------
SldcLoControl_ptr SldcControlServerPimpl::LoControl( )
try {
    return SldcLoControl::_duplicate( loControlServer_ );
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
    return SldcLoControl::_nil( ); // Squash compiler warnings
}

// -----------------------------------------------------------------------------
BlockDownconverterControl_ptr
SldcControlServerPimpl::BlockDownconverter( const CORBA::Short inputNo )
try {
    const BlockDownconverterServerMap::iterator
        dci = blockDcServers_.find( inputNo );
    if ( dci == blockDcServers_.end( ) ) {
        throw CARMA_EXCEPTION(UserException, "No block dc for inputNo." );
    }

    {
        ostringstream msg;
        msg << "Retrieving block downconverter DO for inputNo="
            << inputNo << ".";

        programLogInfoIfPossible( msg.str() );
    }

    return BlockDownconverterControl::_duplicate( dci->second );
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
    return BlockDownconverterControl::_nil( ); // Squash compiler warnings
}

// -----------------------------------------------------------------------------
BlockDownconverterControl_ptr
SldcControlServerPimpl::GlobalBlockDownconverter( )
try {
    {
        ostringstream msg;
        msg << "Retrieving global block downconverter DO.";

        programLogInfoIfPossible( msg.str() );
    }
    return BlockDownconverterControl::_duplicate( gBlockDcServer_ );
} catch ( ... ) {
    logCaughtAsErrorAndRethrowAsUser( );
    return BlockDownconverterControl::_nil( ); // Squash compiler warnings
}

// -----------------------------------------------------------------------------
void SldcControlServerPimpl::selectSidebandFrequency(
            carma::downconverter::DownconverterControl::SidebandType sideband,
            CORBA::Double lofreq,
            CORBA::UShort bandNo)
try {
    nodeType node;
    SldcServerMap::iterator i;

    {
        ostringstream info;
        info << "SldcControlServerPimpl::selectSidebandFrequency( "
            << "sideband="
            << (sideband == DownconverterControl::UPPER_SIDEBAND ?"USB":"LSB")
            << ", "
            << "lofreq=" << lofreq << ", "
            << "bandNo=" << bandNo << " ).";
        programLogInfoIfPossible( info.str() );
    }

    // Validate bandNo
    if ( bandNo == 0 || bandNo > Global::nSpectralLineBands() )
        throw CARMA_EXCEPTION( UserException, "Invalid band number. ");

    // UGLY HACK TIME
    // This is a quick hack to assure that the selectSideband and setLoFrequency
    // commands are interleaved per input.  DO NOT CHANGE - THINGS WILL
    // QUIT WORKING PROPERLY.  ONCE AGAIN, THIS IS ANOTHER WONDERFULLY
    // UGLY HACK TO GET AROUND THE FACT THAT OUR XACS ARE TOO SLOW FOR THE
    // EVEN SLOWER CAN BUS.

    // Loop over downconverters of a given band and select sb & freq.
    for (unsigned input = 1; input <= Global::nSpectralStations(); ++input)
    {
        node = SpectralDownconverter::calculateNodeId( input, bandNo );
        i = sldcServers_.find( node );
        if ( i == sldcServers_.end() ) {
            ostringstream oss;
            oss << "Logic error - unable to find node " << node
                << "(input=" << input << ", band=" << bandNo << ")."
                " Please file a bug report and include this message.";
            throw CARMA_EXCEPTION( UserException, oss.str().c_str() );
        }
        i->second->disableCommandLogging( );
        i->second->selectSideband( sideband );
    } // End loop over inputs.

    for (unsigned input = 1; input <= Global::nSpectralStations(); ++input)
    {
        node = SpectralDownconverter::calculateNodeId( input, bandNo );
        i = sldcServers_.find( node );
        if ( i != sldcServers_.end() ) {
            i->second->setLOFrequency( lofreq );
            i->second->enableCommandLogging( );
        }
    }

    // END UGLY HACK

    loControlServer_->setLoFrequency( bandNo, lofreq );

} catch ( ... ) {
    logAndRethrowCaughtExceptionAsUserException( Priority::ERROR );
}

// -----------------------------------------------------------------------------
void SldcControlServerPimpl::selectFilter(
    carma::downconverter::DownconverterControl::FilterType filter,
    CORBA::UShort bandNo )
try {
    nodeType node;
    SldcServerMap::iterator i;

    {
        ostringstream info;
        info << "SldcControlServerPimpl::selectFilter( "
            << "filter=" << SpectralDownconverter::filterAsString( filter )
            << ", bandNo=" << bandNo << ").";
        programLogInfoIfPossible( info.str() );
    }

    // Validate bandNo
    if ( bandNo == 0 || bandNo > Global::nSpectralLineBands() )
        throw CARMA_EXCEPTION( UserException, "Invalid band number. ");

    // Loop over downconverters of a given band and select sb & freq.
    for (unsigned input = 1; input <= Global::nSpectralStations(); ++input)
    {
        node = SpectralDownconverter::calculateNodeId( input, bandNo );
        i = sldcServers_.find( node );
        if ( i == sldcServers_.end() ) {
            ostringstream oss;
            oss << "Logic error - unable to find node " << node
                << "(input=" << input << ", band=" << bandNo << ")."
                " Please file a bug report and include this message.";
            throw CARMA_EXCEPTION( UserException, oss.str().c_str() );
        }
        i->second->disableCommandLogging( );
        i->second->selectOutputFilter( filter );
        i->second->enableCommandLogging( );

    } // End loop over inputs.

} catch (...) {
    logAndRethrowCaughtExceptionAsUserException( Priority::ERROR );
}

// -----------------------------------------------------------------------------
void SldcControlServerPimpl::setPsysPreset( CORBA::UShort inputNo,
                                            CORBA::UShort bandNo )
try {

    if ( inputNo > Global::nSpectralStations( ) )
        throw CARMA_EXCEPTION( UserException, "Invalid input number. ");

    if ( bandNo > Global::nSpectralLineBands() )
        throw CARMA_EXCEPTION( UserException, "Invalid band number. ");

    {
        ostringstream info;
        info << "SldcControlServerPimpl::setPsysPreset( "
            << "inputNo=" << inputNo << ", "
            << "bandNo=" << bandNo << " ).";
        programLogInfoIfPossible( info.str() );
    }

    unsigned inputBegin = inputNo, inputEnd = inputNo;
    if ( inputNo == 0 ) {
        inputBegin = 1;
        inputEnd = Global::nSpectralStations( );
    }

    unsigned bandBegin = bandNo, bandEnd = bandNo;
    if ( bandBegin == 0 ) {
        bandBegin = 1;
        bandEnd = Global::nSpectralLineBands( );
    }

    for ( unsigned input = inputBegin; input <= inputEnd; ++input )
    {
        for (unsigned band = bandBegin; band <= bandEnd; ++band)
        {
            const nodeType node =
                SpectralDownconverter::calculateNodeId( input, band );
            SldcServerMap::iterator i = sldcServers_.find( node );
            if ( i == sldcServers_.end( ) ) {
                ostringstream oss;
                oss << "Logic error - unable to find node " << node
                    << "(input=" << input << ", band=" << band << ")."
                    " Please file a bug report and include this message.";
                throw CARMA_EXCEPTION( UserException, oss.str().c_str() );
            }

            i->second->disableCommandLogging( );
            i->second->setPsysPreset( );
            i->second->enableCommandLogging( );
        } // End loop over bands.
    } // End loop over inputs

} catch (...) {
    logAndRethrowCaughtExceptionAsUserException( Priority::ERROR );
}

// -----------------------------------------------------------------------------
void SldcControlServerPimpl::setPsys(
    CORBA::Float psys,
    CORBA::UShort inputNo,
    CORBA::UShort bandNo )
try {

    if ( inputNo > Global::nSpectralStations( ) )
        throw CARMA_EXCEPTION( UserException, "Invalid input number. ");

    if ( bandNo > Global::nSpectralLineBands() )
        throw CARMA_EXCEPTION( UserException, "Invalid band number. ");

    {
        ostringstream info;
        info << "SldcControlServerPimpl::setPsys( "
            << "psys=" << psys << ", "
            << "inputNo=" << inputNo << ", "
            << "bandNo=" << bandNo << " ).";
        programLogInfoIfPossible( info.str() );
    }

    unsigned inputBegin = inputNo, inputEnd = inputNo;
    if ( inputNo == 0 ) {
        inputBegin = 1;
        inputEnd = Global::nSpectralStations( );
    }

    unsigned bandBegin = bandNo, bandEnd = bandNo;
    if ( bandBegin == 0 ) {
        bandBegin = 1;
        bandEnd = Global::nSpectralLineBands( );
    }

    for ( unsigned input = inputBegin; input <= inputEnd; ++input )
    {
        for (unsigned band = bandBegin; band <= bandEnd; ++band)
        {
            const nodeType node =
                SpectralDownconverter::calculateNodeId( input, band );
            SldcServerMap::iterator i = sldcServers_.find( node );
            if ( i == sldcServers_.end( ) ) {
                ostringstream oss;
                oss << "Logic error - unable to find node " << node
                    << "(input=" << input << ", band=" << band << ")."
                    " Please file a bug report and include this message.";
                throw CARMA_EXCEPTION( UserException, oss.str().c_str() );
            }

            i->second->disableCommandLogging( );
            i->second->setPsys( psys );
            i->second->enableCommandLogging( );
        } // End loop over bands.
    } // loop over inputs

} catch (...) {
    logAndRethrowCaughtExceptionAsUserException( Priority::ERROR );
}

// -----------------------------------------------------------------------------
void SldcControlServerPimpl::reset()
{
    try {
        master_.reset();
    } catch (const ::std::exception & ex) {
        throw CARMA_EXCEPTION(UserException, ex.what());
    } catch (...) {
        throw CARMA_EXCEPTION(UserException, "Unknown exception.");
    }
}

// -----------------------------------------------------------------------------
void SldcControlServerPimpl::softReset()
{
    try {
        master_.softReset();
    } catch (const ::std::exception & ex) {
        throw CARMA_EXCEPTION(UserException, ex.what());
    } catch (...) {
        throw CARMA_EXCEPTION(UserException, "Unknown exception.");
    }
}

// -----------------------------------------------------------------------------
void SldcControlServerPimpl::quit()
{
    ScopedPthreadMutexLock scopelock(doneMutex_);
    done_ = true;
}

// -----------------------------------------------------------------------------
bool SldcControlServerPimpl::isDone()
{
    ScopedPthreadMutexLock scopelock(doneMutex_);
    return done_;
}

// -----------------------------------------------------------------------------
SldcControlServer::SldcControlServer( carma::corba::Server & server,
                                      carma::downconverter::SldcMaster & master,
                                      carma::monitor::SldcSubsystem & sldcMon )
    :
    pimpl_( new SldcControlServerPimpl( server, master, sldcMon ) ),
    server_( server )
{
    // Nothing
}

// -----------------------------------------------------------------------------
SldcControlServer::~SldcControlServer()
{
    // Delete the private implementation
    delete pimpl_;
}

// -----------------------------------------------------------------------------
void SldcControlServer::run()
{
    const ::timespec amillisecond = { 0, 1000000 };
    while ( ! server_.terminated() ) {
        server_.work();
        nanosleep( &amillisecond, 0 ); // Sleep a bit to not monopolize the CPU.
    }
}
