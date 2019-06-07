/**@file
 * Carma Wideband Downconverter Master implementation.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.66 $
 * $Date: 2013/04/19 17:47:02 $
 * $Id: WbdcMaster.cc,v 1.66 2013/04/19 17:47:02 abeard Exp $
 */

#include "carma/downconverter/wideband/WbdcMaster.h"

#include "carma/canbus/exceptions.h"
#include "carma/canbus/Utilities.h"
#include "carma/corba/Server.h"
#include "carma/downconverter/common/Downconverter.h"
#include "carma/downconverter/common/DownconverterControl.h"
#include "carma/downconverter/common/DownconverterControl_skel.h"
#include "carma/downconverter/common/DownconverterControl_skel_tie.h"
#include "carma/downconverter/common/LoMonitor.h"
#include "carma/downconverter/common/LoMonitorControl.h"
#include "carma/downconverter/common/LoMonitorControl_skel.h"
#include "carma/downconverter/common/LoMonitorControl_skel_tie.h"
#include "carma/downconverter/common/NoiseSource.h"
#include "carma/downconverter/common/NoiseSourceControl.h"
#include "carma/downconverter/common/NoiseSourceControl_skel.h"
#include "carma/downconverter/common/NoiseSourceControl_skel_tie.h"
#include "carma/downconverter/common/QuadratureModulator.h"
#include "carma/downconverter/common/QuadModControl.h"
#include "carma/downconverter/common/QuadModControl_skel.h"
#include "carma/downconverter/common/QuadModControl_skel_tie.h"
#include "carma/downconverter/common/SldcLoControl.h"
#include "carma/downconverter/spectral/BlockDownconverterControl.h"
#include "carma/downconverter/spectral/SpectralDownconverterControl.h"
#include "carma/monitor/WbdcSubsystem.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/Time.h"
#include "carma/services/Global.h"

#include <unistd.h>

using namespace std;
using namespace carma;
using namespace carma::canbus;
using namespace carma::corba;
using namespace carma::downconverter;
using namespace carma::monitor;
using namespace carma::services;
using namespace carma::util;
using namespace log4cpp;

// -----------------------------------------------------------------------------
WbdcMaster::WbdcMaster( Server & server, monitor::WbdcSubsystem & mon )
    : server_( server ),
      mon_( mon ),
      emulate_(true),
      hostname_(Program::getHostname(true))
{
    initialize();
}

// -----------------------------------------------------------------------------
WbdcMaster::WbdcMaster( Server & server, int modulBusNo, int slotNo,
                        monitor::WbdcSubsystem & mon ) :
    Master(modulBusNo, slotNo),
    server_( server ),
    mon_( mon ),
    emulate_(false),
    hostname_(Program::getHostname(true))
{
    initialize();
}

// -----------------------------------------------------------------------------
WbdcMaster::WbdcMaster( Server & server, int modulBusNo, 
                        monitor::WbdcSubsystem & mon ) 
    : Master(modulBusNo),
    server_( server ),
    mon_( mon ),
    emulate_(false),
    hostname_(Program::getHostname(true))
{
    initialize();
}

// -----------------------------------------------------------------------------
WbdcMaster::~WbdcMaster()
{
    void *result;
    int status;
    try {
        // Kill our run thread...
        status = pthread_cancel(runThreadId_);
        if (status != 0) {
            throw CARMA_EXCEPTION(carma::canbus::PthreadFailException,
                    "WbdcMaster::~WbdcMaster() - Error destroying run thread. "
                    + (string)strerror(status));
        }
        // Block on the run thread to quit.
        status = pthread_join(runThreadId_, &result);
        if (status != 0) {
            throw CARMA_EXCEPTION(carma::canbus::PthreadFailException,
                    "WbdcMaster::~WbdcMaster() - Error joining on run thread. "
                    + (string)strerror(status));
        }

        // Check return status and make sure it's good.
        if (result != PTHREAD_CANCELED) {
            throw CARMA_EXCEPTION(carma::canbus::PthreadFailException,
                "Master::~Master() - Read thread returned with invalid value.");
        }

        stop();

        // Remove all devices from the Master::device_ map and invoke
        // _remove_ref on them. Note the CORBA will delete the memory when
        // it is done with it.
        removeDevices();

        // Stop the autowriter: You can call this regardless of whether the
        // autowriter is started...
        mon_.stopAutoWriter();

    } catch (carma::util::ErrorException &eex) {
        carma::util::ErrorException newErr(
            (string)eex.what() + " Caught in WbdcMaster::~WbdcMaster. "
            "Destructor needs to be smarter.\n", __FILE__, __LINE__);
        newErr.report();
        newErr.log(Priority::DEBUG);
    }
}

// -----------------------------------------------------------------------------
void WbdcMaster::initialize()
{
    int status;
    done_ = false;


    // Define our CAN network...
    addDevices( );

    // Start the run method in a new seperate thread.
    status = pthread_create(&runThreadId_, NULL,
        runThreadEntry, (void *)this);

    if (status != 0) {
        throw CARMA_EXCEPTION(carma::canbus::PthreadFailException,
            "WbdcMaster::initialize() - Unable to start run thread. " +
            (const string) strerror(status));
    }

    // Initialize the done mutex.
    pthread_mutex_init(&doneMutex_, NULL);

}

// -----------------------------------------------------------------------------
void WbdcMaster::addDevices()
{
    // The wideband downconverter network consist of 120 downconverter modules,
    // 8 Quadrature modulator modules, 1 Noise source module and 1 LO monitor
    // module.  Downconverters use a node id naming scheme of 0xAB where
    // A is the antenna number (1-8) and B is the band index (0x0-0xf).
    namespace POA_cd = POA_carma::downconverter;

    downconverter::NoiseSource * ns = new downconverter::NoiseSource( 
        1, *this, mon_.noiseSourceContainer() );
    Master::addDevice( ns );
    server_.addServant< POA_cd::NoiseSourceControl_tie >( *ns, nsControl_ );

    downconverter::LoMonitor * loMon = new downconverter::LoMonitor(1, *this,
        mon_.loMonitorContainer().state(),
        mon_.loMonitorContainer().loMonitor(),
        mon_.loMonitorContainer().xac(),
        true );
    Master::addDevice( loMon );
    server_.addServant< POA_cd::LoMonitorControl_tie >( *loMon, loMonControl_ );

    for (unsigned short input = 1; input <= Global::nSzaAntennas(); input++) {

        // Add quadrature modulators.
        downconverter::QuadratureModulator * qm = 
            new downconverter::QuadratureModulator(
                static_cast<nodeType>(input), 
                *this, 
                mon_.quadModContainer( input - 1 ) );
        Master::addDevice( qm );
        server_.addServant< POA_cd::QuadModControl_tie >( *qm, 
                                                          qmControls_[input] );

        for (unsigned int band = 1; band <= Global::nWidebandBands(); band++) {
            // Add the downconverters.

            const nodeType node = Downconverter::calculateNodeId(input, band);
            downconverter::Downconverter * dc = new
                carma::downconverter::Downconverter( node, *this, mon_ );
            Master::addDevice( dc );
            downconverters_[ node ] = dc;
            server_.addServant< POA_cd::DownconverterControl_tie >( *dc,
                dcControls_[ InputBandNoPair( input, band ) ] );
                                                                    
        }
    }

    globalDc_ = new carma::downconverter::Downconverter( 0, *this, mon_ );
    server_.addServant< POA_cd::DownconverterControl_tie >( *globalDc_,
                                                           globalDcControl_ );
    globalQm_ = new carma::downconverter::QuadratureModulator(
        0, *this, mon_.quadModContainer( 0 ) );
    server_.addServant< POA_cd::QuadModControl_tie >( *globalQm_,
                                                      globalQmControl_ );
    globalLoMon_ = new carma::downconverter::LoMonitor(
        0, *this,
        mon_.loMonitorContainer().state(),
        mon_.loMonitorContainer().loMonitor(),
        mon_.loMonitorContainer().xac() );
    server_.addServant< POA_cd::LoMonitorControl_tie >( *globalLoMon_,
                                                        globalLoMonControl_ );

    // Set their busIds to ALL_BUSSES
    globalDc_->setBusId(ALL_BUSSES);
    globalQm_->setBusId(ALL_BUSSES);
    globalLoMon_->setBusId(ALL_BUSSES);
}

// -----------------------------------------------------------------------------
void WbdcMaster::removeDevices()
{
    // Remove all devices from the Master device map 
    apiType api;
    nodeType node;
    Device *dev;

    // Remove noise source.
    dev = getDevice(NoiseSource::getApiId(), 1);
    Master::removeDevice(NoiseSource::getApiId(), 1);

    // Remove the Lo monitor.
    dev = getDevice(LoMonitor::getApiId(), 1);
    Master::removeDevice(LoMonitor::getApiId(), 1);

    for (unsigned short input = 1; input <= Global::nSzaAntennas(); input++) {

        // Remove quadrature modulators.
        api = globalQm_->getApi();
        dev = getDevice(api, static_cast<nodeType>(input));
        Master::removeDevice(api, static_cast<nodeType>(input));

        for (unsigned int band = 1; band <= Global::nWidebandBands(); band++) {
            // Remove downconverters.
            api = globalDc_->getApi();
            node = Downconverter::calculateNodeId(input, band);
            dev = getDevice(api, node);
            Master::removeDevice(api, node);
        }
    }
}

// -----------------------------------------------------------------------------
void WbdcMaster::updateStatus()
{
    map<canbus::busIdType, canbus::busStatusType> busStatus;
    map<canbus::busIdType, canbus::busStatusType>::iterator busIter;
    unsigned int busIndex = 0;
    int nOnlineNodes, nOfflineNodes;
    int unknownPackets, donglelessPackets;
    unsigned int latePackets;

    // Retrieve status information from canbus, master, etc.
    // This information refers to the state of the host machine running
    // this process and its associated CAN interface(s).
    busStatus = getBusStatus();
    nOnlineNodes = getOnlineNodeCount();
    nOfflineNodes = getOfflineNodeCount();
    unknownPackets = getUnknownPacketCount();
    donglelessPackets = getDonglelessPacketCount();
    latePackets = getLatePacketCount();

    // Process and place into monitor stream...
    // I've assumed no more than two busses in this application, the below
    // check enforces this. It is difficult to do anything other than
    // hardcode the number of busses.  The reason for this is that the # of
    // busses must be hardcoded in the monitor system.
    if (busStatus.size() > 2)
        throw CARMA_EXCEPTION(carma::util::ErrorException,
            "WbdcMaster::updateStatus()"
            " - Size of busStatus map exceeds 2 (assumed max size).");

    busIter = busStatus.begin();

    while (busIter != busStatus.end()) {
        mon_.can().bus(busIndex).busId().setValue(busIter->first);
        mon_.can().bus(busIndex).halfSecRxMsgRate().
            setValue(busIter->second.rxMsgRate);
        mon_.can().bus(busIndex).halfSecTxMsgRate().
            setValue(busIter->second.txMsgRate);
        mon_.can().bus(busIndex).avgRxMsgRate().
            setValue(busIter->second.oneMinRxMsgRate);
        mon_.can().bus(busIndex).avgTxMsgRate().
            setValue(busIter->second.oneMinTxMsgRate);
        mon_.can().bus(busIndex).nRxErrors().setValue(busIter->second.rxErrors);
        mon_.can().bus(busIndex).nTxErrors().setValue(busIter->second.txErrors);
        mon_.can().bus(busIndex).nLostFastMsgs().
            setValue(busIter->second.fastMsgsLost);
        mon_.can().bus(busIndex).nLostSlowMsgs().
            setValue(busIter->second.slowMsgsLost);
        mon_.can().bus(busIndex).busState().
            setValue( static_cast<Bus::BusStateMonitorPointEnum::BUSSTATE>
            ( busIter->second.state ));
        busIter++;
        busIndex++;
    }

    mon_.can().host().nActiveNodes().setValue(nOnlineNodes);
    mon_.can().host().nOfflineNodes().setValue(nOfflineNodes);
    mon_.can().host().nLatePackets().setValue(latePackets);
    mon_.can().host().nUnknownPackets().setValue(unknownPackets);
    mon_.can().host().nDonglelessPackets().setValue(donglelessPackets);
    mon_.can().host().hostname().setValue(hostname_);
    mon_.timestamp().setValue(Time::MJD());
    mon_.online().setValue(true);
}


// -----------------------------------------------------------------------------
bool WbdcMaster::isDone()
{
    bool tmp;
    pthread_mutex_lock(&doneMutex_);
    tmp = done_;
    pthread_mutex_unlock(&doneMutex_);
    return tmp;
}

// -----------------------------------------------------------------------------
DownconverterControl_ptr WbdcMaster::GlobalDownconverter()
try {
    programLogInfoIfPossible( "WbdcMaster::GlobalDownconverter()" );
    return downconverter::DownconverterControl::_duplicate( globalDcControl_ );
} catch (...) {
    rethrowCaughtAsUser();
    return downconverter::DownconverterControl::_nil( );
}

// -----------------------------------------------------------------------------
DownconverterControl_ptr WbdcMaster::Downconverter(short input, short band)
try {
    ostringstream os;
    os << "WbdcMaster::Downconverter( input=" << input
       << ", band=" << band << " )";
    programLogInfoIfPossible( os.str() );

    const InputBandNoPair inputBandPair( input, band );
    DownconverterControlsMap::iterator dci = dcControls_.find( inputBandPair );
    if ( dci != dcControls_.end() )
        return downconverter::DownconverterControl::_duplicate( dci->second );

    os << " - Object not found.";
    throw CARMA_EXCEPTION( carma::util::UserException, os.str().c_str() );

} catch (...) {
    rethrowCaughtAsUser();
    return downconverter::DownconverterControl::_nil( );
}

// -----------------------------------------------------------------------------
SpectralDownconverterControl_ptr
WbdcMaster::SpectralDownconverter( const CORBA::Short input,
                                   const CORBA::Short band )
{
    ostringstream oss;
    oss << "WbdcMaster::SpectralDownconverter( "
        << "input=" << input << ", band=" << band << " ) not implemented.";
   throw CARMA_EXCEPTION(carma::util::UserException, oss.str().c_str() );
   return downconverter::SpectralDownconverterControl::_nil( );
}

// -----------------------------------------------------------------------------
QuadModControl_ptr WbdcMaster::GlobalQuadMod()
try {
    programLogInfoIfPossible( "WbdcMaster::GlobalQuadMod()" );
    return downconverter::QuadModControl::_duplicate( globalQmControl_ );
} catch (...) {
    rethrowCaughtAsUser();
    return downconverter::QuadModControl::_nil( );
}

// -----------------------------------------------------------------------------
QuadModControl_ptr WbdcMaster::QuadMod(short input)
try {
    ostringstream os;
    os << "WbdcMaster::QuadMod( input=" << input << " )";
    programLogInfoIfPossible( os.str() );

    if ( qmControls_.find( input ) == qmControls_.end() )
        throw CARMA_EXCEPTION( util::UserException, "Invalid quad mod input." );

    return downconverter::QuadModControl::_duplicate( qmControls_[ input ] );

} catch (...) {
    rethrowCaughtAsUser();
    return downconverter::QuadModControl::_nil( );
}
    
// -----------------------------------------------------------------------------
NoiseSourceControl_ptr WbdcMaster::GlobalNoiseSource()
try {
    programLogInfoIfPossible( "WbdcMaster::GlobalNoiseSource()" );
    return downconverter::NoiseSourceControl::_duplicate( nsControl_ );
} catch (...) {
    rethrowCaughtAsUser();
    return downconverter::NoiseSourceControl::_nil( );
}

// -----------------------------------------------------------------------------
NoiseSourceControl_ptr WbdcMaster::NoiseSource()
try {
    programLogInfoIfPossible( "WbdcMaster::NoiseSource()" );
    return downconverter::NoiseSourceControl::_duplicate( nsControl_ );
} catch (...) {
    rethrowCaughtAsUser();
    return downconverter::NoiseSourceControl::_nil( );
}

// -----------------------------------------------------------------------------
LoMonitorControl_ptr WbdcMaster::GlobalLoMonitor()
try {
    programLogInfoIfPossible( "WbdcMaster::GlobalLoMonitor()" );
    return downconverter::LoMonitorControl::_duplicate( globalLoMonControl_ );
} catch (...) {
    rethrowCaughtAsUser();
    return downconverter::LoMonitorControl::_nil( );
}

// -----------------------------------------------------------------------------
LoMonitorControl_ptr WbdcMaster::LoMonitor()
try {
    programLogInfoIfPossible( "WbdcMaster::LoMonitor()" );
    return downconverter::LoMonitorControl::_duplicate( loMonControl_ );
} catch (...) {
    rethrowCaughtAsUser();
    return downconverter::LoMonitorControl::_nil( );
}

// -----------------------------------------------------------------------------
SldcLoControl_ptr WbdcMaster::LoControl( )
{
    programLogInfoIfPossible( "WbdcMaster::LoControl()" );
    throw CARMA_EXCEPTION( UserException, "No LoControl in wideband system." );
    return downconverter::SldcLoControl::_nil();
}

// -----------------------------------------------------------------------------
BlockDownconverterControl_ptr
WbdcMaster::BlockDownconverter( const CORBA::Short inputNo )
{
    ostringstream os;
    os << "WbdcMaster::BlockDownconverter( inputNo=" << inputNo << " )";
    programLogInfoIfPossible( os.str() );
    throw CARMA_EXCEPTION( UserException, "No Block DC in wideband system." );
    return BlockDownconverterControl::_nil();
}

// -----------------------------------------------------------------------------
BlockDownconverterControl_ptr
WbdcMaster::GlobalBlockDownconverter( )
{
    programLogInfoIfPossible( "WbdcMaster::GlobalBlockDownconverter()" );
    throw CARMA_EXCEPTION( UserException, "GlobalBlockDownconverter not "
        "implemented on wbdc system." );

    return BlockDownconverterControl::_nil();
}

// -----------------------------------------------------------------------------
void WbdcMaster::selectSidebandFrequency(
    DownconverterControl::SidebandType sideband,
    CORBA::Double lofreq,
    CORBA::UShort bandNo)
{
    programLogInfoIfPossible( "WbdcMaster::selectSidebandFrequency() "
        "called but not implemented." );
    throw CARMA_EXCEPTION( UserException, "setSidebandFrequency() - not "
        "implemented on wideband system!" );
}

// -----------------------------------------------------------------------------
void WbdcMaster::selectFilter(
    DownconverterControl::FilterType filter,
    CORBA::UShort bandNo )
{
    programLogInfoIfPossible( "WbdcMaster::selectFilter() "
        "called but not implemented." );
    throw CARMA_EXCEPTION( UserException, "selectFilter() - not "
        "implemented on wideband system!" );
}

// -----------------------------------------------------------------------------
void WbdcMaster::setPsysPreset( CORBA::UShort inputNo, CORBA::UShort bandNo )
try {

    ScopedLogNdc ndc( __PRETTY_FUNCTION__ );

    if ( inputNo > Global::nWidebandStations( ) )
        throw CARMA_EXCEPTION( UserException, "Invalid input no." );

    if ( bandNo > Global::nWidebandBands( ) )
        throw CARMA_EXCEPTION( UserException, "Invalid band no." );

    {
        ostringstream info;
        info << __PRETTY_FUNCTION__ << "( "
            << "inputNo=" << inputNo << ", "
            << "bandNo=" << bandNo << " ).";
        programLogInfoIfPossible( info.str() );
    }

    unsigned inputBegin = inputNo, inputEnd = inputNo;
    if ( inputNo == 0 ) {
        inputBegin = 1;
        inputEnd = Global::nWidebandStations( );
    }

    unsigned bandBegin = bandNo, bandEnd = bandNo;
    if ( bandBegin == 0 ) {
        bandBegin = 1;
        bandEnd = Global::nWidebandBands( );
    }

    for ( unsigned input = inputBegin; input <= inputEnd; ++input )
    {
        for (unsigned band = bandBegin; band <= bandEnd; ++band)
        {
            const nodeType node =
                Downconverter::calculateNodeId( input, band );

            const DcByNodeMap::const_iterator i = downconverters_.find( node );
            if ( i == downconverters_.end( ) ) {
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
    } //

} catch (...) {
    logAndRethrowCaughtExceptionAsUserException( Priority::ERROR );
}

// -----------------------------------------------------------------------------
void WbdcMaster::setPsys( CORBA::Float psys,
                          CORBA::UShort inputNo,
                          CORBA::UShort bandNo )
try {

    ScopedLogNdc ndc( __PRETTY_FUNCTION__ );

    if ( inputNo > Global::nWidebandStations( ) )
        throw CARMA_EXCEPTION( UserException, "Invalid input no." );

    if ( bandNo > Global::nWidebandBands( ) )
        throw CARMA_EXCEPTION( UserException, "Invalid band no." );

    {
        ostringstream info;
        info << __PRETTY_FUNCTION__ << "( "
            << "inputNo=" << inputNo << ", "
            << "bandNo=" << bandNo << " ).";
        programLogInfoIfPossible( info.str() );
    }

    unsigned inputBegin = inputNo, inputEnd = inputNo;
    if ( inputNo == 0 ) {
        inputBegin = 1;
        inputEnd = Global::nWidebandStations( );
    }

    unsigned bandBegin = bandNo, bandEnd = bandNo;
    if ( bandBegin == 0 ) {
        bandBegin = 1;
        bandEnd = Global::nWidebandBands( );
    }

    for ( unsigned input = inputBegin; input <= inputEnd; ++input )
    {
        for (unsigned band = bandBegin; band <= bandEnd; ++band)
        {
            const nodeType node =
                Downconverter::calculateNodeId( input, band );

            const DcByNodeMap::const_iterator i = downconverters_.find( node );
            if ( i == downconverters_.end( ) ) {
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
    } //

} catch (...) {
    logAndRethrowCaughtExceptionAsUserException( Priority::ERROR );
}

// -----------------------------------------------------------------------------
void WbdcMaster::reset()
{
    // The exceptions caught below are CORBA User Exceptions and are
    // transferred over the wire to a client.
    try {
        programLogInfoIfPossible( "WbdcMaster::reset()" );
        // Make sure you always call Master::reset otherwise you will have
        // an infinite recursive call to WbdcMaster::reset!
        Master::reset();
    } catch (carma::util::ErrorException &eex) {
        throw CARMA_EXCEPTION(carma::util::UserException, eex.what());
    } catch (std::exception &ex) {
        throw CARMA_EXCEPTION(carma::util::UserException, ex.what());
    }
}

// -----------------------------------------------------------------------------
void WbdcMaster::softReset()
{
    // The exceptions caught below are CORBA User Exceptions and are
    // transferred over the wire to a client.
    try {
        programLogInfoIfPossible( "WbdcMaster::softReset()" );
        softwareReset();
    } catch (carma::util::ErrorException &eex) {
        throw CARMA_EXCEPTION(carma::util::UserException, eex.what());
    } catch (std::exception &ex) {
        throw CARMA_EXCEPTION(carma::util::UserException, ex.what());
    }
}

// -----------------------------------------------------------------------------
void WbdcMaster::quit()
{
    pthread_mutex_lock(&doneMutex_);
    done_ = true;
    pthread_mutex_unlock(&doneMutex_);
}

// -----------------------------------------------------------------------------
void * WbdcMaster::runThreadEntry(void *arg)
{
    WbdcMaster *This = static_cast<WbdcMaster *>(arg);
    This->run();
    return EXIT_SUCCESS;
}

// -----------------------------------------------------------------------------
void WbdcMaster::run()
{
    Master::run();
}
