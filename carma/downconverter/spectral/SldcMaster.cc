/**@file
 * SldcMaster class definition.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.27 $
 * $Date: 2012/01/31 00:22:04 $
 * $Id: SldcMaster.cc,v 1.27 2012/01/31 00:22:04 abeard Exp $
 *
 * $CarmaCopyright$
 */

// Carma includes
#include "carma/canbus/exceptions.h"
#include "carma/downconverter/spectral/SldcMaster.h"
#include "carma/downconverter/spectral/SpectralDownconverter.h"
#include "carma/monitor/SignalPathSubsystem.h"
#include "carma/monitor/SldcSubsystem.h"
#include "carma/services/Global.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/programLogging.h"
#include "carma/util/Program.h"
#include "carma/util/ScopedPthreadMutexLock.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"

using namespace carma;
using namespace carma::canbus;
using namespace carma::downconverter;
using namespace carma::monitor;
using namespace carma::services;
using namespace carma::switchyard;
using namespace carma::util;
using namespace std;

namespace {

    const bool RESET_ON_STARTUP = true;

} // namespace < unnamed >

// -----------------------------------------------------------------------------
SldcMaster::SldcMaster( 
        ::carma::monitor::SldcSubsystem & sldcMon,
        ::carma::monitor::SignalPathSubsystem & signalPathMon ) :
    isRunning_(false),
    sldcMon_( sldcMon ),
    signalPathMon_( signalPathMon ),
    loControl_( *this, 
                sldcMon_.loControlContainer( ).state( ),
                sldcMon_.loControlContainer( ).loControl( ),
                sldcMon_.loControlContainer( ).xac( ) ),
    loMonitor_( 1, *this,  
                sldcMon_.loMonitorContainer( ).state( ),
                sldcMon_.loMonitorContainer( ).loMonitor( ),
                sldcMon_.loMonitorContainer( ).xac( ) ),
    noiseSource_( 1, *this, 
                  sldcMon_.noiseSourceContainer() ),
    switchyard_( carma::switchyard::IFSWITCHYARD_NODE,
                 *this, 
                 signalPathMon_.iFSwitchyard().state(),
                 signalPathMon_.iFSwitchyard().switchyard(),
                 signalPathMon_.iFSwitchyard().xac() ),
    dcLoSwitchyard_( carma::switchyard::DCLOSWITCHYARD_NODE,
                 *this, 
                 signalPathMon_.dCLOSwitchyard().state(),
                 signalPathMon_.dCLOSwitchyard().switchyard(),
                 signalPathMon_.dCLOSwitchyard().xac() ),
    globalQuadMod_( 0, *this, 
                    sldcMon_.quadModContainer( 0 ) ),
    globalSldc_( 0, *this, sldcMon_ ),
    globalBlockDownconverter_( 0, *this, sldcMon_ ),
    hostname_( Program::getHostname(true) )
{
    initialize();
}

// -----------------------------------------------------------------------------
SldcMaster::SldcMaster( const vector< CanDio::DevTermPair > & devTermPairs,
                        const bool simOfflineNodes,
                        SldcSubsystem & sldcMon,
                        SignalPathSubsystem & signalPathMon ) :
    Master( devTermPairs, simOfflineNodes, RESET_ON_STARTUP),
    isRunning_(false),
    sldcMon_( sldcMon ),
    signalPathMon_( signalPathMon ),
    loControl_( *this, 
                sldcMon_.loControlContainer( ).state( ),
                sldcMon_.loControlContainer( ).loControl( ),
                sldcMon_.loControlContainer( ).xac( ) ),
    loMonitor_( 1, *this,  
                sldcMon_.loMonitorContainer( ).state( ),
                sldcMon_.loMonitorContainer( ).loMonitor( ),
                sldcMon_.loMonitorContainer( ).xac( ) ),
    noiseSource_( 1, *this, sldcMon_.noiseSourceContainer() ),
    switchyard_( carma::switchyard::IFSWITCHYARD_NODE,
                 *this, 
                 signalPathMon_.iFSwitchyard().state(),
                 signalPathMon_.iFSwitchyard().switchyard(),
                 signalPathMon_.iFSwitchyard().xac() ),
    dcLoSwitchyard_( carma::switchyard::DCLOSWITCHYARD_NODE,
                 *this, 
                 signalPathMon_.dCLOSwitchyard().state(),
                 signalPathMon_.dCLOSwitchyard().switchyard(),
                 signalPathMon_.dCLOSwitchyard().xac() ),
    globalQuadMod_( 0, *this, 
                    sldcMon_.quadModContainer( 0 ) ),
    globalSldc_( 0, *this, sldcMon_ ),
    globalBlockDownconverter_( 0, *this, sldcMon_ ),
    hostname_( Program::getHostname(true) )
{
    initialize();
}

// -----------------------------------------------------------------------------
SldcMaster::~SldcMaster()
{
    // Delete all devices and remove them from the map...
    SldcDeviceMap::iterator dit = sldcDevices_.begin();

    while (dit != sldcDevices_.end()) {
        Master::removeDevice( dit->second->getApi(), dit->second->getNode() );
        delete dit->second;
        sldcDevices_.erase(dit++);
    }

    QuadModDeviceMap::iterator qmit = quadMods_.begin();
    while ( qmit != quadMods_.end() ) {
        Master::removeDevice( qmit->second->getApi(), qmit->second->getNode() );
        delete qmit->second;
        quadMods_.erase( qmit++ );
    }

    BlockDcDeviceMap::iterator bdcit = blockDownconverters_.begin();
    while ( bdcit != blockDownconverters_.end() ) {
        Master::removeDevice( bdcit->second->getApi(), 
                              bdcit->second->getNode() );
        delete bdcit->second;
        blockDownconverters_.erase( bdcit++ );
    }

    Master::removeDevice( noiseSource_.getApi(), noiseSource_.getNode() );
    Master::removeDevice( loMonitor_.getApi(), loMonitor_.getNode() );
    Master::removeDevice( loControl_.getApi(), loControl_.getNode() );
    Master::removeDevice( dcLoSwitchyard_.getApiId( ), DCLOSWITCHYARD_NODE );
    Master::removeDevice( switchyard_.getApiId( ), IFSWITCHYARD_NODE );
}

// -----------------------------------------------------------------------------
void SldcMaster::initialize()
{
    nodeType node;

    CPTRACE(Trace::TRACE6, "SldcMaster::SldcMaster() - Creating SldcMaster.");

    Master::addDevice( &switchyard_ );
    Master::addDevice( &dcLoSwitchyard_ );
    Master::addDevice( &loControl_ );
    Master::addDevice( &loMonitor_ );
    Master::addDevice( &noiseSource_ );

    for (unsigned input = 1; input <= Global::nSpectralStations(); input++) {

        CPTRACE(Trace::TRACE4, "SldcMaster::SldcMaster() - Adding block dc.");
        // Create block downconverter instances...
        blockDownconverters_[ input ] = new BlockDownconverter( 
            input, 
            *this, 
            sldcMon_ );

        quadMods_[ input ] = new QuadratureModulator( 
            input,
            *this,
            sldcMon_.quadModContainer( input - 1 ) );
            
        Master::addDevice( blockDownconverters_[input] );
        Master::addDevice( quadMods_[ input ] );
        
        for (unsigned band = 1; band <= Global::nSpectralLineBands(); band++) {

            CPTRACE(Trace::TRACEALL, "SldcMaster::initialize() - Adding "
                "spectral line downconverter to master for input " << input <<
                ", band " << band << ".");
        
            // Calculate node id and add the device.
            node = SpectralDownconverter::calculateNodeId(input, band);
            
            sldcDevices_[node] = new SpectralDownconverter( node, 
                                                            *this, 
                                                            sldcMon_ );
            
            Master::addDevice( sldcDevices_[node] );

        } // End loop over bands
    } // End loop over correlator stations

    CPTRACE(Trace::TRACEALL, "SldcMaster::initialize() - Done.");
}

// -----------------------------------------------------------------------------
void SldcMaster::addDevice(Device *device)
{
    Master::addDevice(device);    
}

// -----------------------------------------------------------------------------
std::map<carma::canbus::msgType, std::string> SldcMaster::getControls() const
{
    // Return an empty map
    map<msgType, string> empty;
    return empty;
}

// -----------------------------------------------------------------------------
void SldcMaster::updateStatus() 
{
    map<canbus::busIdType, canbus::busStatusType> busStatus = getBusStatus();
    map<canbus::busIdType, canbus::busStatusType>::iterator busIter;
    unsigned int busIndex;

    if ( busStatus.size() > 4 )
        throw CARMA_EXCEPTION(carma::util::ErrorException,
             "SldcMaster::updateStatus()"
             " - Size of busStatus map exceeds assumed max size of 4 busses.");

    busIndex = 0;
    busIter = busStatus.begin();

    while ( busIter != busStatus.end() ) {
        sldcMon_.can().bus(busIndex).busId().
            setValue(busIter->first);
        sldcMon_.can().bus(busIndex).halfSecRxMsgRate().
            setValue(busIter->second.rxMsgRate);
        sldcMon_.can().bus(busIndex).halfSecTxMsgRate().
            setValue(busIter->second.txMsgRate);
        sldcMon_.can().bus(busIndex).avgRxMsgRate().
            setValue(busIter->second.oneMinRxMsgRate);
        sldcMon_.can().bus(busIndex).avgTxMsgRate().
            setValue(busIter->second.oneMinTxMsgRate);
        sldcMon_.can().bus(busIndex).nRxErrors().
            setValue(busIter->second.rxErrors);
        sldcMon_.can().bus(busIndex).nTxErrors().
            setValue(busIter->second.txErrors);
        sldcMon_.can().bus(busIndex).nLostFastMsgs().
            setValue(busIter->second.fastMsgsLost);
        sldcMon_.can().bus(busIndex).nLostSlowMsgs().
            setValue(busIter->second.slowMsgsLost);
        sldcMon_.can().bus(busIndex).busState().
            setValue( static_cast<Bus::BusStateMonitorPointEnum::BUSSTATE>
                    ( busIter->second.state ));
        sldcMon_.can().bus(busIndex).maxTsLatency().
            setValue(busIter->second.tsEchoLatency);
        busIter++;
        busIndex++;
    }

    sldcMon_.can().host().nActiveNodes().setValue( getOnlineNodeCount() );
    sldcMon_.can().host().nOfflineNodes().setValue( getOfflineNodeCount() );
    sldcMon_.can().host().nLatePackets().setValue( getLatePacketCount() );
    sldcMon_.can().host().nDonglelessPackets().setValue(
        getDonglelessPacketCount( ) );    
    sldcMon_.can().host().nUnknownPackets().setValue( getUnknownPacketCount() );
    sldcMon_.can().host().hostname().setValue( hostname_ );
    sldcMon_.timestamp().setValue(Time::MJD());
    sldcMon_.online().setValue(true);
}

// -----------------------------------------------------------------------------
void SldcMaster::reset()
{
    // Hardware reset.
    CanDio::reset();
}

// -----------------------------------------------------------------------------
void SldcMaster::softReset()
{
    // Software reset
    Master::softwareReset();
}

// -----------------------------------------------------------------------------
SpectralDownconverter & SldcMaster::getSldc(
    unsigned short inputIndex,
    unsigned short bandIndex)
{
    nodeType node = SpectralDownconverter::calculateNodeId(
        inputIndex, bandIndex);
    SldcDeviceMap::iterator sldc = sldcDevices_.find(node);

    if ( sldc == sldcDevices_.end() ) 
        throw CARMA_EXCEPTION(
            carma::util::IllegalArgumentException,
            "SldcMaster::getSldc() - Invalid input or band index.");

    return *(sldc->second);
}

// -----------------------------------------------------------------------------
SpectralDownconverter & SldcMaster::getGlobalSldc() 
{
    return globalSldc_;
}

// -----------------------------------------------------------------------------
carma::downconverter::LoControl & SldcMaster::getLoControl( ) 
{
    return loControl_;
}

// -----------------------------------------------------------------------------
carma::downconverter::LoMonitor & SldcMaster::getLoMonitor( ) 
{
    return loMonitor_;
}

// -----------------------------------------------------------------------------
carma::downconverter::NoiseSource & SldcMaster::getNoiseSource( )
{
    return noiseSource_;
}

// -----------------------------------------------------------------------------
carma::downconverter::QuadratureModulator & 
SldcMaster::getQuadMod( const unsigned short inputNo )
{
    QuadModDeviceMap::iterator i = quadMods_.find( inputNo );
    if ( i == quadMods_.end() ) 
        throw CARMA_EXCEPTION( ErrorException, "Invalid inputNo." );

    return *( i->second );
}

// -----------------------------------------------------------------------------
carma::downconverter::QuadratureModulator & 
SldcMaster::getGlobalQuadMod( )
{
    return globalQuadMod_;
}

// -----------------------------------------------------------------------------
carma::downconverter::BlockDownconverter &
SldcMaster::getBlockDownconverter( const unsigned short inputNo )
{
    {
        ostringstream log;
        log << "SldcMaster::getBlockDownconverter(inputNo=" << inputNo << ").";
        programLogInfoIfPossible( log.str() );
    }

    const BlockDcDeviceMap::iterator dc = blockDownconverters_.find( inputNo );
    if ( dc == blockDownconverters_.end( ) ) 
        throw CARMA_EXCEPTION( ErrorException, "Invalid inputNo." );

    return *( dc->second );
}

// -----------------------------------------------------------------------------
carma::downconverter::BlockDownconverter &
SldcMaster::getGlobalBlockDownconverter( )
{
    programLogInfoIfPossible( "SldcMaster::getGlobalBlockDownconverter()." );
    return globalBlockDownconverter_;
}

// -----------------------------------------------------------------------------
carma::switchyard::Switchyard &
SldcMaster::getSwitchyard( )
{
    programLogInfoIfPossible( "SldcMaster::getSwitchyard()." );
    return switchyard_;
}

// -----------------------------------------------------------------------------
carma::switchyard::Switchyard &
SldcMaster::getDcLoSwitchyard( )
{
    programLogInfoIfPossible( "SldcMaster::getDcLoSwitchyard()." );
    return dcLoSwitchyard_;
}

// -----------------------------------------------------------------------------
void SldcMaster::start()
{
    // Start Master::run thread
    if (!isRunning_) {
        int status;

        // Basic method here is to use a condition variable to ping-pong with
        // the runThread which will then signal when it has indeed started.
        // After that this routine can return.  Care is taken not to block this
        // guy forever.  There is a small performance hit here but it avoids
        // the start/stop race.
        ScopedPthreadMutexLock lock(isRunningGuard_);

        // Create separate thread to block on Master::run()...
        status = pthread_create(&runThreadId_, NULL,
                runThreadEntry, static_cast<void *>(this));
        if (status != 0)
            throw CARMA_EXCEPTION(ErrorException,
                    "SldcMaster::start() - Unable to create "
                    "run thread - " + static_cast<string>(strerror(status)));

        // Block on condition variable until the runThread signals.  In
        // pthreadanese - lock mutex, test predicate, wait (returns with mutex
        // locked), test predicate again, unlock mutex, continue...  Note that
        // the runThreadStartedGuard_ is unlocked via scoped lock.
        while (!isRunning_)
            isRunningCond_.Wait(isRunningGuard_);
            
    } else {
        throw CARMA_EXCEPTION(ErrorException,
            "SldcMaster::start() - start() called twice.");
    }
}
     
// -----------------------------------------------------------------------------
void SldcMaster::stop()
{
    // This whole stop/start thing is pretty shoddy.  Really I need a
    // better way to hide and partition threads in the canbus::Master.
    // Perhaps someday in the future...
    void *result;
    int status;
    ScopedPthreadMutexLock lock(isRunningGuard_);

    if (isRunning_) {

        // OK, here I must cancel the runThread prior to stopping master.
        // The reason for this is that Master::run() blocks on pthread_join
        // with the timer thread.  In addition Master::stop(), calls
        // cancel and then pthread_join on the timer thread as well.  This
        // is an error if stop() is called prior to cancellation of the
        // run thread because only one thread can be joined on a thread!
        // This problem is not trivial and needs a more general fix in
        // the Master base class.

        // Cancel run thread...
        status = pthread_cancel(runThreadId_);
        if (status != 0)
            throw CARMA_EXCEPTION(carma::canbus::PthreadFailException,
                    "SldcMaster::stop() - Error cancelling run thread."
                    + (string)strerror(status));

        // Block on it for the return value...
        status = pthread_join(runThreadId_, &result);
        if (status != 0)
            throw CARMA_EXCEPTION(carma::canbus::PthreadFailException,
                    "SldcMaster::stop() - Error joining on run thread."
                    + (string)strerror(status));

        // Finally, double check that the result is what we want.
        if (result != PTHREAD_CANCELED)
            throw CARMA_EXCEPTION(carma::canbus::PthreadFailException,
                    "SldcMaster::stop() = Run thread returned with invalid "
                    "value.");

        // Stop base Master - See above note...
        Master::stop();

        isRunning_ = false;

    } else {
        // Thread wasn't running in the first place.
    }
}

// -----------------------------------------------------------------------------
void *SldcMaster::runThreadEntry(void *arg)
{
    SldcMaster *This;

    This = static_cast<SldcMaster *>(arg);

    try {
        // start() method is blocked here and waiting for this thread to
        // signal back to it. I'm pretty sure that it is safe to not disable
        // cancellation here as start() is blocked until it is signalled below.

        // Lock mutex, set predicate and signal to waiters.
        This->isRunningGuard_.Lock();
        This->isRunning_ = true;
        This->isRunningGuard_.Unlock();
        This->isRunningCond_.Signal();

        This->run();
        return EXIT_SUCCESS;

    } catch (...) {
        try {
            // Things went south somewhere...
            // Make sure we've signalled to the object creator as it is
            // absolutely essential that we unblock start() to prevent
            // deadlock.
            if (This->isRunningGuard_.TryLock()) {
                if (!This->isRunning_) {
                    This->isRunning_ = true;
                    This->isRunningCond_.Signal();
                }
                This->isRunningGuard_.Unlock();
            }
            programLogWarnIfPossible( 
                "SldcMaster::runThreadEntry() - Unknown exception caught. "
                "Thread is exsiting but successfully signalled "
                "SldcMaster::start() (main should continue on)." );

        } catch (...) {
            programLogErrorIfPossible( 
                "SldcMaster::runThreadEntry() - Exception caught within "
                "retry block.  Cannot guarantee that SldcMaster::start "
                "has been signalled to avoid deadlock. Aborting." );
            exit(EXIT_FAILURE);
        }
    }
    return EXIT_SUCCESS;
}

                                                                                          
