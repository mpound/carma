/** @file 
 * OvroMaster class implementation.
 * 
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.86 $
 * $Date: 2012/12/18 18:50:12 $
 * $Id: OvroMaster.cc,v 1.86 2012/12/18 18:50:12 abeard Exp $
 */

#include "carma/antenna/ovro/canbus/OvroMaster.h"

#include "carma/antenna/common/Tiltmeter.h"
#include "carma/antenna/ovro/canbus/CryoCompressor.h"
#include "carma/antenna/ovro/canbus/CryoTemperatures.h"
#include "carma/antenna/ovro/canbus/SecondaryMirror.h"
#include "carma/antenna/ovro/control/DriveEngine.h"
#include "carma/canbus/exceptions.h"
#include "carma/canbus/Utilities.h"
#include "carma/monitor/OvroSubsystem.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedPthreadMutexLock.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"

// Carma Tools includes
#include <log4cpp/Category.hh>
#include <log4cpp/Priority.hh>

// STL Includes
#include <iostream>

using namespace std;
using namespace carma;
using namespace carma::antenna::ovro;
using namespace carma::canbus;
using namespace carma::monitor;
using namespace carma::util;

namespace { // Anonymous namespace for local constants

    const ::carma::canbus::msgType DEFAULT_NODE_ID             = 1;
    const ::carma::canbus::msgType GUNN_3MM_NODE_ID            = 2;
    const ::carma::canbus::msgType GUNN_1MM_NODE_ID            = 3;
    const ::carma::canbus::msgType GUNN_1MM_RIGHT_POL_NODE_ID  = 4;

} // End anonymous namespace 

// -----------------------------------------------------------------------------
OvroMaster::OvroMaster( unsigned short antNo, 
                        bool simOfflineNodes,
                        carma::monitor::OvroSubsystem & ovroSubsystem ) :
    Master( simOfflineNodes ),
    log_(Program::getLogger()), 
    // WARNING: Initialization order dependency.
    sharedOpticsSeqNo_( ovroSubsystem.antennaCommon() ), 
    compressor_(DEFAULT_NODE_ID, *this, ovroSubsystem ),
    dewar_(DEFAULT_NODE_ID, *this, ovroSubsystem ),
    enviro_(DEFAULT_NODE_ID, *this, ovroSubsystem ),
    ifLeftPol_(common::AntennaIF::IF_LEFT_POL_NODE_ID, *this, ovroSubsystem ),
    ifRightPol_(common::AntennaIF::IF_RIGHT_POL_NODE_ID, *this, ovroSubsystem ),
    loref_(DEFAULT_NODE_ID, *this, 
           ovroSubsystem.loReferenceContainer().state(),
           ovroSubsystem.loReferenceContainer().loReference(),
           ovroSubsystem.loReferenceContainer().xac() ),
    optics_(DEFAULT_NODE_ID, *this, ovroSubsystem ),
    rxtemp_(DEFAULT_NODE_ID, *this, ovroSubsystem ),
    // WARNING: Initialization order dependency on above sharedOpticsSeqNo.
    secondary_(DEFAULT_NODE_ID, *this, ovroSubsystem, sharedOpticsSeqNo_ ),
    // WARNING: Initialization order depenedency on below drive
    tiltmeter_(DEFAULT_NODE_ID, *this, ovroSubsystem, antNo),
    yig_(DEFAULT_NODE_ID, *this, ovroSubsystem ),
    gunn1mm_(GUNN_1MM_NODE_ID, *this, ovroSubsystem ),
    gunn3mm_(GUNN_3MM_NODE_ID, *this, ovroSubsystem ),
    gunn1cm_(DEFAULT_NODE_ID, *this, ovroSubsystem.gunn1cm().varactor(),
             ovroSubsystem.gunn1cm().xac(), ovroSubsystem.gunn1cm().state() ),
    // WARNING: Initialization order dependency on if_
    rx1mmLeftPol_(
        antenna::common::SisReceiver::RX_1MM_LEFT_POL_NODE_ID, *this, 
        ovroSubsystem.antennaCommon(), 
        ovroSubsystem.rx1mm( 0 ).state(), 
        ovroSubsystem.rx1mm( 0 ).sisReceiver(),
        ovroSubsystem.rx1mm( 0 ).xac(), ifLeftPol_ ),
    rx1mmRightPol_(
        antenna::common::SisReceiver::RX_1MM_RIGHT_POL_NODE_ID, *this, 
        ovroSubsystem.antennaCommon(), 
        ovroSubsystem.rx1mm( 1 ).state(), 
        ovroSubsystem.rx1mm( 1 ).sisReceiver(),
        ovroSubsystem.rx1mm( 1 ).xac(), ifRightPol_ ),
    rx3mm_(antenna::common::SisReceiver::RX_3MM_LEFT_POL_NODE_ID, *this, 
           ovroSubsystem.antennaCommon(),
           ovroSubsystem.rx3mm().state(), ovroSubsystem.rx3mm().sisReceiver(),
           ovroSubsystem.rx3mm().xac(), ifLeftPol_ ),
    rx1cm_( DEFAULT_NODE_ID, *this, 
            ovroSubsystem.rxBias( ), 
            &( ovroSubsystem.rxBiasTemps( ) ) ),
    // WARNING: Following four elements are initialization order dependent
    drive_( *this, ovroSubsystem, antNo ),
    azEncoder_( Encoder::AZIMUTH, *this, ovroSubsystem, drive_ ),
    elEncoder_( Encoder::ELEVATION, *this, ovroSubsystem, drive_ ),
    driveEngine_( drive_, azEncoder_, elEncoder_, tiltmeter_, ovroSubsystem ),
    mon_( ovroSubsystem ),
    hostname_( Program::getHostname(true) ),
    initialized_( false )
{
    initialize(antNo);
}

// -----------------------------------------------------------------------------
OvroMaster::OvroMaster(
    int board, 
    unsigned short antNo, 
    bool simOfflineNodes,
    bool reset,
    bool terminate, 
    carma::monitor::OvroSubsystem & ovroSubsystem ) 
    : 
    Master(board, simOfflineNodes, reset, terminate),
    log_(Program::getLogger()), 
    // WARNING: Initialization order dependency.
    sharedOpticsSeqNo_( ovroSubsystem.antennaCommon() ), 
    compressor_(DEFAULT_NODE_ID, *this, ovroSubsystem ),
    dewar_(DEFAULT_NODE_ID, *this, ovroSubsystem ),
    enviro_(DEFAULT_NODE_ID, *this, ovroSubsystem ),
    ifLeftPol_(common::AntennaIF::IF_LEFT_POL_NODE_ID, *this, ovroSubsystem ),
    ifRightPol_(common::AntennaIF::IF_RIGHT_POL_NODE_ID, *this, ovroSubsystem ),
    loref_(DEFAULT_NODE_ID, *this,
           ovroSubsystem.loReferenceContainer().state(),
           ovroSubsystem.loReferenceContainer().loReference(),
           ovroSubsystem.loReferenceContainer().xac() ),
    optics_(DEFAULT_NODE_ID, *this, ovroSubsystem ),
    rxtemp_(DEFAULT_NODE_ID, *this, ovroSubsystem ),
    // WARNING: Initialization order dependency on above sharedOpticsSeqNo.
    secondary_(DEFAULT_NODE_ID, *this, ovroSubsystem, sharedOpticsSeqNo_ ),
    tiltmeter_(DEFAULT_NODE_ID, *this, ovroSubsystem, antNo),
    yig_(DEFAULT_NODE_ID, *this, ovroSubsystem ),
    gunn1mm_(GUNN_1MM_NODE_ID, *this, ovroSubsystem ),
    gunn3mm_(GUNN_3MM_NODE_ID, *this, ovroSubsystem ),
    gunn1cm_(DEFAULT_NODE_ID, *this, ovroSubsystem.gunn1cm().varactor(),
             ovroSubsystem.gunn1cm().xac(), ovroSubsystem.gunn1cm().state() ),
    // WARNING: Initialization order dependency on if_
    rx1mmLeftPol_(
        antenna::common::SisReceiver::RX_1MM_LEFT_POL_NODE_ID, *this, 
        ovroSubsystem.antennaCommon(), 
        ovroSubsystem.rx1mm( 0 ).state(), 
        ovroSubsystem.rx1mm( 0 ).sisReceiver(),
        ovroSubsystem.rx1mm( 0 ).xac(), ifLeftPol_ ),
    rx1mmRightPol_(
        antenna::common::SisReceiver::RX_1MM_RIGHT_POL_NODE_ID, *this, 
        ovroSubsystem.antennaCommon(), 
        ovroSubsystem.rx1mm( 1 ).state(), 
        ovroSubsystem.rx1mm( 1 ).sisReceiver(),
        ovroSubsystem.rx1mm( 1 ).xac(), ifRightPol_ ),
    rx3mm_(antenna::common::SisReceiver::RX_3MM_LEFT_POL_NODE_ID, *this, 
           ovroSubsystem.antennaCommon(),
           ovroSubsystem.rx3mm().state(), ovroSubsystem.rx3mm().sisReceiver(),
           ovroSubsystem.rx3mm().xac(), ifLeftPol_ ),
    rx1cm_( DEFAULT_NODE_ID, *this, 
            ovroSubsystem.rxBias( ), 
            &( ovroSubsystem.rxBiasTemps( ) ) ),
    // WARNING: Following four elements are initialization order dependent
    drive_( *this, ovroSubsystem, antNo ),
    azEncoder_( Encoder::AZIMUTH, *this, ovroSubsystem, drive_ ),
    elEncoder_( Encoder::ELEVATION, *this, ovroSubsystem, drive_ ),
    driveEngine_( drive_, azEncoder_, elEncoder_, tiltmeter_, ovroSubsystem ),
    mon_( ovroSubsystem ),
    hostname_( Program::getHostname(true) ),
    initialized_( false )
{
    initialize(antNo);
}

// -----------------------------------------------------------------------------
OvroMaster::OvroMaster(
    int board, 
    int bus, 
    unsigned short antNo, 
    bool simOfflineNodes, 
    bool reset,
    bool terminate,
    carma::monitor::OvroSubsystem & ovroSubsystem )
    : 
    Master(board, bus, simOfflineNodes, reset, terminate), 
    log_(Program::getLogger()), 
    // WARNING: Initialization order dependency.
    sharedOpticsSeqNo_( ovroSubsystem.antennaCommon() ), 
    compressor_(DEFAULT_NODE_ID, *this, ovroSubsystem ),
    dewar_(DEFAULT_NODE_ID, *this, ovroSubsystem ),
    enviro_(DEFAULT_NODE_ID, *this, ovroSubsystem ),
    ifLeftPol_(common::AntennaIF::IF_LEFT_POL_NODE_ID, *this, ovroSubsystem ),
    ifRightPol_(common::AntennaIF::IF_RIGHT_POL_NODE_ID, *this, ovroSubsystem ),
    loref_(DEFAULT_NODE_ID, *this,
           ovroSubsystem.loReferenceContainer().state(),
           ovroSubsystem.loReferenceContainer().loReference(),
           ovroSubsystem.loReferenceContainer().xac() ),
    optics_(DEFAULT_NODE_ID, *this, ovroSubsystem ),
    rxtemp_(DEFAULT_NODE_ID, *this, ovroSubsystem ),
    // WARNING: Initialization order dependency on above sharedOpticsSeqNo.
    secondary_(DEFAULT_NODE_ID, *this, ovroSubsystem, sharedOpticsSeqNo_ ),
    tiltmeter_(DEFAULT_NODE_ID, *this, ovroSubsystem, antNo),
    yig_(DEFAULT_NODE_ID, *this, ovroSubsystem ),
    gunn1mm_(GUNN_1MM_NODE_ID, *this, ovroSubsystem ),
    gunn3mm_(GUNN_3MM_NODE_ID, *this, ovroSubsystem ),
    gunn1cm_(DEFAULT_NODE_ID, *this, ovroSubsystem.gunn1cm().varactor(),
             ovroSubsystem.gunn1cm().xac(), ovroSubsystem.gunn1cm().state() ),
    // WARNING: Initialization order dependency on if_
    rx1mmLeftPol_(
        antenna::common::SisReceiver::RX_1MM_LEFT_POL_NODE_ID, *this, 
        ovroSubsystem.antennaCommon(), 
        ovroSubsystem.rx1mm( 0 ).state(), 
        ovroSubsystem.rx1mm( 0 ).sisReceiver(),
        ovroSubsystem.rx1mm( 0 ).xac(), ifLeftPol_ ),
    rx1mmRightPol_(
        antenna::common::SisReceiver::RX_1MM_RIGHT_POL_NODE_ID, *this, 
        ovroSubsystem.antennaCommon(), 
        ovroSubsystem.rx1mm( 1 ).state(), 
        ovroSubsystem.rx1mm( 1 ).sisReceiver(),
        ovroSubsystem.rx1mm( 1 ).xac(), ifRightPol_ ),
    rx3mm_(antenna::common::SisReceiver::RX_3MM_LEFT_POL_NODE_ID, *this, 
           ovroSubsystem.antennaCommon(),
           ovroSubsystem.rx3mm().state(), ovroSubsystem.rx3mm().sisReceiver(),
           ovroSubsystem.rx3mm().xac(), ifLeftPol_ ),
    rx1cm_( DEFAULT_NODE_ID, *this, 
            ovroSubsystem.rxBias( ), 
            &( ovroSubsystem.rxBiasTemps( ) ) ),
    // WARNING: Following four elements are initialization order dependent
    drive_( *this, ovroSubsystem, antNo ),
    azEncoder_( Encoder::AZIMUTH, *this, ovroSubsystem, drive_ ),
    elEncoder_( Encoder::ELEVATION, *this, ovroSubsystem, drive_ ),
    driveEngine_( drive_, azEncoder_, elEncoder_, tiltmeter_, ovroSubsystem ),
    mon_( ovroSubsystem ),
    hostname_( Program::getHostname(true) ),
    initialized_( false )
{
    initialize(antNo);
}

// -----------------------------------------------------------------------------
OvroMaster::~OvroMaster()
{
    try {

        const ScopedLogNdc ndc( "OvroMaster::~OvroMaster" );

        programLogInfoIfPossible( "Removing devices from OvroMaster." );

        Master::removeDevice(compressor_.getApi(), compressor_.getNode());
        Master::removeDevice(dewar_.getApi(), dewar_.getNode());
        Master::removeDevice(azEncoder_.getApi(), azEncoder_.getNode());
        Master::removeDevice(elEncoder_.getApi(), elEncoder_.getNode());
        Master::removeDevice(drive_.getApi(), drive_.getNode());
        Master::removeDevice(enviro_.getApi(), enviro_.getNode());
        Master::removeDevice(ifLeftPol_.getApi(), ifLeftPol_.getNode());
        Master::removeDevice(ifRightPol_.getApi(), ifRightPol_.getNode());
        Master::removeDevice(loref_.getApi(), loref_.getNode());
        Master::removeDevice(optics_.getApi(), optics_.getNode());
        Master::removeDevice(rxtemp_.getApi(), rxtemp_.getNode());
        Master::removeDevice(secondary_.getApi(), secondary_.getNode());
        Master::removeDevice(tiltmeter_.getApi(), tiltmeter_.getNode());
        Master::removeDevice(yig_.getApi(), yig_.getNode());
        Master::removeDevice(gunn1mm_.getApi(), gunn1mm_.getNode());
        Master::removeDevice(gunn3mm_.getApi(), gunn3mm_.getNode());
        Master::removeDevice(gunn1cm_.getApi(), gunn1cm_.getNode());
        Master::removeDevice(rx1mmLeftPol_.getApi(), rx1mmLeftPol_.getNode());
        Master::removeDevice(rx1mmRightPol_.getApi(), rx1mmRightPol_.getNode());
        Master::removeDevice(rx3mm_.getApi(), rx3mm_.getNode());

        programLogInfoIfPossible( "Stopping runThread." );

        this->stop();

        programLogInfoIfPossible( "Teardown complete." );

    } catch ( ... ) {
        logCaughtAsError( );
    }
}

// -----------------------------------------------------------------------------
void OvroMaster::initialize(unsigned short antNo)
{
    isRunning_ = false;

    // Add all of our device to the device map.
    Master::addDevice(&compressor_);
    Master::addDevice(&dewar_);
    Master::addDevice(&drive_);
    Master::addDevice(&azEncoder_);
    Master::addDevice(&elEncoder_);
    Master::addDevice(&enviro_);
    Master::addDevice(&ifLeftPol_);
    Master::addDevice(&ifRightPol_);
    Master::addDevice(&loref_);
    Master::addDevice(&optics_);
    Master::addDevice(&rxtemp_);
    Master::addDevice(&secondary_);
    Master::addDevice(&tiltmeter_);
    Master::addDevice(&yig_);
    Master::addDevice(&gunn1mm_);
    Master::addDevice(&gunn3mm_);
    Master::addDevice(&gunn1cm_);
    Master::addDevice(&rx1mmLeftPol_);
    Master::addDevice(&rx1mmRightPol_);
    Master::addDevice(&rx3mm_);
    Master::addDevice(&rx1cm_);
}

// -----------------------------------------------------------------------------
map<msgType, string> OvroMaster::getControls() const
{
    // Return an empty map for now...
    map<msgType, string> empty;
    return empty;
}

// -----------------------------------------------------------------------------
void OvroMaster::updateStatus()
{
    map<canbus::busIdType, canbus::busStatusType> busStatus = getBusStatus();
    map<canbus::busIdType, canbus::busStatusType>::iterator busIter;
    unsigned int busIndex;
    

    if ( busStatus.size() > 2 )
        throw CARMA_EXCEPTION(carma::util::ErrorException, 
            "OvroMaster::updateStatus()"
            " - Size of busStatus map exceeds assumed max size of 2 busses.");

    busIndex = 0;
    busIter = busStatus.begin();
    
    while ( busIter != busStatus.end() ) {
        mon_.can().bus(busIndex).busId().
            setValue(busIter->first);
        mon_.can().bus(busIndex).halfSecRxMsgRate().
            setValue(busIter->second.rxMsgRate);
        mon_.can().bus(busIndex).halfSecTxMsgRate().
            setValue(busIter->second.txMsgRate);
        mon_.can().bus(busIndex).avgRxMsgRate().
            setValue(busIter->second.oneMinRxMsgRate);
        mon_.can().bus(busIndex).avgTxMsgRate().
            setValue(busIter->second.oneMinTxMsgRate);
        mon_.can().bus(busIndex).nRxErrors().
            setValue(busIter->second.rxErrors);
        mon_.can().bus(busIndex).nTxErrors().
            setValue(busIter->second.txErrors);
        mon_.can().bus(busIndex).nLostFastMsgs().
            setValue(busIter->second.fastMsgsLost);
        mon_.can().bus(busIndex).nLostSlowMsgs().
            setValue(busIter->second.slowMsgsLost);
        mon_.can().bus(busIndex).busState().
            setValue( static_cast<Bus::BusStateMonitorPointEnum::BUSSTATE> 
                ( busIter->second.state ));
        mon_.can().bus(busIndex).maxTsLatency().
            setValue( busIter->second.tsEchoLatency );
        if ( isTerminated() ) {
            mon_.can().bus(busIndex).terminationState().setValue(
                Bus::TerminationStateMonitorPointEnum::TERMINATED );
        } else {
            mon_.can().bus(busIndex).terminationState().setValue(
                Bus::TerminationStateMonitorPointEnum::OPEN );
        }
        busIter++;
        busIndex++;
    }

    mon_.can().host().nActiveNodes().setValue( getOnlineNodeCount() );
    mon_.can().host().nOfflineNodes().setValue( getOfflineNodeCount() );
    mon_.can().host().nLatePackets().setValue( getLatePacketCount() );
    mon_.can().host().nDonglelessPackets().setValue(getDonglelessPacketCount());
    mon_.can().host().nUnknownPackets().setValue( getUnknownPacketCount() );
    mon_.can().host().hostname().setValue( hostname_ );
    mon_.timestamp().setValue(Time::MJD());
    mon_.online().setValue(true);
    mon_.antennaCommon().initialized().setValue( initialized_ );
}

// -----------------------------------------------------------------------------
void OvroMaster::reset() 
{
    // Perform a hardware reset.
    CanDio::reset();
}

// -----------------------------------------------------------------------------
void OvroMaster::softReset() 
{
    // Perform a software reset to all modules.
    Master::softwareReset();
}

// -----------------------------------------------------------------------------
void OvroMaster::setInitialization( const bool state ) 
{
    initialized_ = state;
}

// -----------------------------------------------------------------------------
void OvroMaster::run()
{
    Master::run();
}

// -----------------------------------------------------------------------------
void OvroMaster::start()
{
    // Start Master::run thread...
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
                    "OvroMaster::start() - Unable to create "
                    "run thread - " + (string) strerror(status));

        // Block on condition variable until the runThread signals.  In
        // pthreadanese - lock mutex, test predicate, wait (returns with mutex
        // locked), test predicate again, unlock mutex, continue...  Note that
        // the runThreadStartedGuard_ is unlocked via scoped lock.
        while (!isRunning_)
            isRunningCond_.Wait(isRunningGuard_);

        programLogInfoIfPossible( "OvroMaster::start() - Master is purportedly running." );

    } else {
        throw CARMA_EXCEPTION(ErrorException,
                "OvroMaster::start() - start() called twice.  Must call only "
                "once or call stop() prior to start().");
    }
}

// -----------------------------------------------------------------------------
void OvroMaster::stop()
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
                "OvroMaster::stop() - Error cancelling run thread."
                + (string)strerror(status));
              
        // Block on it for the return value...
        status = pthread_join(runThreadId_, &result);
        if (status != 0)
            throw CARMA_EXCEPTION(carma::canbus::PthreadFailException,
                "OvroMaster::stop() - Error joining on run thread."
                + (string)strerror(status));
        
        // Finally, double check that the result is what we want.
        if (result != PTHREAD_CANCELED)
            throw CARMA_EXCEPTION(carma::canbus::PthreadFailException,
                "OvroMaster::stop() = Run thread returned with invalid "
                "value.");
            
        // Stop base Master - See above note...
        Master::stop();
            
        isRunning_ = false;

    } else {
        // Thread wasn't running in the first place.
    }
}

// -----------------------------------------------------------------------------
void *OvroMaster::runThreadEntry(void *arg)
{
    const ScopedLogNdc ndc( "OvroMaster::runThreadEntry" );

    OvroMaster *This;

    // Cast arg (this) to This
    This = static_cast<OvroMaster *>(arg);

    try {
    
        // start() method is blocked here and waiting for this thread to 
        // signal back to it. I'm pretty sure that it is safe to not disable
        // cancellation here as start() is blocked until it is signalled below.

        programLogInfoIfPossible( "About to signal 'running' to waiter." );

        // Lock mutex, set predicate and signal to waiters.
        This->isRunningGuard_.Lock();
        This->isRunning_ = true;
        This->isRunningCond_.Signal();
        This->isRunningGuard_.Unlock();

        programLogInfoIfPossible( "About to run." );

        This->run();
        
        programLogInfoIfPossible( "Run has exited." );

        return EXIT_SUCCESS;

    } catch (...) {
        programLogInfoIfPossible( "OvroMaster::runThreadEntry() - caught something." );
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
           
        } catch (...) { 
            // Another exception!!!  Must have come from Pthread objects...
            // Not much we can do here - get out of dodge!
            This->log_ << log4cpp::Priority::CRIT
                << "Exception caught within retry block.  Cannot guarantee "
                << "that OvroMaster::start has been signalled to avoid "
                << "deadlock. Aborting.";
            exit(EXIT_FAILURE);
        }

        try {
            // Rethrow and try to find out what we've got.
            throw;
        } catch ( const carma::util::ErrorException & ex ) {
            This->log_ << log4cpp::Priority::ERROR 
                << "ErrorException caught with msg: " << ex.getErrorMessage();
        } catch ( const CORBA::UserException & ex ) {
            This->log_ << log4cpp::Priority::ERROR 
                << "OvroMaster::runThreadEntry() - CORBA::UserException caught"
                << " with msg: " << ex;
        } catch ( const CORBA::SystemException & ex ) {
            This->log_ << log4cpp::Priority::ERROR 
                << "CORBA::SystemException caught with msg: " << ex;
        } catch ( const ::std::exception & ex ) {
            This->log_ << log4cpp::Priority::ERROR
                << "::std::exception caught with msg: " << ex.what( );
        } catch ( ... ) {
            This->log_ << log4cpp::Priority::WARN
                << "Caught unknown exception. Rethrowing (this is probably an "
                << "anonymous exception for forced nptl stack unwinding).";
            throw; // Rethrow.
        }
    }
    return EXIT_SUCCESS;
}

// -----------------------------------------------------------------------------
carma::antenna::ovro::AntennaIF& OvroMaster::getAntennaIF( 
    const enum AntennaIFType ifType )
{
    if ( ifType == IF1 ) 
        return ifLeftPol_;
    else if ( ifType == IF2 )
        return ifRightPol_;
    else
        throw CARMA_EXCEPTION( ErrorException, "Invalid Antenna IF Type." );
}

// -----------------------------------------------------------------------------
CryoCompressor& OvroMaster::getCryoCompressor()
{
    return compressor_;
}

// -----------------------------------------------------------------------------
DriveEngine & OvroMaster::getDriveEngine()
{
    return driveEngine_;
}

// -----------------------------------------------------------------------------
CryoTemperatures& OvroMaster::getCryoTemperatures()
{
    return dewar_;
}

// -----------------------------------------------------------------------------
Tiltmeter& OvroMaster::getTiltmeter()
{
    return tiltmeter_;
}

// -----------------------------------------------------------------------------
SecondaryMirror& OvroMaster::getSecondary()
{
    return secondary_;
}

// -----------------------------------------------------------------------------
Optics& OvroMaster::getOptics()
{
    return optics_;
}

// -----------------------------------------------------------------------------
EnvironmentalMonitor& OvroMaster::getEnvironmentalMonitor()
{
    return enviro_;
}

// -----------------------------------------------------------------------------
YigPll& OvroMaster::getYigPll()
{
    return yig_;
}

// -----------------------------------------------------------------------------
RxTemperatures& OvroMaster::getRxTemperatureController()
{
    return rxtemp_;
}

// -----------------------------------------------------------------------------
carma::antenna::common::SisReceiver & 
OvroMaster::getSisReceiver( const enum SisRxType rx,
                            const enum SisRxPolType pol ) 
{
    switch (rx) {
        case SIS_RX1MM:
            if ( pol == LEFT_CIRCULAR ) 
                return rx1mmLeftPol_;
            else if ( pol == RIGHT_CIRCULAR )
                return rx1mmRightPol_;
            else
                throw CARMA_EXCEPTION(ErrorException, "Invalid polarization.");
            break;
        case SIS_RX3MM:
            // Ignore pol argument in 3mm case.
            if ( pol != SINGLE ) 
                throw CARMA_EXCEPTION(ErrorException, "Invalid polarization.");
            return rx3mm_;
            break;
        default:
            throw CARMA_EXCEPTION(ErrorException, "Invalid SisRxType.");
            break;
    }
}
    
// -----------------------------------------------------------------------------
carma::antenna::common::CMReceiver & OvroMaster::getCMReceiver( ) 
{
    return rx1cm_;
}

// -----------------------------------------------------------------------------
carma::antenna::ovro::GunnPll& OvroMaster::getGunn(enum GunnType gunn)
{
    switch (gunn) {
        case LO1MM:
            return gunn1mm_;
            break;
        case LO3MM:
            return gunn3mm_;
            break;
        default:
            throw CARMA_EXCEPTION(ErrorException, "Invalid request for Gunn "
                "module which hasn't been implemented.");
            break;
    }
}

// -----------------------------------------------------------------------------
carma::antenna::common::Varactor& OvroMaster::getVaractor( )
{
    return gunn1cm_;
}

// -----------------------------------------------------------------------------
carma::antenna::common::LOReferenceMonitor& OvroMaster::getLOReferenceMonitor() 
{
    return loref_;    
}
