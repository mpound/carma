/**@file
 * Carma Clock Master implementation.
 *
 * <dl><dt><b>Author </b></dt><dd>Chul Gwon </dl>
 * $Revision: 1.15 $
 * $Date: 2011/08/26 15:02:16 $
 * $Id: ClockMaster.cc,v 1.15 2011/08/26 15:02:16 abeard Exp $
 */

// Carma includes
#include "carma/clock/ClockMaster.h"
#include "carma/clock/ClockControl_skel.h"
#include "carma/canbus/exceptions.h"
#include "carma/canbus/Utilities.h"
#include "carma/corba/Server.h"
#include "carma/util/ErrorException.h"
#include "carma/services/Global.h"
#include "carma/util/Program.h"
#include "carma/util/Time.h"

#include <unistd.h>

using namespace std;
using namespace carma;
using namespace carma::canbus;
using namespace carma::clock;
using namespace carma::monitor;
using namespace carma::services;
using namespace carma::util;
using namespace log4cpp;

// -----------------------------------------------------------------------------
ClockMaster::ClockMaster( bool enableAutowriter,
                          const double autoWriteDelayInS )
    : clockControlPtr_( 0 ),
      globalClockControlPtr_( 0 ),
      emulate_(true)
{
    initialize();
    if (enableAutowriter)
      mon_->startAutoWriter( autoWriteDelayInS );
}

// -----------------------------------------------------------------------------
ClockMaster::ClockMaster( int modulBusNo,
                          int slotNo,
                          const double autoWriteDelayInS )
    : Master(modulBusNo, slotNo), 
      clockControlPtr_( 0 ),
      globalClockControlPtr_( 0 ),
      emulate_(false)
{
    initialize();
    mon_->startAutoWriter( autoWriteDelayInS );
}

// -----------------------------------------------------------------------------
ClockMaster::ClockMaster( int modulBusNo,
                          const double autoWriteDelayInS )
    : Master(modulBusNo), 
      clockControlPtr_( 0 ),
      globalClockControlPtr_( 0 ),
      emulate_(false)
{
    initialize();
    mon_->startAutoWriter( autoWriteDelayInS );
}

// -----------------------------------------------------------------------------
ClockMaster::~ClockMaster()
{
    void *result;
    int status;
    try {
        // Kill our run thread...
        status = pthread_cancel(runThreadId_);
        if (status != 0) {
            throw CARMA_EXCEPTION(carma::canbus::PthreadFailException,
                    "ClockMaster::~ClockMaster() - Error destroying run thread. "
                    + (string)strerror(status));
        }
        // Block on the run thread to quit.
        status = pthread_join(runThreadId_, &result);
        if (status != 0) {
            throw CARMA_EXCEPTION(carma::canbus::PthreadFailException,
                    "ClockMaster::~ClockMaster() - Error joining on run thread. "
                    + (string)strerror(status));
        }

        // Check return status and make sure it's good.
        if (result != PTHREAD_CANCELED) {
            throw CARMA_EXCEPTION(carma::canbus::PthreadFailException,
                "Master::~Master() - Read thread returned with invalid value.");
        }

        stop();

        // Remove all devices from the Master::device_ map
        removeDevices();

	// stop autowriter .. can call even if not started
	mon_->stopAutoWriter();


    } catch (carma::util::ErrorException &eex) {
        carma::util::ErrorException newErr(
            (string)eex.what() + " Caught in ClockMaster::~ClockMaster. "
            "Destructor needs to be smarter.\n", __FILE__, __LINE__);
        newErr.report();
        newErr.log(Priority::DEBUG);
    }
}

// -----------------------------------------------------------------------------
void ClockMaster::initialize()
{
    int status;
    char hostname[100];
    done_ = false;

    // Get hostname of host machine and set the member var hostname_.
    gethostname(hostname, sizeof(hostname));
    hostname_ = hostname;

    // Initialize the monitor system.
    mon_ = auto_ptr< MasterClockSubsystem >( new MasterClockSubsystem() );

    // Define our CAN network...
    addDevices();

    // Start the run method in a new seperate thread.
    status = pthread_create(&runThreadId_, NULL,
        runThreadEntry, (void *)this);

    if (status != 0) {
        throw CARMA_EXCEPTION(carma::canbus::PthreadFailException,
            "ClockMaster::initialize() - Unable to start run thread. " +
            (const string) strerror(status));
    }

    // Initialize the done mutex.
    pthread_mutex_init(&doneMutex_, NULL);

}

// -----------------------------------------------------------------------------
void ClockMaster::addDevices()
{
    corba::Server & corbaServer = Program::getProgram().getCorbaServer( );

    clock_ = auto_ptr< carma::clock::Clock >( 
        new carma::clock::Clock(1, *this, mon_.get() ) );
    globalClock_ = auto_ptr< carma::clock::Clock >( 
        new carma::clock::Clock(0, *this, mon_.get() ) );

    Master::addDevice( clock_.get() );

    namespace POA_cc = POA_carma::clock;
    corbaServer.addServant< POA_cc::ClockControl_tie >( *clock_, 
                                                        clockControlPtr_ ); 
    corbaServer.addServant< POA_cc::ClockControl_tie >( *globalClock_, 
                                                        globalClockControlPtr_ );

    // Set their busIds to ALL_BUSSES
    globalClock_->setBusId(ALL_BUSSES);
}

// -----------------------------------------------------------------------------
void ClockMaster::removeDevices()
{
    // Remove all devices from the Master device map and invoke remove_ref

    const apiType api = Clock::getApiId();
    Master::removeDevice(api, 1);  // Remove from Master (doesn't delete dev).
    Master::removeDevice(api, 0);
}

// -----------------------------------------------------------------------------
void ClockMaster::updateStatus()
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
            "ClockMaster::updateStatus()"
            " - Size of busStatus map exceeds 2 (assumed max size).");

    busIter = busStatus.begin();

    while (busIter != busStatus.end()) {
        mon_->host().busId(busIndex).setValue(busIter->first);
        mon_->host().halfSecRxMsgRate(busIndex).setValue(busIter->second.rxMsgRate);
        mon_->host().halfSecTxMsgRate(busIndex).setValue(busIter->second.txMsgRate);
        mon_->host().avgRxMsgRate(busIndex).setValue(busIter->second.oneMinRxMsgRate);
        mon_->host().avgTxMsgRate(busIndex).setValue(busIter->second.oneMinTxMsgRate);
        mon_->host().nRxErrors(busIndex).setValue(busIter->second.rxErrors);
        mon_->host().nTxErrors(busIndex).setValue(busIter->second.txErrors);
        mon_->host().nLostFastMsgs(busIndex).setValue(busIter->second.fastMsgsLost);
        mon_->host().nLostSlowMsgs(busIndex).setValue(busIter->second.slowMsgsLost);

        busIter++;
        busIndex++;
    }

    mon_->host().nActiveNodes().setValue(nOnlineNodes);
    mon_->host().nOfflineNodes().setValue(nOfflineNodes);
    mon_->host().nLatePackets().setValue(latePackets);
    mon_->host().nUnknownPackets().setValue(unknownPackets);
    mon_->host().nDonglelessPackets().setValue(donglelessPackets);
    mon_->host().hostname().setValue(hostname_);
    mon_->timestamp().setValue(Time::MJD());
}


// -----------------------------------------------------------------------------
bool ClockMaster::isDone()
{
    bool tmp;
    pthread_mutex_lock(&doneMutex_);
    tmp = done_;
    pthread_mutex_unlock(&doneMutex_);
    return tmp;
}

// -----------------------------------------------------------------------------
ClockControl_ptr ClockMaster::GlobalClock()
{
    return ClockControl::_duplicate( globalClockControlPtr_ );
}

// -----------------------------------------------------------------------------
ClockControl_ptr ClockMaster::Clock()
{
    return ClockControl::_duplicate( clockControlPtr_ );
}

// -----------------------------------------------------------------------------
void ClockMaster::reset()
{
    // The exceptions caught below are CORBA User Exceptions and are
    // transferred over the wire to a client.
    try {
        // Make sure you always call Master::reset otherwise you will have
        // an infinite recursive call to ClockMaster::reset!
        Master::reset();
    } catch (carma::util::ErrorException &eex) {
        throw CARMA_EXCEPTION(carma::util::UserException, eex.what());
    } catch (std::exception &ex) {
        throw CARMA_EXCEPTION(carma::util::UserException, ex.what());
    }
}

// -----------------------------------------------------------------------------
void ClockMaster::softReset()
{
    // The exceptions caught below are CORBA User Exceptions and are
    // transferred over the wire to a client.
    try {
        softwareReset();
    } catch (carma::util::ErrorException &eex) {
        throw CARMA_EXCEPTION(carma::util::UserException, eex.what());
    } catch (std::exception &ex) {
        throw CARMA_EXCEPTION(carma::util::UserException, ex.what());
    }
}

// -----------------------------------------------------------------------------
void ClockMaster::quit()
{
    pthread_mutex_lock(&doneMutex_);
    done_ = true;
    pthread_mutex_unlock(&doneMutex_);
}

// -----------------------------------------------------------------------------
void * ClockMaster::runThreadEntry(void *arg)
{
    ClockMaster *This = static_cast<ClockMaster *>(arg);
    This->run();
    return EXIT_SUCCESS;
}

// -----------------------------------------------------------------------------
void ClockMaster::run()
{
    Master::run();
}
