/** @file
 * Definition for carma::canbus::Master class.
 * 
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.66 $
 * $Date: 2012/07/19 18:41:48 $
 * $Id: Master.cc,v 1.66 2012/07/19 18:41:48 abeard Exp $
 */

// Carma includes
#include "carma/canbus/Device.h"
#include "carma/canbus/Master.h"
#include "carma/canbus/exceptions.h"
#include "carma/canbus/Utilities.h"
#include "carma/util/Program.h"
#include "carma/util/ScopedPthreadCancelDisable.h"
#include "carma/util/ScopedPthreadMutexLock.h"
#include "carma/util/Time.h"

#include <log4cpp/Category.hh>
#include <log4cpp/Priority.hh>

using namespace ::std;
using namespace carma::canbus;
using namespace carma::util;

namespace { // Anonymous namespace for local constants

    const msgType RESET                         = 0x000;
    const msgType SET_TIME                      = 0x001;
    const msgType SYSTEM_MONITOR_PACKET_1       = 0x120;
    const nodeType DONGLELESS_NODE              =   511;
    const int TIMESTAMP_LATENCY_THRESHOLD_WARN  = 100000; // 100 milliseconds
    const int TIMESTAMP_LATENCY_THRESHOLD_ERR   = 200000; // 200 milliseconds
	const double ONLINE_TIMEOUT                 =   1.0; // 1 second
	const double STARTING_TIMEOUT               = 20.00; // 20 seconds

} // End anonymous namespace

// -----------------------------------------------------------------------------
Master::Master( bool simOfflineNodes ) : 
    simOfflineNodes_( simOfflineNodes ) 
{
    int status = pthread_mutex_init( &deviceMutex_, 0 );
    if ( status ) {
        throw CARMA_EXCEPTION(carma::canbus::PthreadFailException,
            "Master::Master() - Unable to initialize deviceMutex_ " 
            + (string) strerror(status));
    }
    nOnlineNodes_ = 0;
    nOfflineNodes_ = 0;
    nUnknownPackets_ = 0;
    nDonglelessPackets_ = 0;
    running_ = false;
}

// -----------------------------------------------------------------------------
Master::Master(
    int boardId, 
    bool simOfflineNodes, 
    bool reset, 
    bool terminate) : 
    CanDio(boardId, reset, terminate),
    simOfflineNodes_(simOfflineNodes)
{ 
    int status = pthread_mutex_init( &deviceMutex_, 0 );
    if ( status ) {
        throw CARMA_EXCEPTION(carma::canbus::PthreadFailException,
            "Master::Master() - Unable to initialize deviceMutex_ " 
            + (string) strerror(status));
    }
    nOnlineNodes_ = 0;
    nOfflineNodes_ = 0;
    nUnknownPackets_ = 0;
    nDonglelessPackets_ = 0;
    running_ = false;
}

// -----------------------------------------------------------------------------
Master::Master(
    int boardId, 
    int modulbusId, 
    bool simOfflineNodes, 
    bool reset,
    bool terminate) : 
    CanDio(boardId, modulbusId, reset, terminate),
    simOfflineNodes_(simOfflineNodes)
{ 
  cout << "Entering Master constructor..." << std::endl;

    int status = pthread_mutex_init( &deviceMutex_, 0 );
    if ( status ) {
        throw CARMA_EXCEPTION(carma::canbus::PthreadFailException,
            "Master::Master() - Unable to initialize deviceMutex_: " 
            + (string) strerror(status));
    }
    nOnlineNodes_ = 0;
    nOfflineNodes_ = 0;
    nUnknownPackets_ = 0;
    nDonglelessPackets_ = 0;
    running_ = false;

    cout << "Leaving Master constructor..." << std::endl;
}

// -----------------------------------------------------------------------------
Master::Master( const vector< DevTermPair > & devTermPairs,
                const bool simOfflineNodes, 
                const bool reset ) :
    CanDio( devTermPairs, reset ),
    simOfflineNodes_(simOfflineNodes)
{ 
    int status = pthread_mutex_init( &deviceMutex_, 0 );
    if ( status ) {
        throw CARMA_EXCEPTION(carma::canbus::PthreadFailException,
            "Master::Master() - Unable to initialize deviceMutex_: " 
            + (string) strerror(status));
    }
    nOnlineNodes_ = 0;
    nOfflineNodes_ = 0;
    nUnknownPackets_ = 0;
    nDonglelessPackets_ = 0;
    running_ = false;
}

// -----------------------------------------------------------------------------
Master::~Master() 
{    
    int status;
    try {

        runningMutex_.Lock();
        if (running_) {
            runningMutex_.Unlock();
            stop();
        } else {
            runningMutex_.Unlock();
        }

        // Destroy the mutex...
        
        // First make sure nobody is blocked on the mutex - if somebody is
        // it's an error in my program logic or a derivative classes program
        // logic.
        status = pthread_mutex_trylock(&deviceMutex_);
        if (status != 0) {
            throw CARMA_EXCEPTION(PthreadFailException,
                "Master::~Master() - Failed to obtain device mutex for "
                "destruction!!!" + (string)strerror(errno));
        } else {
            // Unlock the mutex and destroy it.
            status = pthread_mutex_unlock(&deviceMutex_);
            if (status != 0) {
                throw CARMA_EXCEPTION(PthreadFailException,
                    "Master::~Master() - Error unlocking device mutex. "
                    + (string)strerror(errno));
            }
            status = pthread_mutex_destroy(&deviceMutex_);
            if (status != 0) {
                throw CARMA_EXCEPTION(PthreadFailException,
                    "Master::~Master() - Error destroying device mutex. "
                    + (string)strerror(errno));
            }
        }

    } catch (carma::util::ErrorException& err) {
        carma::util::ErrorException newErr(
            (string)err.what() + 
            " Caught in Master::~Master. Destructor needs to be smarter.\n",
            __FILE__, __LINE__);
        newErr.report();
        newErr.log(log4cpp::Priority::DEBUG);
    } catch (...) {
        carma::util::ErrorException err(
            "Master::~Master() - Unkown exception caught. "
            "Destructor needs to be smarter.\n", __FILE__, __LINE__);
        err.report();
        err.log(log4cpp::Priority::DEBUG);
    }
}

// -----------------------------------------------------------------------------
void Master::run() 
{
    // The order of constructor initialization guarantees that if resetting,
    // at this stage, no communication is occurring on the CAN 
    // bus(ses) and that they are in a reset state (resetHi has
    // been issued but not released with a resetLo.  
    // At this stage we want to start the Master timer and read
    // threads, then release the resetLo pulse to start everything
    // off.
    int status;
    
    {
        // Disable thread cancellation here...
        // This guarantees that the thread won't be cancelled with the
        // running mutex locked.
        ScopedPthreadCancelDisable threadCancelDisabled;
        runningMutex_.Lock();

        // Start the read thread.
        status = pthread_create(&readThreadId_, NULL, 
                readThreadEntry, (void *) this);
        if (status != 0) {    
            throw CARMA_EXCEPTION(carma::canbus::PthreadFailException, 
                    "Master::run() - Unable to create Master read thread. " 
                    + (string) strerror(status));
        }

        // Start the timer thread.
        status = pthread_create(&timerThreadId_, NULL,
                timerThreadEntry, (void *) this);
        if (status != 0) {
            throw CARMA_EXCEPTION(carma::canbus::PthreadFailException, 
                    "Master::run() - Unable to create Master timer thread. " 
                    + (string) strerror(status));
        }

        // Make sure we're driving the reset lines low - if the user specified
        // that they would like to reset upon startup, this will complete 
        // the reset.  If the line isn't being driven, this will drive it.
        // If the line is already being driven low, this is redundant. 
        reset();

        // Ok we're running now - do I need to mutex protect this???
        running_ = true;

        runningMutex_.Unlock();
    }
    
    // Block on read thread until done - pthread_join is a cancellation point,
    // but the thread we are joined on IS NOT cancelled.
    status = pthread_join(timerThreadId_, NULL);
    if (status != 0) {
        throw CARMA_EXCEPTION(carma::canbus::PthreadFailException, 
            "Master::run() - Error joining on Master timer thread." 
            + (string) strerror(status));
    }
}

// -----------------------------------------------------------------------------
void Master::addDevice(Device *dev) 
{
    if (dev->getNode() == DONGLELESS_NODE) {
        throw CARMA_EXCEPTION(carma::canbus::BadParameterException,
            "Master::addDevice() - Tried adding a "
            "device with the dongleless node id.  This node id is reserved "
            "for detecting a device which can't read its dongle.");
    }
   
    keyType key = dev->getKey();
    
    // Check that a device doesn't already exist for this node
    pthread_mutex_lock(&deviceMutex_);	

    if (devices_.find(key) != devices_.end()) {
        pthread_mutex_unlock(&deviceMutex_);
        throw CARMA_EXCEPTION(carma::canbus::BadParameterException,
            "Master::addDevice() - Adding a device "
            "with a node and api which already exists.  Check keys.");
    }	

    // Initialize the device to either OFFLINE or SIMULATED
    if ( dev->getState() != OFFLINE || dev->getState() != SIMULATED ) {
        if ( simOfflineNodes_ ) 
            dev->setState(SIMULATED);
        else 
            dev->setState(OFFLINE);
    }

    // Device checks out, add it to the map.
    devices_[key] = dev; 
    pthread_mutex_unlock(&deviceMutex_);	
}

// -----------------------------------------------------------------------------
void Master::removeDevice(apiType api, nodeType node) 
{
    keyType key = Device::createKey(api, node);
    pthread_mutex_lock(&deviceMutex_);
    
    map<keyType, Device*>::iterator i = devices_.find(key);

    if (i == devices_.end()) {
        pthread_mutex_unlock(&deviceMutex_);
        throw CARMA_EXCEPTION(carma::canbus::BadParameterException,
            "Master::removeDevice() - Tried removing "
            "a node which doesn't exist. Check keys.");
    } else {
        devices_.erase(i);
    }
    pthread_mutex_unlock(&deviceMutex_);
}

// -----------------------------------------------------------------------------
Device* Master::getDevice(apiType api, nodeType node)
{
   keyType key = Device::createKey(api, node);
   map<keyType, Device*>::iterator i = devices_.find(key);

  if (i == devices_.end()) {
     throw CARMA_EXCEPTION(carma::canbus::BadParameterException,
        "Master::getDevice() - Device doesn't exist. Check keys.");
  } else {
     return i->second;
  }
}

// -----------------------------------------------------------------------------
int Master::getOnlineNodeCount() const
{
    return nOnlineNodes_;
}

// -----------------------------------------------------------------------------
int Master::getOfflineNodeCount() const
{
    return nOfflineNodes_;
}

// -----------------------------------------------------------------------------
int Master::getUnknownPacketCount() const
{
    return nUnknownPackets_;
}

// -----------------------------------------------------------------------------
int Master::getDonglelessPacketCount() const
{
    return nDonglelessPackets_;
}

// -----------------------------------------------------------------------------
unsigned int Master::getLatePacketCount() 
{
    // TODO: Determine if this needs to lock deviceMutex (or trylock it). 
    unsigned int lpc = 0; // Late packet count
	
    // Iterate through all devices.
	map<keyType, Device*>::iterator i = devices_.begin();
	
    // Summarize late packet counts for ONLINE nodes only!
	while (i != devices_.end()) {
        if (i->second->getState() == ONLINE) {
            lpc += i->second->getNlatePackets();
        }
        ++i;
    }
    return lpc;
}

// -----------------------------------------------------------------------------
std::map<msgType, std::string> Master::getControls() const 
{
    static bool init = false;
    static map<msgType, string> controls;

    if (!init) {
        controls[RESET]         = "Global software reset";
        controls[SET_TIME]      = "Time Sync";
        init = true;
    } 
    return controls;
}

// -----------------------------------------------------------------------------
void Master::updateStatus() 
{
    cerr << "Called base Master::updateStatus()" << endl;
}

// -----------------------------------------------------------------------------
void Master::softwareReset()
{
    idType address = createId(false, 0x00, 0x00, RESET);		
    vector<byteType> data;
    data.push_back(0xE1);
    data.push_back(0x1E);
    data.push_back(0xA5);
    data.push_back(0x5A);
    data.push_back(0xC3);
    data.push_back(0x3C);
    data.push_back(0x96);
    data.push_back(0x69);
    Message msg(address, data, ALL_BUSSES);
    postMessage(msg, HIGH);
}

// -----------------------------------------------------------------------------
void Master::setTime() 
{
    const idType address = createId(false, 0x0, 0x0, SET_TIME); 
    double fullMjd;     // Full MJD.
    unsigned short mjd; // Mean Julian Day only.
    map<busIdType, busStatusType> busStats = getBusStatus();
    map<busIdType, busStatusType>::iterator bi = busStats.begin();
   
    // Loop over the busses and check the bus state
    while (bi != busStats.end()) {
        // Try to send messages unless the bus state is BUS_OFF
        if (bi->second.state != BUS_OFF) {
            Message msg(address, bi->first); // Create the message.
            fullMjd = Time::MJD();       // Get MJD
            mjd = static_cast<unsigned short>( fullMjd );
            msg << mjd 
                << static_cast<unsigned long int>( ( ( fullMjd - mjd ) * 1e9 ) )
                << static_cast<unsigned short>( 0x0000 ); // Pad with zeros.
            postMessage(msg, HIGH);      // Post at HIGH priority.
        }
        ++bi;
    }
}

// -----------------------------------------------------------------------------
void Master::updateDevicesStates() 
{
    // Note that this routine is internally called by the timer thread 
    // which mutex protects the device map.  Thus it is incorrect to 
    // try and mutex protect the device map from this routine.

    double mjd = Time::MJD();
    double diff;
    int onlineNodes = 0;
    int offlineNodes = 0;

	// Iterate through all devices.
	map<keyType, Device*>::iterator i = devices_.begin();
	
	while (i != devices_.end()) {
	    // Exclude node 0 devices as they are not real devices.
        if (i->second->getNode() != 0) {
            Device* dev = i->second;           // Retrieve the device	
            dev->updateFrameData();            // Call update frame data.
            diff = mjd - dev->getLastRxTime(); // Calculate diff since last rx
            diff = diff * Time::SECONDS_PER_DAY;     // Convert to seconds

            // If device is online but not responding    
            if ((dev->getState() == ONLINE) &&  (diff > ONLINE_TIMEOUT)) {
                if (simOfflineNodes_) 
                    dev->setState(SIMULATED);
                else 
                    dev->setState(OFFLINE);
            } else if ((dev->getState() == STARTING) && 
                    (diff > STARTING_TIMEOUT)) {
                if (simOfflineNodes_) 
                    dev->setState(SIMULATED);
                else 
                    dev->setState(OFFLINE);
            } else {
                // Nothing
            }
            switch (dev->getState()) {
                case ONLINE :
                case STARTING :    
                    onlineNodes++;
                    break;
                case OFFLINE :
                case SIMULATED :
                    offlineNodes++;
                    break;
                default:
                    break;
            }

        }
		++i;
	} // End loop over devices.  	
    nOnlineNodes_ = onlineNodes;
    nOfflineNodes_ = offlineNodes;
}

// -----------------------------------------------------------------------------
void * Master::timerThreadEntry(void *arg)
{
    try {
        Master *This = static_cast<Master *>(arg);
        This->runTimerThread();
    } catch (carma::util::ErrorException &err) {
        ostringstream os;
        os << (string)err.what()
           << "Master::timerThreadEntry() - Caught util::ErrorException: " 
           << err.what() << endl << ends;
        carma::util::ErrorException newErr(os, __FILE__, __LINE__);
        newErr.report();
        newErr.log(log4cpp::Priority::CRIT);
        exit(EXIT_FAILURE);
    } catch (std::exception &ex) {
        ostringstream os;
        os << "Master::timerThreadEntry() - Caught std::exception: "
           << ex.what() << endl << ends;
        carma::util::ErrorException newErr(os, __FILE__, __LINE__);
        newErr.report();
        newErr.log(log4cpp::Priority::CRIT);
        exit(EXIT_FAILURE);
    } catch(...) { 
        // Well this sucks - gcc 3.4 has implemented forced stack unwinding
        // in the form of an anonymous exception which get's caught here.
        // To work properly I need to rethrow it, but if it isn't the 
        // anonymous unwind exception, we're screwed - catch 22.  Fortunately 
        // the catch alls above will catch any carma or std exception.
        
        // Log it anyway but not as critical.
        Program::getLogger() << log4cpp::Priority::WARN 
            << "Master::timerThreadEntry() - Caught unknown exception and "
            << "rethrowing (this is probably an anonymous unwind exception).";

        throw; // Rethrow
    }
    return EXIT_SUCCESS;
}
    
// -----------------------------------------------------------------------------
void Master::runTimerThread() 
{
    int halfSecCount = 10;              // Half second counter. 
    bool fiveSecondBoundary;            // True time is five sec boundary
    timespec sleep, rem;                // Current time and sleep value
    map<keyType, Device*>::iterator i;  // Device iterator
    carma::canbus::Message msg;         // Simulated message
    int oldState;                       // The old cancellation state.

    // Start on next five second boundary by sleeping until then  
    sleep = calculateTimeToNextSlowBoundary();

    if (nanosleep(&sleep, &rem) == -1) {
        // Returned early, keep sleeping
        while (nanosleep(&rem, &rem) == -1);
    }

    while (true) {

        // Disable cancellation - we don't want any cancellation to occur
        // until the nanosleep call at the end of this method...
        if (pthread_setcancelstate(PTHREAD_CANCEL_DISABLE, &oldState) != 0)
            throw CARMA_EXCEPTION(carma::canbus::PthreadFailException, 
                "Master::runTimerThread() - Error setting cancel state.");
        
        // Lock the device mutex... 
        pthread_mutex_lock(&deviceMutex_);

        // Check to see if it's time to send time sync.
        if (halfSecCount%20 == 0) {
            try {
                setTime();  // Send time sync
            } catch (const carma::canbus::TxBufferFullException& ex) {
                ex.report();
                ex.log(log4cpp::Priority::DEBUG);
            }
        }	

        // Is this a five second boundary...
        fiveSecondBoundary = (halfSecCount%10 == 0 ? true : false);

        // Check active and sim status as well as bus status. 
        updateDevicesStates();

        // Loop through all devices and simulate SIM nodes.
        i = devices_.begin();
        while (i != devices_.end()) {

            // If an exception is thrown while simulating a message,
            // the device is considered to be unstable...  Log it,
            // remove the device and continue....
            try {
                if (i->second->getState() == SIMULATED && 
                        i->second->getNode() != 0) {

                    // Get a map of the monitor points for this device.
                    map<msgType, string> monitors = 
                        i->second->getHalfSecMonitors();

                    if (fiveSecondBoundary) { 
                        map<msgType, string> slowMonitors = 
                            i->second->getSlowMonitors();
                        // Add them to the monitors.
                        monitors.insert(slowMonitors.begin(), 
                                slowMonitors.end());
                    }

                    // Loop through monitors and call devs simulateMsg 
                    // method and queue it. 
                    map<msgType, string>::iterator m;
                    for (m = monitors.begin(); m != monitors.end(); ++m) {
                        // Get a simulated message.
                        msg = i->second->simulateMsg(m->first);
                        msg.setSimFlag(true);
                        // Force the rxTime on the simulated msg to
                        // the last rx time from the device.
                        msg.setRxMjd(i->second->getLastRxTime());
                        queueMessage(msg);
                    }
                }  // End if device is SIMULATED  
                
                // Increment iterator (I'M ASSUMING THIS DOES NOT THROW).
                ++i;
                
            } catch (carma::util::ErrorException &err) {
                // Form, notify and log the exception.
                ostringstream os;
                os << (string)err.what() 
                    << "Caught in Master::runTimerThread(). Exception "
                    << "caught while simulating msg for api " 
                    << dec << i->second->getApi() << " node " 
                    << i->second->getNode() << ": " << endl 
                    << "Removing device and proceeding." << endl << ends;
                carma::util::ErrorException newErr(os, __FILE__, __LINE__);
                newErr.report();
                newErr.log(log4cpp::Priority::ERROR);

                // Remove the device.
                // Note that we must do this explicitly to avoid deadlock
                // since removeDevice locks the device mutex!!!!
                devices_.erase(i++);
            } catch (...) {
                // Form, notify and log the exception.
                ostringstream os;
                os << "Master::runTimerThread() - Unknown exception caught "
                    << "while simulating msg" 
                    << " from api " << dec << i->second->getApi() 
                    << " node " << i->second->getNode() 
                    << " - Removing device and proceeding." << endl << ends;
                carma::util::ErrorException newErr(os, __FILE__, __LINE__);
                newErr.report();
                newErr.log(log4cpp::Priority::ERROR);

                // Remove the device
                devices_.erase(i++);
            }
        }  // End loop over devices.

        // Call updateStatus to allow users to process Master
        // and Bus specific monitor points.
        updateStatus();
        halfSecCount++;

        pthread_mutex_unlock(&deviceMutex_);	
        
        // Enable cancellation once again
        if (pthread_setcancelstate(oldState, NULL) != 0)
            throw CARMA_EXCEPTION(carma::canbus::PthreadFailException, 
                "Master::runTimerThread() - Error reenabling cancel state.");

        // Sleep until next half second boundary.
        sleep = calculateTimeToNextHalfSec();

        // Sleep for however long - this is the only cancellation point in 
        // this thread.
        if (nanosleep(&sleep, &rem) == -1) {
            // nanosleep returned early
            while (nanosleep(&rem, &rem) == -1);
        }
    
    } // End loop forever
}

// -----------------------------------------------------------------------------
void * Master::readThreadEntry(void *arg)
{
    try {
        Master *This = static_cast<Master *>(arg);
        This->runReadThread();
    } catch ( const carma::util::ErrorException & err ) {
        ostringstream oss;
        
        oss << "Master::readThreadEntry() - "
            << "Caught carma::util::ErrorException: " << err.getLogString( );
        
        carma::util::ErrorException newErr( oss.str( ), __FILE__, __LINE__ );
        
        newErr.report();
        newErr.log(log4cpp::Priority::CRIT);
        
        exit(EXIT_FAILURE);
    } catch ( const carma::util::BaseException & err ) {
        ostringstream oss;
        
        oss << "Master::readThreadEntry() - "
            << "Caught carma::util::BaseException: " << err.getLogString( );
           
        carma::util::ErrorException newErr( oss.str( ), __FILE__, __LINE__ );
        
        newErr.report();
        newErr.log(log4cpp::Priority::CRIT);
        
        exit(EXIT_FAILURE);
    } catch ( const ::std::exception & err ) {
        ostringstream oss;
        
        oss << "Master::readThreadEntry() - "
            << "Caught ::std::exception: " << err.what( );
        
        carma::util::ErrorException newErr( oss.str( ), __FILE__, __LINE__ );
        
        newErr.report();
        newErr.log(log4cpp::Priority::CRIT);
        
        exit(EXIT_FAILURE);
    } catch(...) {
        // Well this sucks - gcc 3.4 has implemented forced stack unwinding
        // in the form of an anonymous exception which get's caught here.
        // To work properly I need to rethrow it, but if it isn't the 
        // anonymous unwind exception, we're screwed - catch 22.  Fortunately 
        // the catch alls above will catch any carma or std exception.
        
        // Log it
        Program::getLogger() << log4cpp::Priority::WARN 
            << "Master::readThreadEntry() - Caught unknown exception and "
            << "rethrowing (this is probably an anonymous unwind exception).";

        throw; // Rethrow
    }
    
    return EXIT_SUCCESS;
}

// -----------------------------------------------------------------------------
void Master::runReadThread() 
{
    Message msg;
    idType id;
    vector<byteType> data;
    busIdType busId;
    bool host, mode, sim; // Address mode 0 api/node, 1 bt/sn
    apiType api;
    nodeType node;
    msgType mid;
    map<keyType, Device*>::iterator i;
    double rxTime, timestamp; 	// Receive time for a message.
    int oldState, diff; // difference in microseconds.

    while (true) { // Run forever. 
        
        // Block for message - this is the only cancellation point.

        msg = getMessage();
        
        // Disable cancellation for the rest of this scope.
        if (pthread_setcancelstate(PTHREAD_CANCEL_DISABLE, &oldState) != 0)
            throw CARMA_EXCEPTION(carma::canbus::PthreadFailException, 
               "Master::runReadThread() - Error setting cancel state.");
        
        // If we got here, we have a message - lock the device mutex 
        // while we process it.
        pthread_mutex_lock(&deviceMutex_);	
        
        // Extract the canId, data, and busId from message.
        id = msg.getId();
        data = msg.getData();
        busId = msg.getBusId();
        rxTime = msg.getRxMjd(); 
        sim = msg.isSimulated();
        
        // Extract carma message id parameters from canId
        host = isToHost(id);
        mode = getMode(id);
        fromId(api, node, mid, id);

        if (node == DONGLELESS_NODE) {
            // Address contains the reserved node id.  Don't process.
            nDonglelessPackets_++;
        
        } else if (host && (mode == APPLICATION)) {
            // Message is addressed to the host and the addressing
            // mode is application (non-engineering) - process this message.
           
           // Find the device in the device map.
           i = devices_.find( Device::createKey(api, node) );

           // Did we find it?
           if (i != devices_.end()) {
               
               // Ok, process the message.
               Device* dev = i->second; // Retrieve device
               dev->setLastRxTime(rxTime); // Set last rx time to msg rx mjd.
               if (dev->getBusId() != busId) // Set the busId if changed.
                   dev->setBusId(busId);

               // If the message isn't simulated, consider changing state
               // to STARTING or ONLINE.
               if ( !sim ) { 
                   switch ( dev->getState() ) {
                       case ONLINE :
                           // Nothing 
                           break;
                       case STARTING :
                           dev->setState( ONLINE ); 
                           break;
                       case OFFLINE :
                       case SIMULATED :
                           if ( mid == SYSTEM_MONITOR_PACKET_1 ) 
                               dev->setState( STARTING );
                           else 
                               dev->setState( ONLINE );
                           break;
                       default:
                               break;
                   }
               }
               
               // Try to process the message.
               // If an exception is thrown while processing the message
               // the device is unstable and can't be trusted.  Remove
               // the device from the devices_ map and continue.
               try {
                   dev->processMsg(mid, data, sim);
               } catch (carma::util::ErrorException &err) {
                   // First log the error.
                   ostringstream os;
                   os << err.what()
                       << "Caught in Master::runReadThread() while processing "
                       << "msg 0x" << hex << mid << dec 
                       << " from api " << dev->getApi() << " node " 
                       << dev->getNode() << endl 
                       << "Removing device and proceeding." << endl << ends;
                   carma::util::ErrorException newErr(os, __FILE__, __LINE__);
                   newErr.report();
                   newErr.log(log4cpp::Priority::ERROR);

                   // Remove the device. (Don't call removeDevice as it too
                   // locks the device mutex resulting in deadlock.
                   devices_.erase(i);
               } catch (...) {
                   ostringstream os;
                   os << "Master::runReadThread() - Unknown exception caught"
                       << " while processing msg 0x" << hex << mid << dec 
                       << " from api " << dev->getApi() << " node " 
                       << dev->getNode() << ": " << endl 
                       << "Removing device and proceeding." << endl << ends;
                   carma::util::ErrorException err(os, __FILE__, __LINE__);
                   err.report();
                   err.log(log4cpp::Priority::ERROR);

                   // Remove the device. (Don't call removeDevice as it too
                   // locks the device mutex resulting in deadlock.
                   devices_.erase(i);
               }
                   
           } else {
               // Device not found.
               nUnknownPackets_++;
           } // If device exists.
        } else if (!host && mid == SET_TIME) { // Timestamp watchdog
            // This is an echoed timestamp message

	  std::cout << "About to process timestamp message" << std::endl;

            timestamp = static_cast<double>( dataToUshort(data) ) + 
                       (static_cast<double>( dataToUlong(data) ) * 1.0e-9);
            diff = static_cast<int>( (rxTime - timestamp) * 
                                     Time::MICROSECONDS_PER_DAY );

	    std::cout << "About to process timestamp message: done" << std::endl;

            setTimestampEchoLatency(diff, busId);
            if (diff > TIMESTAMP_LATENCY_THRESHOLD_ERR) 
                Program::getLogger() << log4cpp::Priority::ERROR
                    << "Echoed timestamp latency of " << diff 
                    << " microseconds exceeded threshold of "
                    << TIMESTAMP_LATENCY_THRESHOLD_ERR << "!";
            else if (diff > TIMESTAMP_LATENCY_THRESHOLD_WARN) 
                Program::getLogger() << log4cpp::Priority::WARN 
                    << "Echoed timestamp latency of " << diff 
                    << " microseconds exceeded threshold of "
                    << TIMESTAMP_LATENCY_THRESHOLD_WARN << "!";
             
        } else {
            // Message was not addressed to the host or is an
            // engineering message - don't process.
            nUnknownPackets_++;
        }
        pthread_mutex_unlock(&deviceMutex_);
        if (pthread_setcancelstate(oldState, NULL) != 0)
           throw CARMA_EXCEPTION(carma::canbus::PthreadFailException, 
               "Master::runReadThread() - Error setting cancel state.");
    }  // End loop forever
}

// -----------------------------------------------------------------------------
void Master::stop()
{
    void *result;
    int status;

   ScopedPthreadMutexLock scopeLock(runningMutex_);

    if (!running_) 
        throw CARMA_EXCEPTION(carma::util::ErrorException, 
            "Master::stop() - stop() called prior to run()!");

    // Block on threads to quit. 
    // Wait on read thread to quit.
    // Cancel read thread.
    status = pthread_cancel(readThreadId_);
    if (status != 0) {
        throw CARMA_EXCEPTION(carma::canbus::PthreadFailException,
            "Master::stop() - Error cancelling read thread." 
            + (string)strerror(status));
    }
    
    status = pthread_join(readThreadId_, &result);
    if (status != 0) {
        throw CARMA_EXCEPTION(carma::canbus::PthreadFailException,
                "Master::~stop() - Error joining on read thread. " 
                + (string)strerror(status));
    }
    if (result != PTHREAD_CANCELED) {
        throw CARMA_EXCEPTION(carma::canbus::PthreadFailException,
                "Master::~stop() - Read thread returned with "
                "invalid value.");
    }

    // Wait on timer thread to quit.
    status = pthread_cancel(timerThreadId_);
    if (status != 0) {
        throw CARMA_EXCEPTION(carma::canbus::PthreadFailException,
            "Master::stop() - Error cancelling timer thread." 
            + (string)strerror(status));
    }
    status = pthread_join(timerThreadId_, &result);
    if (status != 0) {
        throw CARMA_EXCEPTION(carma::canbus::PthreadFailException,
                "Master::~stop() - Error joining on timer thread. " 
                + (string)strerror(status));
    }
    if (result != PTHREAD_CANCELED) {
        throw CARMA_EXCEPTION(carma::canbus::PthreadFailException,
                "Master::~stop() -Timer thread returned with "
                "invalid value.");
    }
    running_ = false;

}
