/**@file
 * Definition of carma::canbus::JanzCanIo class.
 * 
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.3 $
 * $Date: 2013/11/05 00:20:21 $
 * $Id: JanzCanIo.cc,v 1.3 2013/11/05 00:20:21 eml Exp $
 */

#include "carma/canbus/JanzCanIo.h"

#include "carma/util/compileTimeCheck.h"

#include <boost/foreach.hpp>
#include <errno.h>
#include <fcntl.h>
#include <pthread.h>
#include <sys/select.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <signal.h>

// stl includes
#include <map>
#include <string>
#include <iomanip>
#include <sstream>

// Janz includes
#include <janz/bcan.h>
#include <janz/can_lib.h>
#include <janz/defs.h>
#include <janz/dpm.h>
#include <janz/mitop.h>
#include <janz/vmod.h>

// log4cpp includes
#include <log4cpp/Priority.hh>

// Carma includes
#include "carma/canbus/exceptions.h"
#include "carma/canbus/JanzMessage.h"
#include "carma/canbus/Types.h"
#include "carma/canbus/Utilities.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/IPQreader.h"
#include "carma/util/IPQwriter.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/ScopedLock.h"
#include "carma/util/ScopedLockManager.h"
#include "carma/util/ThreadQuit.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"

using namespace carma::canbus;
using namespace carma::util;
using namespace log4cpp;    
using namespace std;

namespace {

    // Janz slow and fast interface read and write queue sizes.
    // The Janz VMOD-ICAN contains onboard communication buffers 
    // with 256 windows of length 256 bytes each.  Windows 0-8
    // are reserved for use by the firmware.  Windows 9-255 are 
    // available for communication using the 'new style host interface'
    // (referred to a slow) and the 'fast style host interface'.  
    // The 'new' interface is used for general purpose communication 
    // with the VMOD-ICAN modules, the 'fast' interface is for CAN 
    // messages.  New interface buffers take a single window per buffer
    // while the fast interface can fit 16 buffers per window.  With
    // the 'fast interface' each buffer corresponds to a CAN message.
    // The total number of windows split among the interfaces
    // must not exceed 247 (256 total - 9 reserved).  
    //
    // In addition, we create and use prioritized tx message queues. 
    // To do so, you specify the size of the queues and how many prioritized
    // queues you want.  The janz lib then creates the prioritize queues and an
    // additional non-prioritized queue of the same size!!!
    //
    // Finally, I've been unable to exactly use the entire 247 dpm windows,
    // rather, I've only been able to allocate at most 245 windows without any 
    // problems but I've been unable to find the reason for this.  My guess
    // is that there is a small bug in the janz c-library wrapper for creating
    // and initializing DPM queues.

    const int BOARD_SLOW_READ_LENGTH       = 20;   // 20 windows
    const int BOARD_SLOW_WRITE_LENGTH      = 20;   // 20 windows
    const int BOARD_FAST_READ_LENGTH       = 1680; // 105 windows
    const int BOARD_FAST_WRITE_LENGTH      = 400;  // 25 windows
    const int N_PRIORITIZED_WRITE_QUEUES   = 3;

    // Maximum delay for an extended can message with the highest priority.
    // 154 bits @ 1mbit/s in nanoseconds (see Lawrenz pg. 95).
    const int MAX_CAN_TX_TIME = 154000; // 154 bits @ 1mbit/s in nanoseconds.
    const double FUDGE = 0.5; // Fudge factor for fine tuning.
    const int ADJUSTED_CAN_TX_TIME = static_cast<int>(MAX_CAN_TX_TIME * FUDGE);
    
    // Amount of time to wait for the CAN card to flush a full write buffer .  
    // The FUDGE factor above assures that we don't wait for the entire buffer
    // to empty out and thus ensure a high throughput even when throttled.
    // This is used to throttle down the amount of messages placed on the bus 
    // during high bandwidth bursts, without dropping messages.
    const timespec CAN_FLUSH_INTERVAL = {
        static_cast<time_t>(
            (BOARD_FAST_WRITE_LENGTH * ADJUSTED_CAN_TX_TIME) /
             NANOSECS_PER_SEC),
        static_cast<long>(
            (BOARD_FAST_WRITE_LENGTH * ADJUSTED_CAN_TX_TIME) %
             NANOSECS_PER_SEC)
    };

    // This is the number of times we will try to resend a CAN message if
    // the onboard buffer is full.
    const int MAX_FLUSH_RETRYS = 2;

    const Trace::TraceLevel TRACE_THREAD_SHUTDOWN = Trace::TRACE5;

    typedef carma::util::ScopedLock< ::pthread_mutex_t > Guard;
    typedef carma::util::ScopedLockManager< ::pthread_mutex_t > ManagedGuard;
    
    void
    compileTimeVerifyFastMessageEquivalency( )
    {
        compileTimeCheck< 
            sizeof( ::BYTE_t ) == sizeof( carma::canbus::BYTE_t ) >( );
        compileTimeCheck< sizeof( ::FastMessage ) == 
                          sizeof( carma::canbus::FastMessage ) >( );
    }
} // End anonymous namespace

// -----------------------------------------------------------------------------
carma::canbus::JanzCanIo::JanzCanIo( ) : 
    emulate_( true ),
    echo_( true )
{
    int status;

    // Add a device with bus Id 0 to devices_
    addDevice(0);

    // Set up the device and bus.
    devices_[0]->name = "/dev/null";
    devices_[0]->arg = (void *)this;

    // Initialize queue access mutex.
    status = pthread_mutex_init( &rxQueueAccessor_.mutex, NULL );

    failIfPosixError( status );

    // Initialize queue access condition variable.
    status = pthread_cond_init( &rxQueueAccessor_.cond, NULL );

    failIfPosixError( status );
    
    // Initialize the write mutex.
    status = pthread_mutex_init( &( devices_[0]->mutex ), NULL );

    failIfPosixError( status );

    // Open null device.
    devices_[0]->fd = open( devices_[0]->name.c_str( ), O_WRONLY );

    if ( devices_[0]->fd < 0 ) {
        throw CARMA_EXCEPTION( 
            carma::canbus::SystemException,
            "JanzCanIo::JanzCanIo() - Error opening device " + devices_[0]->name + 
            " " + std::string( strerror( errno ) ) );
    }

    // Initialize IPQs for Direct CAN.
    initializeIpqs( );
}

// -----------------------------------------------------------------------------
carma::canbus::JanzCanIo::JanzCanIo( const char * deviceName, bool terminate ) : 
    emulate_( false ),
    echo_( true )
{ 	
  cout << "Entering JanzCanIo constructor...0" << std::endl;
    busIdType busId;

    // Take advantage of automatic type conversion from string to char * 
    busId = extractBusId( deviceName );

  cout << "Entering JanzCanIo constructor...1" << std::endl;

    // Determine busIds from device name.
    addDevice(busId); 

  cout << "Entering JanzCanIo constructor...2" << std::endl;
    // Set up the device and bus.
    devices_[busId]->name = deviceName;
    devices_[busId]->arg = static_cast<void *>( this );

    // Initialize bus
    devices_[busId]->bus.terminate = terminate;	

  cout << "Entering JanzCanIo constructor...3" << std::endl;
    // Initialize the devices for communication.
    initialize( );

  cout << "Entering JanzCanIo constructor...4" << std::endl;
    // Initialize IPQs for Direct CAN.
    initializeIpqs( );

  cout << "Entering JanzCanIo constructor...5" << std::endl;
    // Start threads to perform communication.
    createThreads( );
  cout << "Leaving JanzCanIo constructor..." << std::endl;
}
	
// -----------------------------------------------------------------------------
carma::canbus::JanzCanIo::JanzCanIo(
    const char* dev0, const bool term0, 
    const char* dev1, const bool term1 ) :
    emulate_( false ),
    echo_( true )
{
    busIdType busId0, busId1;
    
    // Take advantage of automatic type conversion from string to char * 
    busId0 = extractBusId( dev0 );
    busId1 = extractBusId( dev1 );

    addDevice( busId0 );
    addDevice( busId1 );

    // Set up the devices and busses.
    // Initialize first device structure
    devices_[busId0]->name = dev0;
    devices_[busId0]->arg = static_cast<void *>( this );

    // Initialize first bus structure
    devices_[busId0]->bus.terminate = term0;

    // Initialize second device structure
    devices_[busId1]->name = dev1;
    devices_[busId1]->arg = static_cast<void *>( this );

    // Initialize second bus structure
    devices_[busId1]->bus.terminate = term1;

    // Initialize devices for communication.
    initialize( );

    // Initialize IPQs for Direct CAN.
    initializeIpqs( );

    // Create and start threads to communicate.
    createThreads( );
}
	
// -----------------------------------------------------------------------------
carma::canbus::JanzCanIo::JanzCanIo( const NameTermPairVec & nameTermPairs ) :
    emulate_( false ),
    echo_( true )
{
    const NameTermPairVec::const_iterator pBegin = nameTermPairs.begin();
    const NameTermPairVec::const_iterator pEnd = nameTermPairs.end();
    for ( NameTermPairVec::const_iterator p = pBegin; p != pEnd; ++p ) 
    {
        const busIdType busId = extractBusId( p->first );

        addDevice( busId );

        // Set up the devices and busses.
        // Initialize first device structure
        devices_[busId]->name = p->first;
        devices_[busId]->arg = static_cast<void *>( this );

        // Initialize first bus structure
        devices_[busId]->bus.terminate = p->second;
    }

    // Initialize devices for communication.
    initialize( );

    // Initialize IPQs for Direct CAN.
    initializeIpqs( );

    // Create and start threads to communicate.
    createThreads( );
}
	
// -----------------------------------------------------------------------------
carma::canbus::JanzCanIo::~JanzCanIo() 
{   	
    // Method to destruction.  In order for JanzCanIo to be useable as a 
    // standalone object, we must assure that all threads and devices
    // are properly canceled and closed when the object is destroyed.  
    // The proper way to destroy a thread is to cancel the thread and 
    // then join with it and wait for the thread to return.  
    // The canceled thread will cancel at the next 'cancelation point' 
    // which is basically any blocking io calls, sleeps or pthread_cond_waits.
    // As a direct result of this, two considerations were taken
    // into account when writing the thread code a) Never place 
    // cancellation points within mutex locks as this could prevent
    // another thread from cancelling (i.e. this destructor would block 
    // forever) and b) Never detach created threads since we must eventually 
    // join on them when the destructor is called. 
    try {
        int status = 0;
        void *result;
        std::map<busIdType,deviceType*>::iterator di;

        if (!emulate_) { 

            CARMA_CPTRACE( TRACE_THREAD_SHUTDOWN, "Killing write thread." );

            // Kill write thread
            RequestThreadQuit( writeThreadId_ );

            // Wait on write thread to quit.
            status = pthread_join(writeThreadId_, &result);

            failIfPosixError( status );
            
            if (result != EXIT_SUCCESS ) {
                throw CARMA_EXCEPTION(
                        carma::canbus::PthreadFailException,
                        "JanzCanIo::~JanzCanIo() - Write thread returned fail code." );
            }

            CARMA_CPTRACE( TRACE_THREAD_SHUTDOWN, "Killing update thread." );

            // Kill update thread
            RequestThreadQuit( updateThreadId_ );

            // Wait on update thread to quit.
            status = pthread_join( updateThreadId_, &result );

            failIfPosixError( status );

            if ( result != EXIT_SUCCESS ) {
                throw CARMA_EXCEPTION(
                    carma::canbus::PthreadFailException,
                    "JanzCanIo::~JanzCanIo() - Update thread returned failure." );
            }
            
            CARMA_CPTRACE( TRACE_THREAD_SHUTDOWN, "Killing device threads." );

            // Kill device threads. and mutexes...
            for ( di = devices_.begin();  di != devices_.end(); ++di ) {
                // Destroy device's read thread and join 
                status = pthread_cancel( di->second->threadId );

                failIfPosixError( status );
                
                status = pthread_join( di->second->threadId, &result );

                failIfPosixError( status );
                
                if ( result != PTHREAD_CANCELED ) {
                    throw CARMA_EXCEPTION(
                        carma::canbus::PthreadFailException,
                        "JanzCanIo::~JanzCanIo() - Write thread returned failure." );
                }
            }
                
            CARMA_CPTRACE( TRACE_THREAD_SHUTDOWN, 
                           "Destroying mutexes and condition vars." );

            // We are assured now that no thread is accessing any mutexes
            // or condition variables so we can safely destroy them.
            for ( di = devices_.begin(); di != devices_.end(); ++di ) {   
                // Close the device
                can_close(di->second->fd);
                di->second->fd = -1;

                // Destroy write mutex.  
                status = pthread_mutex_destroy(&(di->second->mutex));

                failIfPosixError( status );
            }
            
            status = pthread_mutex_destroy( &rxQueueAccessor_.mutex ); 

            failIfPosixError( status );

            status = pthread_cond_destroy( &rxQueueAccessor_.cond );

            failIfPosixError( status );
        } else { // If emulate
            
            // Close null devices using close
            for ( di = devices_.begin();  di != devices_.end(); ++di ) {
                close( di->second->fd );
                di->second->fd = -1;
            }

        } // End if !emulate

        // Delete the devices in the device map.
        for ( di = devices_.begin(); di != devices_.end(); ++di ) {
            delete di->second;
        }
        
    } catch ( const carma::util::ErrorException & err ) {
        // Catch exception here and print out the error message...
        // If an exception is thrown during normal execution of a program,
        // it is possible that the JanzCanIo object will go out of scope and 
        // the destructor called BEFORE the exception is caught, if another 
        // exception is then thrown in this destructor above and not caught 
        // here, neither exception will be caught at all and terminate() 
        // called. This is a general problem common to all exception 
        // handling and destruction.  
        carma::util::ErrorException newErr(
            (std::string)err.what() +  
            " Caught in JanzCanIo::~JanzCanIo. Destructor needs to be smarter.\n",
            __FILE__, __LINE__);
        newErr.report();
        newErr.log(Priority::DEBUG);
    } catch (...) {
        carma::util::ErrorException err(
            " JanzCanIo::~JanzCanIo() - Unknown exception caught in destructor.",
            __FILE__, __LINE__);
        err.report();
        err.log(Priority::DEBUG);
    }
}

// -----------------------------------------------------------------------------
void carma::canbus::JanzCanIo::initialize() 
{
	::Message msg;			// Message for vmod-ican
	int bytesRead = 0;
	unsigned char readBuf;	
	int status;
    std::map<busIdType,deviceType*>::iterator di;
	
	// Initialize all synchronization variables...

	// Initialize queue access mutex.
	status = pthread_mutex_init( &rxQueueAccessor_.mutex, NULL );

    failIfPosixError( status );
	
	// Initialize queue access condition variable.
	status = pthread_cond_init( &rxQueueAccessor_.cond, NULL );

    failIfPosixError( status );
	
	// Initialize all device specific information and devices themselves.
	for ( di = devices_.begin( ); di != devices_.end( ); ++di ) {
        // Initialize the write mutex.
        status = pthread_mutex_init( &( di->second->mutex ), NULL );

        failIfPosixError( status );
	
		// Open the device (No "const" in JANZ driver - cast away).
		di->second->fd = 
            can_open( const_cast<char *>( di->second->name.c_str( ) ) );

		if ( di->second->fd < 0 ) {
            throw CARMA_EXCEPTION(
                carma::canbus::JanzFailException,
                "JanzCanIo::initialize() - Couldn't open " + di->second->name );
		}

		// Initialize the CAN controller and interfaces to the
		// VMOD-ICAN device.
		
    	// Set termination if required
        SwitchCanTermination( &msg, 
                              static_cast<int>( di->second->bus.terminate ) );
        while ( can_send( di->second->fd, &msg ) <= 0 );

    	// Set to baud rate to 1Mbit/s
		IcWriteBtrBCAN( &msg, 0x2300 );
		while ( can_send( di->second->fd, &msg ) <= 0 );

		// Set Software acceptance mask such that standard (11 bit id) and
		// extended (29 bit id) can messages are received.
    	IcRangeSetAfil( &msg, 0x00, 0x7ff, 2 );
		while ( can_send( di->second->fd, &msg ) <= 0 );
		
		// Switch device to the 'new style host interface'.
		// The 'new style host interface' is the Janz way of saying that
		// all board communication will use interrupt notification via read().
    	ican2_select_hostif( di->second->fd, 
                             BOARD_SLOW_READ_LENGTH, 
                             BOARD_SLOW_WRITE_LENGTH );
	
		// All CAN activity will be through the fast interface - use 
        // tx prioritized queues.
		if( ican2_init_fast_can_prio( di->second->fd, 
                                      BOARD_FAST_READ_LENGTH, 
                                      BOARD_FAST_WRITE_LENGTH,
                                      N_PRIORITIZED_WRITE_QUEUES ) < 0 ) 
        {
			throw CARMA_EXCEPTION(
                carma::canbus::JanzFailException,
                "JanzCanIo::initialize() - ican2_init_fast_can_prio() failed." );
		}
		
		// Make sure we're on the bus
		IcBusOnBCAN( &msg );
    	while ( can_send( di->second->fd, &msg ) <= 0 );
			
		// Probe the card to verify that all interfaces are set up.
		while ( !( di->second->initialized ) ) {
		
			InquiryHostInterface( &msg );
			while ( can_send( di->second->fd, &msg ) <= 0 );
		
			// Read 1 byte from CAN device (new interface).	
			bytesRead = read( di->second->fd, &readBuf, 1 );
			
			// And only 1 byte...
			if ( bytesRead != 1 ) {
				throw CARMA_EXCEPTION( 
                    carma::canbus::JanzFailException,
                    "JanzCanIo::initialize() - Invalid number of bytes read from " 
                    + di->second->name );
			}
			
			switch ( readBuf ) {
				case PLAIN_QUEUE:
					processPlainMessage( *( di->second ) );
					break;
				default:
					// Ignore other queues since they may not be setup.
					break;
			}
		}
	}
}

// -----------------------------------------------------------------------------
void carma::canbus::JanzCanIo::initializeIpqs()
{
    // Setup IPQs for DirectCan
    // Try to open an existing write IPQ, if this fails, open a new one.
    writeIpq_ = new IPQwriter<carma::canbus::Message>(CAN_OUTPUT_IPQ,
        true, IPQ_BUFFER_SIZE);
        
    // Same with the read ipq.
    readIpq_ = new IPQreader<carma::canbus::Message>(CAN_INPUT_IPQ,
        true, IPQ_BUFFER_SIZE);
    
    // We want to start reading the readIpq at the top so that we 
    // don't read and post a bunch of old messages to the CANbus.
    readIpq_->setNoneAvailable();
}
    
// -----------------------------------------------------------------------------
void carma::canbus::JanzCanIo::createThreads( ) 
{    
	int status;
    std::map<busIdType, deviceType*>::iterator di; // Device map iterator.
	
	// Start read thread for each device. 
	for (di = devices_.begin(); di != devices_.end(); ++di ) {
        
        status = pthread_create( &(di->second->threadId), 
                                 NULL,
                                 readThreadEntry, 
                                 static_cast<void *>( di->second ) );
        
        failIfPosixError( status );
	}
		
	// Start the update thread...
    status = pthread_create( &updateThreadId_, 
                             NULL, 
                             updateThreadEntry, 
                             static_cast<void *>( this ) );

    failIfPosixError( status );

    // Start the ipq reader/ CAN writer thread...
    status = pthread_create( &writeThreadId_, 
                             NULL,
                             writeThreadEntry, 
                             static_cast<void *>( this ) );

    failIfPosixError( status );
}
	
// -----------------------------------------------------------------------------
bool carma::canbus::JanzCanIo::waitForMessage(double waitForMaxSeconds) 
{
	struct timespec delay;  // Timeout.
	int status = 0;         // Return status of timedwait.
	bool result;            // Could be due to a timeout or signal.
	
	delay.tv_sec = static_cast<int>( waitForMaxSeconds );
	delay.tv_nsec = static_cast<int>( ( waitForMaxSeconds - delay.tv_sec ) 
                                      * 1000000000 ); 

	// Lock mutex, test predicate, wait (unlocks mutex, 
	// then returns with mutex locked), retest predicate, unlock mutex.
    pthread_cleanup_push( &waitCleanupHandler, static_cast<void *>( this ) );

    Guard scopelock( rxQueueAccessor_.mutex );
    
    // Loop until we timeout or a message is placed in the queue
    while (!(rxQueueAccessor_.messageQueue.size() > 0) && status != ETIMEDOUT) {
		if (waitForMaxSeconds >= 0) {	
            // Cancellation Point which will cancel with mutex locked!
			status = pthread_cond_timedwait(&rxQueueAccessor_.cond, 
					&rxQueueAccessor_.mutex, &delay);
		} else {
            // Cancellation Point which will cancel with mutex locked!
			status = pthread_cond_wait(&rxQueueAccessor_.cond,
					&rxQueueAccessor_.mutex);
		}
	}

    // Mutex is locked here regardless of the exit status.
    if ( status != 0 && status != ETIMEDOUT ) {
	    throw CARMA_EXCEPTION(carma::canbus::PthreadFailException,
                "JanzCanIo::waitForMessage() - Conditional timeout error.");
    }
    
    result = ( rxQueueAccessor_.messageQueue.size() > 0 );
    pthread_cleanup_pop( 0 );
    
    return result;
}

// -----------------------------------------------------------------------------
void carma::canbus::JanzCanIo::waitCleanupHandler(void *arg)
{
    JanzCanIo *This = static_cast<JanzCanIo *>(arg);
    pthread_mutex_unlock( &(This->rxQueueAccessor_.mutex) );
}
    
// -----------------------------------------------------------------------------
carma::canbus::Message carma::canbus::JanzCanIo::getMessage() 
{
	while ( !waitForMessage( ) );

    Guard lock( rxQueueAccessor_.mutex );

	Message tmp = rxQueueAccessor_.messageQueue.front();

    rxQueueAccessor_.messageQueue.pop(); 

	return tmp;
}


// -----------------------------------------------------------------------------
void carma::canbus::JanzCanIo::postMessage( const carma::canbus::Message & msg, 
                                        txPriorityType prio) 
{
    JanzMessage jmsg( msg );

    if ( echo_ ) jmsg.enableEcho();

    const busIdType busId = jmsg.getBusId();
    try {
        if (busId != ALL_BUSSES) {
            canFastSend(busId, jmsg, prio);
        } else {
            // Declare a device iterator.
            map<busIdType,deviceType*>::iterator di;

            // Post message to all busses.
            for (di = devices_.begin(); di != devices_.end(); di++) {
                canFastSend(di->first, jmsg, prio);
            }
        }
    } catch (carma::canbus::TxBufferFullException &txErr) {
        // Rethrow exception with more information (Bus Id).
        // The rethrow is done because exceptions raised in this
        // method will need more info to be useful (i.e. the Bus Id,
        // which allows a user to figure out which Janz device is acting up).
        ostringstream os;
        os << (string)txErr.what()
            << "Caught by JanzCanIo::postMessage() for Bus " << busId
            << ". Rethrown." << ends;
        throw CARMA_EXCEPTION(carma::canbus::TxBufferFullException, os);

    } catch (carma::util::ErrorException &err) {
        ostringstream os;
        os << (string)err.what()
            <<" Caught by JanzCanIo::postMessage() for Bus " << busId
            << ". Rethrown." << ends;
        throw CARMA_ERROR (os);
    }
}

// -----------------------------------------------------------------------------
void carma::canbus::JanzCanIo::echoAll(bool enable)
{
    echo_ = enable;
}

// -----------------------------------------------------------------------------
BusStatusMap carma::canbus::JanzCanIo::getBusStatus() const
{
	BusStatusMap tmp;

    BOOST_FOREACH( const DeviceMap::value_type & dev, devices_ ) 
    {
        Guard scopedlock( dev.second->mutex );
        tmp[ dev.first ] = dev.second->bus.status;
    }

	return tmp;
}


// -----------------------------------------------------------------------------
void carma::canbus::JanzCanIo::updateBusStatus() 
{
	::Message msg;
	int status;
    int rxCount, txCount;     // Message counts since last update.
    int oneMinRx, oneMinTx;   // Message counts for 1 min updates.
    timespec now, last, diff; // Times for now, last update, and the difference.
    timespec lastOneMin;
    std::map<busIdType, deviceType*>::iterator di; // Device iterator.
    
    for (di = devices_.begin(); di != devices_.end(); di++) {
    
        Guard scopedlock( di->second->mutex );

        // Inquire to the CAN bus about its' state and check the return value.
        InquiryStatus(&msg);
        status = can_send(di->second->fd, &msg);

        if (status == -1) {
            throw CARMA_EXCEPTION(carma::canbus::JanzFailException,
                "JanzCanIo::updateBusStatus() - Failed on send " 
                + (std::string)strerror(errno));
        } else if (status == 0) {
            throw CARMA_EXCEPTION(carma::canbus::JanzFailException,
                "JanzCanIo::updateBusStatus() - Couldn't send (no space?)");
        }

        // Update the half second rx and tx msg rates.    
        last = di->second->bus.lastUpdateTime;
        rxCount = di->second->bus.rxCount;
        txCount = di->second->bus.txCount;

        clock_gettime(CLOCK_REALTIME, &now);

        // Calculate the difference between the last update and now.
        if (now.tv_nsec >= last.tv_nsec) {
            diff.tv_sec = (now.tv_sec - last.tv_sec);
            diff.tv_nsec = (now.tv_nsec - last.tv_nsec);
        } else {
            diff.tv_sec = (now.tv_sec - last.tv_sec) - 1;
            diff.tv_nsec = NANOSECS_PER_SEC - 
                abs(now.tv_nsec - last.tv_nsec);
        }

        // Calculate the actual message rates.
        di->second->bus.status.rxMsgRate = rxCount /
            ((double)(diff.tv_sec + (1.0e-9) * diff.tv_nsec));
        di->second->bus.status.txMsgRate = txCount /
            ((double)(diff.tv_sec + (1.0e-9) * diff.tv_nsec));

        // Reset running values.
        di->second->bus.rxCount = 0;
        di->second->bus.txCount = 0;

        di->second->bus.lastUpdateTime = now;

        // Update the one minute rx and tx msg rates.
        lastOneMin = di->second->bus.lastOneMinUpdate;
        
        // Calculate the difference between the last update and now.
        if (now.tv_nsec >= lastOneMin.tv_nsec) {
            diff.tv_sec = (now.tv_sec - lastOneMin.tv_sec);
            diff.tv_nsec = (now.tv_nsec - lastOneMin.tv_nsec);
        } else {
            diff.tv_sec = (now.tv_sec - lastOneMin.tv_sec) - 1;
            diff.tv_nsec = NANOSECS_PER_SEC - 
                abs(now.tv_nsec - lastOneMin.tv_nsec);
        }

        // Only perform the calculation if the diff is > 60 seconds.
        if (diff.tv_sec >= 60) {
            oneMinRx = di->second->bus.oneMinRxCount;
            oneMinTx = di->second->bus.oneMinTxCount;
            
            di->second->bus.status.oneMinRxMsgRate = oneMinRx /
                ((double)(diff.tv_sec + (1.0e-9) * diff.tv_nsec));
            di->second->bus.status.oneMinTxMsgRate = oneMinTx /
                ((double)(diff.tv_sec + (1.0e-9) * diff.tv_nsec));

            // Reset running values
            di->second->bus.oneMinRxCount = 0;
            di->second->bus.oneMinTxCount = 0;
            di->second->bus.lastOneMinUpdate = now;
        }
	}
}	

// -----------------------------------------------------------------------------
void carma::canbus::JanzCanIo::clearReadQueue() 
{
    Guard scopedlock( rxQueueAccessor_.mutex );

    while (!rxQueueAccessor_.messageQueue.empty()) {
        rxQueueAccessor_.messageQueue.pop();
    }
}

// -----------------------------------------------------------------------------
void carma::canbus::JanzCanIo::queueMessage(const Message &msg)
{
    // Copy it into the queue - critical section
    Guard scopedlock( rxQueueAccessor_.mutex );

	// Set predicate, signal to blocked tasks, unlock.
	rxQueueAccessor_.messageQueue.push(msg);

    // Signal to waiting threads.
	pthread_cond_signal(&rxQueueAccessor_.cond);
}

// -----------------------------------------------------------------------------
void carma::canbus::JanzCanIo::setTimestampEchoLatency(
    int tsLatency, 
    busIdType busId)
{
    std::map<busIdType, deviceType*>::iterator di = devices_.find(busId); 
    
    if ( di == devices_.end() )
        throw CARMA_EXCEPTION(carma::util::ErrorException, "Invalid bus id.");

	pthread_mutex_lock(&(di->second->mutex));
    di->second->bus.status.tsEchoLatency = tsLatency;
	pthread_mutex_unlock(&(di->second->mutex));
}

// ----------------------------------------------------------------------------
void carma::canbus::JanzCanIo::processFastMessage(deviceType& dev) 
{	
	::FastMessage fmsg;
    
    ManagedGuard managedGuard( dev.mutex );

    managedGuard.lock( );
	
	if(can_recv_fast(dev.fd, &fmsg) < 0) {
        throw CARMA_EXCEPTION(carma::canbus::JanzFailException,
            "JanzCanIo::processFastMessage() - Error reading fast queue. " 
            + dev.name + " " + (std::string)strerror(errno));
	}
    
    carma::canbus::FastMessage * pfmsg = reinterpret_cast< carma::canbus::FastMessage *>( &fmsg );

	// Convert message to Message object and
	// transfer to queue.
	JanzMessage jmsg( *pfmsg, dev.bus.id);
    
    // Increment rxCount
    dev.bus.rxCount++;
    dev.bus.oneMinRxCount++;

    Message msg( jmsg.getId(), jmsg.getData(), jmsg.getBusId() );
    msg.setRxMjd( Time::MJD() );

    // Write to the direct CAN write ipq as well
    *((Message *)writeIpq_) = msg;
    writeIpq_->write();
	
    managedGuard.unlock( );

    queueMessage(msg);
	
}

// -----------------------------------------------------------------------------
void carma::canbus::JanzCanIo::processPlainMessage(deviceType &dev) 
{
	char writeBuffer[600];  // Buffer to print messages to.	
	::Message msg;          // Janz plain message
	Guard scopedlock( dev.mutex );
	busStatusType& status = dev.bus.status;

	if (can_recv(dev.fd, &msg) < 0) {
        throw CARMA_EXCEPTION(carma::canbus::JanzFailException,
            "JanzCanIo::processPlainMessage() - Error reading plain queue. " 
            + dev.name + " " + (std::string)strerror(errno));
	}	
	
	// Low level stuff...
	
	// Check and see if this is a 'Module Management message.'
	if ((msg.cmd >= M_CONNECT) && (msg.cmd <= M_FMSG_LOST)) {
		
		unsigned short subspec = word_from_bytes(msg.data[0], msg.data[1]);
		
		// Update the bus status.
		switch (msg.cmd) {
			case M_INQUIRY :
				if (subspec == MS_INQUIRY_EXTD_STATUS &&
					msg.data[2] == INQUIRY_CTRL_SJA1000) {
					status.rxErrors = static_cast<short> (msg.data[5]);
					status.txErrors = static_cast<short> (msg.data[6]);
				} else if (subspec == MS_INQUIRY_HOST_IF) {
					bool newInterface = static_cast<bool>(msg.data[2]);
					bool fastInterface = static_cast<bool>(msg.data[3]);
					dev.initialized = newInterface && fastInterface;
				}
				break;
			case M_MSG_LOST :
				status.slowMsgsLost += 
                    static_cast<unsigned char>(msg.data[0]);
				break;
			case M_FMSG_LOST :
				status.fastMsgsLost += 
                    static_cast<unsigned short>(msg.data[0]);	
				break;
			default :
				// There are tons of these kinds of messages
				// most don't concern us but we want to know
				// about it anyway for debugging.
				format_modulmgr(writeBuffer, 0, &msg);
				std::cout << writeBuffer << std::endl;
				break;
		}
	
	// Check if this is a bus state message.
	} else if ((msg.cmd >= M_TIMER_START_req) && 
			 (msg.cmd <= M_SNIFFFIL_MASK_req)) { 
		
		// The M_BCAN_EVENT_ind message contains information about the 
		// state of the bus and thus is used to set the BusErrorState
		// variable according to the following rules (see Lawrenz pg. 89).
		// NO_ERRORS - No interrupt received from module (note that actual
		// 			   error count may be between 1-96 although unlikely).
		// ERROR_ACTIVE - A Bus Error interrupt has been received with 
		// 				a Rx AND Tx Error count of <= 127.
		// ERROR_PASSIVE - A Bus Error interrupt has been received with 
		// 				an Rx OR Tx Error count of >= 128.
		// BUS_OFF - A Bus Error interrupt has been received with a 
		// 			Tx Error count >= 255.
		if ((msg.cmd == M_BCAN_EVENT_ind) && (msg.data[0] == 1) 
				&& (msg.data[1] == 2)){
	
			// We've received an error interrupt from an SJA1000. 
			if (msg.data[4] < 96 && msg.data[5] < 96) {
				status.state = NO_ERRORS;
			} else if ((msg.data[4] <= 127) && msg.data[5] <= 127) {
				status.state = ERROR_ACTIVE;
			} else if (msg.data[4] >= 127 || msg.data[5] >= 127) {
				status.state = ERROR_PASSIVE;
			} else {
                // Sadly, we should never get here.
				status.state = BUS_OFF;
			}
		} else {
			format_icanos(writeBuffer, 0, &msg);
			std::cout << writeBuffer << std::endl;
		}
		
	} else {
	}
}

// -----------------------------------------------------------------------------
void * carma::canbus::JanzCanIo::readThreadEntry(void *arg) 
{
    try {
        deviceType *dev = static_cast<deviceType *>(arg);

        // Retrieve the dev
        JanzCanIo *This = static_cast<JanzCanIo *>(dev->arg);	
        This->runReadThread(*(dev));
    } catch ( ... ) {
        // This is an anonymous stack unwind exception - rethrow.
        throw;
    }
	return EXIT_SUCCESS;
}

// -----------------------------------------------------------------------------
void carma::canbus::JanzCanIo::runReadThread(deviceType &dev) {	

  sigset_t allSignals;
  sigfillset(&allSignals);
  pthread_sigmask(SIG_BLOCK, &allSignals, NULL);

	try {	
		
		int bytesRead;
		unsigned char charBuf;
			
		while (true) {
			bytesRead = read(dev.fd, &charBuf, 1);
		
			// 1 and only 1!
			if (bytesRead == 1) {
				switch(charBuf) {
					case FAST_QUEUE: // Fast interface queue
						processFastMessage(dev);
						break;
					case PLAIN_QUEUE: // Plain interface queue
						processPlainMessage(dev);
						break;
				}
			} else { 
                throw CARMA_EXCEPTION(carma::canbus::JanzFailException,
                    "JanzCanIo::runReadThread() - Error reading from new "
                    "interface. " + dev.name + " " + (std::string)strerror(errno));
			}
		} // End loop forever 
	} catch ( carma::util::ErrorException &error) {
        std::ostringstream os;
        os << (std::string)error.what() 
            << " Caught in JanzCanIo::runReadThread() - Bus "
            << dev.bus.id << " " << dev.name << " fd "
            << dev.fd << ". Error message:" << error.getErrorMessage()
            << " Exiting!" << std::endl << std::ends;
        carma::util::ErrorException newErr(os, __FILE__, __LINE__);
        newErr.report();
        newErr.log(Priority::CRIT);
		exit(EXIT_FAILURE);
	} 
}

// -----------------------------------------------------------------------------
void * carma::canbus::JanzCanIo::writeThreadEntry(void *arg)
{
    ScopedThreadQuitRegisterSelf quittable;

    try { 
        JanzCanIo *This = static_cast<JanzCanIo *>(arg);

        This->runWriteThread();
    } catch ( ... ) {
        if ( ! MarkCaughtExceptionOkayToDestructIfThreadQuitRequestedError() ) {
            logCaught( Priority::CRIT );

		    exit( EXIT_FAILURE );
        } 
    }
    
    return EXIT_SUCCESS;
}

// -----------------------------------------------------------------------------
void carma::canbus::JanzCanIo::runWriteThread() 
{
    busIdType busId;
    int lostMsgs = 0; // Lost message count from IPQ read
    // Run forever.
    while (true) {

        ThreadQuitTestSelf( );

        // Block on the read ipq until a message is received.
        // Use no lock variant to avoid priority inversion of any
        // other higher priority writer/reader (possible if another
        // JanzCanIo process exists and is running at a higher or realtime
        // priority level).  Cancelation point.
        lostMsgs = readIpq_->read();

        ThreadQuitTestSelf( );

        ScopedThreadQuitDeferSelf deferThreadQuit;

        if (lostMsgs > 0) {
            std::ostringstream err;
            err << " JanzCanIo::runWriteThread() - DirectCan write IPQ is "
                << "being written to faster than the messages can be "
                << "posted to the CAN bus. " << lostMsgs << " messages "
                << "dropped." << std::endl;
            carma::canbus::BufferOverflowException ex(err,
                    __FILE__, __LINE__);
            ex.report();
            ex.log(Priority::WARN);
        }

        // Only post CAN messages for busses that this object controls
        // 
        // In the interest of stability, if a TxBufferFull exception
        // occurs, the message is dropped and a warning is logged.
        // 
        // This section can become a severe bottleneck if either a 
        // DirectCan or CanOverIp message is going to an empty bus 
        // by a misused send to ALL_BUSSES or a send to the wrong bus 
        // - Setting disableTxRetry will aleviate this at the expense
        // of lost messages if arbitration fails. 
        //
        // Ugly hack time - don't send CAN over IP messages if there are
        // errors on the bus.  In particular, if the queue is full for
        // whatever reason, and there is no module to ack.
        busId = readIpq_->getBusId();

        if ( busId == ALL_BUSSES ) {
            // Loop over devices
            DeviceMap::iterator di = devices_.begin();
            while ( di != devices_.end() ) {
                // Don't post if bus is not active
                if ( di->second->bus.status.state == NO_ERRORS || 
                        di->second->bus.status.state == ERROR_ACTIVE ) {
                    try {
                        // MUST set bus id explicitly to avoid sending 
                        // duplicates under ALL_BUSSES!!!
                        readIpq_->setBusId( di->first );
                        postMessage( *readIpq_ ); // Cancellation point
                    } catch (carma::canbus::TxBufferFullException &err) {
                        err.report();
                        err.log(Priority::WARN);
                    }
                }
                ++di;
            }
        } else if ( devices_.find( busId ) != devices_.end() ) {
            try {
                postMessage( *readIpq_ ); // Cancellation point
            } catch (carma::canbus::TxBufferFullException &err) {
                err.report();
                err.log(Priority::WARN);
            }
        } else {
            // Drop the message
        }
    }
} 
        
// -----------------------------------------------------------------------------
void * carma::canbus::JanzCanIo::updateThreadEntry(void *arg) 
{
    ScopedThreadQuitRegisterSelf quittable;

    try {
        JanzCanIo *This = static_cast<JanzCanIo *>(arg);

        This->runUpdateThread();
    } catch ( ... ) {
        if ( ! MarkCaughtExceptionOkayToDestructIfThreadQuitRequestedError() ) {
            logCaught( Priority::CRIT );

		    exit( EXIT_FAILURE );
        } 
    }
    
    return EXIT_SUCCESS;
}

// -----------------------------------------------------------------------------
void carma::canbus::JanzCanIo::runUpdateThread() 
{
    int result = 0;
    timespec sleep, rem;

    while (true) {

        ThreadQuitTestSelf( );

        // Update the bus status.
        updateBusStatus();

        ThreadQuitTestSelf( );

        ScopedThreadQuitDeferSelf deferThreadQuit;

        // Sleep for 1/2 second to middle of next frame. Cancellation point.
        sleep = calculateTimeToNextHalfSec();
        sleep.tv_nsec += 250000000;
        result = nanosleep(&sleep, &rem);
        while (result == -1) {
            result = nanosleep(&rem, &rem);
        }
    }
}

// -----------------------------------------------------------------------------
void carma::canbus::JanzCanIo::addDevice(busIdType busId)
{
    deviceType *dev = new deviceType;

    // Initialize the device.
    dev->name = "";
    dev->fd = -1;
    dev->initialized = false;
    dev->arg = NULL;

    // Initialize bus
    dev->bus.terminate = false;
    dev->bus.rxCount = 0;
    dev->bus.txCount = 0;
    dev->bus.oneMinRxCount = 0;
    dev->bus.oneMinTxCount = 0;
    dev->bus.id = busId;
    dev->bus.status.state = NO_ERRORS;
    dev->bus.status.rxMsgRate = 0.0;
    dev->bus.status.txMsgRate = 0.0;
    dev->bus.status.oneMinRxMsgRate = 0.0;
    dev->bus.status.oneMinTxMsgRate = 0.0;
    dev->bus.status.rxErrors = 0;
    dev->bus.status.txErrors = 0;
    dev->bus.status.slowMsgsLost = 0;
    dev->bus.status.fastMsgsLost = 0;
    dev->bus.status.tsEchoLatency = 0;
    clock_gettime(CLOCK_REALTIME, &(dev->bus.lastUpdateTime));
    dev->bus.lastOneMinUpdate = dev->bus.lastUpdateTime;

    if (devices_.find(busId) != devices_.end()) {
        throw CARMA_EXCEPTION(carma::canbus::BadParameterException,
                "JanzCanIo::addDevice() - Device already exists for busId. ");
    } else {
        // Add the device to the device list.
        devices_[busId] = dev;
    }
}

// -----------------------------------------------------------------------------
void carma::canbus::JanzCanIo::canFastSend(
        busIdType busId,
        const carma::canbus::JanzMessage &jmsg,
        txPriorityType prio)
{
    int result = 0;
    int attempts = 0;
    deviceType *dev;
    map<busIdType,deviceType*>::iterator devIter;

    // Retrieve the device controlling this bus
    devIter = devices_.find(busId);
    if (devIter != devices_.end()) {
        dev = devIter->second;
    } else {
        ostringstream oss;
        oss << "JanzCanIo::canFastSend() - Bus " << busId << " not found.";
        throw CARMA_EXCEPTION(carma::canbus::BadParameterException, oss.str() );
    }

    // Retrieve the raw message.
    carma::canbus::FastMessage cfmsg = jmsg.getRawMessage();

    ::FastMessage fmsg = *(reinterpret_cast< ::FastMessage * >( &cfmsg ) );

    // Lock the mutex in order to synchronize write access to the
    // device and to synchronize which thread will wait when the buffer
    // is full (we want any waiting threads to only wait one
    // CAN_FLUSH_INTERVAL - without the mutex, several calling threads
    // may separately wait CAN_FLUSH_INTERVAL inefficiently).
    Guard scopedlock( dev->mutex );

    while (attempts <= MAX_FLUSH_RETRYS) {
        // If we aren't emulating write using can_fast_send
        // otherwise write using write (to /dev/null).
        if (!emulate_) {
            result = can_fast_send_prio(dev->fd, prio, &fmsg);
        } else {
            result = write(dev->fd, &fmsg, sizeof(::FastMessage));
        }

        switch (result) {
            case -1 :
                throw CARMA_EXCEPTION(carma::canbus::JanzFailException,
                        "JanzCanIo::canFastSend - can_fast_send failed! "
                        + (string)strerror(errno));
                break;
            case 0 :
                // Most likely the buffer is full.
                // Block calling process and then try resending.
                if (attempts != MAX_FLUSH_RETRYS) {
                    nanosleep(&CAN_FLUSH_INTERVAL, NULL);
                    attempts++;
                } else {
                    throw CARMA_EXCEPTION(carma::canbus::TxBufferFullException,
                            "JanzCanIo::canFastSend - Tx buffer is full! "
                            "Retried several times.  Message dropped. "
                            "Check cables and make sure a node is"
                            " connected to the bus.");
                }
                break;
            default :
                // Break the loop.
                attempts = MAX_FLUSH_RETRYS + 1;
                break;
        }

    }
    // Increment the tx count unlock the mutex and exit.
    devIter->second->bus.txCount++;
    devIter->second->bus.oneMinTxCount++;
}


