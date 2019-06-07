/** @file
 * Declaration of carma::canbus::JanzCanIo class.
 * 
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.2 $
 * $Date: 2012/08/04 00:00:45 $
 * $Id: JanzCanIo.h,v 1.2 2012/08/04 00:00:45 abeard Exp $
 */

#ifndef CARMA_CANBUS_JANZCANIO_H
#define CARMA_CANBUS_JANZCANIO_H

#include "carma/canbus/CanIo.h"
#include "carma/canbus/Message.h"

#include <pthread.h>
#include <queue>
#include <string>

namespace carma {

namespace util {
    template <class ElementType> class IPQreader;
    template <class ElementType> class IPQwriter;
}

namespace canbus {

    class JanzMessage;

    /**
     *   JanzCanIo class.
     *
     *   Performs all input and output with Janz CAN cards using Janz character 
     *   driver.  This driver is obsolete with the introduction of the Janz
     *   socketcan framework driver which is implemented in canbus::SocketCanIo.
     *   
     *   The JanzCanIo class is meant for use with Janz VMOD-ICAN single port
     *   can modules.  However, it can be used as well with CAN-PCI2 
     *   (Single device - dual port) cards although only Port 0 on the card 
     *   will be accessible.  
     *   
     *   JanzCanIo can be used in several ways.  
     *   
     *   JanzCanIo can be used to control a single or multiple VMOD-ICAN 
     *   devices with a single JanzCanIo object.  This is reflected in the 
     *   below single and double device constructors.  In these cases, 
     *   all CAN Messages are read from their respective devices, assigned
     *   a BusId (see below) and placed into a shared message queue for
     *   retrieval using the JanzCanIo::getMessage routine.  
     *   
     *   JanzCanIo may also be used with the below described default constructor 
     *   for emulation.  In this case, JanzCanIo will open /dev/null instead 
     *   of a specified CAN device.  All writes will be posted to /dev/null 
     *   and reads will in general block forever.  The single caveat to this 
     *   is that a simulated Message object may be placed into the shared 
     *   message queue using the JanzCanIo::queueMessage method.  In this case, 
     *   reads (i.e. getMessage) will return with the simulated message 
     *   as if it had come directly from the CANbus. 
     *
     *   BusId.  It is important that any user of carma::canbus classes
     *   understand the concept of a BusId.  The BusId is a unique Id 
     *   assigned to each CANbus being controlled by JanzCanIo classes.  The
     *   uniqueness of the Id is assured not only within a process but also
     *   within the context of any process on the linux host machine.  This
     *   is accomplished by deriving the busId from the device filename.
     *   The most important thing to remember regarding BusIds is that any
     *   program logic that uses JanzCanIo to postMessages to a CANbus must set
     *   the busId correctly on the carma::canbus::Message object being 
     *   posted.  This is most easily done by setting the busId of a message
     *   to be posted to the same busId of a message already received from 
     *   the device or node one wishes to talk to 
     *   (e.g. txMsg.setBusId(rxMsg.getBusId)).  If this isn't possible 
     *   then the carma::canbus::extractBusId routine can be used to 
     *   determine the busId from the filename or, the constant busId
     *   carma::canbus::ALL_BUSSES can be used to send the message to all 
     *   busses. See carma::canbus::Message for more information.
     */
    class JanzCanIo : public CanIo {
        public:

            // Constructors...

            /**
             * Default constructor - for emulation only.
             * This constructor is intended for use when no Janz 
             * hardware exists and the user wishes to emulate JanzCanIo.  This
             * is accomplished by opening /dev/null.  Reads will block 
             * until a JanzCanIo derivative queues messages explicitly whereas
             * writes will write to /dev/null.
             * @see queueMessage 
             * @throws carma::util::ErrorException for a variety of situations.
             * Several different exceptions can be thrown during construction.
             * All exceptions are either util::ErrorException or canbus
             * derivatives thereof.  Catching util::ErrorException by reference
             * and calling util::ErrorException.what() on it will reveal the 
             * full details.
             */
            JanzCanIo ();

            /**
             * Constructor for JanzCanIo object with single device.  
             * This constructor initializes a JanzCanIo object for use with 
             * a single VMOD-ICAN device.
             * @param deviceName: The name of the CAN device 
             * (e.g. /dev/dpm_00).
             * @param terminate Flag to control setting of onboard 
             * bus termination resistor.  Set true if card is connected 
             * to the beginning or end of a CANbus (the last or first node). 
             * @throws carma::util::ErrorException for a variety of situations.
             * Several different exceptions can be thrown during construction.
             * All exceptions are either util::ErrorException or canbus
             * derivatives thereof.  Catching util::ErrorException by reference
             * and calling util::ErrorException.what() on it will reveal the 
             * full details.
             */ 		
            JanzCanIo (const char * deviceName, bool terminate);

            /**
             * Constructor for JanzCanIo object controlling dual devices.
             * This constructor intializes a JanzCanIo object for use with 
             * two VMOD-ICAN devices.
             * @param dev0  The name of the first CAN device to 
             * open (e.g. /dev/dpm_00)
             * @param term0 Flag to control setting of onboard bus termination
             * resistor.  Set true if card is connected to the beginning or end
             * of a CAN bus.
             * @param dev1 The name of the second CAN device (e.g./dev/dpm_01)
             * @param term1 Termination flag (see term0 above).
             * @throws carma::util::ErrorException for a variety of situations.
             * Several different exceptions can be thrown during construction.
             * All exceptions are either util::ErrorException or canbus
             * derivatives thereof.  Catching util::ErrorException by reference
             * and calling util::ErrorException.what() on it will reveal the 
             * full details.
             */
            JanzCanIo (const char* dev0, bool term0, const char* dev1, bool term1);
            
            typedef 
            ::std::vector< ::std::pair< ::std::string, bool > > NameTermPairVec;

            /**
             * Constructor for JanzCanIo object controlling an arbitrary number of
             * CAN devices.
             * @param Vector of pairs containing dev name and bus termination.
             */
            JanzCanIo ( const NameTermPairVec & nameTermPairs );

            /**
             *  Destructor for JanzCanIo object.
             */  
            virtual ~JanzCanIo ();

            // Input methods.

            /** 
             * Retrieve a CAN message.
             * This routine retrieves a message from the shared CAN message
             * queue.  When a message is received, it is retrieved from it's
             * respective device and placed into a shared canbus::Message
             * queue.  This routine blocks until a message is available.  
             * If the JanzCanIo object is using the default constructor for
             * emulation, this method blocks forever, or until a message
             * is explicitly placed into the queue using 
             * JanzCanIo::queueMessage.
             * @return Message object containing CAN message.
             * @throws carma::canbus::PthreadFailException on condition 
             * timeout error. Unrecoverable.
             */ 
            carma::canbus::Message getMessage();

            /**
             * Post a CAN message.
             * This routine sends a carma::canbus::Message to a single 
             * or multiple CANbusses.  The bus which the message will 
             * be posted to must be specified in the Message object using
             * the Message::setBusId method.  If a user wishes to send 
             * the message to all busses controlled by JanzCanIo, use the
             * carma::canbus::ALL_BUSSES constant. If no modules are on 
             * a bus to receive a message, messages will remain in onboard 
             * buffers until a module is connected to accept them.  If this
             * onboard buffer fills up, postMessage will throw
             * an exception. 
             * @param msg carma::canbus::Message object.
             * @param prio carma::canbus::txPriorityType of message.
             * @throws carma::canbus::TxBufferFullException if unable to
             * post message due to a full transmit buffer.  This is likely
             * due to a disconnected bus.  Message is dropped.
             * @throws carma::util::ErrorException for a variety of other 
             * unrecoverable problems.  This error generally signals a 
             * programming error, or a misconfigured system.
             */
            void postMessage(
                const carma::canbus::Message &msg,
                carma::canbus::txPriorityType prio = carma::canbus::NORMAL); 

            /**
             * Echo all messages posted to the canbus back through the 
             * read interface.
             * Typically, the ability to echo a posted message is set on the
             * carma::canbus::Message object.  However, often during debugging
             * it is useful to echo every written message back through the
             * read interface.  This enables sent messages to be verified via
             * canOverIp and DirectCan. This method enables echoing on all
             * messages posted to the CANbus.
             */
            void echoAll(bool enable);

            /** 
             * Get bus status for all busses.
             * @return map containing status of all busses keyed by bus Id.
             */
            BusStatusMap getBusStatus() const;

            // Utilities...

            /**
             *  Clear the read message queue.
             *  Clear all messages waiting for retrieval in the internal 
             *  message queue.  This is useful for bus resets and should 
             *  be done when the hardware is in a reset state and hence
             *  known not to be transmitting.
             *  @see carma::canbus::Dio::resetHi
             *  @see carma::canbus::Dio::resetLo
             */
            void clearReadQueue();
            
            /**
             * Place a carma::canbus::Message in the shared msg queue.
             * This method is provided so that a simulator in an inherited 
             * class may place 'CAN' Messages into the queue as if they had 
             * been received from the CANbus.  Subsequently, any blocks
             * on JanzCanIo::getMessage will return with the queued message.
             * @param msg carma::canbus::Message object to queue.
             */
            void queueMessage(const Message &msg);

            /**
             * Set the timestamp echo latency for the specified bus.  
             * The timestamp echo latency is the time elapsed from the 
             * creation of the timestamp to the time it is read back from
             * the read interface (timestamp messages are echoed).
             */
            void setTimestampEchoLatency(int tsLatency, busIdType busId);
        
        protected:

        private:
            

            // Local structure typedefs for busses, devices and queue 
            // information...
            // A bus is a single physical CAN bus and contains information
            // pertinent to the bus and CAN messages.  The device is mutex
            // protected to synchronize access to it.
            
            /**
             * Bus type.
             * Contains information specific to a bus.  
             */
            typedef struct {
                bool terminate;     /**< True if software termination set.*/
                int rxCount;        /**< Running rx count for msg rates. */
                int txCount;        /**< Running tx count for msg rates. */
                int oneMinRxCount;  /**< One Minute rx count for msg rates. */
                int oneMinTxCount;  /**< One Minute tx count for msg rates. */
                busIdType id;       /**< The bus Id.*/
                timespec lastUpdateTime; /**< Time of last msg rate update.*/
                timespec lastOneMinUpdate; /**< Last 1min msg rate update.*/
                carma::canbus::busStatusType status; /**< Bus status. */ 
            } busType;

            /** 
             * Device type.
             * This structure contains all information needed to access
             * a device. In general two threads (Direct CAN and DO classes) 
             * will write CAN packets to a device, hence the mutex.
             */
            typedef struct {
                std::string name;	     /**< Device name (e.g. /dev/dpm_00). */
                int fd;	                 /**< File Descriptor to access it.  */
                bool initialized;        /**< New and fast interfaces inited.*/
                pthread_mutex_t mutex;   /**< Synchronize access to a device.*/
                pthread_t threadId;      /**< Maintain for proper destruction.*/
                void * arg;              /**< An argument for thread entry.  */
                busType bus;             /**< Information pertinent to bus.  */
            } deviceType;

            /**
             *  Shared message queue type. 
             *  Contains mutex, condition variable and queue
             *  of messages to access.
             */
            typedef struct queueAccessStruct {
                pthread_mutex_t mutex;
                pthread_cond_t cond;
                std::queue<carma::canbus::Message> messageQueue;
            } queueAccessType;

            // Private member data

            pthread_t updateThreadId_;         // Thread Id for update thread
            pthread_t writeThreadId_;          // Thread Id for write thread
            queueAccessType rxQueueAccessor_;  // Received CAN message queue 
            // Direct CAN IPQs.
            carma::util::IPQwriter<carma::canbus::Message> * writeIpq_; 
            carma::util::IPQreader<carma::canbus::Message> * readIpq_;  
            
            typedef std::map<busIdType, deviceType*> DeviceMap;
            
            /** Map of CAN card devices keyed by busId. */  
            DeviceMap devices_;
            const bool emulate_;               /**< Emulate Janz hardware? */
            bool echo_; // Echo messages posted to canbus?
            
            // CanOutput routines
            
            /**
             * Add a device with a specified bus Id.
             * This routine creates a device, initializes the 
             * device structure and adds it to the device map
             * using the busId as a key.  It does not open or start
             * the device. 
             * @param busId of device
             * @throws carma::canbus::BadParameterException if device already 
             * exists (bus id has previously been used).
             */
            void addDevice(busIdType busId);

            /**
             * canFastSend wrapper method.
             * This method is provided as a wrapper to the janz c-lib
             * can_fast_send function.  It is used to switch between
             * using can_fast_send and write in emulation mode. It 
             * is also declared virtual so that an inheriting class
             * may define how to send a message differently most notably
             * if this class is modified for use with CAN-PCI2 cards which
             * use the same drivers to control a single device dual port
             * CAN card.
             * @param busId Id of bus to send message too.
             * @param msg carma::canbus::Message object to send.
             * @throws carma::canbus::TxBufferFullException if janz onboard
             * Tx buffer is full (likely due to a disconnected bus).
             * @throws carma::canbus::BadParameterException if invalid busId.
             * @throws carma::canbus::JanzFailException if unable to send 
             * through janz interface.
             * @throws carma::canbus::TxBufferFullException if unable to send
             * because janz onboard messages are full.
             */
            virtual void canFastSend(busIdType busId, const JanzMessage &jmsg,
                txPriorityType prio);
            
            // Initialization and startup routines...

            /**
             * Initialize vmod-ican devices.
             * This must be called before the bus will start receiving 
             * messages.  It will effectively reset the VMOD-ICAN 
             * module(s), set up all interfaces (new and fast), and clear 
             * the message queue.
             */
            void initialize();

            /**
             * Initialize Direct CAN ipqs.
             * This method is essentially just a helper method to 
             * isolate the common task of creating and initializing
             * the Direct CAN ipqs among multiple constructors.
             */
            void initializeIpqs();

            /**
             * Create read/write threads as well as update thread.
             */
            void createThreads();

            // Utility routines...

            // The following two methods are protected to avoid cluttering the
            // interface.  They are implemented in the getMessage variants.

            /** 
             * Poll for arriving messages.  
             * If no argument is specified the routine will wait forever.
             * @param maxWaitInSeconds timeout. 
             */	
            bool waitForMessage(double maxWaitInSeconds = -1.0);

            /**
             * Cleanup handler for waitForMessage
             */
            static void waitCleanupHandler(void *arg);

            /**
             * Process a message from the 'fast' interface.
             * This routine converts a raw CAN message into a 
             * Message object and places it in the read queue.
             * @param dev reference to device from which message 
             * was obtained. 
             */ 
            void processFastMessage(deviceType &dev);

            /**
             * Process message from the 'new' interface.  
             * In general these are not CAN messages but rather 
             * consist of error and status messages from a CAN device.
             * @param dev device from which message originated.
             */
            void processPlainMessage(deviceType &dev);

            /**
             * JanzCanIo copy constructor.
             * Declared protected so that it is undefined and inaccessible
             * preventing multiple instances of JanzCanIo through copying.
             */
            JanzCanIo (const JanzCanIo &);

            /**
             * Operator =.
             * Also undefined and inaccessible for same reasons as copy
             * constructor.
             */
            JanzCanIo &operator=(const JanzCanIo &);

            // Utilities...

            /**
             * Update bus status.
             * Janz CAN modules must be probed explicitly for their
             * bus status.  This routine probes a bus for it's status
             * and is called at 1/2 second intervals by the update 
             * thread.  
             */ 
            void updateBusStatus();

            // Thread based utilities...

            /**
             * Entry point for read thread.
             * Thread entry points are needed to allow C-style calls
             * to pthreads and are thus static to provide C linkage.  
             * They are placed here (directly in the class) as opposed to
             * the global namespace (using an extern "C" linkage specifier) so
             * that we can maintain the visibility rules provided by C++.  
             */
            static void *readThreadEntry(void *arg);

            /**
             * Thread routine to read incoming CAN messages from controller and 
             * subsequently place them on a shared queue for retrieval and 
             * processing.  One thread instance will exists for each device
             * being controlled.
             * @param dev Device from which to read.
             */
            void runReadThread(deviceType &dev);

            /**
             * Entry point for write thread.
             * @param arg Naked input parameter of this*.
             */
            static void *writeThreadEntry(void *arg);

            /**
             * Thread routine to write messages from the input IPQ 
             * to the CAN bus.
             * This thread services Direct CAN messages by continuously
             * reading the input CAN ipq and posting properly addressed
             * messages to the CAN bus.
             */
            void runWriteThread();

            /**
             * Entry point for update thread.
             */	
            static void *updateThreadEntry(void *arg);

            /**
             * Thread routine to update bus status every 1/2 second.
             */
            void runUpdateThread();

    };
  } // namespace canbus
} // namespace carma
#endif // CARMA_CANBUS_JANZCANIO_H 
