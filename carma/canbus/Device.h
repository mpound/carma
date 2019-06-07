/** @file
 * Declaration of carma::canbus::Device class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.51 $
 * $Date: 2011/01/27 20:59:57 $
 * $Id: Device.h,v 1.51 2011/01/27 20:59:57 abeard Exp $
 */

#ifndef CARMA_CANBUS_DEVICE_H
#define CARMA_CANBUS_DEVICE_H

// C++ STL includes
#include <vector>
#include <map>
#include <string>

// Carma includes
#include "carma/canbus/CanOutput.h"
#include "carma/canbus/Message.h"
#include "carma/canbus/Types.h"

namespace carma {
namespace canbus {

    /**
     * Alias for an ::std::map containing a string description of a message id,
     * keyed by the described ::carma::canbus::msgType message id.
     */
    typedef ::std::map< ::carma::canbus::msgType, ::std::string > MsgIdInfoMap;

	/**
	 * Abstract base class for all CAN Bus devices. 
	 * Device provides an abstract base class for CANbus devices 
     * implementing the <A HREF="http://www.mmarray.org/system/CanAPI/Docs/CANbusMessageIDprotocol.pdf"> Carma CANbus Message ID Protocol.</A>  
     * It is intended 
     * that all API CAN device implementations will derive directly from this 
     * class and define all API specific details such as control interfaces,
     * how to pack/unpack CAN messages, message ids, and where to place 
     * monitor data.  These details are described in the Carma CANbus API 
     * documents available <A HREF="http://www.mmarray.org/project/system/CanAPI/index.html">here</A>. Implementation is mostly accomplished 
     * by overloading the 
     * Device::processMsg routine and defining additional control methods
     * that may or may not be exposed to higher level processes through
     * CORBA or some other means.  By using Device as a base class for all
     * CANbus devices, it is possible for the carma::canbus::Master class 
     * to easily manage multiple types of devices with different API ids 
     * while only retaining a list of pointers to this abstract base class.
     * As a result, the carma::canbus::Master class handles nearly all of 
     * the details and bookeeping regarding retrieval and processing of 
     * CAN messages.
     *
     * Device State. It is essential that anybody deriving from this base
     * understands how the state of a Device is set and used.  There are four
     * possible states that a Device may have which are defined by the
     * carma::canbus::deviceStateEnum enumeration. This state can be set and 
     * retrieved with the Device::getState and Device::setState methods. All 
     * devices are initialized to OFFLINE unless explicitly reinitialized in 
     * the derived class.  The carma::canbus::Master class 
     * will automatically maintain the state of any CAN devices registered with
     * it.  It will set any ONLINE or STARTING Device to OFFLINE if no CAN 
     * messages are received from it in a designated time frame of about 1 
     * second for ONLINE devices and 20 seconds for STARTING devices. Likewise,
     * Master will set modules ONLINE when receiving packets from it - first
     * transitioning through STARTING if the modules sends out a slow monitor
     * packet for identification purposes.
     *  
     * Memory allocation.  Any implementor of the device class will need to 
     * be cognizant of how memory should be created and destroyed for their 
     * device.  In particular any Device implementation that is also 
     * inheriting from a CORBA control interface cannot simply call delete.  
     * They should instead call CORBA routines to decrement reference counts 
     * and destroy memory.  Please see Henning & Vinoski, Ch 11.7 pg 493 for 
     * detailed information on deleting CORBA servant objects.
     *
     * Node 0 Devices. As described in the 
     * <A HREF="http://www.mmarray.org/system/CanAPI/Docs/CANbusMessageIDprotocol.pdf"> Carma CANbus Message ID Protocol.</A>
     * a CAN address with a node location code of 0 has a special meaning. 
     * Addresses containing this node id will be processed by all CAN Devices
     * with the same API Id.  This enables one to issue a single control command
     * to all CAN modules of the same type at once.  Likewise, a Device class
     * derivative with a node id of 0 refers to all CAN modules of that API.
     * Users may declare a node 0 Device if more than one identical CAN Device 
     * exists which needs to be controlled homogenously.  This node will not
     * receive monitor packets and should exist purely for control purposes.
     * This can be asserted programatically by declaring the node 0 device
     * constant and I urge users to do so when possible.  It may not be
     * possible to declare node 0 devices constant if your device class
     * inherits from an IDL interface but if using tie classes or composition
     * there should be no barriers to doing so.
	 */    
    class Device {
    public:

        /**
         * Device constructor.  
         * Constructs a device with the given api and node.
         * @param api unique code identifying a specific set of 
         * instructions and CAN message formats.  
         * @param node unique location id on a CAN bus. 
         * @param io Reference to CanOutput class - should be obtained
         * from downcasting the canbus::Master reference in inherited
         * canbus::Master class.
         */ 	
        Device(apiType api, nodeType node, CanOutput &io);

        /**
         * Device destructor.
         * Device class destructor is declared virtual here because
         * in general the Master class will maintain only a container
         * of Device class pointers.  When this container or any
         * of the objects in the container are destroyed, we must assure
         * that the associated object destructors get called as well 
         * (i.e. not just the Device destructor but the destructor for
         * it's derivatives as well).  
         */
        virtual ~Device();

        /**
         * Return api code of device. 
         * @return 8 bit api code.
         */ 
        apiType getApi() const;

        /** 
         * Return node location code of device.
         * @return 9 bit node id.
         */ 		
        nodeType getNode() const;

        /**
         * Create a key unique to this api and node.
         */
        keyType getKey( ) const;

        static keyType createKey( apiType api, nodeType node );

        /** 
         * Return board type id of device.
         * @return 8 bit board type id.
         */  
        boardType getBoardType() const;

        /**
         * Return serial number of device.
         * @return 9 bit serial number id.
         */ 
        serialNumberType getSerialNumber() const;

        /**
         * Return bus Id that node resides on. 
         * @return bus Id of device. 
         */
        busIdType getBusId() const;

        /**
         * Status access routine. 
         * @return status of device. 
         */ 
        deviceStateType getState() const;

        /**
         * Get Last RX Time for device.  
         * The last rx time is the mjd time of the last CAN message
         * received for this device.  Note that it does not apply to 
         * simulated CAN messages.
         * @return MJD of last message received. 
         */ 
        double getLastRxTime() const;

        /**
         * Get number of late packets.
         * Late packets are packets which arive outside of a designated 
         * time window around the frame-time (half second) clock tick.  
         * CAN modules should be closely synchronized to the CAN host 
         * system time and broadcast their monitor packets on these half-second
         * boundaries.  It is the responsibility of the developer
         * implementing a particular device class to update the number
         * of late packets using the protected Device::nLatePackets_ 
         * variable.  In general, this value is a good indicator of the system
         * and/or module timing health.
         * @return number of late packets for this device
         * @see Device::isPacketLate
         */
        unsigned int getNlatePackets() const;

        /**
         * Get API Version of this Device.
         * The API Version indicates exactly which version of the CARMA
         * CANbus API document was used to create this Device.  The 
         * corresponding CAN node will likewise broadcast the API version
         * it was created from to the host machine.  These versions should
         * match!!!
         * @see Device::setApiVersion
         * @see CorbaXacDevice::processSystemMonitorPacket4
         */
        char getApiVersion() const;

        /**
         * Return a map of devices controls.
         * This method returns a map of control messages that can be 
         * sent to the device from this class.  It must be defined
         * by the implementor.
         * @return MsgIdInfoMap containing a string description of the CAN 
         * message keyed by it's message id.
         */
        virtual MsgIdInfoMap getControls() const;

        /**
         * Return a map of devices half second monitor points.
         * This routine returns a map of the half second CAN messages
         * that are processed by this Device class.  These CAN messages 
         * will contain the half second monitor points as described in the
         * CANbus API for the Device. This function is necessary for 
         * properly simulating a device when the device is OFFLINE, 
         * since master first calls this method to determine which
         * messages to simulate on a half second timescale. 
         * @return MsgIdInfoMap containing a string description of the CAN 
         * message keyed by it's message id.
         * @see Device::simulateMsg
         * @see carma::canbus::Master
         */ 
        virtual MsgIdInfoMap getHalfSecMonitors() const = 0;

        /**
         * Return a map of devices slow monitor points.
         * This routine returns a map of slow CAN messages (5 second)
         * that are processed by this Device class. These messages will
         * contain the 'slow' monitor points as described in the CANbus 
         * API for the device.  It must be defined by
         * the implementor of this base class and is needed to properly
         * simulate a device when OFFLINE.
         * @return MsgIdInfoMap containing a string description of the CAN 
         * message keyed by it's message id.
         * @see Device::simulateMsg
         * @see carma::canbus::Master
         */
        virtual MsgIdInfoMap getSlowMonitors() const = 0;

        /** 
         * Set state of device. 
         * The implementor should define the logic that sets the state to
         * ONLINE.  This is generally determined by checking the Devices 
         * lastRxTime variable via Device::getLastRxTime. canbus::Master is 
         * responsible for setting a devices state to OFFLINE when messages
         * haven't been received for that device in a certain amount of time. 
         * @param state device state.
         */ 
        virtual void setState(deviceStateType state);

        /** 
         * Set busId of device. 
         * This routine is called internally by canbus::Master whenever 
         * it receives a CAN message addressed to the Device. The busId is
         * generally construted from the Janz CAN device name to assure
         * uniqueness. The user in general should not have to set a Devices
         * busId explicitly.
         * @param busId of device. 
         */
        void setBusId(busIdType busId);

        /**
         * Set serial number of device.
         */
        void setSerialNumber(serialNumberType sn);

        /**
         * Set board type of device.
         */
        void setBoardType(boardType bt);

        /**
         * Set latest rx time. 
         * This method should be called to set the MJD of the last time
         * a message was received for this device.  In general it is called
         * every time a message is received addressed to a particular device.
         * There are however caveats related to simulating OFFLINE nodes.
         * @param rxMjd Mean Julian Day of message rx.
         * @see Device::simulateMsg
         */  
        virtual void setLastRxTime(double rxMjd);
        
        /**
         * Process a CAN message. 
         * This routine is responsible for defining how to process CAN message 
         * data based on the type of message received (msgType).
         * It should encapsulate either directly or indirectly all 
         * details related to unpacking CAN messages, processing the data
         * and placing it into the monitor system. The sim flag indicates
         * that the message came from a simulated source.   
         * @param messageId the 10bit canbus::msgType indicating message 
         * function.
         * @param data reference to a byte vector containing raw 
         * CAN data.
         * @param sim Flag indicating if the message is a simulated one - this
         * can be used to set Device state to ONLINE.
         * @see Device::setState
         * @see Device::simulateMsg
         */
        virtual void processMsg(msgType messageId, 
                                std::vector<byteType>& data,
                                bool sim) = 0;

        /**
         * Simulate a CAN message.
         * This routine is intended to simulate CAN messages when the Device
         * is in the OFFLINE state.  An implementer of this class
         * is required to overload this method and return the appropriate 
         * carma::canbus::Message for a given msgType (message id).  The 
         * Message is automatically placed into the shared CAN Message queue
         * and processed by Master exactly as if it came from the CANbus.
         * This enables a user to completely emulate the CANbus device and
         * Janz hardware as well, while exercising the majority of code 
         * involved in processing a CAN message.
         * Note that the msgTypes which simulateMsg is called for by 
         * canbus::Master are retrieved via the above Device::getHalfSecMonitors
         * and Device::getSlowMonitors routines, thus simulateMsg must properly
         * handle all msgTypes returned from these routines.
         * @param messageId 10bit id indicating message function.
         * @see carma::canbus::Message::setRxMjd
         * @see carma::canbus::Device::getSlowMonitors
         * @see carma::canbus::Device::getHalfSecMonitors
         * @see carma::canbus::Device::setLastRxTime
         * @see carma::canbus::Device::setState
         * @see carma::canbus::Device::createMsgToHost
         * @see carma::canbus::Device::createMsgToNode
         */
        virtual carma::canbus::Message simulateMsg(msgType messageId) = 0;

        /**
         * Create a CAN message addressed from this Device to the host.
         * Returns a message with the api and node of this device object,
         * the input msg id, the host bit addressed to the host and the current
         * bus id of the node.  This is most convenient for creating simulated 
         * messages in the overridden simulateMsg routine.  Note that the 
         * returned message contains no data.
         * @param messageId Message id or command id to place in address.
         */
        carma::canbus::Message createMsgToHost(msgType messageId) const;

        /**
         * Create a CAN message addressed to this device from the host.
         * Returns a message with the api and node of this device object,
         * the input msg id, the host bit addressed to the node and the current
         * bus id of the node.  This is most convenient for creating control 
         * messages.
         * @param messageId Message id or command id to place in address.
         */
        carma::canbus::Message createMsgToNode(msgType messageId) const;

        /**
         * Create a CAN message addressed to all nodes of this type.
         * Returns a message with the api of this device object and a node
         * of 0 indicating all nodes with this api.  The host bit is addressed
         * to the nodes and the BUSID is set to carma::canbus::ALL_BUSSES. 
         * @param messageId Message id or command id to place in CAN address.
         */
        carma::canbus::Message createMsgToAllNodes(msgType messageId) const;
        
        /**
         * Create a dummy CAN message.
         * Dummy messages are required because the XAC module is not fast
         * enough to pull messages off the bus at bus speed. Message
         * will be addressed to the bus that the module currently resides on.
         * If it needs to go to a different bus or all busses, change 
         * accordingly on the returned message prior to posting.
         */
        carma::canbus::Message createDummyMsg( ) const;

        // Standard Control messages.
        /**
         * Perform a software reset of module.
         * Software resets differ from hardware resets in that they are
         * triggered via a CAN message rather than writing the buswide dio line
         * high (canbus::CanDio::reset). Thus they can be directed to 
         * individual devices.
         * @throws carma::canbus::TxBufferFullException via CanIo::postMessage.
         * @see carma::canbus::CanIo::postMessage.
         */
        void reset();

        /**
         * Stop fast sampling on channel 1
         * This routine will stop fast sampling on channel 1 regardless of 
         * whether this node is fast sampling the channel or not!  In other
         * words the request goes out globally.
         */
        void stopChannelOneFastSampling();

        /**
         * Stop fast sampling on channel 2 
         * This routine will stop fast sampling on channel 2 regardless of 
         * whether this node is fast sampling the channel or not!  In other
         * words the request goes out globally.
         */
        void stopChannelTwoFastSampling();

        /**
         * Start fast sampling the specified data item on channel 1.
         * This routine starts fast sampling as described in the CARMA 
         * CANbus API template.  It first issues a request to all nodes
         * to stop fast sampling on channel one to reasonably assure that only 
         * one module is using the channel.  
         * @param fastItem Id of data item to fast sample.  This id is API
         * specific.
         * @throws carma::canbus::TxBufferFullException via CanIo::postMessage.
         * @see carma::canbus::CanIo::postMessage.
         */
        void startChannelOneFastSampling(unsigned short fastItem);
        
        /**
         * Start fast sampling the specified data item on channel 2.
         * This routine starts fast sampling as described in the CARMA 
         * CANbus API template.  It first issues a request to all nodes
         * to stop fast sampling on channel two to reasonably assure that only 
         * one module is using the channel.  
         * @param fastItem Id of data item to fast sample.  This id is API
         * specific.
         * @throws carma::canbus::TxBufferFullException via CanIo::postMessage.
         * @see carma::canbus::CanIo::postMessage.
         */
        void startChannelTwoFastSampling(unsigned short fastItem);

        /**
         * Update device or monitor data on frame timescale...
         * This routine is called every half second for every device 
         * registered with a carma::canbus::Master derivative.  The default
         * implementation does nothing but a user should overload it to 
         * guarantee that certain monitor points are updated every half
         * second regardless of CAN activity.  A good example of this is a 
         * monitor point representing Device state (ONLINE, OFFLINE, etc).  
         * We don't want a monitor point representing this state to be 
         * persistent because we want the monitor system to invalidate it if 
         * the application quits or data is otherwise not moving through the 
         * system.  Thus we need to explicitly set the monitor point every half
         * second.  There are probably many monitor points where the same 
         * mechanism is needed.  See the Carma Monitor System for more info or
         * carma::monitor.
         */
        virtual void updateFrameData();

    protected:
        
        // This is shared data but it never changes. Let inherited classes 
        // use directly if they want.  
        const apiType api_;  
        const nodeType node_;
        const keyType key_;

        // Standard controls.
        // These controls are common message id's shared by all CANbus 
        // Devices.  Note that not all devices will implement fast
        // sampling.  Also note that RESET refers to a software reset
        // message, not a 'hardware reset' which is performed by the 
        // canbus::Dio class.
        
        /** 
         * @var RESET
         * Software reset message id.
         */

        /**
         * @var STOP_CHANNEL_1_FAST_SAMPLING
         * Stop channel 1 fast sampling message id.
         */

        /**
         * @var STOP_CHANNEL_1_FAST_SAMPLING
         * Stop channel 1 fast sampling message id.
         */
         
        /**
         * @var START_CHANNEL_1_FAST_SAMPLING
         * Begin channel 1 fast sampling message id.
         */
         
        /**
         * @var START_CHANNEL_2_FAST_SAMPLING
         * Begin channel 2 fast sampling message id.
         */
        static const msgType RESET                         = 0x0000;
        static const msgType STOP_CHANNEL_1_FAST_SAMPLING  = 0x0002; 
        static const msgType STOP_CHANNEL_2_FAST_SAMPLING  = 0x0003;
        static const msgType START_CHANNEL_1_FAST_SAMPLING = 0x0004; 
        static const msgType START_CHANNEL_2_FAST_SAMPLING = 0x0005; 
        
        // Member objects
        /**
         * Reference to CanOutput object. 
         * This reference is for use by Device derivatives to 
         * post messages to the CAN bus.  Note that only output
         * access is allowed since the carma::canbus::Master class
         * is responsible for handling input from the CANbus.
         */
        CanOutput& io_; 

        // Helper functions for derived classes
        /**
         * Increment the late packet count by 1
         */
        void incrementLatePacketCount();

        /**
         * Reset the late packet count to 0.
         * A developer may want to do this when a device goes OFFLINE and
         * perhaps a new device is plugged in or a new firmware version is
         * installed.  In general it is probably easiest to just reset it
         * when a device requests initialization.
         */
        void resetLatePacketCount();

        /**
         * Determine if the last packet sent from this device was late.
         * This method should be used within an implemented processMsg 
         * method to determine if the message being processed is 'late'.
         * Late packets are packets which arive outside of a designated 
         * time frame (default to 100ms, 10ms buffer window before half second
         * tick and 100ms after).  This should only be used to check monitor
         * and slow monitor packets after the device is determined to be 
         * ONLINE.  
         * @param window Valid time period after half second tick in ms.
         * @return true if packet is late, false otherwise.
         * @see carma::canbus::Device::incrementLatePacketCount
         * @see carma::canbus::Device::resetLatePacketCount
         * @see carma::canbus::Device::getNlatePackets
         */
        bool isPacketLate(double window = 100.0);

        /**
         * Set API Version.
         * The API Version should be obtained from the CANbus API document
         * used to create a particular device class.  
         * @param ver API Version
         * @see Device::getApiVersion
         */
        void setApiVersion(char api);

    private:

        // Structure to hold shared data and associated mutex. 
        struct {
            carma::canbus::boardType boardType;      
            serialNumberType serialNumber;
            busIdType busId; 
            deviceStateType state;
            unsigned int nLatePackets;
            double lastRxMjd;
            char apiVer;
            mutable pthread_mutex_t mutex; // Mutable to allow const func access
        } sharedData_;
        
        // Prevent assignment and copy construction
        Device(const Device &);
        Device &operator=(const Device &);

    };
  } // End namespace canbus
} // namespace carma
#endif // CARMA_CANBUS_DEVICE_H
