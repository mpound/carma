/** @file
 * Declaration of carma::canbus::Master class.
 * 
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.44 $
 * $Date: 2010/05/28 20:52:57 $
 * $Id: Master.h,v 1.44 2010/05/28 20:52:57 eml Exp $
 */ 

#ifndef CARMA_CANBUS_MASTER_H 
#define CARMA_CANBUS_MASTER_H

// System includes
#include <pthread.h>

// C++ Standard Library includes
#include <map>
#include <string>

// Carma includes
#include "carma/canbus/CanDio.h"
#include "carma/util/PthreadMutex.h"

namespace carma {
  namespace canbus {

      // Forward dec
      class Device;

      /** 
       *  Carma Canbus Master class.
       *  Master provides the core of a framework responsible for maintaining 
       *  monitoring and controlling devices on a CANbus network. Its main 
       *  purpose is to retrieve messages from the CANbus using the base 
       *  CanDio class and then dispatch them to the appropriate 
       *  carma::canbus::Device class derivative for processing. In addition, 
       *  it also provides core
       *  control functionality such as sending time syncs to devices and
       *  hardware resets through the CanDio base.  Finally, it also contains
       *  a built in simulator to simulate monitor packets and ship them 
       *  through the monitor system pipeline.  It is intimately connected
       *  with the carma::canbus::Device class and both classes public and
       *  protected interfaces should be well understood before trying to 
       *  implement.
       *  
       *  In practice, a user will overload the Device class for each type 
       *  of device that will reside on the CAN network.  The concrete device
       *  implementation should multiply inherit from both Device
       *  and a CORBA defined control interface.  
       *  Next a user will need to inherit from this Master class to define
       *  the makeup of the CAN network.  This is done by instantiating Device 
       *  derived objects in the constructor and calling the Master::addDevice
       *  method to add them to the Master. This derived Master class
       *  will also generally inherit from a CORBA interface to simultaneously 
       *  define the control device access interface to this network.  This 
       *  interface will typically consist of getDeviceX methods that 
       *  retrieve CORBA references to specific devices.  In 
       *  addition the CORBA interface will need to define control 
       *  commands that refer to the CAN bus(ses)
       *  as a whole such as reset(). In this way, the individual Device
       *  implementations and the Master as a whole become true distributed
       *  objects.  Any application using this framework will be required to set
       *  up the CORBA environment (declaring Orbs, connecting to nameservers,
       *  publishing object references, etc).  For a concrete example see
       *  the detailed client/server test programs in the Test/ClientServerTest
       *  subdirectory.
       *
       *  Memory management: As described above, derivatives of Master are
       *  generally responsible for defining the CAN network and adding 
       *  the Device derivatives using the Master::addDevice method. It is
       *  important that the derivative class also properly destroy these
       *  Device derivatives properly.  Devices must first be removed from
       *  the Master instance by calling Master::removeDevice.  After this
       *  is done, the Devices may be destroyed in an implementation dependent
       *  manner.  This may be by calling delete on heap allocated Devices or 
       *  simply via scoping of the class instance.  In either instance a Master
       *  derived class destructor should call Master::removeDevice for each
       *  Device added to Master.  For heap allocated objects it may be 
       *  necessary to call Master::getDevice, then Master::removeDevice and 
       *  finally delete on the pointer (depending on if the derivative class
       *  also maintains pointers to it's Devices.
       *
       *  Simulation:  Master starts a thread that automatically calls 
       *  Device::simulateMsg on it's OFFLINE Devices.  This allows a user to 
       *  completely simulate and test his software, including DO control calls,
       *  without actually having any Janz CAN hardware on the system.  Master 
       *  calls Device::simulateMsg for fast and slow monitor packet message 
       *  ids returned from the Device::getHalfSecMonitors and 
       *  Device::getSlowMonitors methods.  Note that these messages will 
       *  only be simulated on the frame (1/2 second) timescale and slow
       *  monitor (5 second) timescale.  Messages sent asynchronously or on
       *  other time scales are not supported by the Master class simulator but
       *  may be implemented by the user. 
       *  
       *  The significant advantage of this framework is that all monitor 
       *  messages, timing information and core control functionality are 
       *  handled directly by this Master base class.  The user needs only 
       *  worry about the detailed API implementation for devices and the 
       *  higher level control interfaces. All intermediate details are 
       *  automatically handled via the framework.
       *
       *  This Class, taken in conjunction with its derivatives, the Device 
       *  class and the Device class derivatives, composes a Factory Method
       *  Object-Oriented Design Pattern.  Within this context, the Device
       *  class is a 'Product', and its derivatives are 'Concrete Products'.
       *  The Master class is a 'Creator' and its derivative is a 
       *  'Concrete Creator'.  For more information see "Design Patterns -
       *  Elements of Reusable Object-Oriented Software".
       */
      class Master : public CanDio {
      public:

          /**
           * Default constructor for emulation.
           * This constructor should be used when emulation of the 
           * underlying CAN hardware is desired.  When Master is constructed
           * with this constructor, the base CanIo classes both write to 
           * /dev/null.  Nonetheless, the functionality of all 
           * classes remains the same.  This allows one to completely define
           * their CAN network, publish DOs and completely simulate both 
           * the monitor and control system.
           * @param simOfflineNodes Simulate offline nodes if set true.
           * @throws carma::util::ErrorException upon failure of any derived
           * class constructors.
           * @see Device::simulateMsg
           */
          Master( bool simOfflineNodes = true );

          /**
           *  Master constructor.  
           *  This constructor should be used when the Master class will 
           *  simultaneously control both CAN busses on a Janz/Carma CAN/Dio
           *  cPCI board.  
           * @param boardId Board number of Janz modulbus carrier board 
           * (0 - 0xf). The carrier board is the cPCI board which contains 
           * four separate mezzanine modules (2 - CAN, 1 - DIO and 1 - RJ45 
           * Breakout Board), each on a separate 'modulbus'.  The boardId is 
           * set via a hex switch near the back of the cPCI board.  On older 
           * models, it is determined via a clearly labeled PLD chip near 
           * the back of the board.  The chip will be labeled "mbus X" - do
           * not confuse this with the mobulbus numbers on the front.
           * @param simOfflineNodes Simulate offline nodes if set true.
           * @param reset If true, perform a hard reset upon creation.
           * @param terminate Terminate both CAN busses if true.
           * @throws carma::canbus::BadParameterException if modboard number
           * is invalid.
           * @throws carma::util::ErrorException upon failure of any derived
           * class constructors.
           */	  	 	
          Master(
            int boardId, 
            bool simOfflineNodes = false, 
            bool reset = false,
            bool terminate = true );

          /**
           * Master constructor.  
           * This constructor should be used when the Master class will 
           * control one of the two CAN busses on the Janz/Carma CAN/Dio board.
           * @param boardId Board number of Janz modulbus carrier board 
           * (0 - 0xf). The carrier board is the cPCI board which contains 
           * four separate mezzanine modules (2 - CAN, 1 - DIO and 1 - RJ45 
           * Breakout Board), each on a separate 'modulbus'.  The boardId is 
           * set via a hex switch near the back of the cPCI board.  On older 
           * models, it is determined via a clearly labeled PLD chip near 
           * the back of the board.  The chip will be labeled "mbus X" - do
           * not confuse this with the mobulbus numbers on the front.
           * @param modulbusId The modulbus which contains the CAN
           * card you wish to control.  On the Janz CAN/Dio boards, the 
           * CAN cards typically reside in modulbus 0 and modulbus 1 (CAN
           * cards have d-sub 9 connectors), however the signals are 
           * rerouted out the RJ-45 ports on the board in modulbus 2. Here
           * you still must specify which CAN card the RJ-45 port is connected
           * to (i.e. modulbus 0 or 1 but not 2 where the RJ-45 breakout board
           * resides).  The modulbus numbers are clearly labeled on the front
           * of the Janz cPCI carrier board. Note that  the CAN card in 
           * modulbus 0 is connected to the top blue RJ45 connector and 
           * the CAN card in modulbus 1 is connected to the bottom blue RJ45 
           * connector.  
           * @param simOfflineNodes Simulate offline nodes if set true.
           * @param reset If true, perform a hard reset upon creation.
           * @param terminate Terminate CAN bus if true.
           * @throws carma::canbus::BadParameterException if modulbus number
           * is invalid.
           * @throws carma::util::ErrorException upon failure of any derived
           * class constructors.
           */	  	 	
          Master(
            int boardId, 
            int modulbusId, 
            bool simOfflineNodes = false,
            bool reset = false,
            bool terminate = true );

          /**
           * Constructor for arbitrary number of CanDio ports.
           * @param Vector containing pairs of board, modulbus pairs and 
           *  bus termination state.
           * @param simOfflineNodes Simulate offline nodes if set true.
           * @param reset If true, perform a hard reset upon creation.
           * @throws carma::canbus::BadParameterException if modulbus number
           * is invalid.
           * @throws carma::util::ErrorException upon failure of any derived
           * class constructors.
           */	  	 	
          Master( const ::std::vector< DevTermPair > & devTermPairs,
                  bool simOfflineNodes = false,
                  bool reset = false );

          /** 
           * Master destructor.
           * Declared virtual so that all derivatives destructors are
           * called as well if only a pointer to this base Master is
           * destroyed or deleted.
           */
          virtual ~Master();

          /** 
           * Run the Master.
           * This routine is responsible for kicking off the whole shabang.  
           * It blocks on dependent threads permanently.  It is included to
           * allow a developer a degree of flexibility when designing a 
           * CAN application.  If the user is developing an application with
           * a CORBA control mechanism, it is likely their main application
           * will block on running the orb.  In this case the developer will
           * want to call this Master::run method in a separately spawned 
           * thread.  However, if no CORBA control mechanism is used, main
           * can call this routine which blocks indefinitely. 
           * @throws carma::canbus::PthreadFailException upon failure to 
           * properly startup or initialize pthread dependent resources.
           */
          void run();

          /**
           * Stop running internal Master threads.
           * This routine is meant to help derivative Master classes to 
           * shutdown Master cleanly.  It should be called first and 
           * foremost in the derivative classes destructor to prevent
           * Master from calling any virtual functions (such as 
           * Master::updateStatus) which have already been destructed.
           */
          void stop();

      protected:

          /** 
           * Add a device to the master.
           * Master maintains a map of all devices on the CAN network(s)
           * it controls.  This routine adds a device to this map and
           * initializes the device to OFFLINE.  
           * @param device pointer to base Device class of a device. 
           * @see Device::setState
           * @throws carma::canbus::BadParameterException if device has 
           * already been added.
           */
          void addDevice(Device *device);

          /** 
           * Remove a device. 
           * Remove a device with the specified api and node id from the master 
           * device map.  Note that removeDevice is not responsible for 
           * deleting the object itself, it only removes the object from 
           * Master's device map.  If the user 
           * intends on dynamically adding and removing devices, he must
           * destroy his objects explicitly.  This can be done by calling 
           * getDevice, followed by removeDevice and then deleting the 
           * object in a manner appropriate for the implementation.  For
           * a detailed discussion of this see carma::canbus::Device.
           * @param api API Id of device pointer to retrieve.
           * @param node Node Id of device pointer to retrieve. 
           * @throws carma::canbus::BadParameterException if device doesn't
           * exist in Master.
           * @see carma::canbus::Device
           */
          void removeDevice(apiType api, nodeType node);

          /**
           * Get pointer to a device.
           * This method retrieves a pointer to the specified device.  
           * @param api API Id of device pointer to retrieve.
           * @param node Node Id of device pointer to retrieve. 
           * @return pointer to specified device.
           * @throws carma::canbus::BadParameterException if device doesn't
           * exist.
           */
          Device* getDevice(apiType api, nodeType node);

          /**
           * Get number of online nodes.
           * @return number of nodes currently online.
           */
          int getOnlineNodeCount() const;

          /**
           * Get number of offline nodes.
           * @return number of nodes currently offline.
           */
          int getOfflineNodeCount() const;

          /**
           * Get number of unknown packets
           * This method returns the number of CAN packets that 
           * canbus::Master does not recognize.  A non-zero count
           * does not necessarily indicate an error condition as
           * engineering packets and any CAN packets not sent by
           * carma::canbus::Device class derivatives added to the 
           * canbus::Master class will be added to the count.  These
           * unrecognized devices may legitimitely exist either
           * persistently or intermitently on a CANbus for engineering
           * or troubleshooting purposes.
           * @return unknown packet count
           */
          int getUnknownPacketCount() const;

          /**
           * Get 'Dongleless node' packet count
           * This routine returns the number of Dongleless node
           * packets detected by Master.  Dongleless nodes are 
           * allocated Device nodes that return a node id of 
           * 511.  This node id is the default node of a device
           * that cannot detect its true node id from the 1-wire
           * dongle device.  If the number is continuously increasing
           * it represents an error condition.
           * @return dongleless packet count
           */
          int getDonglelessPacketCount() const;
           
          /**
           * Get combined late packet count from all devices.
           * This method is responsible for calling Device::getNlatePackets
           * on all devices registered with the map, summing them up 
           * and returning the result.  It is a good indicator of overall 
           * system timing health.  It is the responsibility of a developer
           * implementing canbus::Device class derivatives to update and 
           * maintain late packet counts.
           * @return combined number of late packets from all devices.
           * @see carma::canbus::Device::getNlatePackets
           */
          unsigned int getLatePacketCount();

          /**
           * Get controls.
           * Master controls correspond to CAN bus control commands
           * that are accepted by all modules regardless of API 
           * implementation or functionality.  They consist of 
           * basic commands such as setTime, Software reset, 
           * Hardware reset...  
           * @return map<msgType, string> map containing the message
           * id as a key (msgType) and a string describing the control.
           */
          virtual std::map<msgType, std::string> getControls() const;

          /**
           * Update the status of the master.
           * This routine is intended to be overloaded by a user
           * implementing a master subclass.  It is included to allow 
           * the user to retrieve information from the bus(ses) and 
           * the Master via getBusStatus() and other Master get 
           * methods and then place the information into the monitor 
           * stream which will be implementation defined.  It is 
           * automatically called once every half second frame.  
           * If a user doesn't wish to place any bus specific or master 
           * status info into the monitor stream, this routine must be 
           * no-oped.
           * @see CanIo::getBusStatus
           */
          virtual void updateStatus() = 0;

          /**
           * Send a software reset command to all modules.
           * This method sends a global reset CAN message to all
           * modules.  It differs from the CanDio reset which issues
           * a hardware reset.
           * @throws carma::canbus::TxBufferFullException if unable to
           * post message due to a full transmit buffer.  This is likely
           * due to a disconnected bus.  Message is dropped.
           * @throws carma::util::ErrorException if invalid busId,
           * or driver failure.  This error generally signals a programming
           * error, or a misconfigured system.
           */
          void softwareReset();

          /**
           * Send a time sync message.
           * This routine is responsible for sending a time
           * sync message to the bus(ses).  It is declared virtual
           * to allow different implementations to send differently
           * formatted time syncs.  The default implementation
           * is as follows...
           * CAN Byte 0 and 1 - Modified Julian Day as an unsigned short
           * integer.
           * CAN Byte 2 - 5 - Time of Day in billionths of a day as
           * an unsigned long integer.
           * Time is retrieved from the system time and this routine
           * is called every ten seconds.  The default implementation 
           * sends a time sync to all busses.  Do not use this if you 
           * intend on tying Bus 0 to Bus 1 as this will (eventually)
           * cause an error state on the bus.
           * @throws carma::canbus::TxBufferFullException if unable to
           * post message due to a full transmit buffer.  This is likely
           * due to a disconnected bus.  Message is dropped.
           * @throws carma::util::ErrorException if invalid busId,
           * or driver failure.  This error generally signals a programming
           * error, or a misconfigured system.
           * @see CanIo::postMessage(idType id, vector<byteType> &data)
           */
          virtual void setTime();

      protected:

          // Undefined and inaccessible to prevent copying.
          Master(const Master &);
          Master &operator=(const Master &);

          /**
           * Update the devices states.
           * Each device has an associated state that must be 
           * updated from the master.  The states, STARTING, 
           * ONLINE and OFFLINE are updated based on the last time 
           * we have received a message from the Device.
           */ 
          void updateDevicesStates();

          /**
           * Entry point for timer thread.
           * Thread entry points are needed to allow C-style calls
           * to pthreads and are thus static to provide C linkage.
           * They are placed here (directly in the class) as opposed to
           * the global namespace (using an extern "C" linkage specifier)
           * so that we can utilize the visibility rules provided by C++.
           */
          static void *timerThreadEntry(void *arg);

          /**
           * Timer thread.
           * The timer thread is responsible for performing time critical
           * functions.  These include updating the devices status,
           * processing simulated messages if needed (i.e. the device is
           * OFFLINE) and sending time syncs.
           */
          void runTimerThread(); 

          /**
           * Entry point for main thread.
           * Thread entry points are needed to allow C-style calls
           * to pthreads and are thus static to provide C linkage.
           * They are placed here (directly in the class) as opposed to
           * the global namespace (using an extern "C" linkage specifier)
           * so that we can utilize the visibility rules provided by C++.
           */
          static void *readThreadEntry(void *arg);

          /**
           * Thread to read and process incoming CAN messages.
           * This is known as the main thread because it is the essential
           * thread in the framework. 
           */
          virtual void runReadThread();	

          std::map<keyType, Device*> devices_; 
          mutable pthread_mutex_t deviceMutex_;
          int nOnlineNodes_;       // Number of nodes in !OFFLINE state
          int nOfflineNodes_;      // Number of nodes in OFFLINE state
          int nUnknownPackets_;    // Number of packets unrecognized by master
          int nDonglelessPackets_; // Number of packets from node 512
          pthread_t timerThreadId_;
          pthread_t readThreadId_;
          const int simOfflineNodes_;

          bool running_;  // Is master running (needed for proper cleanup)
          carma::util::PthreadMutex runningMutex_;

      }; // End of class Master
  } // End namespace canbus 
} // End namespace carma
#endif // End #ifndef CARMA_CANBUS_MASTER_H
