#ifndef ANTENNAMASTER_H
#define ANTENNAMASTER_H

/**
 * @file AntennaMaster.h
 * 
 * Tagged: Thu Nov 13 16:53:28 UTC 2003
 * 
 * @author Erik Leitch
 */
// C++ includes

#include <string>
#include <vector>

// C includes

#include <signal.h>

// The base class include

#include "carma/szautil/AntNum.h"
#include "carma/szautil/Directives.h"
#include "carma/szautil/GenericMasterTask.h"
#include "carma/szautil/SignalTask.h"

// SZA includes

#include "carma/antenna/sza/antenna/control/AntennaTask.h"
#include "carma/antenna/sza/antenna/control/AntennaMasterMsg.h"
#include "carma/antenna/sza/antenna/control/SzaShare.h"
#include "carma/antenna/sza/antenna/control/SzaTask.h"

#define ANTENNA_HEARTBEAT_SIGNAL SIGRTMIN+1
#define ANTENNA_DRIVE_SIGNAL     SIGRTMIN+2
#define ANTENNA_MONITOR_SIGNAL   SIGRTMIN+3
#define STROBE_PMAC_SIGNAL       SIGRTMIN+4
#define CONNECT_SIGNAL           SIGRTMIN+5

// This timer will cause parent tasks to send a heartbeat request to
// their children

#define ANTENNA_HEARTBEAT_SEC    5         

// Every second, the pmac will be sent new positions.

#define ANTENNA_DRIVE_SEC        1         

// The drive system strobe will fire a quarter second out of sync with
// the pmac data strobe, so that data from PMAC DPRAM are already in
// shared memory when the monitoring data strobe arrives.

#define ANTENNA_DRIVE_DELAY_NSEC 500000000 // Delay after the integral
					   // second boundary.  On
					   // receipt of this signal,
					   // the tracker task will
					   // deliver a new commanded
					   // position to the pmac,
					   // valid 1.5 seconds in the
					   // future.

// We will read out the entire shared memory object every half-second.

#define ANTENNA_MONITOR_NSEC     500000000 // 2Hz data pulse

// Every half-second, we will read out the pmac DPRAM into shared
// memory, in preparation for the data strobe which will cause the
// shared memory to be read and sent back to the control computer.  We
// don't want to read the DPRAM on the strobe, because the
// interlocking and readout operations from the pmac can be slow.

#define STROBE_PMAC_NSEC         500000000 

// The pmac strobe will fire a quarter second out of sync with the
// data strobe, so that data from PMAC DPRAM are already in shared
// memory when the data strobe arrives.

#define STROBE_PMAC_DELAY_NSEC   250000000 // Delay after the integral
					   // second boundary.  This
					   // puts the pmac strobe out
					   // of sync with the monitor
					   // strobe by a quarter
					   // second.

#define CONNECT_SEC         2              // Connect timer

#define ANTENNAMASTER_TASK_FWD_FN(fn) void (fn)(AntennaMasterMsg* msg)

namespace sza {
  namespace antenna {
    namespace control {
      
      /**
       * Forward declarations so we don't have to include the header
       * files.
       */
      class AntennaDrive;
      class AntennaControl;
      class AntennaMonitor;
      class AntennaRx;
      class UmacControl;
      
      /**
       * Define a class to encapsulate the entire Antenna control
       * system.  Instantiating this object will spawn all subsystem
       * threads of the Antenna control system, which currently
       * include a receiver task, a drive system task and a
       * monitoring task.
       */
      class AntennaMaster : 
	public SzaTask,
	public sza::util::GenericMasterTask<AntennaMasterMsg> {
	
	public:
	
	/**
	 * Constructor
	 * 
	 * @throws Exception;
	 */
	AntennaMaster(std::string host, 
		      std::string nameServer, 
		      const std::string& objectName,
		      std::string eventServer, 
		      const std::string& eventChannelName,
		      std::string notifyServer, 
		      bool simulateCanbus,
		      const sza::util::AntNum& antNum,
		      bool simPmac=false,
		      unsigned ifNodeId=1,
		      bool connect=true,
		      bool newcaltert=true,
		      unsigned nArg=0,
		      char** argv=0,
		      bool ignoreWrapLogic=true);
	
	/**
	 * Destructor
	 *
	 * @throws Exception
	 */
	~AntennaMaster();
	
	/**
	 * Return a pointer to the Control task resources
	 */
	sza::antenna::control::AntennaControl* AntennaControl();
	
	/**
	 * Return a pointer to the Drive control task resources
	 */
	sza::antenna::control::AntennaDrive*   AntennaDrive();
	
	/**
	 * Return a pointer to the Monitor task resources
	 */
	sza::antenna::control::AntennaMonitor* AntennaMonitor();
	
	/**
	 * Return a pointer to the Receiver task resources
	 */
	sza::antenna::control::AntennaRx*      AntennaRx();
	
	/**
	 * Return a pointer to the Receiver task resources
	 */
	sza::antenna::control::UmacControl*   UmacControl();

	/**
	 * Public interface to startThreads()
	 *
	 * @throws Exception	
	 */
	void restartServices();
	
	//------------------------------------------------------------
	// Public methods for sending messages to this task.
	//------------------------------------------------------------
	
	/**
	 * A no-op signal handler for signals we wish to explicitly
	 * disable.
	 */
	SIGNALTASK_HANDLER_FN(doNothing);
	
	/**
	 * Send a message to the AntennaMaster to tell it to send out
	 * a heartbeat request to all subordinate threads.
	 *
	 * @throws Exception (via MsgQ::sendMsg)
	 */
	static SIGNALTASK_HANDLER_FN(sendSendHeartBeatMsg);
	
	/**
	 * Send a message to the AntennaMaster to tell it to shut down
	 *
	 * @throws Exception (via MsgQ::sendMsg)
	 */
	static SIGNALTASK_HANDLER_FN(sendShutDownMsg);
	
	/**
	 * Send a message to the monitor task that it's time to pack a
	 * data frame for transmission back to the ACC.
	 */
	static SIGNALTASK_HANDLER_FN(sendPackDataFrameMsg);
	
	/**
	 * Tell the Tracker task that the 1-second pulse has arrived.
	 *
	 * @throws Exception
	 */
	static SIGNALTASK_HANDLER_FN(sendDriveTickMsg);
	
	/**
	 * Tell the Control task to attempt to connect to the host.
	 *
	 * @throws Exception
	 */
	static SIGNALTASK_HANDLER_FN(sendConnectControlMsg);
	
	/**
	 * Tell the Tracker to attempt to connect to the pmac.
	 *
	 * @throws Exception
	 */
	static SIGNALTASK_HANDLER_FN(sendConnectPmacMsg);
	
	/**
	 * Tell the Control task to attempt to connect to the host.
	 *
	 * @throws Exception
	 */
	static SIGNALTASK_HANDLER_FN(sendConnectScannerMsg);

	/**
	 * Tell the Tracker to readout the pmac DPRAM.
	 *
	 * @throws Exception
	 */
	static SIGNALTASK_HANDLER_FN(sendStrobePmacMsg);
	
	/**
	 * Method by which other tasks can ask us for control of
	 * boards.
	 *
	 * @throws Exception (via MsgQ::sendMsg)
	 */
	void sendAdoptBoardMsg(unsigned short, AntennaTask::Id taskId);
	
	//------------------------------------------------------------
	// Communications methods
	//------------------------------------------------------------
	
	/**
	 * Public method by which other tasks can forward message to us.
	 */
	static ANTENNAMASTER_TASK_FWD_FN(forwardMasterMsg);
	
	//------------------------------------------------------------
	// Public accessor methods
	//------------------------------------------------------------
	
	/**
	 * Public method to get a reference to our shared resource
	 * object.
	 */
	SzaShare* getShare();
	
	/**
	 * Public method to get a reference to our antenna enumerator.
	 */
	sza::util::AntNum* getAnt();
	
	/**
	 * A copy of the Nameserver name to which we will attach
	 */
	inline std::string host() {return host_;}
	
	/**
	 * A copy of the Nameserver name to which we will attach
	 */
	inline std::string nameServer() {return nameServer_;}
	
	/**
	 * A copy of the CORBA object name that the nameserver will
	 * associate with us.
	 */
	inline std::string objectName() {return objectName_;}
	
	/**
	 * A copy of the Eventserver name to which we will attach.
	 */
	inline std::string eventServer() {return eventServer_;}
	
	/**
	 * A copy of the Notifyserver name to which we will attach.
	 */
	inline std::string notifyServer() {return notifyServer_;}

	/**
	 * A copy of the event channel name on which we will send data
	 * back to the outside world.
	 */
	inline std::string eventChannelName() {return eventChannelName_;}
	
	inline unsigned getIFNodeId() {
	  return ifNodeId_;
	}

	void setArgs(unsigned nArg, char** argv);
	std::vector<std::string> getArgs();

	bool newCaltert() {
	  return newCaltert_;
	}

	bool ignoreWrapLogic() {
	  return ignoreWrapLogic_;
	}

	private:

	bool newCaltert_;

	// Argument string passed to the main program which
	// instantiated this object

	unsigned int nArg_;
	std::vector<std::string> args_;

	unsigned ifNodeId_;

	// True if connecting to the control system

	bool connect_;

	/**
	 * True if simulating the pmac
	 */
	bool simPmac_;

	// True if ignoring wrap logic

	bool ignoreWrapLogic_;

	/**
	 * A static pointer to ourselves for use in static functions
	 */
	static AntennaMaster* master_;
	
	/**
	 * An enumerator specifying which antenna we are.
	 */
	sza::util::AntNum* antNum_;
	
	/**
	 * A copy of the Nameserver name to which we will attach
	 */
	std::string host_;
	
	/**
	 * A copy of the Nameserver name to which we will attach
	 */
	std::string nameServer_;
	
	/**
	 * A copy of the CORBA object name that the nameserver will
	 * associate with us.
	 */
	std::string objectName_;
	
	/**
	 * A copy of the Eventserver name to which we will attach.
	 */
	std::string eventServer_;
	
	/**
	 * A copy of the Notifyserver name to which we will attach.
	 */
	std::string notifyServer_;

	/**
	 * A copy of the event channel name on which we will send data
	 * back to the outside world.
	 */
	std::string eventChannelName_;
	
	//------------------------------------------------------------
	// Thread management methods
	//------------------------------------------------------------
	
#if DIR_HAVE_CARMA
	/**
	 * Startup routine for the CAN master thread
	 */
	static THREAD_START(startAntennaCanBus);
#endif

	/**
	 * Startup routine for the Drive task thread
	 */
	static THREAD_START(startAntennaDrive);
	
	/**
	 * Startup routine for the Monitor task thread
	 */
	static THREAD_START(startAntennaMonitor);
	
	/**
	 * Startup routine for the control task thread
	 */
	static THREAD_START(startAntennaControl);
	
	/**
	 * Startup routine for the Receiver task thread
	 */
	static THREAD_START(startAntennaRx);
	
	/**
	 * Startup routine for the signal task thread
	 */
	static THREAD_START(startAntennaSignal);
	
	/**
	 * Startup routine for the strip control task thread
	 */
	static THREAD_START(startUmacControl);
	
	//------------------------------------------------------------
	// Thread cleanup handlers methods
	
	/**
	 * Cleanup routine for the Drive task thread
	 */
	static THREAD_CLEAN(cleanAntennaDrive);
	
	/**
	 * Cleanup routine for the Monitor task thread
	 */
	static THREAD_CLEAN(cleanAntennaMonitor);
	
	/**
	 * Cleanup routine for the canbus task thread
	 */
	static THREAD_CLEAN(cleanAntennaCanbus);

	/**
	 * Cleanup routine for the communications task thread
	 */
	static THREAD_CLEAN(cleanAntennaControl);
	
	/**
	 * Cleanup routine for the Receiver task thread
	 */
	static THREAD_CLEAN(cleanAntennaRx);
	
	/**
	 * Cleanup routine for the signal task thread
	 */
	static THREAD_CLEAN(cleanAntennaSignal);
	
	/**
	 * Cleanup routine for the strip task thread
	 */
	static THREAD_CLEAN(cleanUmacControl);

	//------------------------------------------------------------
	// Ping routines for the (pingable) subsystem threads
	
	/**
	 * Ping routine for the Control task thread
	 *
	 * @throws Exception
	 */
	static THREAD_PING(pingAntennaControl);

	/**
	 * Ping routine for the Drive task thread
	 *
	 * @throws Exception
	 */
	static THREAD_PING(pingAntennaDrive);
	
	/**
	 * Ping routine for the Monitor task thread
	 *
	 * @throws Exception
	 */
	static THREAD_PING(pingAntennaMonitor);
	
	/**
	 * Ping routine for the Receiver task thread
	 *
	 * @throws Exception
	 */
	static THREAD_PING(pingAntennaRx);
	
	//------------------------------------------------------------
	// Pointers to the subsystem resources.  These pointers are
	// initialized by the subsystem threads on startup
	
	/**
	 * An object which will handle communications with the outside
	 * world.
	 */
	sza::antenna::control::AntennaControl*      controlTask_;
	
	/**
	 * Pointer to the Drive system resources
	 */
	sza::antenna::control::AntennaDrive*      driveTask_; 
	
	/**
	 * Pointer to the Receiver Task resources
	 */
	sza::antenna::control::AntennaRx*         rxTask_; 
	
	/**
	 * Pointer to the Monitor Task resources
	 */
	sza::antenna::control::AntennaMonitor*    monitorTask_; 

	/**
	 * Pointer to the strip control Task resources
	 */
	sza::antenna::control::UmacControl*       umacControlTask_; 
	
	//------------------------------------------------------------
	// Members pertaining to communication with this task
	
	/**
	 * Process a message received on our message queue.  Processes
	 * all messages intended for tasks spawned by the
	 * AntennaMaster.
	 *
	 * @throws Exception
	 */
	void processMsg(AntennaMasterMsg* taskMsg);
	
	//------------------------------------------------------------
	// Message forwarding methods.  These are private, as they
	// should not be used directly by tasks to send messages to
	// each other, or they will run the risk of a message getting
	// dropped if these methods are called before the recipient's
	// queue exists.
	//
	// Messages to all tasks should be sent to the master, whose
	// message queue is guaranteed to exist before any other tasks
	// are spawned, and which will not service messages until all
	// spawned tasks have allocated their resources.  This
	// guarantees that no messages will be lost, but simply queued
	// until the master calls its serviceMsgQ() method.
	
	/**
	 * Method for forwarding a message to the Control task. 
	 */
	static ANTENNAMASTER_TASK_FWD_FN(forwardControlMsg);
	
	/**
	 * Method for forwarding a message to the Drive task. 
	 */
	static ANTENNAMASTER_TASK_FWD_FN(forwardDriveMsg);
	
	/**
	 * Method for forwarding a message to the Monitor task.
	 */
	static ANTENNAMASTER_TASK_FWD_FN(forwardMonitorMsg);
	
	/**
	 * Method for forwarding a message to the Receiver task.
	 */
	static ANTENNAMASTER_TASK_FWD_FN(forwardRxMsg);
	
	/**
	 * Method for forwarding a message to the UmacControl task.
	 */
	static ANTENNAMASTER_TASK_FWD_FN(forwardUmacControlMsg);

	/**
	 * An array which will record one task forwarding function per
	 * SZA register map board. These are used to forward
	 * board-control commands to tasks that adopt boards.
	 */
	ANTENNAMASTER_TASK_FWD_FN(**brd_fns_); 
	
	/**
	 * Initialize the above array.
	 */
	void initBoardFlagging();
	
	//------------------------------------------------------------
	// Private send methods for messages intended for the
	// AntennaMaster
	//------------------------------------------------------------
	
	/**
	 * Send a message to the AntennaMaster to restart threads
	 *
	 * @throws Exception (via MsgQ::sendMsg)
	 */
	static SIGNALTASK_HANDLER_FN(sendRestartMsg);
	
	/**
	 * Record the adoption of flagging operations for a board
	 * 
	 * @throws Exception
	 */
	void recordAdoption(AntennaMasterMsg* msg);
	
	/**
	 * Process a message to perform a flagging operation on a
	 * board
	 *
	 * @throws Exception
	 */
	void flagBoard(AntennaMasterMsg* msg);	  
	
	//------------------------------------------------------------
	// Methods pertaining to timers managed by this task
	
	/**
	 * Send a heartbeat to all threads managed by this task.
	 */
	void sendHeartBeat();
	
	/**
	 * Install timers we are interested in.
	 */
	void installTimers();
	
	/**
	 * Install signals we are interested in.
	 */
	void installSignals();
	
      }; // End class AntennaMaster
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif
