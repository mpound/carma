#ifndef DRIVETASK_H
#define DRIVETASK_H

/**
 * @file AntennaDrive.h
 * 
 * Tagged: Thu Nov 13 16:53:27 UTC 2003
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/GenericTask.h"
#include "carma/szautil/SignalTask.h" // SIGNALTASK_HANDLER_FN

#include "carma/antenna/sza/antenna/control/AntennaDriveMsg.h"
#include "carma/antenna/sza/antenna/control/SzaShare.h"
#include "carma/antenna/sza/antenna/control/SzaTask.h"
#include "carma/antenna/sza/antenna/control/Tracker.h"

#define ANTENNADRIVE_TASK_FWD_FN(fn) void (fn)(AntennaDriveMsg* msg)

// Create an AntennaDrive class in namespace carma

namespace sza {
  namespace antenna {
    namespace control {
      
      /**
       * Incomplete type specification for AntennaMaster lets us declare
       * it as a friend below without defining it
       */
      class AntennaMaster;
      
      /**
       * The AntennaDrive collects together the functionality of the base
       * class Drive and its descendants, namely PointingModel.
       * Remotely, there will be a single CORBA DO for an Antenna
       * object, from which antenna->Drive()->Pointingmodel() and its
       * methods can be accessed directly.  However, we want these
       * methods to be routed through the AntennaDrive message queue, and
       * so the antenna->Drive() portion of the DO is managed by the
       * AntennaDrive class.
       */
      class AntennaDrive : 
	public SzaTask,
	public sza::util::GenericTask<AntennaDriveMsg> {
	
	public:
	
	/**
	 * Send a message that the pmac is dis/connected
	 */
	void  sendPmacConnectedMsg(bool connected);
	
	/**
	 * Send a message to the parent that the pmac is disconnected
	 */
	void sendPmacDoneMsg(unsigned int seq);

	/**
	 * Send a message to the parent that the source has set.
	 */
	void sendSourceSetMsg(unsigned int seq);

	/**
	 * Send a message to the parent that a CARMA sequence number
	 * should be written
	 */
	void sendCarmaSeqNoMsg(unsigned long seq, 
			       sza::util::GenericTaskMsg::CarmaSeqNoType type, 
			       bool success);

	private:
	
	bool simPmac_;

	/**
	 * We declare AntennaMaster a friend because its
	 * startAntennaDrive() method will call serviceMsgQ().
	 */
	friend class AntennaMaster;
	
	/**
	 * Pointer to the parent task resources
	 */
	AntennaMaster* parent_;
	
	/**
	 * Private constructor prevents instantiation by anyone but
	 * AntennaMaster.
	 *
	 * @throws Exception
	 */
	AntennaDrive(AntennaMaster* parent, bool simPmac=false);
	
	/**
	 * Destructor.
	 *
	 * @throws Exception
	 */
	~AntennaDrive();
	
	/**
	 * Process a message received on the AntennaDrive message queue
	 *
	 * @throws Exception
	 */
	void processMsg(AntennaDriveMsg* taskMsg);
	
	/**
	 * Tell the Antenne Task we want to take responsibility for
	 * flagging/unflagging operations for our own boards.
	 *
	 * @throws Exception
	 */
	void adoptBoards();
	
	//------------------------------------------------------------
	// Thread management functions.
	//------------------------------------------------------------
	
	/**
	 * Startup routine for the pmac task thread
	 */
	static THREAD_START(startTracker);
	
	//------------------------------------------------------------
	// Thread cleanup handlers methods
	
	/**
	 * Cleanup routine for the pmac task thread
	 */
	static THREAD_CLEAN(cleanTracker);
	
	//------------------------------------------------------------
	// Ping routines for the (pingable) subsystem threads
	
	/**
	 * Ping routine for the pmac task thread
	 */
	static THREAD_PING(pingTracker);
	
	//------------------------------------------------------------
	// Subsystem resources.
	//
	// Pointers to the subsystem resources.  These pointers are
	// initialized by the subsystem threads on startup
	
	/**
	 * Pointer to the pmac system resources
	 */
	Tracker* trackertask_;
	
	/**
	 * Method for forwarding a message to the Tracker task.
	 */
	ANTENNADRIVE_TASK_FWD_FN(forwardTrackerMsg);
	
	/**
	 * Override GenericTask::respondToHeartBeat()
	 */
	void respondToHeartBeat();
	
      }; // End class AntennaDrive
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif


