// C++ includes

#include <iostream>
#include <list>

// C includes

#include <ctime>
#include <sys/time.h>
#include <csignal>
#include <csetjmp>
#include <cerrno>
#include <sys/types.h>
#include <unistd.h>

// SZA includes

#include "carma/szautil/Exception.h"
#include "carma/szautil/Debug.h"

#include "carma/antenna/sza/antenna/control/AntennaDrive.h"
#include "carma/antenna/sza/antenna/control/AntennaMaster.h"
#include "carma/antenna/sza/antenna/control/PmacComms.h"

using namespace std;
using namespace sza::antenna::control;
using namespace sza::util;

//-----------------------------------------------------------------------
// Constructor/Destructors

/**.......................................................................
 * Create a AntennaDrive class in namespace carma
 */
AntennaDrive::AntennaDrive(AntennaMaster* parent, bool simPmac) :
  SzaTask(), sza::util::GenericTask<AntennaDriveMsg>::GenericTask(), simPmac_(simPmac)
{
  // Initialize member pointers to NULL

  parent_       = 0;
  trackertask_  = 0;

  // Sanity check arguments

  if(parent == 0)
    throw Error("AntennaDrive::AntennaDrive: Received NULL parent argument\n");

  // Keep a pointer to the AntennaMaster object for use in sending
  // messages to the AntennaMaster message queue

  parent_ = parent;

  // Keep a pointer to the shared resource object of the parent task.

  share_ = parent_->getShare();

  if(share_ == 0)
    throw Error("AntennaDrive::AntennaDrive: "
		"share argument is NULL.\n");

  // Initialize the array of threads managed by this task.  We don't
  // start these until all of our resources have been allocated.
  
  threads_.push_back(new Thread(&startTracker,      &cleanTracker, 
    				&pingTracker,       "Tracker"));

  // Start up threads managed by this task.

  COUT("Strarting Drive threads...");
  startThreads(this);
  COUT("Strarting Drive threads... done");

  // Arrange for the boards controlled by the Tracker object
  // to be adopted by the Drive Task
  
  adoptBoards();
};

/**.......................................................................
 * Destructor
 */
AntennaDrive::~AntennaDrive() {};

//-----------------------------------------------------------------------
// Communication methods

/**.......................................................................
 * Process a message received on our message queue
 */
void AntennaDrive::processMsg(AntennaDriveMsg* msg)
{
  switch (msg->type) {
  case AntennaDriveMsg::TRACKER_MSG:
    forwardTrackerMsg(msg);
    break;
  default:
    {
      ostringstream os;
      os << "AntennaDrive::processMsg: Unrecognized message type: "
	 << msg->type << endl << ends;
      throw Error(os.str());
    }
    break;
  }; 
}

/**.......................................................................
 * Send requests to the AntennaMaster to adopt the boards controlled by
 * the Tracker object
 */
void AntennaDrive::adoptBoards()
{
  if(trackertask_ == 0)
    throw Error("AntennaDrive::adoptBoards: trackertask_ is NULL.\n");

  // Get the list of boards controlled by the Tracker object

  list<Board*> boards = trackertask_->listBoards();

  // For each board, send a message to the AntennaMaster via its message
  // queue that we want to adopt it.

  for(list<Board*>::iterator ilist=boards.begin();  
      ilist != boards.end();ilist++) {
    Board* board = *ilist;
    parent_->sendAdoptBoardMsg(board->getIndex(), AntennaTask::DRIVE);
  }
}

/**.......................................................................
 * Tracker thread startup function
 */
THREAD_START(AntennaDrive::startTracker)
{
  bool waserr=false;
  AntennaDrive* drive = (AntennaDrive*) arg;
  Thread* thread = 0;

  try {
    // Get the Thread object which will manage this thread.
    
    thread = drive->getThread("Tracker");
    
    // Instantiate the subsystem object
    
    drive->trackertask_ = new sza::antenna::control::Tracker(drive, 
							     drive->simPmac_,
							     drive->parent_->ignoreWrapLogic());
    
    // Set our internal thread pointer pointing to the Tracker thread
    
    drive->trackertask_->thread_ = thread;
    
    // Let other threads know we are ready
    
    thread->broadcastReady();
    
    // Finally, block, running our message service:
    
    drive->trackertask_->run();
  } catch(Exception& err) {
    cout << err.what() << endl;
    throw err;
  }

  return 0;
}

/**.......................................................................
 * A cleanup handler for the Tracker thread.
 */
THREAD_CLEAN(AntennaDrive::cleanTracker)
{
  AntennaDrive* drive = (AntennaDrive*) arg;

  // Call the destructor function explicitly for rxtask
    
  if(drive->trackertask_ != 0) {
    delete drive->trackertask_;
    drive->trackertask_ = 0;
  }
}

/**.......................................................................
 * A function by which we will ping the Tracker thread
 */
THREAD_PING(AntennaDrive::pingTracker)
{
  bool waserr=0;
  AntennaDrive* drive = (AntennaDrive*) arg;

  if(drive == 0)
    throw Error("AntennaDrive::pingTracker: NULL argument.\n");

  drive->trackertask_->sendHeartBeatMsg();
}

/**.......................................................................
 * We will piggyback on the heartbeat signal received from the parent
 * to send a heartbeat to our own tasks.
 */
void AntennaDrive::respondToHeartBeat()
{
  // Respond to the parent heartbeat request

  if(thread_ != 0)
    thread_->setRunState(true);

  // And ping any threads we are running.

  if(threadsAreRunning()) {
    pingThreads(this);
  } else {
    cout << "Sending restart" << endl;
    sendRestartMsg();
  } 
}

/**.......................................................................
 * Forward a message to the pmac task
 */
ANTENNADRIVE_TASK_FWD_FN(AntennaDrive::forwardTrackerMsg)
{
  if(trackertask_ == 0)
    throw Error("AntennaDrive::forwardTrackerMsg: "
		"Tracker task pointer is NULL.\n");

  trackertask_->sendTaskMsg(msg->getTrackerMsg());
}

/**.......................................................................
 * Send a message to the parent that the pmac is disconnected
 */
void AntennaDrive::sendPmacConnectedMsg(bool connected)
{
  AntennaMasterMsg msg;

  msg.packPmacConnectedMsg(connected);

  parent_->forwardMasterMsg(&msg);
}

/**.......................................................................
 * Send a message to the parent that the pmac is disconnected
 */
void AntennaDrive::sendPmacDoneMsg(unsigned int seq)
{
  AntennaMasterMsg msg;
  NetMsg* netMsg = msg.getControlMsg()->getNetMsg();

  netMsg->setAntId(parent_->getAnt()->getId());
  netMsg->packPmacDoneMsg(seq);

  parent_->forwardMasterMsg(&msg);
}

/**.......................................................................
 * Send a message to the parent that the source has set.
 */
void AntennaDrive::sendSourceSetMsg(unsigned int seq)
{
  AntennaMasterMsg msg;
  NetMsg* netMsg = msg.getControlMsg()->getNetMsg();

  netMsg->setAntId(parent_->getAnt()->getId());
  netMsg->packSourceSetMsg(seq);

  parent_->forwardMasterMsg(&msg);
}

/**.......................................................................
 * Send a message to the parent that a CARMA sequence number
 * should be written
 */
void AntennaDrive::sendCarmaSeqNoMsg(unsigned long seq, 
				     sza::util::GenericTaskMsg::CarmaSeqNoType type, 
				     bool success)
{
  AntennaMasterMsg msg;
  msg.getControlMsg()->packCarmaSeqNoMsg(seq, type, success);
  parent_->forwardMasterMsg(&msg);
}

