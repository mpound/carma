#include <iostream>

#include <time.h>
#include <sys/time.h>
#include <signal.h>
#include <setjmp.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>

#include "carma/szautil/Debug.h"     // DBPRINT
#include "carma/szautil/Directives.h"
#include "carma/szautil/Exception.h" // Error()

#include "carma/antenna/sza/antenna/control/AntennaControl.h"
#include "carma/antenna/sza/antenna/control/AntennaDrive.h"
#include "carma/antenna/sza/antenna/control/AntennaMaster.h"
#include "carma/antenna/sza/antenna/control/AntennaMonitor.h"
#include "carma/antenna/sza/antenna/control/AntennaRx.h"
#include "carma/antenna/sza/antenna/control/UmacControl.h"

#if DIR_USE_ANT_CORBA
#include "carma/antenna/sza/antenna/corba/SzaShareCorba.h"
#endif

#if DIR_HAVE_CARMA

#include "carma/antenna/sza/antenna/canbus/CanMaster.h"

using namespace sza::antenna::canbus;

#endif

using namespace std;
using namespace sza::util;

using namespace sza::antenna::control;

/**.......................................................................
 * Initialize the static AntennaMaster pointer to NULL.  This will be
 * used inside static functions (ie, signal handlers)
 */
AntennaMaster* AntennaMaster::master_ = 0;

/**.......................................................................
 * Constructor method.  This should start up all subsystem threads 
 *
 * Input:
 *
 *  host                    string  The name of the host we are running on.
 *  nameServer              string  The name of the host running the Orbacus 
 *                                  name server.
 *  objectName       const  string& A handle by which the CORBA DO served up 
 *                                  by this task will be known.
 *  eventServer             string  The name of the host running the Orbacus 
 *                                  event server.
 *  eventChannelName const  string& A handle by which the CORBA DO served up 
 *                                  by this task will be known.
 *  notifyServer            string  The name of the host running the Orbacus 
 *                                  notification server.
 *  antNum                  AntNum  The antenna specifier for this antenna.
 */
AntennaMaster::AntennaMaster(string host, 
			     string nameServer, 
			     const string& objectName, 
			     string eventServer, 
			     const string& eventChannelName,
			     string notifyServer, 
			     bool simulateCanbus,
			     const AntNum& antNum,
			     bool simPmac,
			     unsigned IFModNodeId,
			     bool connect,
			     bool newCaltert,
			     unsigned nArg,
			     char** argv,
			     bool ignoreWrapLogic)
{
  DBPRINT(true, Debug::DEBUG11, "0");

  // Before we do anything that might throw an exception, initialize
  // data members to the point where ~AntennaMaster() can safely be
  // called.

  // Keep a static pointer to ourselves; we will need this for access
  // to the AntennaMaster resources in signal handlers

  master_       = this;

  ignoreWrapLogic_ = ignoreWrapLogic;
  simPmac_         = simPmac;
  antNum_          = 0;
  brd_fns_         = 0;
  controlTask_     = 0;
  driveTask_       = 0;
  monitorTask_     = 0;
  rxTask_          = 0;
  share_           = 0;

  ifNodeId_        = IFModNodeId;
  connect_         = connect;
  newCaltert_      = newCaltert;

  // Store copies of relevant strings.

  host_             = host;
  nameServer_       = nameServer;
  objectName_       = objectName;
  eventServer_      = eventServer;
  notifyServer_     = notifyServer;
  eventChannelName_ = eventChannelName;

  // Store a copy of any extra arguments passed into main()

  setArgs(nArg, argv);
  
  // Output debug info
  CTOUT("AntennaMaster initialized for sza" << antNum << "with ignoreWrapLogic="
        << boolalpha << ignoreWrapLogic_);

  // Initialize the array of threads managed by this task.

  // We start the communications task first, since AntennaMonitor will
  // need its resources.  Commands received before other tasks have
  // started will simply be pushed onto the Master message queue and
  // will not be parsed until the call to serviceMsgQ() below, so even
  // though we start the control task first, we are not in danger of
  // losing messages while other tasks are starting.


  threads_.push_back(new Thread(&startAntennaControl, &cleanAntennaControl,
  				&pingAntennaControl, "AntennaControl",  0, 5));

  threads_.push_back(new Thread(&startAntennaRx,      &cleanAntennaRx, 
  				&pingAntennaRx,       "AntennaRx",      1, 4));

  threads_.push_back(new Thread(&startAntennaDrive,   &cleanAntennaDrive, 
  				&pingAntennaDrive,    "AntennaDrive",   2, 3));

  threads_.push_back(new Thread(&startAntennaMonitor, &cleanAntennaMonitor, 
				&pingAntennaMonitor,  "AntennaMonitor", 3, 2));

#if DIR_HAVE_CARMA

  // Start the CAN bus master thread after any tasks which might add
  // devices to the network.

  threads_.push_back(new Thread(&startAntennaCanBus,  &cleanAntennaCanbus,
				0,                    "AntennaCanBus",  5, 6));
#endif

  threads_.push_back(new Thread(&startAntennaSignal,  &cleanAntennaSignal,
  				0,                    "AntennaSignal",  6, 0));
  
  // Initialize the antenna enumerator

  antNum_ = new AntNum(antNum);

  // Initialize the resource object which will be shared between
  // threads spawned by this task

  CTOUT("About to allocate share object: " << pthread_self());

#if DIR_USE_ANT_CORBA
  share_  = new sza::antenna::corba::SzaShareCorba(host);
#else
  share_  = new SzaShare(host);
#endif

  CTOUT("About to allocate share object: done: " << pthread_self());

  if(share_ == 0)
    throw Error("AntennaMaster::AntennaMaster: share is NULL.\n");

  // Allocate an array of network forwarding commands, one per
  // register-map board.

  unsigned int nboard = share_->getNboard();

  brd_fns_ = static_cast<ANTENNAMASTER_TASK_FWD_FN(**)>
    (malloc(sizeof(ANTENNAMASTER_TASK_FWD_FN(*)) * nboard));

  if(brd_fns_ == 0) 
    throw Error("AntennaMaster::AntennaMaster: "
  		"Insufficient memory for board forwarder array.\n");

  // Initialize flagging operations for all boards.

  initBoardFlagging();

  // Initialize the CAN master.

  LogStream logStr;

#if DIR_HAVE_CARMA
  if(simulateCanbus) {
    logStr.appendMessageSimple(false, "Simulating CANbus");
    logStr.report();
    canMaster_    = new sza::antenna::canbus::CanMaster();
  } else {
    logStr.appendMessageSimple(false, "Attempting to initialize the CANbus");
    logStr.report();

    // First argument is the modboard number, i.e., the number set by
    // the hex switch on the janz card.  Second number is which slot
    // on the janz card the CAN network is plugged into.  At the
    // moment, the janz cards have been set to modboard=1, and the
    // network is plugged via a T splitter into both slots of the janz
    // card.

    COUT("Here ...0 ");
    canMaster_    = new sza::antenna::canbus::CanMaster(0, 0);
    COUT("Here ...1 ");
  }
#endif

  // Install signals of interest to us.

  installSignals();

  // Install timers of interest to us.  Do this before we start up our
  // threads, that way we ensure all timers are defined before a
  // thread may send a message to disable or enable one.

  installTimers();

  // Finally, start up the threads.

  startThreads(this);

  COUT("Done starting master threads");

  // Start up our message queue

  run();
}

/**.......................................................................
 * AntennaMaster destructor method.  This should cause all subsystem
 * threads to shut down.
 */
AntennaMaster::~AntennaMaster() 
{
  DBPRINT(true, Debug::DEBUG7, "About to delete brd_fns");

  // Free the dynamically allocated array of board flagging functions

  if(brd_fns_ != 0) {
    free(brd_fns_);
    brd_fns_ = 0;
  }

  DBPRINT(true, Debug::DEBUG7, "About to delete share");

  // Delete the dynamically allocated shared resource object

  if(share_ != 0) {
    delete share_;
    share_ = 0;
  }

  DBPRINT(true, Debug::DEBUG7, "About to delete antNum_");

  // Delete the dynamically allocated antenna enumerator

  if(antNum_ != 0) {
    delete antNum_;
    antNum_ = 0;
  }
}

#if DIR_HAVE_CARMA
//-----------------------------------------------------------------------
// AntennaCanBus thread

/**.......................................................................
 * AntennaCanBus thread startup function
 */
THREAD_START(AntennaMaster::startAntennaCanBus)
{
  bool waserr=false;
  AntennaMaster* ant = (AntennaMaster*) arg;
  Thread* thread = 0;

  try {
    DBPRINT(true, Debug::DEBUG11, "starting canbus thread");
    
    // Get the Thread object which will manage this thread.
    
    thread = ant->getThread("AntennaCanBus");
    
    // Let other threads know we are ready
    
    thread->broadcastReady();
    
    // Finally, block, running the master.
    
    ant->canMaster_->run();
  } catch(Exception& err) {
    cout << err.what() << endl;
    throw err;
  }
    
  return 0;
}
#endif

/**.......................................................................
 * A cleanup handler for the AntennaCanbus thread.
 */
THREAD_CLEAN(AntennaMaster::cleanAntennaCanbus) 
{
  AntennaMaster* master = (AntennaMaster*) arg;

  DBPRINT(true, Debug::DEBUG7, "Inside cleanAntennaCanbus");

#if DIR_HAVE_CARMA
  if(master->canMaster_ != 0) {
    delete master->canMaster_;
    master->canMaster_ = 0;
  }
#endif

  master->sendStopMsg();

  DBPRINT(true, Debug::DEBUG7, "Leaving cleanAntennaCanbus");
}

//-----------------------------------------------------------------------
// AntennaRx subsystem

/**.......................................................................
 * Accessor method for AntennaRx subsystem
 */
AntennaRx* AntennaMaster::AntennaRx() 
{
  return rxTask_;
};

/**.......................................................................
 * AntennaRx thread startup function
 */
THREAD_START(AntennaMaster::startAntennaRx)
{
  bool waserr=false;
  AntennaMaster* ant = (AntennaMaster*) arg;
  Thread* thread = 0;

  try {
    DBPRINT(true, Debug::DEBUG11, "starting Rx thread");
    
    // Get the Thread object which will manage this thread.
    
    thread = ant->getThread("AntennaRx");
    
    // Instantiate the subsystem object
    
    ant->rxTask_ = new sza::antenna::control::AntennaRx(ant);
    
    // Set our internal thread pointer pointing to the AntennaRx thread
    
    ant->rxTask_->thread_ = thread;
    
    // Let other threads know we are ready
    
    thread->broadcastReady();
    
    // Finally, block, running our message service:
    
    DBPRINT(true, Debug::DEBUG11, "About to call Rx run");
    ant->rxTask_->run();
  } catch(Exception& err) {
    cout << err.what() << endl;
    throw err;
  }

  return 0;
}

/**.......................................................................
 * A cleanup handler for the AntennaRx thread.
 */
THREAD_CLEAN(AntennaMaster::cleanAntennaRx) 
{
  AntennaMaster* master = (AntennaMaster*) arg;

  DBPRINT(true, Debug::DEBUG7, "Inside cleanAntennaRx");

  if(master->rxTask_ != 0) {
    delete master->rxTask_;
    master->rxTask_ = 0;
  }
  
  master->sendStopMsg();

  DBPRINT(true, Debug::DEBUG7, "Leaving cleanAntennaRx");
}

/**.......................................................................
 * A function by which we will ping the AntennaRx thread
 */
THREAD_PING(AntennaMaster::pingAntennaRx)
{
  bool waserr=0;
  AntennaMaster* ant = (AntennaMaster*) arg;

  if(ant == 0)
    throw Error("AntennaMaster::pingAntennaRx: NULL argument.\n");

  ant->rxTask_->sendHeartBeatMsg();
}

//-----------------------------------------------------------------------
// AntennaDrive subsystem

/**.......................................................................
 * AntennaDrive subsystem accessor
 */
AntennaDrive* AntennaMaster::AntennaDrive() 
{
  return driveTask_;
};

/**.......................................................................
 * AntennaDrive thread startup function
 */
THREAD_START(AntennaMaster::startAntennaDrive)
{
  bool waserr=false;
  AntennaMaster* ant = (AntennaMaster*) arg;
  Thread* thread = 0;

  DBPRINT(true, Debug::DEBUG11, "starting drive thread");

  try {
    // Get a reference to the Thread object which will manage this
    // thread.
    
    thread = ant->getThread("AntennaDrive");
    
    // Instantiate the subsystem object
    
    ant->driveTask_ = new sza::antenna::control::AntennaDrive(ant, ant->simPmac_);
    
    // Set our internal thread pointer pointing to the AntennaDrive thread
    
    ant->driveTask_->thread_ = thread;
    
    // Let other threads know we are ready
    
    thread->broadcastReady();
    
    // Finally, block, running our message service:
    
    ant->driveTask_->run();
  } catch(Exception& err) {
    cout << "Error was: " << err.what() << endl;
    err.report();
    throw err;
  }

  return 0;  
};

/**.......................................................................
 * A cleanup handler for the AntennaDrive thread.
 */
THREAD_CLEAN(AntennaMaster::cleanAntennaDrive) 
{
  AntennaMaster* master = (AntennaMaster*) arg;

  if(master->driveTask_ != 0) {
    delete master->driveTask_;
    master->driveTask_ = 0;
  }
  
  master->sendStopMsg();
}

/**.......................................................................
 * A function by which we will ping the AntennaDrive thread
 */
THREAD_PING(AntennaMaster::pingAntennaDrive)
{
  bool waserr=0;
  AntennaMaster* ant = (AntennaMaster*) arg;

  if(ant == 0)
    throw Error("AntennaMaster::pingAntennaDrive: NULL argument.\n");

  ant->driveTask_->sendHeartBeatMsg();
}

//-----------------------------------------------------------------------
// AntennaMonitor subsystem

/**.......................................................................
 * Accessor method for AntennaMonitor subsystem
 */
AntennaMonitor* AntennaMaster::AntennaMonitor() 
{
  return monitorTask_;
};

/**.......................................................................
 * AntennaMonitor thread startup function
 */
THREAD_START(AntennaMaster::startAntennaMonitor)
{
  bool waserr=false;
  AntennaMaster* ant = (AntennaMaster*) arg;
  Thread* thread = 0;

  try {
    DBPRINT(true, Debug::DEBUG11, "starting monitor thread");
    
    // Get a reference to the Thread object which will manage this
    // thread.
    
    thread = ant->getThread("AntennaMonitor");
    
    // Instantiate the subsystem object
    
    DBPRINT(true, Debug::DEBUG3, "startMonitor:: about to call new");
    
    ant->monitorTask_ = new 
      sza::antenna::control::AntennaMonitor(ant);
    
    // Set the internal thread pointer pointing to the AntennaMonitor thread
    
    DBPRINT(true, Debug::DEBUG3, "startMonitor::  about to set thread");
    
    ant->monitorTask_->thread_ = thread;
    
    // Let other threads know we are ready
    
    DBPRINT(true, Debug::DEBUG3, "startMonitor:: Just about to broadcast ready");
    
    thread->broadcastReady();
    
    // Finally, block, running our message service:
    
    ant->monitorTask_->run();
  } catch(Exception& err) {
    cout << err.what() << endl;
  }

  return 0;    
};

/**.......................................................................
 * A cleanup handler for the AntennaMonitor thread.
 */
THREAD_CLEAN(AntennaMaster::cleanAntennaMonitor) 
{
  AntennaMaster* master = (AntennaMaster*) arg;

  if(master->monitorTask_ != 0) {
    delete master->monitorTask_;
    master->monitorTask_ = 0;
  }
  
  master->sendStopMsg();
}

/**.......................................................................
 * A function by which we will ping the AntennaMonitor thread
 */
THREAD_PING(AntennaMaster::pingAntennaMonitor)
{
  bool waserr=0;
  AntennaMaster* ant = 0;

  ant = (AntennaMaster*) arg;

  if(ant == 0)
    throw Error("AntennaMaster::pingAntennaMonitor: NULL argument.\n");

  ant->monitorTask_->sendHeartBeatMsg();
}

//-----------------------------------------------------------------------
// UmacControl subsystem

/**.......................................................................
 * Accessor method for UmacControl subsystem
 */
UmacControl* AntennaMaster::UmacControl() 
{
  return umacControlTask_;
};

/**.......................................................................
 * UmacControl thread startup function
 */
THREAD_START(AntennaMaster::startUmacControl)
{
  bool waserr=false;
  AntennaMaster* ant = (AntennaMaster*) arg;
  Thread* thread = 0;

  try {
    DBPRINT(true, Debug::DEBUG11, "starting monitor thread");
    
    // Get a reference to the Thread object which will manage this
    // thread.
    
    thread = ant->getThread("UmacControl");
    
    // Instantiate the subsystem object
    
    DBPRINT(true, Debug::DEBUG3, "startMonitor:: about to call new");
    
    ant->umacControlTask_ = new 
      sza::antenna::control::UmacControl(ant);
    
    // Set the internal thread pointer pointing to the UmacControl thread
    
    DBPRINT(true, Debug::DEBUG3, "startMonitor::  about to set thread");
    
    ant->umacControlTask_->thread_ = thread;
    
    // Let other threads know we are ready
    
    DBPRINT(true, Debug::DEBUG3, "startMonitor:: Just about to broadcast ready");
    
    thread->broadcastReady();
    
    // Finally, block, running our message service:
    
    ant->umacControlTask_->run();
  } catch(Exception& err) {
    cout << err.what() << endl;
    throw err;
  }
  return 0;    
};

/**.......................................................................
 * A cleanup handler for the UmacControl thread.
 */
THREAD_CLEAN(AntennaMaster::cleanUmacControl) 
{
  AntennaMaster* master = (AntennaMaster*) arg;

  if(master->umacControlTask_ != 0) {
    delete master->umacControlTask_;
    master->umacControlTask_ = 0;
  }
  
  master->sendStopMsg();
}

//-----------------------------------------------------------------------
// AntennaSignal subsystem

/**.......................................................................
 * AntennaSignal thread startup function
 */
THREAD_START(AntennaMaster::startAntennaSignal)
{
  bool waserr=false;
  AntennaMaster* ant = (AntennaMaster*) arg;
  Thread* thread = 0;

  try {
    DBPRINT(true, Debug::DEBUG11, "startSignal:: Inside");
    
    // Get a reference to the Thread object managing this thread.
    
    thread = ant->getThread("AntennaSignal");
    
    // Instantiate the subsystem object
    
    ant->signal_ = new 
      sza::util::SignalTask();
    
    // Let other threads know we are ready
    
    thread->broadcastReady();
    
    // Finally, block in SignalTask::run(), which services all handled
    // signals.
    
    DBPRINT(true, Debug::DEBUG3, "About to call signalTask: run()");
    
    ant->signal_->run();
  } catch(Exception& err) {
    cout << err.what() << endl;
    throw err;
  }

  return 0;  
}

/**.......................................................................
 * A cleanup handler for the AntennaSignal thread.
 */
THREAD_CLEAN(AntennaMaster::cleanAntennaSignal) 
{
  AntennaMaster* master = (AntennaMaster*) arg;

  DBPRINT(true, Debug::DEBUG7, "Inside cleanSignal");

  if(master->signal_ != 0) {
    delete master->signal_;
    master->signal_ = 0;
  }
  
  master->sendStopMsg();

  DBPRINT(true, Debug::DEBUG7, "Leaving cleanSignal");
}

//-----------------------------------------------------------------------
// AntennaControl subsystem

/**.......................................................................
 * Accessor method for the AntennaControl subsystem
 */
AntennaControl* AntennaMaster::AntennaControl() 
{
  return controlTask_;
};

/**.......................................................................
 * AntennaControl thread startup function
 */
THREAD_START(AntennaMaster::startAntennaControl)
{
  bool waserr=false;
  AntennaMaster* ant = (AntennaMaster*) arg;
  Thread* thread = 0;

  try {
    DBPRINT(true, Debug::DEBUG11, "startAntennaControl:: Inside");
    
    // Get a reference to the Thread object which will manage this
    // thread.
    
    thread = ant->getThread("AntennaControl");
    
    // Initialize communications with the outside world
    
    ant->controlTask_  = new sza::antenna::control::AntennaControl(ant);
    
    // Set our internal thread pointer pointing to the AntennaControl thread
    
    ant->controlTask_->thread_ = thread;
    
    // Let other threads know we are ready
    
    thread->broadcastReady();
    
    // Block in the communications run method.
    
    ant->controlTask_->run();
  } catch(Exception& err) {
    cout << err.what() << endl;
    throw err;
  }

  return 0;  
}

/**.......................................................................
 * AntennaControl thread startup function
 */
THREAD_CLEAN(AntennaMaster::cleanAntennaControl) 
{
  AntennaMaster* master = (AntennaMaster*) arg;

  DBPRINT(true, Debug::DEBUG7, "About to delete controlTask");

  if(master->controlTask_ != 0) {
    delete master->controlTask_;
    master->controlTask_ = 0;
  }
  
  master->sendStopMsg();
}

/**.......................................................................
 * Restart subsystem threads
 */
void AntennaMaster::restartServices()
{
  // Forcibly stop all threads

  cancelThreads();

  // Reset flagging operations.

  initBoardFlagging();

  // Restart the threads.

  startThreads(this);

  // Install signals.

  DBPRINT(false, Debug::DEBUG31, "Just about to re-install signals");

  installSignals();

  // Restart timers.

  DBPRINT(false, Debug::DEBUG31, "Just about to re-install timers");

  installTimers();

  // And service our message queue

  serviceMsgQ();
};

/**.......................................................................
 * A function by which we will ping the AntennaControl thread
 */
THREAD_PING(AntennaMaster::pingAntennaControl)
{
  bool waserr=0;
  AntennaMaster* ant = (AntennaMaster*) arg;

  if(ant == 0)
    throw Error("AntennaMaster::pingAntennaControl: NULL argument.\n");

  ant->rxTask_->sendHeartBeatMsg();
}

/**.......................................................................
 * Send a heartbeat request to all threads.  
 */
void AntennaMaster::sendHeartBeat()
{
  if(threadsAreRunning()) {
    pingThreads(this);
  } else {
    cout << "Sending restart" << endl;
    sendRestartMsg(0, NULL);
  } 
};

//-----------------------------------------------------------------------
// Communications methods
//-----------------------------------------------------------------------

/**.......................................................................
 * Process a message specific to the AntennaMaster.
 */
void AntennaMaster::processMsg(AntennaMasterMsg* msg)
{
  switch (msg->type) {
  case AntennaMasterMsg::ADOPT_BOARD:    // Message from a task
				         // claiming responsibility
				         // for flagging operations
				         // for a board
    recordAdoption(msg);
    break;
  case AntennaMasterMsg::CONTROL_MSG:    // Message for the
					 // AntennaControl thread
    forwardControlMsg(msg);
    break;
  case AntennaMasterMsg::DRIVE_MSG:      // Message for the
					 // AntennaDrive thread
    forwardDriveMsg(msg);
    break;
  case AntennaMasterMsg::FLAG_BOARD:     // Message to flag a board
    flagBoard(msg);
    break;
  case AntennaMasterMsg::CONTROL_CONNECTED: // Add or remove a handler to
					 // the connect timer in      
					 // response to an update of  
					 // the host connection status
    DBPRINT(true, Debug::DEBUG3, "Got a control connected message: "
	    << msg->body.controlConnected.connected);

    sendAddHandlerMsg("connect", &sendConnectControlMsg, 
		      !msg->body.controlConnected.connected);
    break;
  case AntennaMasterMsg::SCANNER_CONNECTED: // Add or remove a handler to
					 // the connect timer in      
					 // response to an update of  
					 // the host connection status
    DBPRINT(true, Debug::DEBUG3, "Got a scanner connected message: "
	    << msg->body.scannerConnected.connected);

    // If the scanner successfully connected, remove the handler from
    // the connect timer.

    if(msg->body.scannerConnected.connected)
      sendAddHandlerMsg("connect", &sendConnectScannerMsg, false);
    // Else reenable handlers for both the scanner and the control
    // process.  Otherwise if we've lost our connection to the control
    // program, the control thread will never know until it tries to
    // send something
    else {
      sendAddHandlerMsg("connect", &sendConnectScannerMsg, true);
      sendAddHandlerMsg("connect", &sendConnectControlMsg, true);
    }

    break;
  case AntennaMasterMsg::MONITOR_MSG:    // Message for the
					 // AntennaMonitor thread
    forwardMonitorMsg(msg);
    break;
  case AntennaMasterMsg::PMAC_CONNECTED: // Add or remove a handler to
					 // the connect timer in      
					 // response to an update of  
					 // the pmac connection status
    DBPRINT(true, Debug::DEBUG5, "Got a pmac connected message: " 
	    << msg->body.pmacConnected.connected);

    sendAddHandlerMsg("connect", &sendConnectPmacMsg, 
		      !msg->body.pmacConnected.connected);
    break;
  case AntennaMasterMsg::RX_MSG:         // Message for the AntennaRx
					 // thread
    forwardRxMsg(msg);
    break;
  case AntennaMasterMsg::SEND_HEARTBEAT: // Message to send a
					 // heartbeat request to all
					 // pingable threads
    sendHeartBeat();
    break;
  case AntennaMasterMsg::STRIP_CONTROL_MSG:      // Message for the
					 // UmacControl thread
    forwardUmacControlMsg(msg);
    break;
  default:
    {
      ostringstream os;
      os << "AntennaMaster::processMsg: Unrecognized message type: "
	 << msg->type << "." << endl << ends;
      throw Error(os.str());
    }
    break;
  }
}

//-----------------------------------------------------------------------
// Signal handlers

/**.......................................................................
 * A no-op signal handler for signals we wish to explicitly disable.
 */
SIGNALTASK_HANDLER_FN(AntennaMaster::doNothing) {}

/**.......................................................................
 * Send a message to initiate a shutdown
 */
SIGNALTASK_HANDLER_FN(AntennaMaster::sendShutDownMsg)
{
  DBPRINT(true, Debug::DEBUG7, "Sending shutdown message");
  master_->sendStopMsg();
}

/**.......................................................................
 * Send a message to restart
 */
SIGNALTASK_HANDLER_FN(AntennaMaster::sendRestartMsg)
{
  master_->GenericTask<AntennaMasterMsg>::sendRestartMsg();
}

/**.......................................................................
 * Send a message to initiate a heartbeat.
 */
SIGNALTASK_HANDLER_FN(AntennaMaster::sendSendHeartBeatMsg)
{
  AntennaMasterMsg msg;

  msg.packSendHeartBeatMsg();

  master_->sendTaskMsg(&msg);
}

/**.......................................................................
 * Send a message that the 1pps tick has arrived
 */
SIGNALTASK_HANDLER_FN(AntennaMaster::sendDriveTickMsg)
{
  AntennaMasterMsg masterMsg;
  TrackerMsg* msg = masterMsg.getDriveMsg()->getTrackerMsg();

  // In the VxWorks DASI code, the tick was timestamped with the value
  // returned from tickGet().  This will have to be fixed here.

  msg->packTickMsg(1);

  master_->forwardDriveMsg(&masterMsg);
};

/**.......................................................................
 * Send a message to the monitor task to send a data frame back to the ACC.
 */
SIGNALTASK_HANDLER_FN(AntennaMaster::sendPackDataFrameMsg)
{
  AntennaMasterMsg masterMsg;
  AntennaMonitorMsg* msg = masterMsg.getMonitorMsg();

  msg->packPackDataFrameMsg();

  master_->forwardMonitorMsg(&masterMsg);

  //------------------------------------------------------------
  // Also forward a message to write to the CARMA monitor system
  //------------------------------------------------------------

  masterMsg.getControlMsg()->packWriteCarmaMonitorsMsg();
  master_->forwardControlMsg(&masterMsg);
};

/**.......................................................................
 * Send a message to the tracker task to connect to the pmac
 */
SIGNALTASK_HANDLER_FN(AntennaMaster::sendConnectPmacMsg)
{
  AntennaMasterMsg masterMsg;
  TrackerMsg* msg = masterMsg.getDriveMsg()->getTrackerMsg();

  msg->packConnectPmacMsg();

  master_->forwardDriveMsg(&masterMsg);
};

/**.......................................................................
 * Send a message to the tracker task to strobe the pmac.
 */
SIGNALTASK_HANDLER_FN(AntennaMaster::sendStrobePmacMsg)
{
  AntennaMasterMsg masterMsg;
  TrackerMsg* msg = masterMsg.getDriveMsg()->getTrackerMsg();

  msg->packStrobePmacMsg();

  master_->forwardDriveMsg(&masterMsg);
};

/**.......................................................................
 * Send a message to the control task to connect to the ACC.
 */
SIGNALTASK_HANDLER_FN(AntennaMaster::sendConnectControlMsg)
{
  AntennaMasterMsg masterMsg;
  AntennaControlMsg* msg = masterMsg.getControlMsg();

  msg->packConnectMsg();

  master_->forwardControlMsg(&masterMsg);
};

/**.......................................................................
 * Send a message to the control task to connect to the ACC.
 */
SIGNALTASK_HANDLER_FN(AntennaMaster::sendConnectScannerMsg)
{
  AntennaMasterMsg masterMsg;
  AntennaMonitorMsg* msg = masterMsg.getMonitorMsg();

  msg->packConnectMsg();

  master_->forwardMonitorMsg(&masterMsg);
};

/**.......................................................................
 * Send a request to the Antenna Task via its message queue to adopt a
 * board
 */
void AntennaMaster::sendAdoptBoardMsg(unsigned short board, 
				      AntennaTask::Id taskId)
{
  AntennaMasterMsg msg;

  msg.packAdoptBoardMsg(board, taskId);

  sendTaskMsg(&msg);
}

/**.......................................................................
 * Record the adoption of flagging operations for a board
 */
void AntennaMaster::recordAdoption(AntennaMasterMsg* msg)
{
  // Get the number of boards in the SZA register map.

  unsigned int nboard = share_->getNboard();

  // Check that the board index makes sense.

  unsigned short iboard = msg->body.adoptBoard.board;

  if(iboard >= nboard) {
    ostringstream os;
    os << "AntennaMaster::recordAdoption: Board index out of range: "
       << iboard << "." << endl << ends;
    throw Error(os.str());
  }
  
  // Record the forwarding function of the adoptee.
  
  switch (msg->body.adoptBoard.taskId) {
  case AntennaTask::DRIVE:
    brd_fns_[iboard] = &forwardDriveMsg;
    break;
  case AntennaTask::RX:
    brd_fns_[iboard] = &forwardRxMsg;
    break;
  case AntennaTask::MASTER:
    brd_fns_[iboard] = &forwardMasterMsg;
    break;
  case AntennaTask::MONITOR:
    brd_fns_[iboard] = &forwardMonitorMsg;
    break;
  default:
    {
      ostringstream os;

      os << "AntennaMaster::recordAdoption: Unrecognized AntennaTask::Id: "
	 << msg->body.adoptBoard.taskId << endl << ends;
      
      throw Error(os.str());
    }
    break;
  }
}

/**.......................................................................
 * Process a message to flag a board
 */
void AntennaMaster::flagBoard(AntennaMasterMsg* masterMsg)
{
  // Forward the flag message to the task which has adopted this board

  unsigned short iboard = masterMsg->body.flagBoard.board;

  (brd_fns_[iboard])(masterMsg);
}

//-----------------------------------------------------------------------
// Miscellaneous methods
//-----------------------------------------------------------------------

/**.......................................................................
 * Public method to get access to our shared resource object
 */
SzaShare* AntennaMaster::getShare()
{
  return share_;
}

/**.......................................................................
 * Public method to get access to our antenna enumerator.
 */
AntNum* AntennaMaster::getAnt()
{
  return antNum_;
}

/**.......................................................................
 * Install signals of interest to us.
 */
void AntennaMaster::installSignals()
{
  sendInstallSignalMsg(SIGINT, &sendShutDownMsg);
}

/**.......................................................................
 * Install timers of interest to us.
 */
void AntennaMaster::installTimers()
{
  // Install and enable the heartbeat timer.

  sendInstallTimerMsg("heartbeat", ANTENNA_HEARTBEAT_SIGNAL,
		      ANTENNA_HEARTBEAT_SEC, 0, 
		      &sendSendHeartBeatMsg);

  sendEnableTimerMsg("heartbeat", false);

  // Receipt of this signal will cause us to send a message to the
  // drive task that a 1-second tick has arrived.
  
  sendInstallTimerMsg("drive", ANTENNA_DRIVE_SIGNAL,
		      0, ANTENNA_DRIVE_DELAY_NSEC,
		      ANTENNA_DRIVE_SEC, 0, 
		      &sendDriveTickMsg);

  sendEnableTimerMsg("drive", true);

  // Receipt of this signal will cause us to send a message to the
  // monitor task that it's time to send a data frame back to the
  // ACC.

  sendInstallTimerMsg("monitor", ANTENNA_MONITOR_SIGNAL,
		      0, ANTENNA_MONITOR_NSEC, 
		      &sendPackDataFrameMsg);

  sendEnableTimerMsg("monitor", true);

  // Receipt of this signal will cause us to strobe the pmac

  sendInstallTimerMsg("pmac", STROBE_PMAC_SIGNAL, 
		      0, STROBE_PMAC_DELAY_NSEC, 
		      0, STROBE_PMAC_NSEC, 
		      &sendStrobePmacMsg);

  sendEnableTimerMsg("pmac", true);

  // Receipt of this signal will cause us to send a message to the
  // drive task to attempt to connect to the pmac, and, if applicable,
  // to the control host.  

  // We install handlers at this point and activate the timer so that
  // the attempt to connect to the control program will come only
  // after we are listening to our message queue.  This is important,
  // since when the AntennaControl thread connects, it may receive a
  // slew of initialization commands which it will translate into task
  // commands and forward to us for dispersal to the relevant tasks.

  // If this happens before we are reading our message queue, and the
  // queue fills, then the AntennaControl thread will block, waiting
  // for us to read our message queue, but we will be blocked, waiting
  // for the AntennaControl thread to signal it is ready, causing a
  // deadlock.

  sendInstallTimerMsg("connect", CONNECT_SIGNAL, 
		      0,0,
		      CONNECT_SEC, 0, 
		      &sendConnectControlMsg);

  sendAddHandlerMsg("connect", &sendConnectScannerMsg, true);

  // We enable the timer, but note that it will not activate unless a
  // handler is installed

  if(connect_)
    sendEnableTimerMsg("connect", true);
}

/**.......................................................................
 * Forward a message to ourselves
 */
ANTENNAMASTER_TASK_FWD_FN(AntennaMaster::forwardMasterMsg)
{
  master_->sendTaskMsg(msg);
}

/**.......................................................................
 * Forward a message to the control task
 */
ANTENNAMASTER_TASK_FWD_FN(AntennaMaster::forwardControlMsg)
{
  if(master_->controlTask_ == 0)
    return;

  AntennaControlMsg* controlMsg = msg->getControlMsg();

  master_->controlTask_->sendTaskMsg(controlMsg);
}

/**.......................................................................
 * Forward a message to the drive task
 */
ANTENNAMASTER_TASK_FWD_FN(AntennaMaster::forwardDriveMsg)
{
  if(master_->driveTask_ == 0)
    return;

  AntennaDriveMsg* driveMsg = msg->getDriveMsg();

  if(msg->type == AntennaMasterMsg::FLAG_BOARD) 
    driveMsg->packFlagBoardMsg(msg->body.flagBoard.board, 
			       msg->body.flagBoard.flag);

  master_->driveTask_->sendTaskMsg(driveMsg);
}

/**.......................................................................
 * Forward a message to the monitor task
 */
ANTENNAMASTER_TASK_FWD_FN(AntennaMaster::forwardMonitorMsg)
{
  if(master_->monitorTask_ == 0)
    return;

  AntennaMonitorMsg* monitorMsg = msg->getMonitorMsg();

  if(msg->type == AntennaMasterMsg::FLAG_BOARD) 
    monitorMsg->packFlagBoardMsg(msg->body.flagBoard.board, 
			       msg->body.flagBoard.flag);

  master_->monitorTask_->sendTaskMsg(monitorMsg);
}

/**.......................................................................
 * Forward a message to the rx task
 */
ANTENNAMASTER_TASK_FWD_FN(AntennaMaster::forwardRxMsg)
{
  if(master_->rxTask_ == 0)
    return;

  AntennaRxMsg* rxMsg = msg->getRxMsg();

  if(msg->type == AntennaMasterMsg::FLAG_BOARD) 
    rxMsg->packFlagBoardMsg(msg->body.flagBoard.board, 
			       msg->body.flagBoard.flag);

  master_->rxTask_->sendTaskMsg(rxMsg);
}

/**.......................................................................
 * Forward a message to the rx task
 */
ANTENNAMASTER_TASK_FWD_FN(AntennaMaster::forwardUmacControlMsg)
{
  if(master_->umacControlTask_ == 0)
    return;

  UmacControlMsg* umacControlMsg = msg->getUmacControlMsg();

  master_->umacControlTask_->sendTaskMsg(umacControlMsg);
}

/**.......................................................................
 * Reset flagging operations for all boards.
 */
void AntennaMaster::initBoardFlagging()
{
  // Get the number of boards in the SZA register map.

  unsigned int nboard = share_->getNboard();

  // Flagging operations for all boards are the responsibility of the
  // monitor task until a board is adopted by another task

  for(unsigned int iboard = 0; iboard < nboard; iboard++)
    brd_fns_[iboard] = &forwardMonitorMsg;
}

void AntennaMaster::setArgs(unsigned int nArg, char** argv)
{
  std::ostringstream os;

  nArg_ = nArg;
  args_.resize(nArg);

  for(unsigned i=0; i < nArg; i++) {
    args_[i] = argv[i];
  }
}

std::vector<string> AntennaMaster::getArgs()
{
  return args_;
}

