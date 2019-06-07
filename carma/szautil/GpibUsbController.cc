#include "carma/szautil/GpibUsbController.h"
#include "carma/szautil/String.h"

#include <signal.h>

using namespace std;
using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
GpibUsbController::GpibUsbController(bool doSpawn) : 
  SerialClient(),
  SpawnableTask<GpibUsbControllerMsg>(true) 
{
  initialize(doSpawn);
}

/**.......................................................................
 * Constructor.
 */
GpibUsbController::GpibUsbController(std::string port, bool doSpawn) :
  SerialClient(port),
  SpawnableTask<GpibUsbControllerMsg>(true) 
{
  initialize(doSpawn);
}

void GpibUsbController::initialize(bool doSpawn)
{
  cmdTimeOut_.setIntervalInSeconds(1);
  cmdTimeOut_.activate(false);

  expectingResponse_ = false;
  eotEnabled_        = false;
  condVar_           = 0;

  fdSet_.registerReadFd(msgq_.fd());

  if(doSpawn)
    spawn();
}

/**.......................................................................
 * Destructor.
 */
GpibUsbController::~GpibUsbController() 
{
  sendStopMsg();
  disconnect();
}

/**.......................................................................
 * Main Task event loop: when this is called, the task blocks forever
 * in select(), or until a stop message is received.
 */
void GpibUsbController::serviceMsgQ(void) 
{
  DBPRINT(true, Debug::DEBUG7, "Entering serviceMsgQ " << pthread_self());

  bool stop=false;
  int nready; // number of file descriptors ready for reading

  if(msgq_.fd() < 0) {
    ThrowError("Received NULL file descriptor");
  }
  
  // Loop, checking the message queue file descriptor for readability
  
  while(!stop) {

    DBPRINT(true, Debug::DEBUG7, "Entering select: " << pthread_self());
    nready = select(fdSet_.size(), fdSet_.readFdSet(), NULL, NULL, cmdTimeOut_.tVal());
    DBPRINT(true, Debug::DEBUG7, "Dropping out of select: " << pthread_self()); 

    // See what we got

    if(nready > 0) {

      // Process messages received on our message queue
      
      if(fdSet_.isSetInRead(msgq_.fd())) {

	if(Debug::debugging(Debug::DEBUG7)) {
	  DBPRINT(true, Debug::DEBUG7, "Processing task message");
	  Debug::addLevel(Debug::DEBUG2);
	}

	processTaskMsg(&stop);

	if(Debug::debugging(Debug::DEBUG7)) {
	  Debug::remLevel(Debug::DEBUG2);
	}
      }
      
      // Process responses read from the serial port

      if(fdSet_.isSetInRead(getFd())) {

	DBPRINT(true, Debug::DEBUG7, "Fd was set in read");

	if(expectingResponse_) {
	  DBPRINT(true, Debug::DEBUG7, "Reading response");
	  readResponse();
	  DBPRINT(true, Debug::DEBUG7, "Reading response... done");

	} else {
	  DBPRINT(true, Debug::DEBUG7, "NOT expecting response");
	  respondToUnexpectedInput();
	}

      }

      // Did we time out?

    } else if(nready == 0) {
      respondToTimeOut();

      // Or get an error?

    } else {
      stop = true;
      ThrowSysError("select()");
    }
  }

  DBPRINT(true, Debug::DEBUG7, "Exiting...");
  spawnedThread_->raise(SIGKILL);

}

/**.......................................................................
 * Suspend or resume processing messages
 */
void GpibUsbController::suspendProcessingMessages(bool suspend)
{
  DBPRINT(true, Debug::DEBUG7, "Suspending  processing messages called with suspend = " << suspend << " " << pthread_self());

  if(suspend) {
    DBPRINT(true, Debug::DEBUG7, "Removing fd = " << msgq_.fd() << " from the read fd set");
    fdSet_.clearFromReadFdSet(msgq_.fd());
  } else {
    DBPRINT(true, Debug::DEBUG7, "Adding fd = " << msgq_.fd() << " back into the read fd set");
    fdSet_.registerReadFd(msgq_.fd());
  }
}

/**.......................................................................
 * Watch the serial port for a response, and set a timeout
 */
void GpibUsbController::watchForResponse(bool watch)
{
  if(watch) {
    fdSet_.registerReadFd(getFd());
  } else {
    fdSet_.clearFromReadFdSet(getFd());
  }

  // And set the timeout

  cmdTimeOut_.reset();
  cmdTimeOut_.activate(true);
}

/**.......................................................................
 * Respond to a timeout while waiting for a response
 */
void GpibUsbController::respondToTimeOut()
{
  // Set our flag to indicate that we are no longer watching for a
  // response

  expectingResponse_ = false;
  DBPRINT(true, Debug::DEBUG7, "Just set expectingResponse (1) to: " << expectingResponse_);

  // Deactivate the timeout

  cmdTimeOut_.activate(false);

  // Resume processing new messages

  suspendProcessingMessages(false);

  // And finally, report the error

  ReportError("Command: " << lastCmd_ << " timed out");

  // Broadcast done

  if(condVar_) {
    condVar_->broadcast();
  }

}

/**.......................................................................
 * Read/continue reading a response
 */
void GpibUsbController::readResponse()
{
  std::string response = readString();

  // Concatenate this onto any response we may already have read

  response_ << response;

  // Call the handler, and check its return value to see if we are
  // done reading this response

  if(checkHandler(this)) {

    if(condVar_) {
      DBPRINT(true, Debug::DEBUG7, "Here RR 2");
      condVar_->broadcast();
    }

    DBPRINT(true, Debug::DEBUG7, "Here RR 3");
    // Reset the response stream

    response_.str("");

    DBPRINT(true, Debug::DEBUG7, "Here RR 4");
    // Set our flag to indicate that we are no longer watching for a
    // response
    
    expectingResponse_ = false;
    DBPRINT(true, Debug::DEBUG7, "Just set expectingResponse (3) to: " << expectingResponse_);

    // Deactivate the timeout
    
    DBPRINT(true, Debug::DEBUG7, "Here RR 5");
    cmdTimeOut_.activate(false);
    
    // And resume processing new messages
    
    suspendProcessingMessages(false);

    DBPRINT(true, Debug::DEBUG7, "Here RR 6");

  }

  DBPRINT(true, Debug::DEBUG7, "Here RR 7");
}

/**.......................................................................
 * Respond to unexpected input from the controller
 */
void GpibUsbController::respondToUnexpectedInput()
{
  ThrowError("Unexpected input received!");
}

/**.......................................................................
 * Main Task event loop: when this is called, the task blocks forever
 * in select(), or until a stop message is received.
 */
void GpibUsbController::processMsg(GpibUsbControllerMsg* msg)
{
  DBPRINT(true, Debug::DEBUG7, "Inside processMsg: " << msg->type_ 
	  << " " << pthread_self());

  switch (msg->type_) {
  case GpibUsbControllerMsg::CONNECT:
    condVar_ = msg->condVar_;
    executeConnect(msg->retVal_);
    break;
  case GpibUsbControllerMsg::GPIB_DEV_MSG:
  case GpibUsbControllerMsg::GPIB_CNT_MSG:
    {
      std::string cmd((const char*)msg->cmd_);
      executeCommand(cmd, msg->expectsResponse_, msg->responseHandler_, msg->condVar_, msg->retVal_);
    }
    break;
  default:
    ThrowError("Unrecognized message type: " << msg->type_);
    break;
  }
  DBPRINT(true, Debug::DEBUG7, "leaving processMsg: " << pthread_self());
}

/**.......................................................................
 * Send a device command
 */
void GpibUsbController::sendDeviceCommand(std::string cmd, bool expectsResponse, 
				GPIB_RESPONSE_HANDLER(*handler), bool block, void* retVal)
{
  // If the device command expects a response, issue the command first
  // without blocking, then issue the control command to read until
  // EOI assertion

  if(expectsResponse) {
    sendCommand(cmd, DEVICE);
    sendControllerCommand("++read eoi", expectsResponse, handler, block, retVal);
  } else {
    sendCommand(cmd, DEVICE, expectsResponse, handler, block, retVal);
  }
}

/**.......................................................................
 * Send a controller command
 */
void GpibUsbController::sendControllerCommand(std::string cmd, bool expectsResponse, 
				    GPIB_RESPONSE_HANDLER(*handler), bool block, void* retVal)
{
  sendCommand(cmd, CONTROLLER, expectsResponse, handler, block, retVal);
}

/**.......................................................................
 * Send a command
 */
void GpibUsbController::sendCommand(std::string cmd, Receiver rx, bool expectsResponse, 
			  GPIB_RESPONSE_HANDLER(*handler), bool block, void* retVal)
{
  GpibUsbControllerMsg msg;

  msg.genericMsgType_ = GenericTaskMsg::TASK_SPECIFIC;
  msg.type_ = (rx == CONTROLLER ? GpibUsbControllerMsg::GPIB_CNT_MSG : GpibUsbControllerMsg::GPIB_DEV_MSG);

  DBPRINT(true, Debug::DEBUG7, "Here 0 (" << pthread_self() << ")" << cmd << " expectsResponse = " << expectsResponse);

  msg.expectsResponse_ = expectsResponse;
  msg.responseHandler_ = handler;

  strncpy((char*)msg.cmd_, &cmd[0], cmd.size());
  msg.cmd_[cmd.size()] = '\0';

  msg.retVal_          = retVal;

  if(block) {
    msg.condVar_ = new CondVar();
  } else {
    msg.condVar_ = 0;
  }

  DBPRINT(true, Debug::DEBUG7, "Here 1 (" << pthread_self() << ")");

  sendTaskMsg(&msg);

  // Block if requested until we are signalled

  DBPRINT(true, Debug::DEBUG7, "Here 2 (" << pthread_self() << ")" << "block = " << block);

  if(block) {
    msg.condVar_->wait();
  }

  // If we created a condition variable, delete it now.
 
  if(msg.condVar_) {
    delete(msg.condVar_);
    msg.condVar_ = 0;
  }
  
  DBPRINT(true, Debug::DEBUG7, "Here 4 (" << pthread_self() << ")");
}

/**.......................................................................
 * Execute a command 
 */
void GpibUsbController::executeCommand(std::string cmd, 
			     bool expectsResponse, 
			     GPIB_RESPONSE_HANDLER(*handler),
			     CondVar* condVar, 
			     void* retVal)
{
  DBPRINT(true, Debug::DEBUG7, "Writing string to fd = " << getFd());

  writeString(cmd);

  lastCmd_           = cmd;
  expectingResponse_ = expectsResponse;
  DBPRINT(true, Debug::DEBUG7, "Just set expectingResponse (2) to: " << expectingResponse_);
  responseHandler_   = handler;

  condVar_           = condVar;
  retVal_            = retVal;

  // If this message expects a response, then don't process any more
  // messages until we get one

 if(expectsResponse) {
   suspendProcessingMessages(true);
    watchForResponse(true);
  }
}

//-----------------------------------------------------------------------
// Task commands
//-----------------------------------------------------------------------

/**.......................................................................
 * Send a message to connect to the serial port
 */
int GpibUsbController::connect()
{
  GpibUsbControllerMsg msg;
  bool block = true;
  bool connected=false;

  msg.type_           = GpibUsbControllerMsg::CONNECT;
  msg.genericMsgType_ = GenericTaskMsg::TASK_SPECIFIC;
  msg.retVal_         = (void*)&connected;

  if(block) {
    msg.condVar_ = new CondVar();
  } else {
    msg.condVar_ = 0;
  }

  sendTaskMsg(&msg);

  if(block) {
    msg.condVar_->wait();
  }

  // If we created a condition variable, delete it now.
 
  if(msg.condVar_) {
    delete(msg.condVar_);
    msg.condVar_ = 0;
  }

  return connected;
}

/**.......................................................................
 * Connect to the controller, and send a message to clear the interface
 */
int GpibUsbController::connectAndClear()
{
  bool connected = connect();

  if(connected) {
    clearInterface();
  }

  return connected;
}

/**.......................................................................
 * Open the psuedo-serial port
 */
void GpibUsbController::executeConnect(void* retVal)
{
  int fd = SerialClient::connect();

  bool* connected = (bool*)retVal;

  if(getFd() < 0) {
    ReportError("Error opening port: " << portName());
    *connected = false;
  } else {
    ReportMessage("Successfully connected to: " << portName());
    *connected = true;
  }

  append("\r\n\0");

  if(condVar_) {
    condVar_->broadcast();
  }
}

//-----------------------------------------------------------------------
// Controller commands
//-----------------------------------------------------------------------

/**.......................................................................
 * Set the GPIB controller mode to automatically listen for a response
 * from the GPIB device
 */
void GpibUsbController::setAuto(bool doAuto)
{
  if(doAuto)
    sendCommand("++auto 1", CONTROLLER); 
  else
    sendCommand("++auto 0", CONTROLLER); 
}

void GpibUsbController::setMode(Mode mode)
{
  switch(mode) {
  case CONTROLLER:
    sendControllerCommand("++mode 1");
    break;
  case DEVICE:
    sendControllerCommand("++mode 0");
    break;
  default:
    break;
  }
}

/**.......................................................................
 * Get the version string from the controller
 */
std::string GpibUsbController::getVersion()
{
  std::string retVal;
  sendCommand("++ver", CONTROLLER, true, checkString, true, (void*)&retVal);
  return retVal;
}

/**.......................................................................
 * Get the help string from the controller
 */
std::string GpibUsbController::getHelp()
{
  std::string retVal;
  sendCommand("++help", CONTROLLER, true, checkString, true, (void*)&retVal);
  return retVal;
}

/**.......................................................................
 * A generic handler for the response
 */
GPIB_RESPONSE_HANDLER(GpibUsbController::checkHandler)
{
  static unsigned count=0;
  GpibUsbController* gpib = (GpibUsbController*)arg;

  bool eos = (gpib->response_.str()[gpib->response_.str().size()-1] == '\n');

  DBPRINT(true, Debug::DEBUG7, "Inside checkHandler: " << eos << " resp = " << gpib->response_.str());

  if(gpib->responseHandler_)
    return eos && gpib->responseHandler_(arg);
  else
    return eos;
}

/**........................................................................
 * Set the GPIB address of the controller device
 */
void GpibUsbController::setAddress(unsigned address)
{
  std::ostringstream os;
  os << "++addr " << address;
  DBPRINT(true, Debug::DEBUG7, "About to call GpibUsbController::sendCommand() " << " " << pthread_self());
  sendCommand(os.str(), CONTROLLER);
  DBPRINT(true, Debug::DEBUG7, "About to call GpibUsbController::sendCommand() " << " " << pthread_self() << " ... done");
}

/**.......................................................................
 * Get the GPIB address of the controller 
 */
unsigned GpibUsbController::getAddress()
{
  unsigned retVal;
  sendCommand("++addr", CONTROLLER, true, checkAddress, true, (void*)&retVal);
  return retVal;
}

/**.......................................................................
 * A handler for the response
 */
GPIB_RESPONSE_HANDLER(GpibUsbController::checkAddress)
{
  GpibUsbController* gpib = (GpibUsbController*)arg;

  unsigned* retVal = (unsigned*)gpib->retVal_;

  if(retVal)
    *retVal = (unsigned) atoi(gpib->response_.str().c_str());

  return true;
}

//-----------------------------------------------------------------------
// Device commands
//-----------------------------------------------------------------------

/**.......................................................................
 * A handler for a string response
 */
GPIB_RESPONSE_HANDLER(GpibUsbController::checkString)
{
  GpibUsbController* gpib = (GpibUsbController*)arg;

  std::string* retVal = (std::string*)gpib->retVal_;

  String str(gpib->response_.str(),false);
  str.replace('\r',' ');

  if(retVal) {
    *retVal = str.str();
    retVal->at(retVal->size()-1) = '\0';
  }

  return true;
}

/**.......................................................................
 * A handler for the response
 */
GPIB_RESPONSE_HANDLER(GpibUsbController::checkDevice)
{
  GpibUsbController* gpib = (GpibUsbController*)arg;

  std::string* retVal = (std::string*)gpib->retVal_;

  if(retVal) {
    *retVal = gpib->response_.str();
    retVal->at(retVal->size()-1) = '\0';
  }

  return true;
}

/**.......................................................................
 * Get the device identification string
 */
std::string GpibUsbController::getDevice()
{
  std::string retVal;
  sendDeviceCommand("*IDN?", true, checkString, true, (void*)&retVal);
  return retVal;
}

void GpibUsbController::resetDevice()
{
  sendCommand("*RST", DEVICE);
}

void GpibUsbController::clearDevice()
{
  sendCommand("*CLR", DEVICE);
}

void GpibUsbController::clearDeviceInterface()
{
  sendCommand("*CLS", DEVICE);
}

/**.......................................................................
 * Query the status of a self test
 */
std::string GpibUsbController::getSelfTest()
{
  std::string retVal;
  sendCommand("*TST?", DEVICE, true, checkString, true, (void*)&retVal);
  return retVal;
}

/**.......................................................................
 * A handler for the response
 */
GPIB_RESPONSE_HANDLER(GpibUsbController::checkCompletion)
{
  GpibUsbController* gpib = (GpibUsbController*)arg;

  bool* retVal = (bool*)gpib->retVal_;

  if(retVal) {
    *retVal = atoi(gpib->response_.str().c_str());
  }

  DBPRINT(true, Debug::DEBUG7, "Operation complete: " << gpib->response_.str() << " " << retVal);

  return true;
}

/**.......................................................................
 * Issue GPIB WAIT command
 */
void GpibUsbController::wait()
{
  sendDeviceCommand("*WAI");
}

/**.......................................................................
 * Clear the interfacae
 */
void GpibUsbController::clearInterface()
{
  sendCommand("++ifc", CONTROLLER);
}

/**.......................................................................
 * Reset the controller
 */
void GpibUsbController::resetController()
{
  sendCommand("++clr", CONTROLLER);
}

void GpibUsbController::stop()
{
  sendStopMsg();
}

void GpibUsbController::setEoi(bool sendEoi)
{
  if(sendEoi)
    sendControllerCommand("++eoi 1");
  else
    sendControllerCommand("++eoi 0");
}

/**.......................................................................
 * Get the EOI state from the controller
 */
std::string GpibUsbController::getEoi()
{
  std::string retVal;
  sendCommand("++eoi", CONTROLLER, true, checkString, true, (void*)&retVal);
  return retVal;
}

void GpibUsbController::setEos(unsigned eos)
{
  ostringstream os;
  os << "++eos " << eos;
  sendControllerCommand(os.str());
}

/**.......................................................................
 * Get the EOS state from the controller
 */
std::string GpibUsbController::getEos()
{
  std::string retVal;
  sendCommand("++eos", CONTROLLER, true, checkString, true, (void*)&retVal);
  return retVal;
}

void GpibUsbController::enableEot(bool enable)
{
  eotEnabled_ = enable;
  ostringstream os;
  os << "++eot_enable " << (enable ? 1 : 0);
  sendControllerCommand(os.str());
}

void GpibUsbController::setEotChar(char eot)
{
  ostringstream os;
  os << "++eot_char " << ((int)eot);
  sendControllerCommand(os.str());
}
