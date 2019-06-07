#include "carma/antenna/sza/antenna/control/UmacControl.h"
#include "carma/antenna/sza/antenna/control/AntennaMaster.h"

#include "carma/szautil/Debug.h"

#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"

#include <sstream>

using namespace std;
using namespace sza::antenna::control;
using namespace sza::util;

#define COMMAND_TIMEOUT_SEC 5

/**.......................................................................
 * Constructor.
 */
UmacControl::UmacControl(AntennaMaster* parent) :
  parent_(parent) 
{
  timeOut_ = 0;

  initSerialConnection();
}
 
/**.......................................................................
 * Set up the client to attach to the power strip telnet port for this
 * antenna
 */
void UmacControl::initSerialConnection() 
{  
  client_ = new SerialClient("/dev/ttyS1", 9600);
  client_->stripUnprintable(true);
  client_->append("\r");
}

/**.......................................................................
 * Destructor.
 */
UmacControl::~UmacControl() 
{
  if(client_ != 0) {
    delete client_;
    client_ = 0;
  }
}

/**.......................................................................
 * Overwrite the base-class method.
 */
void UmacControl::processMsg(UmacControlMsg* msg)
{
  switch (msg->type) {
  case UmacControlMsg::POWER:
    DBPRINT(true, Debug::DEBUG9, "Initiating a connection");

    initiateCommSequence(msg->body.power.breaker, 
			 msg->body.power.power);  
    break;
  default:
    throw Error("UmacControl::processMsg: Unrecognized message type");
    break;
  }
}


/**.......................................................................
 * Initiate sending commands to the power strip.
 */
void UmacControl::initiateCommSequence(unsigned breaker, bool power)
{
  // Compile the command/response stack associated with this command

 compileCommandStateMachine(breaker, power);

  // Initiate a connection to the client

 DBPRINT(true, Debug::DEBUG9, "About to get a connection ");

  if(client_->connect() < 0) {
    LogStream errStr;
    errStr.appendMessageSimple(true, "Unable to connect to the power strip.\n");
    errStr.log();

    DBPRINT(true, Debug::DEBUG9, "Unable to get a connection");

    return;
  }

  // If the connection was successful, register the fd to be watched
  // for readability

  DBPRINT(true, Debug::DEBUG9, "Register read fd");

  fdSet_.registerReadFd(client_->getFd());

  // And enable a timeout for waiting for responses from the power
  // strip

  enableTimeOut(true);
}
			       
/**.......................................................................
 * Terminate a command sequence to the power strip.
 */
void UmacControl::terminateCommSequence()
{
  // Reset the timeout

  enableTimeOut(false);

  // Clear the fd from the set to be watched

  fdSet_.clearFromReadFdSet(client_->getFd());

  // And disconnect the client

  client_->disconnect();
}

/**.......................................................................
 * Compile the state machine we will use for communicating with the
 * strip
 */
void UmacControl::compileCommandStateMachine(unsigned breaker,
					      bool power)
{
  sentStrings_.clear();
  rcvdStrings_.clear();

  rcvdStrings_.push_back("Login:");
  sentStrings_.push_back("power");

  rcvdStrings_.push_back("Enter Request :");
  sentStrings_.push_back("5");

  rcvdStrings_.push_back("Enter Password:");
  sentStrings_.push_back("power");

  rcvdStrings_.push_back("DS-RPC>");
  ostringstream os;
  os << (power ? "on " : "off ") << breaker;
  sentStrings_.push_back(os.str());

  rcvdStrings_.push_back("(Y/N)");
  sentStrings_.push_back("y");

  // And set the iterators pointing to the head of the stack
  
  sentStringIter_ = sentStrings_.begin();
  rcvdStringIter_ = rcvdStrings_.begin();
}

/**.......................................................................
 * Set a timeout for waiting for a response from the strip.
 */
void UmacControl::enableTimeOut(bool enable)
{
  if(!enable) {
    timeOut_ = NULL;
  } else {
    
    // Set up a 1-second timer

    timer_.setTime(COMMAND_TIMEOUT_SEC, 0);

    // This will be used as a timeout in select

    timeOut_ = timer_.timeVal();
  }
}

/**.......................................................................
 * Service our message queue.
 */
void UmacControl::serviceMsgQ()
{
  bool stop   = false;
  int  nready = 0;
  LogStream errStr;
  
  // Our select loop will always check the msgq file descriptor for
  // readability, and the control port, when we have successfully
  // achieved a connection.
  
  fdSet_.registerReadFd(msgq_.fd());

  while(!stop) {

    nready=select(fdSet_.size(), fdSet_.readFdSet(), fdSet_.writeFdSet(), 
		  NULL, timeOut_);
    

    // A message on our message queue?

    if(fdSet_.isSetInRead(msgq_.fd())) 
      processTaskMsg(&stop);
    
    // Did we get a message on the strip fd?

    if(fdSet_.isSetInRead(client_->getFd()))
      processClientMessage();

    // Did we time out waiting for a response?

    if(nready==0) 
      registerTimeOut();
  }
}

/**.......................................................................
 * Read a line from the power strip and determine what to do
 */
void UmacControl::processClientMessage()
{
  String line = client_->readString();

  // If we have sent the last of our commands, terminate the command
  // sequence

  if(sentStringIter_ == sentStrings_.end())
    return terminateCommSequence();

  // If the line just read contains the expected response string, send
  // the next command in our command stack

  DBPRINT(true, Debug::DEBUG9, "Read a line: " << line.str());
  DBPRINT(true, Debug::DEBUG9, "Comparing it to: " << *rcvdStringIter_);

  if(line.contains(*rcvdStringIter_)) {

    DBPRINT(true, Debug::DEBUG9, "Sending string: " << *sentStringIter_);

    client_->writeString(*sentStringIter_);

    rcvdStringIter_++;
    sentStringIter_++;
  }
  
}

/**.......................................................................,
 * React to a failure on the part of the power strip to reply
 */
void UmacControl::registerTimeOut()
{
  // Register the failure to communicate

  LogStream errStr;
  errStr.appendMessageSimple(true, "Timed out waiting for a response from the power strip.\n");
  errStr.log();

  // And terminate the communication sequence

  terminateCommSequence();
}
