#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/Logger.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/SzaPorts.h"

#include "carma/antenna/sza/antenna/control/AntennaMaster.h"
#include "carma/antenna/sza/antenna/control/AntennaControl.h"

#include "carma/antenna/sza/antenna/corba/AntennaCorba.h"

#include "carma/szaarrayutils/arraymaprev.h"

#include "carma/util/ExceptionUtils.h"

using namespace sza::util;
using namespace sza::antenna::control;

/**
 * Initialize the static pointer to NULL.
 */
AntennaControl* AntennaControl::control_ = 0;

/**.......................................................................
 * Constructor to be used if connecting to an array control
 * host via TCP/IP.
 */
AntennaControl::AntennaControl(AntennaMaster* parent) :
  SzaTask(), sza::util::GenericTask<AntennaControlMsg>::GenericTask(),
  parent_(parent)
{
  control_          = this;

  forwarder_  = 0;
  forwarder_ = new AntNetCmdForwarder(parent);

  // Initialize the array of threads controlled by this task.

#if DIR_USE_ANT_CORBA  
  threads_.push_back(new Thread(&startAntennaCorba, 
				&cleanAntennaCorba, 
				NULL, "AntennaCorba"));
#endif

  // I used to attempt to connect to the control program at this
  // point, but this is quite dangerous, since AntennaMaster is
  // waiting for us to exit the constructor before proceeding.  If we
  // were to connect, and receive a slew of initialization commands,
  // which we would forward to the master, we might fill the master's
  // message queue, causing us to block, which would cause a deadlock,
  // since the master is waiting for the thread which instantiated us
  // to call broadcastReady(), which can only happen when we exit this
  // constructor.  Instead, we must rely on the master to tell us when
  // it's ok to connect by sending us a connect message.

  // Start threads managed by this task

  startThreads(this);

#if DIR_USE_ANT_CORBA  
  if(antennaCorba_)
    antennaCorba_->initializeAntenna();
#endif
}

/**.......................................................................
 * Destructor.
 */
AntennaControl::~AntennaControl() 
{
  DBPRINT(true, Debug::DEBUG6, "Inside AntennaControl destructor");

  disconnect();

  if(forwarder_) {
    delete forwarder_;
    forwarder_ = 0;
  }
};

/*.......................................................................
 * Connect to the controller port of the control program.
 */
bool AntennaControl::connect()				 
{
  // Terminate any existing connection.
  
  disconnect();

  // Attempt to open a non-blocking connection to the server
  
  if(client_.connectToServer(parent_->host(), 
			     TRANS_ANT_CONTROL_PORT, true) < 0) {
    ErrorDef(err, "AntennaControl::connect: Error in tcp_connect().\n");
    return false;
  }

  // Once we've successfully achieved a connection, reconfigure the
  // socket for non-blocking I/O

  client_.setBlocking(false);

  // Attach the NetMsg I/O streams to the new client socket.  The
  // NetCmd streams will be attached when the connection has been
  // proven

  netCommHandler_.getNetMsgHandler()->attach(client_.getFd());
  
  // Register a handler to be called when a message has been sent

  netCommHandler_.getNetMsgHandler()->
    installSendHandler(netMsgSentHandler, this);

  // Register a handler to be called when a message has been read

  netCommHandler_.getNetMsgHandler()->
    installReadHandler(netMsgReadHandler, this);

  // Install a handler to be called if any errors occur while
  // communicating with the control program

  netCommHandler_.getNetMsgHandler()->
    installErrorHandler(netErrorHandler, this);

  // Register the socket to be watched for input

  fdSet_.registerReadFd(client_.getFd());
  
  // Send a greeting message to the controller.

  sendAntennaIdMsg();

  return true;
}

/**.......................................................................
 * Pack a greeting message to be sent to the controller.
 */
void AntennaControl::sendAntennaIdMsg()
{
  DBPRINT(true, Debug::DEBUG6, "Inside packGreeting()");

  // Pack the message into our network buffer.

  netCommHandler_.getNetMsgHandler()->
    packAntennaIdMsg(parent_->getAnt()->getIntId());

  // And set the fd to be watched for writability

  fdSet_.registerWriteFd(client_.getFd());
}

/**.......................................................................
 * Disconnect the connection to the control-program control port.
 */
void AntennaControl::disconnect()
{
  if(client_.isConnected()) 
    fdSet_.clear(client_.getFd());

  // Disconnect from the server socket.

  DBPRINT(true, Debug::DEBUG6, "About to disconnect from the server socket");
  client_.disconnect();

  // Detach network handlers

  netCommHandler_.attach(-1);
}

/**.......................................................................
 * Service our message queue.
 */
void AntennaControl::serviceMsgQ()
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
		  NULL, NULL);
    

    // A message on our message queue?

    DBPRINT(true, Debug::DEBUG6, "Checking fd (msgFd): " << msgq_.fd());

    if(fdSet_.isSetInRead(msgq_.fd())) {
      DBPRINT(true, Debug::DEBUG6, "msgq fd is set");
      processTaskMsg(&stop);
    }
    
    // Command input from the controller?  Note that the check for
    // commands should come before the check for messages, as receipt
    // of a greeting message may change the state of the NetCmdHandler
    // fd.

    DBPRINT(true, Debug::DEBUG6, "Checking fd (netCmd read): "
	    << netCommHandler_.getNetCmdHandler()->getReadFd());
    
    if(fdSet_.isSetInRead(netCommHandler_.getNetCmdHandler()->getReadFd())) {
      netCommHandler_.readNetCmd();
    }
    
    // Are we waiting for a message from the controller?
    
    DBPRINT(true, Debug::DEBUG6, "Checking fd (netMsg read): " 
	    << netCommHandler_.getNetMsgHandler()->getReadFd());
    
    if(fdSet_.isSetInRead(netCommHandler_.getNetMsgHandler()->getReadFd())) {
      netCommHandler_.readNetMsg();
    }
    
    // Are we waiting to send a message to the controller?
    
    DBPRINT(true, Debug::DEBUG6, "Checking fd (netMsg send): "
	    << netCommHandler_.getNetMsgHandler()->getSendFd());

    if(fdSet_.isSetInWrite(netCommHandler_.getNetMsgHandler()->getSendFd())) {
      netCommHandler_.sendNetMsg();
    }
  }
}

/**.......................................................................
 * Return true if the scanner connection is open
 */
bool AntennaControl::isConnected() 
{
  return client_.isConnected();
}

/**.......................................................................
 * Process a message received on the AntennaControl message queue
 *
 * Input:
 *
 *   msg AntennaControlMsg* The message received on the AntennaControl 
 *                   message queue.
 */
void AntennaControl::processMsg(AntennaControlMsg* msg)
{
  switch (msg->type) {
  case AntennaControlMsg::CONNECT:
    connectControl(false);
    break;
  case AntennaControlMsg::NETMSG: // A message to be sent to the ACC
    packNetMsg(msg);
    break;
  case AntennaControlMsg::WRITE_CARMA_SEQ_NO:
    writeCarmaSeqNo(msg);
    break;
  case AntennaControlMsg::WRITE_CARMA_MONITORS:
    writeCarmaMonitorPoints();
    break;
  default:
    break;
  }
}

/**.......................................................................
 * Pack a network message intended for the ACC
 */
void AntennaControl::packNetMsg(AntennaControlMsg* msg)
{
  // Pack the message into our network buffer.

  netCommHandler_.packNetMsg(msg->getNetMsg());

  // Arrange to be told when the control program socket is ready for
  // the message. Meanwhile since only one message can be queued to
  // the control program at a time, arrange not to listen for
  // controller message queue messages until this message is sent.
  
  // Do we have a connection?  If not, don't remove the message queue!

  if(client_.getFd() > 0) {
    fdSet_.registerWriteFd(client_.getFd());
    fdSet_.clearFromReadFdSet(msgq_.fd());
  }
}

/**.......................................................................
 * Attempt to connect to the host
 */
void AntennaControl::connectControl(bool reEnable)
{
  DBPRINT(true, Debug::DEBUG6, "In connetControl, client is: "
	  << (client_.isConnected() ? "connected" : "not connected"));

  // Ignore any further connection requests if we are in the process
  // of handshaking with the control program, or if we are already
  // connected (there may be a delay between the message getting
  // through to the master process that we have connected, and receipt
  // of another prod to connect.

  if(client_.isConnected())
    return;

  // Else attempt to connect

  if(connect())
    sendControlConnectedMsg(true);

  // Only send a message to reenable the connect timer if it was
  // previously disabled.

  else if(reEnable)
    sendControlConnectedMsg(false);
}

/**.......................................................................
 * Disconnect from the host.
 */
void AntennaControl::disconnectControl()
{
  // Disconnect and let the parent know it should re-enable the
  // connect timer.

  disconnect();

  // And send a message to reenable the connect timer.

  sendControlConnectedMsg(false);
}

/**.......................................................................
 * Respond to a network error
 */
NET_ERROR_HANDLER(AntennaControl::netErrorHandler)
{
  DBPRINT(true, Debug::DEBUG6, "A network error occurred");

  AntennaControl* control = (AntennaControl*) arg;

  control->disconnectControl();
}

/**.......................................................................
 * Send a message to the parent about the connection status of the
 * host
 */
void AntennaControl::sendControlConnectedMsg(bool connected) 
{
  AntennaMasterMsg msg;

  msg.packControlConnectedMsg(connected);

  parent_->forwardMasterMsg(&msg);
}

/**.......................................................................
 * Send a message to the control thread
 */
void AntennaControl::sendNetMsg(std::string& logStr, bool isErr)
{
  AntennaControlMsg msg;
  NetMsg* netMsg = msg.getNetMsg();

  // Iterate sending the message in segments of size
  // NetMsg::maxMsgLen() until the whole thing is sent

  unsigned seq = control_->logMsgHandler_.nextSeq();
  control_->logMsgHandler_.append(seq, logStr);

  std::string message;
  bool isLast = false;

  do {

    message = control_->logMsgHandler_.getNextMessageSubstr(seq, netMsg->maxMsgLen(), isLast);
    netMsg->packLogMsg(message, isErr, seq, isLast);
    control_->sendTaskMsg(&msg);

  } while(!isLast);
}

/**.......................................................................
 * Send a log string to be logged back to the control program.
 */
LOG_HANDLER_FN(AntennaControl::sendLogMsg)
{
  sendNetMsg(logStr, false);
}

/**.......................................................................
 * Send an error string to be logged back to the control program.
 */
LOG_HANDLER_FN(AntennaControl::sendErrMsg)
{
  sendNetMsg(logStr, true);
}

/**.......................................................................
 * Thread startup function for the Antenna Control task.
 */
THREAD_START(AntennaControl::startAntennaCorba)
{  
  AntennaControl* parent = (AntennaControl*) arg;
  Thread* thread = 0;
  
  thread = parent->getThread("AntennaCorba");
  
  // Instantiate the subsystem object
  
  try {
    parent->antennaCorba_ = new sza::antenna::corba::AntennaCorba(parent);
    parent->antennaCorba_->initialize();
  } catch(Exception& err) {
    COUTCOLOR("Caught an exception: " << err.what(), "red");
    throw err;
  } catch(...) {
    COUTCOLOR("Caught a CARMA error: " << carma::util::getStringForCaught(), "red");
    carma::util::rethrowCaughtAsUser();
  }
  
  // Set our internal thread pointer pointing to the AntennaControl
  // thread
  
  parent->antennaCorba_->thread_ = thread;
  
  // Let other threads know we are ready
  
  thread->broadcastReady();
  
  // Finally, block in the run method.
  
  parent->antennaCorba_->run();
  
  return 0;
}

/**.......................................................................
 * AntennaControl thread cleanup function
 */
THREAD_CLEAN(AntennaControl::cleanAntennaCorba)
{
  AntennaControl* parent = (AntennaControl*) arg;
  
  if(parent->antennaCorba_ != 0) {
    delete parent->antennaCorba_;
    parent->antennaCorba_ = 0;
  }
}

/**.......................................................................
 * A method called when a message has been sent to the control program
 */
NET_SEND_HANDLER(AntennaControl::netMsgSentHandler)
{
  AntennaControl* control = (AntennaControl*)arg;
  NetCommHandler* handler = &control_->netCommHandler_;
  
  DBPRINT(true, Debug::DEBUG7, "");

  // Remove the fd from the set to be watched

  control_->
    fdSet_.clearFromWriteFdSet(handler->getNetMsgHandler()->getSendFd());

  // And make sure we are listening to our message queue.  Apart from
  // the initial id message, sending of all further messages to the
  // control program causes us to stop listening to our message queue
  // until they are sent.

  control_->fdSet_.registerReadFd(control_->msgq_.fd());
}

/**.......................................................................
 * A method called when a message has been read from the control program
 */
NET_READ_HANDLER(AntennaControl::netMsgReadHandler)
{
  AntennaControl* control = (AntennaControl*)arg;
  NetCommHandler* handler = &control_->netCommHandler_;
  NetMsg* msg = handler->getNetMsgHandler()->getLastReadNetMsg();

  // See what the message was.

  switch(msg->type) {

    // After we have sent an ID message to the control program, it
    // should respond with a greeting message

  case NetMsg::GREETING:
    control_->parseGreetingMsg(msg);
    break;

    // Any else is an error.  Once we have received a greeting
    // message, we should receive no furter messages from the control
    // program -- only commands.

  default:
    control_->disconnectControl();
    break;
  }
}

/**.......................................................................
 * A handler called wen a command has been read from the control
 * program.
 */
NET_READ_HANDLER(AntennaControl::netCmdReadHandler)
{
  AntennaControl* control = (AntennaControl*)arg;

  control->forwarder_->
    forwardNetCmd(control->netCommHandler_.getLastReadNetCmd());
}

/**.......................................................................
 * Read a greeting message from the control program, and decide what
 * to do
 */
void AntennaControl::parseGreetingMsg(NetMsg* msg)
{
  DBPRINT(true, Debug::DEBUG6, "");

  if((msg->body.msg.greeting.revision != ARRAYMAP_REVISION) ||
     (msg->body.msg.greeting.nReg     != parent_->getShare()->getNreg()) ||
     (msg->body.msg.greeting.nByte    != parent_->getShare()->getNbyte())) {

    // If there is a register mismatch, we cannot continue.

    LogStream errStr;
    errStr.appendMessage(true, "Register map mismatch wrt host\n");
    throw Error(errStr);

    // Else set up for normal control operations

  } else {

    // Install logging handlers by which other tasks can send messages
    // to the control program

    Logger::setPrefix(parent_->getAnt()->getLoggerPrefix());
    Logger::installLogHandler(AntennaControl::sendLogMsg);
    Logger::installErrHandler(AntennaControl::sendErrMsg);

    NetMsgHandler* msgHandler = netCommHandler_.getNetMsgHandler();
    NetCmdHandler* cmdHandler = netCommHandler_.getNetCmdHandler();
    unsigned fd = msgHandler->getReadFd();

    // Once the greeting message has been received, we shouldn't get
    // any more messages

    fdSet_.clearFromReadFdSet(msgHandler->getReadFd());
    msgHandler->attachReadStream(-1);

    // Now that the greeting message has been received, attach the
    // command handler stream to the control fd, and (re)register the
    // read fd.

    cmdHandler->attachReadStream(fd);
    fdSet_.registerReadFd(fd);

    // And register handlers to be called when a command has been
    // read, or if an error occurs while communicating.

    netCommHandler_.getNetCmdHandler()->
      installReadHandler(netCmdReadHandler, this);

    netCommHandler_.getNetCmdHandler()->
      installErrorHandler(netErrorHandler, this);
  }
}

/**.......................................................................
 * Write a sequence number expected by the CARMA control system
 */
void AntennaControl::writeCarmaSeqNo(AntennaControlMsg* msg)
{
  if(antennaCorba_) {
    antennaCorba_->writeCarmaSeqNo(msg->body.carmaSeqNo.seq_, 
				   msg->body.carmaSeqNo.type_, 
				   msg->body.carmaSeqNo.success_);
  }
}

/**.......................................................................
 * Write CARMA monitor points
 */
void AntennaControl::writeCarmaMonitorPoints()
{
  if(antennaCorba_) {
    antennaCorba_->writeCarmaMonitorPoints();
  }
}

SzaShare* AntennaControl::getShare()
{
  return parent_->getShare();
}
