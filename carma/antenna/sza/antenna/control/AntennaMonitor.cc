#include <iostream>

#include <sys/time.h>
#include <sys/types.h>
#include <signal.h>
#include <setjmp.h>
#include <unistd.h>

#include "carma/szaarrayutils/arraymaprev.h"
#include "carma/szaarrayutils/scanner.h"

#include "carma/szautil/Debug.h"

#include "carma/antenna/sza/antenna/control/AntennaMonitor.h"
#include "carma/antenna/sza/antenna/control/AntennaMaster.h"
#include "carma/antenna/sza/antenna/control/AntennaMonitor.h"
#include "carma/antenna/sza/antenna/control/SzaShare.h"


using namespace sza::util;
using namespace sza::antenna::control;

/**.......................................................................
 * Create a AntennaMonitor class in namespace carma
 */
AntennaMonitor::AntennaMonitor(AntennaMaster* master) :
  SzaTask(), sza::util::GenericTask<AntennaMonitorMsg>::GenericTask(),
  parent_(master), scanner_(0)
{ 
  // Check arguments

  if(master == 0)
    throw Error("AntennaDrive::AntennaDrive: Received NULL arguments\n");

  connectionPending_ = false;
  dispatchPending_   = false;

  // Get a pointer to the shared resource object

  share_   = parent_->getShare();

  // Create the scanner object. 

  scanner_ = new Scanner(share_, parent_->getAnt());

  DBPRINT(true, Debug::DEBUG3, "About to connect to scanner");

  connectScanner(true);
};

/**.......................................................................
 * Destructor function
 */
AntennaMonitor::~AntennaMonitor()
{
  bool waserr = false;

  // Clean up resources allocated by this task

  if(scanner_ != 0)
    delete scanner_;
};

/**.......................................................................
 * Service our message queue.
 */
void AntennaMonitor::serviceMsgQ()
{
  bool stop   = false;
  int  nready = 0;
  int  msgqFd = msgq_.fd();
  
  // Register the msgq to be watched for readability  

  fdSet_.registerReadFd(msgqFd);
  
  // Initially our select loop will only check the msgq file
  // descriptor for readability, but attempt to connect to the control
  // port every two seconds.  Once a connection is achieved, the
  // select loop will block until either a message is received on the
  // message queue, or on the control port.
  
  while(!stop) {
    
    nready=select(fdSet_.size(), fdSet_.readFdSet(), fdSet_.writeFdSet(), 
		  NULL, NULL);

    // A message on our message queue?
    
    if(fdSet_.isSetInRead(msgqFd)) {
      processTaskMsg(&stop);
    }
    
    // Send a pending data frame to the translator?
    
    if(fdSet_.isSetInWrite(netDataFrameHandler_.getSendFd())) {
      netDataFrameHandler_.send();
    }
       
    // Send a message to the translator?
       
    if(fdSet_.isSetInWrite(netMsgHandler_.getSendFd())) {
      netMsgHandler_.send();
    }

    // Read a message from the translator?
       
    if(fdSet_.isSetInRead(netMsgHandler_.getReadFd())) {
      netMsgHandler_.read();
    }
  }
}

/**.......................................................................
 * Process messages received from the AntennaMonitor message queue
 *
 * Input:
 *  taskMsg  *  AntennaMsg  The message received from the message queue
 */
void AntennaMonitor::processMsg(AntennaMonitorMsg* msg)
{
  switch (msg->type) {
  case AntennaMonitorMsg::PACK_DATAFRAME:
    packNextFrame();
    break;
  case AntennaMonitorMsg::DISPATCH_DATAFRAME:
    dispatchNextFrame();
    break;
  case AntennaMonitorMsg::CONNECT:

    // Attempt to connect.  On success, disable the scanner_connect
    // timer.

    if(connect())
      sendScannerConnectedMsg(true);

    break;
  default:
    throw (Error("AntennaMonitor::processMsg: Unrecognized message type.\n"));
    break;
  }
};

/**.......................................................................
 * Send a message to dispatch the next frame.
 */
void AntennaMonitor::sendDispatchDataFrameMsg()
{
  AntennaMonitorMsg msg;

  msg.packDispatchDataFrameMsg();

  sendTaskMsg(&msg);
}

/**.......................................................................
 * A handler to be called when a frame has been completely sent.
 */
NET_SEND_HANDLER(AntennaMonitor::netDataFrameSentHandler)
{
  AntennaMonitor* monitor =  (AntennaMonitor*)arg;

  // Mark the transaction as finished

  monitor->dispatchPending_ = false;

  // And stop watching the file descriptor

  monitor->fdSet_.clearFromWriteFdSet(monitor->netDataFrameHandler_.getSendFd());

  // If there are any legacy frames waiting in the queue, keep sending
  // until the buffer is drained.

  if(monitor->scanner_->getNframesInQueue() > 1)
    monitor->sendDispatchDataFrameMsg();
}

/**.......................................................................
 * A handler to be called when an error occurs in communication
 */
NET_ERROR_HANDLER(AntennaMonitor::netErrorHandler)
{
  AntennaMonitor* scanner =  (AntennaMonitor*)arg;

  scanner->disconnectScanner();
}

/**.......................................................................
 * If there is room in the circular frame buffer, record another
 * data frame and push it onto the event channel.
 */
void AntennaMonitor::packNextFrame()
{
  // Pack the current frame into our frame buffer.

  scanner_->packNextFrame();

  // And send a message to dispatch a frame

  sendDispatchDataFrameMsg();
}

/**.......................................................................
 * If there is room in the circular frame buffer, record another
 * data frame and push it onto the event channel.
 */
void AntennaMonitor::dispatchNextFrame()
{
  // If we are currently in the process of sending a frame, or have no
  // connection to the control program, do nothing.
  
  if(connectionPending_ || dispatchPending_ || !client_.isConnected())
    return;

  // Get the next frame (if any) to be dispatched

  DataFrameManager* frame = scanner_->dispatchNextFrame();

  if(frame == 0)
    return;

  // Pack the frame into our network buffer.

  packFrame(frame->frame());

  // And register the archiver file descriptor to be watched for
  // writability

  fdSet_.registerWriteFd(client_.getFd());

  // Mark the transaction as in progress.

  dispatchPending_ = true;
}

/**.......................................................................
 * Attempt to connect to the host
 */
void AntennaMonitor::connectScanner(bool reEnable)
{
  // Ignore this call if we are already connected.

  if(client_.isConnected())
    return;

  // If we successfully connected, let the parent know.

  if(connect())
    sendScannerConnectedMsg(true);

  else if(reEnable)
    sendScannerConnectedMsg(false);
}

/**.......................................................................
 * Send a message to the parent about the connection status of the
 * host
 */
void AntennaMonitor::sendScannerConnectedMsg(bool connected) 
{
  AntennaMasterMsg msg;

  msg.packScannerConnectedMsg(connected);

  parent_->forwardMasterMsg(&msg);
}

//-----------------------------------------------------------------------
// Network functions
//-----------------------------------------------------------------------

/**.......................................................................
 * Connect to the archiver port of the control program.
 *
 * Return:
 *  return     bool    true  - OK.
 *                     false - Error.
 */
bool AntennaMonitor::connect()
{

  COUT("Attempting to connect to the control program at: " << parent_->host());

  // Terminate any existing connection.
  
  disconnect();

  if(client_.connectToServer(parent_->host(), TRANS_ANT_SCANNER_PORT, true) < 0) {
    ErrorDef(err, "AntennaMonitor::connect: Error in tcp_connect().\n");
    return false;
  }


  COUT("Successfully connected");

  // Once we've successfully achieved a connection, reconfigure the
  // socket for non-blocking I/O

  client_.setBlocking(false);

  // Attach the send stream of the message handler to the new client
  // socket.
  
  netMsgHandler_.attach(client_.getFd());

  // Install a handler to be called when a message is sent

  netMsgHandler_.installSendHandler(netMsgSentHandler, this);

  // Install a handler to be called when a message is read

  netMsgHandler_.installReadHandler(netMsgReadHandler, this);
  
  // Install a handler to be called when an error occurs

  netMsgHandler_.installErrorHandler(netErrorHandler, this);

  // Send a greeting message to the archiver.  We can continue to
  // service our message queue while sending the greeting, since the
  // greeting is the only thing we will ever send via the msg handler.

  // Register the socket to be watched for input

  fdSet_.registerReadFd(client_.getFd());
  
  // And mark the connection as pending

  connectionPending_ = true;

  // Now send a greeting message to the controller.

  sendAntennaIdMsg();

  return true;
}

/**.......................................................................
 * Pack a greeting message to be sent to the controller.
 */
void AntennaMonitor::sendAntennaIdMsg()
{
  // Pack the message into our network buffer.

  netMsgHandler_.packAntennaIdMsg(parent_->getAnt()->getIntId());

  // And set the fd to be watched for writability

  fdSet_.registerWriteFd(netMsgHandler_.getSendFd());
}

/**.......................................................................
 * A method called when a message has been read from the control program
 */
NET_READ_HANDLER(AntennaMonitor::netMsgReadHandler)
{
  AntennaMonitor* scanner = (AntennaMonitor*)arg;
  NetMsgHandler* handler = &scanner->netMsgHandler_;
  NetMsg* msg = handler->getLastReadNetMsg();

  // See what the message was.

  switch(msg->type) {

    // After we have sent an ID message to the control program, it
    // should respond with a greeting message

  case NetMsg::GREETING:
    scanner->parseGreetingMsg(msg);
    break;

    // Anything else is an error.  Once we have received a greeting
    // message, we should receive no furter messages from the control
    // program -- only commands.

  default:
    scanner->disconnectScanner();
    break;
  }
}

/**.......................................................................
 * A method called when the greeting message has been sent to the
 * control program.
 */
NET_SEND_HANDLER(AntennaMonitor::netMsgSentHandler)
{
  AntennaMonitor* scanner = (AntennaMonitor*)arg;
  NetMsgHandler* handler  = &scanner->netMsgHandler_;
  
  // Remove the fd from the set to be watched for writeability

  scanner->
    fdSet_.clearFromWriteFdSet(handler->getSendFd());

  // This is the last message we will send

  handler->attachSendStream(-1);
}

/**.......................................................................
 * Disconnect the connection to the control-program archiver.
 *
 * Input:
 *  sup    Supplier *  The resource object of the program.
 */
void AntennaMonitor::disconnect()
{
  // Clear the client fd from the set to be watched in select()

  if(client_.isConnected())
    fdSet_.clear(client_.getFd());

  // Clear any pending flags

  dispatchPending_ = false;

  // Disconnect from the server socket.

  client_.disconnect();

  // Detach network handlers

  netDataFrameHandler_.attach(-1);
  netMsgHandler_.attach(-1);
}


/**.......................................................................
 * Disconnect from the host.
 */
void AntennaMonitor::disconnectScanner()
{
  // Disconnect and let the parent know it should re-enable the
  // connect timer.

  disconnect();

  // And send a message to the master that we are disconnected.

  sendScannerConnectedMsg(false);
}

/**.......................................................................
 * Install the frame buffer as the network buffer and pre-format the
 * register frame output message.
 */
void AntennaMonitor::packFrame(sza::util::DataFrame* frame)
{
  NetSendStr* nss = netDataFrameHandler_.getSendStr();

  // Install the frame manager's data buffer as the network buffer.

  nss->setBuffer(frame->data(), frame->nByte());
  nss->startPut(0);
  nss->incNput(frame->nByte() - NET_PREFIX_LEN);
  nss->endPut();
}

/**.......................................................................
 * Read a greeting message from the control program, and decide what
 * to do
 */
void AntennaMonitor::parseGreetingMsg(NetMsg* msg)
{
  if((msg->body.msg.greeting.revision != ARRAYMAP_REVISION) ||
     (msg->body.msg.greeting.nReg     != parent_->getShare()->getNreg()) ||
     (msg->body.msg.greeting.nByte    != parent_->getShare()->getNbyte())) {

    // If there is a register mismatch, we cannot continue.

    LogStream errStr;
    errStr.appendMessage(true, "Register map mismatch wrt host\n");
    throw Error(errStr);

    // Else set up for normal control operations

  } else {

    // Now that the greeting message has been received, attach the
    // data frame handler stream to the fd

    netDataFrameHandler_.attachSendStream(netMsgHandler_.getReadFd());

    // And register handlers to be called when a data frame has been
    // sent.

    netDataFrameHandler_.installSendHandler(netDataFrameSentHandler, this);

    // Register a handler to be called if an error occurs while
    // sending a frame

    netDataFrameHandler_.installSendErrorHandler(netErrorHandler, this);

    // Once the greeting message has been received, we shouldn't get
    // any more messages

    fdSet_.clearFromReadFdSet(netMsgHandler_.getReadFd());  
    netMsgHandler_.attachReadStream(-1);

    // Finally, mark the connection as established

    connectionPending_ = false;
  }
}
