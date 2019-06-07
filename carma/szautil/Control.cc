#include <curses.h>
#include <term.h>
#include <readline/readline.h>
#include <readline/history.h>

#include "carma/szautil/Control.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/TimeVal.h"

#include "carma/szaarrayutils/control.h"
#include <cstring>

using namespace sza::util;
using namespace std;

Control* Control::control_ = 0;

/**.......................................................................
 * Constructor.
 */
Control::Control(string host, bool log) :
  stop_(false), client_(host, CP_CONTROL_PORT), signalTask_(true)
{
  LogStream logStr;
  
  control_ = this;
  
  // Register the exit handler with signalTask.
  
  signalTask_.sendInstallSignalMsg(SIGINT,  exitHandler);
  signalTask_.sendInstallSignalMsg(SIGQUIT, exitHandler);
  signalTask_.sendInstallSignalMsg(SIGABRT, exitHandler);

  // Allocate our thread objects

  readThread_ = new Thread(startRead, NULL, NULL, "read");
  sendThread_ = new Thread(startSend, NULL, NULL, "send");

  // Allocate the buffers used to communicate with the control program
  
  netStr_.setReadBuffer(0, NET_PREFIX_LEN + net_max_obj_size(&cc_msg_table));
  netStr_.setSendBuffer(0, NET_PREFIX_LEN + net_max_obj_size(&cc_cmd_table));
  
  // Install handlers for the network streams
  
  netStr_.getSendStr()->installSendHandler(&sendHandler, this);
  netStr_.getReadStr()->installReadHandler(&readHandler, this);

  netStr_.getSendStr()->installErrorHandler(&errorHandler, this);
  netStr_.getReadStr()->installErrorHandler(&errorHandler, this);
  
  // Attempt to connect 
  
  if(client_.connectToServer(true) < 0) {
    logStr.appendMessage(true, "Unable to connect to the control host");
    throw Error(logStr);
  }
  
  // And attach our streams to the file descriptor
  
  netStr_.attach(client_.getFd());

  // Print a greeting message

  cout << "Type 'exit' or cntl-C to exit" << endl;

  // If everything checks out, run our threads
  
  if(log)
    readThread_->start(this);

  // The calling thread will block in startup, since broadcastReady()
  // is not called in the startup function for this thread.

  sendThread_->start(this);
}

/**.......................................................................
 * Non-interactive constructor.
 */
Control::Control(string host) :
  stop_(false), client_(host, CP_CONTROL_PORT), signalTask_(false)
{
  control_ = this;
  
  // Allocate our thread objects

  readThread_ = 0;
  sendThread_ = 0;

  // Allocate the buffers used to communicate with the control program
  
  netStr_.setReadBuffer(0, NET_PREFIX_LEN + net_max_obj_size(&cc_msg_table));
  netStr_.setSendBuffer(0, NET_PREFIX_LEN + net_max_obj_size(&cc_cmd_table));
  
  // Install handlers for the network streams
  
  netStr_.getSendStr()->installSendHandler(&sendHandler, this);
  netStr_.getReadStr()->installReadHandler(&readHandler, this);

  netStr_.getSendStr()->installErrorHandler(&errorHandler, this);
  netStr_.getReadStr()->installErrorHandler(&errorHandler, this);
  
  // Attempt to connect 
  
  if(client_.connectToServer(true) < 0) {
    ThrowError("Unable to connect to the control host");
  }
  
  // And attach our streams to the file descriptor
  
  netStr_.attach(client_.getFd());

  // Print a greeting message

  cout << "Type 'exit' or cntl-C to exit" << endl;
}

void Control::disconnect()
{
  control_->client_.disconnect();
}

/**.......................................................................
 * Destructor.
 */
Control::~Control() 
{
  // Disconnect from the ACC

  control_->client_.disconnect();

  // And delete our threads

  if(readThread_)
    delete readThread_;

  if(sendThread_)
    delete sendThread_;
}

/**.......................................................................
 * Run method which uses readline
 */
void Control::processCommands()
{
  LogStream logStr;
  char* buff=NULL;
  sza::array::CcNetCmd netCmd;

  while(!stop_ && (buff=readline("\rszaCommand> ")) != NULL) {
    
    add_history(buff);
    
    if(strlen(buff) > sza::array::CC_CMD_MAX) {
      logStr.appendMessageSimple(true, "Line is too long");
      logStr.report();
    } else {
      
      string buffStr(buff);

  // Definitions of "standardized" C++ methods appear to be in flux.
  // In particular, the string class compare method changed the order
  // of its arguments somewhere between gcc 2.96 and gcc 3.2.2

#if (__GNUC__ > 2)
      if(buffStr.compare(0, 4, "exit")==0)
#else
      if(buffStr.compare("exit", 0, 4)==0)
#endif
	exitHandler(1, NULL);
      else {

	// Else pack it into our send buffer
	
	strcpy(netCmd.input.cmd, buff);
	
	// Pack the message into the output network buffer.
	
	NetSendStr* nss = netStr_.getSendStr();
	
	nss->startPut(sza::array::CC_INPUT_CMD);
	nss->putObj(&cc_cmd_table, sza::array::CC_INPUT_CMD, &netCmd);
	nss->endPut();
	
	// And send it
	
	netStr_.send();
      }
    }
    free(buff);
    buff = NULL;
  }

  // Call the exit handler on EOF

  if(!stop_)
    exitHandler(1, NULL);
}

/**.......................................................................
 * Send a command to the control program
 */
void Control::sendCommand()
{
  netStr_.send();
}

/**.......................................................................
 * Method called when a command has been sent.
 */
NET_SEND_HANDLER(Control::sendHandler)
{
  Control* control = (Control*)arg;
  control->fdSet_.clearFromWriteFdSet(control->client_.getFd());
}

/**.......................................................................
 * Process messages received from the control program
 */
void Control::processMessages()
{
  int nready;
  fdSet_.registerReadFd(client_.getFd());

  while(!stop_) {

    nready = select(fdSet_.size(), fdSet_.readFdSet(), fdSet_.writeFdSet(),
		    NULL, NULL);

    if(fdSet_.isSetInRead(client_.getFd()))
      readMessage();
  }
}

void Control::readMessage() 
{
  netStr_.read();
};

/**.......................................................................
 * Method called when a message has been read.
 */
NET_READ_HANDLER(Control::readHandler)
{
  Control* control = (Control*)arg;

  sza::array::CcNetMsg netMsg;
  sza::array::CcNetMsgId opcode;

  NetReadStr* nrs = control->netStr_.getReadStr();

  nrs->startGet((int*)&opcode);
  nrs->getObj(&cc_msg_table, opcode, &netMsg);
  nrs->endGet();

  switch(opcode) {
  case sza::array::CC_LOG_MSG:
    cout << "\r" << "           " << "\r" << netMsg.log.text << endl << "szaCommand>";
    fflush(stdout);
    break;
  case sza::array::CC_REPLY_MSG:
    cout << "\r" << "           " << "\r" << netMsg.reply.text << endl << "szaCommand>";
    fflush(stdout);
    break;
  case sza::array::CC_SCHED_MSG:
    cout << "\r" << "           " << "\r" << netMsg.sched.text << endl << "szaCommand>";
    fflush(stdout);
    break;
  case sza::array::CC_ARC_MSG:
    cout << "\r" << "           " << "\r" << netMsg.arc.text << endl << "szaCommand>";
    fflush(stdout);
    break;
  case sza::array::CC_ANT_MSG:
    cout << "\r" << "           " << "\r" << netMsg.ant.text << endl << "szaCommand>";
    fflush(stdout);
    break;
  default:
    break;
  }
}

/**.......................................................................
 * Method called when an error occurs communicating with the control
 * program
 */
NET_SEND_HANDLER(Control::errorHandler)
{
  Control* control = (Control*)arg;
  control->exitHandler(1, NULL);
}

/**.......................................................................
 * Exit handler
 */
SIGNALTASK_HANDLER_FN(Control::exitHandler)
{
  LogStream logStr;

  logStr.appendMessageSimple(sigNo != 0, 
			     "Lost connection to the control program");
  logStr.report();

  // When the exit handler is called, the ready condition variable is
  // signalled, causing the main thread to exit its constructor.

  control_->sendThread_->broadcastReady();

  control_->stop_ = true;
}

THREAD_START(Control::startSend)
{
  Control* control = (Control*)arg;

  control->processCommands();
  return 0;
}

THREAD_START(Control::startRead)
{
  Control* control = (Control*)arg;

  control->readThread_->broadcastReady();
  control->processMessages();
  return 0;
}

void Control::sendCommand(std::string command) 
{
  if(command.size() > sza::array::CC_CMD_MAX) {
    ReportMessage("Line is too long");
  } else {
    
    sza::array::CcNetCmd netCmd;

    // Else pack it into our send buffer
	
    strcpy(netCmd.input.cmd, command.c_str());
	
    // Pack the message into the output network buffer.
	
    NetSendStr* nss = netStr_.getSendStr();
	
    nss->startPut(sza::array::CC_INPUT_CMD);
    nss->putObj(&cc_cmd_table, sza::array::CC_INPUT_CMD, &netCmd);
    nss->endPut();
    
    // And send it
	
    netStr_.send();
  }
}
