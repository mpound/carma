#include "carma/szautil/Client.h"
#include "carma/szautil/SignalTask.h"
#include "carma/szautil/TimeVal.h"

#include<iostream>

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
Client::Client(bool spawnThread, std::string host, unsigned port, 
	       unsigned readBufSize, unsigned sendBufSize, bool spawnSignalHandler) :
  Runnable(spawnThread, runFn)
{
  signalTask_ = 0;
  spawnSignalTask_ = spawnSignalHandler;

  initMembers(host, port, readBufSize, sendBufSize);

  // Put this in inherited classes, so that if we are spawning in a
  // separate thread, the inheritor constructor will finish before
  // spawn() is called.  (if spawnThread is true, then this call to
  // spawn() would block in the thread startup function

  //  spawn(this);
}

void Client::initMembers(std::string host, unsigned port, 
			 unsigned readBufSize, unsigned sendBufSize)
{
  tcp_.setHost(host);
  tcp_.setPort(port);

  handler_.installReadHandler(readHandler, (void*)this);
  handler_.installSendHandler(sendHandler, (void*)this);

  handler_.installReadErrorHandler(errHandler, (void*)this);
  handler_.installSendErrorHandler(errHandler, (void*)this);

  stop_ = false;

  timeOut_.setSeconds(1);
  timeOutPtr_ = timeOut_.timeVal();

  if(spawnSignalTask_) {

    // Finally, spawn a thread for managing signal handling

    signalTask_ = new SignalTask(true);

    // And install a signal handler for SIGINT

    signalTask_->sendInstallSignalMsg(SIGINT, &shutDown, this);
  }
}

void Client::disconnect()
{
  tcp_.disconnect();

  fdSet_.clearFromReadFdSet(handler_.getReadFd());
  fdSet_.clearFromWriteFdSet(handler_.getSendFd());

  handler_.attach(-1);

  // And set a timer to reconnect!
  
  timeOutPtr_ = timeOut_.timeVal();
}

void Client::connect()
{
  if(tcp_.connectToServer(true) > 0) {
    handler_.attach(tcp_.getFd());
    fdSet_.registerReadFd(handler_.getReadFd());
    timeOutPtr_ = NULL;
  } else {
    timeOut_.reset();
  }
}

/**.......................................................................
 * Destructor.
 */
Client::~Client() 
{
  disconnect();

  // Shut down the signal task

  if(signalTask_ != 0) {
    delete signalTask_;
    signalTask_ = 0;
  }
}

/**.......................................................................
 * Send data to the server
 */
void Client::sendServerData(NetDat& dat)
{
  std::vector<unsigned char>& data = dat.getSerializedData();
  unsigned datSize = data.size();

  // Resize the send buffer if this object is larger than the
  // current send buffer size
  
  if(datSize+8 > sendBufSize_)
    setSendBufSize(datSize + 8);

  handler_.getSendStr()->startPut(data.size());
  handler_.getSendStr()->putChar(data.size(), &data[0]);
  handler_.getSendStr()->endPut();
  
  // And register this client fd to be watched for writability

  fdSet_.registerWriteFd(handler_.getSendFd());
}

/**.......................................................................
 * Static method to be called when a message is fully read from the server
 */
NET_READ_HANDLER(Client::readHandler)
{
  Client* client = (Client*)arg;
  client->readServerData(client->handler_);
}

/**.......................................................................
 * Static method to be called when a message is fully sent to the server
 */
NET_SEND_HANDLER(Client::sendHandler)
{
  Client* client = (Client*)arg;
  client->fdSet_.clearFromWriteFdSet(client->handler_.getSendFd());
}

/**.......................................................................
 * Static method to be called when an error occurs reading or sending
 * data
 */
NET_ERROR_HANDLER(Client::errHandler)
{
  Client* client = (Client*)arg;
  client->disconnect();
}

/**.......................................................................
 * Block in select
 */
void Client::run()
{
  int nready=0;

  do {

    // Block in select() until one or more file descriptors are readable
  
    if((nready=select(fdSet_.size(), fdSet_.readFdSet(), fdSet_.writeFdSet(), 
		      NULL, timeOutPtr_)) < 0) 
    {
      ThrowSysError("select()");
    }

    if(nready > 0) {

      // read data received over the socket connection
      
      if(fdSet_.isSetInRead(handler_.getReadFd()))
	handler_.read();

      // send data over the socket connection
      
      if(fdSet_.isSetInWrite(handler_.getSendFd()))
	handler_.send();
   
    } else 
       connect();

  } while(!stop_);
}

/**.......................................................................
 * A blocking run method
 */
RUN_FN(Client::runFn)
{
  Client* runnable = (Client*) arg;
  runnable->run();
}

void Client::setReadBufSize(unsigned size)
{
  handler_.setReadBuffer(0, size);
  readBufSize_ = size;
}

void Client::setSendBufSize(unsigned size)
{
  handler_.setSendBuffer(0, size);
  sendBufSize_ = size;
}

void Client::readServerData(NetHandler& handler)
{
  int size;

  handler.getReadStr()->startGet(&size);

  // Changing this now to allow for variable network object sizes.
  // The network buffer can change size if a message larger than the
  // previously allocated buffer size is encountered, therefore our
  // byte array will be resized accordingly.  
  //
  // Note that the network buffer only changes size to accomodate
  // larger messages, so that we do not reallocate just because a
  // shorter message was encountered.

  if(size > bytes_.size()) {
    bytes_.resize(size);
  }

  handler.getReadStr()->getChar(size, &bytes_[0]);
  handler.getReadStr()->endGet();

  sizeInBytesOfLastMessage_ = size;

  processServerData();
}

/**.......................................................................
 * A shutdown method
 */
SIGNALTASK_HANDLER_FN(Client::shutDown)
{
  Client* client = (Client*) args;
  client->stop_ = true;
  COUT("Setting client stop tp true");
}
