#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/SignalTask.h"
#include "carma/szautil/SerialClient.h"
#include "carma/szautil/String.h"
#include "carma/szautil/StringUtils.h"
#include "carma/szautil/TcpClient.h"
#include "carma/szautil/TcpListener.h"
#include "carma/szautil/Server.h"
#include "carma/szautil/Vector.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>

using namespace std;
using namespace sza::util;

/**.......................................................................
 * Constructor for a generic server
 */
Server::Server(bool spawnThread, int listenPort, unsigned readBufSize, 
	       unsigned sendBufSize) : 
  Runnable(spawnThread, runFn)
{
  initMembers(listenPort, readBufSize, sendBufSize);

  // Put this in inherited classes, so that if we are spawning in a
  // separate thread, the inheritor constructor will finish before
  // spawn() is called.  (if spawnThread is true, then this call to
  // spawn() would block in the thread startup function

  //  spawn(this);
}

/**.......................................................................
 * Initialize members
 */
void Server::initMembers(int listenPort, unsigned readBufSize, 
			 unsigned sendBufSize)
{
  listener_    =  0;
  signalTask_  =  0;
  stop_        =  false;
  readBufSize_ = readBufSize;
  sendBufSize_ = sendBufSize;

  // We will set the timeout to 1 second for our select() loop

  setTimeOutSeconds(1);

  // Finally, spawn a thread for managing signal handling

  signalTask_ = new SignalTask(true);

  // And install a signal handler for SIGINT

  signalTask_->sendInstallSignalMsg(SIGINT, &shutDown, this);

  // Start listening on the requested port

  listen(listenPort);
}

/**.......................................................................
 * Listen for connection requests on the specified port
 */
void Server::listen(unsigned port, unsigned nClients) 
{
  // Listen on the requested socket.
  
  if(nClients > 0) {
    listener_ = new TcpListener(port, nClients);
    fdSet_.registerReadFd(listener_->getFd());
  }
}

/**.......................................................................
 * Destructor.
 */
Server::~Server() 
{
  // Shut down the signal task

  if(signalTask_ != 0) {
    delete signalTask_;
    signalTask_ = 0;
  }

  // Free any memory allocated in this class

  for(std::list<ServerConnection*>::iterator iclient=clients_.begin();
      iclient != clients_.end(); iclient++) {
    delete *iclient;
  }

  // Free the listener

  if(listener_ != 0) {
    delete listener_;
    listener_ = 0;
  }
}

/**.......................................................................
 * Block in select
 */
void Server::run()
{
  int nready=0;

  // On entry to the loop, timeout immediately

  timeOut_.setIntervalInSeconds(0);
  timeOut_.activate(true);

  do {

    // Block in select() until one or more file descriptors are readable
  
    if((nready=select(fdSet_.size(), fdSet_.readFdSet(), fdSet_.writeFdSet(), NULL, 
		      timeOut_.tVal())) < 0) {
      ThrowSysError("select()");
    }

    if(nready > 0) {

      // Service a select()able event

      serviceSelect();

    } else {
      
      // Do whatever it is we are supposed to do on timeout

      timeOutAction();

      // And reset the timeout

      timeOut_.setIntervalInSeconds(timeOutSeconds_);
      timeOut_.reset();
    }

  } while(!stop_);
}

/**.......................................................................
 * Service a select()able event
 */
void Server::serviceSelect() 
{
  // Service requests received over the socket connection
  
  if(listener_ != 0 && fdSet_.isSetInRead(listener_->getFd()))
    acceptConnection();
  
  // Check connected clients for data
  
  checkClientsForReadableData();
  
  // Check connected clients for sendable data
  
  checkClientsForWritability();
}

/**.......................................................................
 * Respond to a connection request from a client
 */
void Server::acceptConnection()
{
  int fd = -1;
  
  // Allow the caller to connect.  The fd returned will be configured
  // for blocking I/O.
  
  fd = listener_->acceptConnection(true);
  
  // Insert a new connection into the list

  ServerConnection* conn = new ServerConnection(fd, readBufSize_, sendBufSize_, this);
  
  // And register the descriptor to be watched for input
  
  fdSet_.registerReadFd(fd);
  
  // Do anything inheritors define when a client connects

  CTOUT("About to call acceptClientAction with conn = " << conn);
  acceptClientAction(conn);

  // Only after acceptClientAction is done should we attempt to insert
  // the client in the list

  clients_.insert(clients_.begin(), conn);
}

/**.......................................................................
 * Check clients for writability
 */
void Server::checkClientsForWritability()
{
  for(std::list<ServerConnection*>::iterator iClient=clients_.begin();
      iClient != clients_.end(); iClient++) 
  {
    ServerConnection* client = *iClient;

    if(fdSet_.isSetInWrite(client->handler_.getSendFd()))
      client->handler_.send();
  }
}

/**.......................................................................
 * Check clients for data to be read
 */
void Server::checkClientsForReadableData() 
{
  std::vector<ServerConnection*> disconnectedClients_;

  for(std::list<ServerConnection*>::iterator iClient=clients_.begin();
      iClient != clients_.end(); iClient++) 
  {
    ServerConnection* client = *iClient;

    if(fdSet_.isSetInRead(client->handler_.getReadFd())) {

      client->handler_.read();

      // If after processing messages from this client, the client is
      // disconnected, mark it for removal

      if(client->handler_.getReadFd() < 0)
	disconnectedClients_.push_back(client);
    }
  }

  // Finally, remove any clients that were disconnected after reading

  for(std::vector<ServerConnection*>::iterator iClient=disconnectedClients_.begin();
      iClient != disconnectedClients_.end(); iClient++) 
  {
    ServerConnection* client = *iClient;
    clients_.remove(client);
    delete client;
  }
}

/**.......................................................................
 * Method to send data to all connected clients
 */
void Server::sendClientData(NetDat& dat, ServerConnection* client)
{
  // If a client was specified, send only to that client.  

  if(client) {
    client->packClientData(dat);

    // Else send to all clients, but only clients that are
    // initialized.  This is to prevent a server from packing data to
    // all clients that may overwrite the initialization data that a
    // client is waiting for

  } else {
    for(std::list<ServerConnection*>::iterator iClient=clients_.begin();
	iClient != clients_.end(); iClient++) {
      ServerConnection* client = *iClient;

      // Only send to this client if the client is initialized

      if(client->initialized_)
	client->packClientData(dat);
    }
  }
}

/**.......................................................................
 * A shutdown method
 */
SIGNALTASK_HANDLER_FN(Server::shutDown)
{
  Server* server = (Server*) args;
  server->stop_ = true;
  COUT("Setting Server STOP to true");
}

/**.......................................................................
 * A blocking run method
 */
RUN_FN(Server::runFn)
{
  Server* runnable = (Server*) arg;
  runnable->run();
}

/**.......................................................................
 * Static method to be called when a message is fully read from a client
 */
NET_ERROR_HANDLER(Server::errHandler)
{
  ServerConnection* conn = (ServerConnection*)arg;

  conn->parent_->fdSet_.clearFromReadFdSet(conn->handler_.getReadFd());
  conn->parent_->fdSet_.clearFromWriteFdSet(conn->handler_.getSendFd());
  conn->handler_.attach(-1);
}

/**.......................................................................
 * Static method to be called when a message is fully read from a client
 */
NET_READ_HANDLER(Server::readHandler)
{
  ServerConnection* conn = (ServerConnection*)arg;
  conn->parent_->readClientData(conn);
}

/**.......................................................................
 * Static method to be called when a message is fully sent to a client
 */
NET_SEND_HANDLER(Server::sendHandler)
{
  ServerConnection* conn = (ServerConnection*)arg;
  conn->parent_->fdSet_.clearFromWriteFdSet(conn->handler_.getFd());

  // Only after the first send is completed can we consider the client
  // initialized

  conn->initialized_ = true;

  // Check msg queue for other queued messages

  conn->checkMsgQueue();
}

void Server::setReadBufSize(unsigned size)
{
  readBufSize_ = size;
}

void Server::setSendBufSize(unsigned size)
{
  sendBufSize_ = size;
}

void Server::setTimeOutSeconds(unsigned int seconds)
{
  timeOutSeconds_ = seconds;
}

//=======================================================================
// ServerConnection class
//=======================================================================

/**.......................................................................
 * Constructor for ServerConnection class
 */
Server::ServerConnection::ServerConnection(int fd, unsigned readBufSize, unsigned sendBufSize, Server* parent)
{
  handler_.attach(fd);
  handler_.setReadBuffer(0, readBufSize);
  handler_.setSendBuffer(0, sendBufSize);
  parent_ = parent;
  initialized_ = false;

  handler_.installReadHandler(Server::readHandler, (void*)this);
  handler_.installSendHandler(Server::sendHandler, (void*)this);

  handler_.installReadErrorHandler(Server::errHandler, (void*)this);
  handler_.installSendErrorHandler(Server::errHandler, (void*)this);

  data_ = 0;
}

/**.......................................................................
 * Destructor for ServerConnection class
 */
Server::ServerConnection::~ServerConnection()
{
  // If any handlers were attached, detach them now

  if(handler_.getFd() > 0) {
    close(handler_.getFd());
    
    handler_.attach(-1);

    ::close(handler_.getFd());
  }

  // Delete any data associated with this connection too

  if(data_) {
    delete data_;
    data_ = 0;
  }

}

void Server::ServerConnection::setSendBufferSize(unsigned size)
{
  handler_.setSendBuffer(0, size);
}

void Server::ServerConnection::setReadBufferSize(unsigned size)
{
  handler_.setReadBuffer(0, size);
}

/**.......................................................................
 * Private method to pack data intended for a client
 */
void Server::ServerConnection::packClientData(NetDat& dat)
{
  std::vector<unsigned char>& data = dat.getSerializedData();
  unsigned datSize = data.size();

  packClientData(&data[0], datSize);
}

/**.......................................................................
 * Private method to pack data intended for a client.  If our message
 * queue for this client is currently empty, stage it directly into
 * the network handler, else push it onto the message queue for later
 * sending.
 */
void Server::ServerConnection::packClientData(unsigned char* buffer, unsigned nbyte)
{
  if(msgQueue_.empty())
    stageClientData(buffer, nbyte);
  else
    msgQueue_.push(buffer, nbyte);
}

/**.......................................................................
 * Check this client's message queue for pending messages.  If the
 * queue is now empty, stage the next message to be sent
 */
void Server::ServerConnection::checkMsgQueue()
{
  if(!msgQueue_.empty()) {
    PipeQueue::QueueNode& node = msgQueue_.front();
    stageClientData(node.buffer_, node.nbyte_);
    msgQueue_.pop();
  }
}

/**.......................................................................
 * Private method to pack data intended for a client
 */
void Server::ServerConnection::stageClientData(unsigned char* buffer, unsigned datSize)
{
  // Resize the send buffer if this object is larger than the
  // current send buffer size
  
  if(datSize+8 > parent_->sendBufSize_) {
    parent_->setSendBufSize(datSize + 8);
    setSendBufferSize(datSize + 8);
  }
  
  // Now send the data
  
  handler_.getSendStr()->startPut(datSize);
  handler_.getSendStr()->putChar(datSize, buffer);
  handler_.getSendStr()->endPut();
  
  // And register this client fd to be watched for writability
  
  parent_->fdSet_.registerWriteFd(handler_.getSendFd());
}

Server::ServerData::ServerData() {}

Server::ServerData::~ServerData() {}

