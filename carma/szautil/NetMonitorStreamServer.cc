#include "carma/szautil/NetMonitorFrame.h"
#include "carma/szautil/NetMonitorStreamServer.h"

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
NetMonitorStreamServer::NetMonitorStreamServer(bool spawnThread, unsigned port, NetMonitorFrame* nmf) :
  Server(spawnThread, port, 8, 8)
{
  nmf_       = nmf;
  fdRead_    = -1;
  haveFrame_ = false;
}

NetMonitorStreamServer::NetMonitorStreamServer(bool spawnThread, unsigned port, NetMonitorFrame* nmf, int fdRead) :
  Server(spawnThread, port, 8, 8)
{
  nmf_       = nmf;
  fdRead_    = fdRead;
  haveFrame_ = false;
}

/**.......................................................................
 * Destructor.
 */
NetMonitorStreamServer::~NetMonitorStreamServer() {}

/**.......................................................................
 * Method called when a new client connects
 */
void NetMonitorStreamServer::acceptClientAction(ServerConnection* conn)
{
  sendRegisterMap(conn);
}

/**.......................................................................
 * Method called whenever new data arrive
 */
void NetMonitorStreamServer::forwardDataFrame()
{
  nmf_->guard_.lock();

  if(nmf_->nadfm_.getType() == NetArrayDataFrameManager::MEM_FRAME) {
    COUT("Forwarding a frame");
  } else {
    COUT("Forwarding a register map");
  }

  nmf_->nadfm_.resize();

  sendClientData(nmf_->nadfm_, 0);
  nmf_->guard_.unlock();
}

/**.......................................................................
 * Method will be called whenever a data frame arrives -- send to all
 * connected clients
 */
void NetMonitorStreamServer::sendDataFrame()
{
  nmf_->guard_.lock();

  nmf_->nadfm_.setTo(NetArrayDataFrameManager::MEM_FRAME);
  nmf_->nadfm_.resize();

  //  CTOUT("Inside sendDataFrame: sending to all client");
  sendClientData(nmf_->nadfm_, 0);

  nmf_->guard_.unlock();
}

/**.......................................................................
 * Method will be called when a new client connects -- only send the
 * register map to that client
 */
void NetMonitorStreamServer::sendRegisterMap(ServerConnection* client)
{
  nmf_->guard_.lock();

  CTOUT("Inside sendRegisterMap with client = " << client);

  nmf_->nadfm_.setTo(NetArrayDataFrameManager::MEM_TEMPLATE);
  nmf_->nadfm_.resize();

  sendClientData(nmf_->nadfm_, client);

  nmf_->guard_.unlock();
}

void NetMonitorStreamServer::checkForDataFrames() 
{
  if(fdRead_ != -1) {
    if(fdSet_.isSetInRead(fdRead_)) {
      
      unsigned byte;
      ::read(fdRead_, &byte, 1);
      
      // Now that a frame has been received, set our internal flag to
      // true

      haveFrame_ = true;

      // Now send connected clients whatever the server sent us
      
      sendDataFrame();
    }
  }
}

void NetMonitorStreamServer::run()
{
  int nready=0;

  // On entry to the loop, deactivate any timeout

  timeOut_.setIntervalInSeconds(0);
  timeOut_.activate(false);

  // If we should listen for arrival of data, then add in the registered read fd

  if(fdRead_ != -1) {
    fdSet_.registerReadFd(fdRead_);
  }

  do {

    // Once done reading, process any client requests

    if((nready=select(fdSet_.size(), fdSet_.readFdSet(), fdSet_.writeFdSet(), NULL, 
		      timeOut_.tVal())) < 0) {
      ThrowSysError("select()");
    }

    if(nready > 0) {

      // Check if our 'message queue' was readable

      checkForDataFrames();

      // Service other select()able events.  But only once a frame has
      // been received.  This means that we refuse connection attempts
      // from clients until we have a valid register map to send them

      if(haveFrame_)
	serviceSelect();
    }
  } while(!stop_);
}
