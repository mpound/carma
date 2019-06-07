#include "carma/szautil/NetMonitorFrame.h"
#include "carma/szautil/NetMonitorFrameServer.h"

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
NetMonitorFrameServer::NetMonitorFrameServer(bool spawnThread, unsigned port, NetMonitorFrame* nmf) :
  Server(spawnThread, port, 8, 8)
{
  nmf_       = nmf;
  fdRead_    = -1;
  haveFrame_ = false;
}

NetMonitorFrameServer::NetMonitorFrameServer(bool spawnThread, unsigned port, NetMonitorFrame* nmf, int fdRead) :
  Server(spawnThread, port, 8, 8)
{
  nmf_       = nmf;
  fdRead_    = fdRead;
  haveFrame_ = false;
}

/**.......................................................................
 * Destructor.
 */
NetMonitorFrameServer::~NetMonitorFrameServer() {}

/**.......................................................................
 * Method called when a new client connects
 */
void NetMonitorFrameServer::acceptClientAction(ServerConnection* conn)
{
  sendRegisterMap(conn);
}

/**.......................................................................
 * Method called whenever new data arrive
 */
void NetMonitorFrameServer::forwardDataFrame()
{
  nmf_->guard_.lock();

  if(nmf_->nadfm_.getType() == NetArrayDataFrameManager::MEM_FRAME) {
    COUT("Forwarding a frame");
  } else {
    COUT("Forwarding a register map");
  }

  nmf_->nadfm_.resize();

  sendClientData(nmf_->nadfm_, 0);

  if(nmf_->nadfm_.getType() == NetArrayDataFrameManager::MEM_FRAME) {
    COUT("Forwarding a frame... done");
  } else {
    COUT("Forwarding a register map... done");
  }

  nmf_->guard_.unlock();
}

/**.......................................................................
 * Method will be called whenever a data frame arrives -- send to all
 * connected clients
 */
void NetMonitorFrameServer::sendDataFrame()
{
  nmf_->guard_.lock();

  COUT("Setting to data frame");
  nmf_->nadfm_.setTo(NetArrayDataFrameManager::MEM_FRAME);
  COUT("Setting to data frame...done (1)");
  nmf_->nadfm_.resize();
  COUT("Setting to data frame...done (2)");

  CTOUT("Inside sendDataFrame: sending to all client");
  sendClientData(nmf_->nadfm_, 0);
  CTOUT("Inside sendDataFrame: sending to all client... done");

  nmf_->guard_.unlock();
}

/**.......................................................................
 * Method will be called when a new client connects -- only send the
 * register map to that client
 */
void NetMonitorFrameServer::sendRegisterMap(ServerConnection* client)
{
  nmf_->guard_.lock();

  CTOUT("Inside sendRegisterMap with client = " << client);

  nmf_->nadfm_.setTo(NetArrayDataFrameManager::MEM_TEMPLATE);
  nmf_->nadfm_.resize();

  sendClientData(nmf_->nadfm_, client);

  CTOUT("Inside sendRegisterMap with client = " << client << " done");

  nmf_->guard_.unlock();
}

void NetMonitorFrameServer::checkForDataFrames() 
{
  if(fdRead_ != -1) {
    if(fdSet_.isSetInRead(fdRead_)) {
      
      COUT("Reading data frame...");

      unsigned byte;
      ::read(fdRead_, &byte, 1);

      COUT("Reading data frame... done");
      
      // Now that a frame has been received, set our internal flag to
      // true

      haveFrame_ = true;

      // Now send connected clients whatever the server sent us
      
      sendDataFrame();
    }
  }
}

void NetMonitorFrameServer::run()
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

  COUT("Exiting run()");
}
