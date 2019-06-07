#include "carma/szautil/NetMonitorFrame.h"
#include "carma/szautil/FastPdbServer.h"

#include <arpa/inet.h>

using namespace std;
using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
FastPdbServer::FastPdbServer(bool spawnThread, unsigned port, std::string file) :
  Server(spawnThread, port, 8, 8)
{
  COUT("About to create database 0");
  pdb_.createDatabase(file);
  COUT("About to create database 1");
}

/**.......................................................................
 * Destructor.
 */
FastPdbServer::~FastPdbServer() {}

/**.......................................................................
 * Method called when a new client connects
 */
void FastPdbServer::acceptClientAction(ServerConnection* conn)
{
  // Allocate the object that will be used to send messages to the
  // client

  conn->data_ = new ClientData();
}

/**.......................................................................
 * Main method of this thread.  Block in select until a descriptor
 * becomes readable, or a timeout occurs
 */
void FastPdbServer::run()
{
  int nready=0;

  // On entry to the loop, deactivate any timeout

  timeOut_.setIntervalInSeconds(0);
  timeOut_.activate(false);

  do {

    // Once done reading, process any client requests

    if((nready=select(fdSet_.size(), fdSet_.readFdSet(), fdSet_.writeFdSet(), NULL, 
		      timeOut_.tVal())) < 0) {
      ThrowSysError("select()");
    }

    if(nready > 0) {

      try {
	serviceSelect();
      } catch(Exception& err) {
	COUTCOLOR(err.what(), "red");
      }
    }

  } while(!stop_);
}

/**.......................................................................
 * Method will be called whenever a new frame arrives
 */
void FastPdbServer::sendResponse(ServerConnection* client)
{
  ClientData* clientData = (ClientData*)client->data_;

  clientData->clientData_.response_ = "hello";
  clientData->clientData_.setTo(FastPdbData::MEM_RESPONSE);
  clientData->clientData_.resize();
    
  sendClientData(clientData->clientData_, client);
}

/**.......................................................................
 * Read data received from a client
 */
void FastPdbServer::readClientData(ServerConnection* client)
{
  int size;

  client->handler_.getReadStr()->startGet(&size);

  // Changing this now to allow for variable network object sizes.
  // The network buffer can change size if a message larger than the
  // previously allocated buffer size is encountered, therefore our
  // byte array will be resized accordingly.  
  //
  // Note that the network buffer only changes size to accomodate
  // larger messages, so that we do not reallocate just because a
  // shorter message was encountered.

  if(size > client->bytes_.size()) {
    client->bytes_.resize(size);
  }

  client->handler_.getReadStr()->getChar(size, &client->bytes_[0]);
  client->handler_.getReadStr()->endGet();

  processClientData(client);
}

/**.......................................................................
 * Now do something with the data read from the client
 */
void FastPdbServer::processClientData(ServerConnection* client)
{
  ClientData* clientData = (ClientData*)client->data_;
  clientData->clientData_.deserialize(client->bytes_);

  // Now act on the type of message we received (should just be
  // messages to add or remove a monitored register)

  clientData->processMessage(this, client);
}

/**.......................................................................
 * Process a message received from a client
 */
void FastPdbServer::ClientData::processMessage(FastPdbServer* server,
					       ServerConnection* client)
{
  COUT("Got a message: " << clientData_);
  switch(clientData_.id_) {
  case FastPdbData::MEM_SRCLIST:
    try {
      clientData_.response_ = server->pdb_.listTrialsMatchingSource(clientData_.source_);
    } catch(Exception& err) {
      clientData_.response_ = err.what();
    }

    clientData_.setTo(FastPdbData::MEM_RESPONSE);
    clientData_.resize();
    server->sendClientData(clientData_, client);
    break;
  case FastPdbData::MEM_PROJLIST:
    try {
      clientData_.response_ = server->pdb_.listTrialsMatchingProject(clientData_.project_);
    } catch(Exception& err) {
      clientData_.response_ = err.what();
    }

    clientData_.setTo(FastPdbData::MEM_RESPONSE);
    clientData_.resize();
    server->sendClientData(clientData_, client);
    break;
  default:
    break;
  }
}

/**.......................................................................
 * Destructor deletes any allocated regs
 */
FastPdbServer::ClientData::~ClientData() {}

