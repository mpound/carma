#include "carma/szautil/NetMonitorFrame.h"
#include "carma/szautil/RtdServer.h"

#include <arpa/inet.h>

using namespace std;
using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
RtdServer::RtdServer(bool spawnThread, unsigned port, NetMonitorFrame* nmf) :
  Server(spawnThread, port, 8, 8)
{
  nmf_        = nmf;
  fdRead_     = -1;
  haveRegMap_ = false;
}

RtdServer::RtdServer(bool spawnThread, unsigned port, NetMonitorFrame* nmf, int fdRead) :
  Server(spawnThread, port, 8, 8)
{
  nmf_        = nmf;
  fdRead_     = fdRead;
  haveRegMap_ = false;
}

/**.......................................................................
 * Destructor.
 */
RtdServer::~RtdServer() {}

/**.......................................................................
 * Method called when a new client connects
 */
void RtdServer::acceptClientAction(ServerConnection* conn)
{
  // Allocate the object that will be used to send messages to the
  // client

  conn->data_ = new ClientData();

  // And send the register map to this client

  sendRegisterMap(conn);
}

/**.......................................................................
 * Main method of this thread.  Block in select until a descriptor
 * becomes readable, or a timeout occurs
 */
void RtdServer::run()
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

      checkForServerData();

      // Service other select()able events.  But only once a frame has
      // been received.  This means that we refuse connection attempts
      // from clients until we have a valid register map to send them

      if(haveRegMap_) {
	try {
	  serviceSelect();
	} catch(Exception& err) {
	  COUTCOLOR(err.what(), "red");
	}
      }
    }

  } while(!stop_);
}

/**.......................................................................
 * Check for data from the server
 */
void RtdServer::checkForServerData()
{
  if(fdRead_ != -1) {
    if(fdSet_.isSetInRead(fdRead_)) {
      
      unsigned byte;
      ::read(fdRead_, &byte, 1);

      // If a template from the server, store the compressed version
      // that we will send to clients

      if(nmf_->nadfm_.getType() == NetArrayDataFrameManager::MEM_TEMPLATE) {

	// Set the template in the global clientData object to the
	// template we just received from the server

	clientData_.template_.setTo(nmf_->nadfm_.getArrayTemplate());

	// Build a map of register blocks

	constructMapOfRegisters();

	// And send any connected clients the new register map

	sendRegisterMap();

	haveRegMap_ = true;

      } else {

	// Now send connected clients whatever the server sent us
	
	sendDataRegisters();
      }
    }
  }

}

/**.......................................................................
 * Method will be called whenever a data frame arrives -- send to all
 * connected clients
 */
void RtdServer::sendDataRegisters()
{
  for(std::list<ServerConnection*>::iterator iclient=clients_.begin();
      iclient != clients_.end(); iclient++)
    sendDataRegisters(*iclient);
}

/**.......................................................................
 * Method will be called whenever a new frame arrives
 */
void RtdServer::sendDataRegisters(ServerConnection* client)
{
  ClientData* clientData = (ClientData*)client->data_;

  // If this client is monitoring any registers, send them now

  if(clientData->monitoredRegs_.size() > 0) {
    clientData->pack();
    
    clientData->clientData_.setTo(RtdClientData::MEM_DATAREGS);
    clientData->clientData_.resize();
    
    sendClientData(clientData->clientData_, client);
  }
}

/**.......................................................................
 * Method that will be called when a new register map becomes
 * available -- send the new register map to all connected clients
 */
void RtdServer::sendRegisterMap()
{
  for(std::list<ServerConnection*>::iterator iclient=clients_.begin();
      iclient != clients_.end(); iclient++) {
    sendRegisterMap(*iclient);
  }
}

/**.......................................................................
 * Method will be called when a new client connects -- only send the
 * register map to that client
 */
void RtdServer::sendRegisterMap(ServerConnection* client)
{
  // Set the global clientData object to send the template to this
  // client

  clientData_.setTo(RtdClientData::MEM_TEMPLATE);
  clientData_.resize();

  sendClientData(clientData_, client);
}

/**.......................................................................
 * Method will be called to acknowledge an ADDREG command
 */
void RtdServer::sendAddRegAck(ServerConnection* client)
{
  ClientData* clientData = (ClientData*)client->data_;

  clientData->clientData_.setTo(RtdClientData::MEM_ADDREGACK);
  clientData->clientData_.resize();

  COUTCOLOR("Sending addreg ACK", "magenta");
  sendClientData(clientData->clientData_, client);
}

/**.......................................................................
 * Method will be called to acknowledge a REMREG command
 */
void RtdServer::sendRemRegAck(ServerConnection* client)
{
  ClientData* clientData = (ClientData*)client->data_;

  clientData->clientData_.setTo(RtdClientData::MEM_REMREGACK);
  clientData->clientData_.resize();

  COUTCOLOR("Sending remdreg ACK", "magenta");
  sendClientData(clientData->clientData_, client);
}

/**.......................................................................
 * Read data received from a client
 */
void RtdServer::readClientData(ServerConnection* client)
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
void RtdServer::processClientData(ServerConnection* client)
{
  ClientData* clientData = (ClientData*)client->data_;
  clientData->clientData_.deserialize(client->bytes_);

  // Now act on the type of message we received (should just be
  // messages to add or remove a monitored register)

  clientData->processMessage(this, client, registerBlockMap_, srcPtrMap_);
}

/**.......................................................................
 * Process a message received from a client
 */
void RtdServer::ClientData::processMessage(RtdServer* server,
					   ServerConnection* client,
					   std::map<unsigned, RegMapBlock*>& regMap,
					   std::map<unsigned, unsigned char*>& srcPtrMap)
{
  switch(clientData_.id_) {
  case RtdClientData::MEM_ADDREG:
    {
      unsigned id = clientData_.addReg_;
      MonitoredReg* reg = new MonitoredReg(regMap[id], srcPtrMap[id]);
      monitoredRegs_[id] = reg;

      COUTCOLOR("Adding register " << id << " map now contains: " << monitoredRegs_.size(), "red");

      // Now recalculate the size of the monitored byte stream

      resize();

      // Lastly, send an acknowledgement.  This is safe to do now
      // because we can't service any new data frames until we return
      // from this call, by which time the acknowledgement will be
      // queued to be sent before the new frame is queued

      server->sendAddRegAck(client);
    }
    break;
  case RtdClientData::MEM_REMREG:
    {
      unsigned id = clientData_.remReg_;

      if(monitoredRegs_.find(id) != monitoredRegs_.end()) {
	MonitoredReg* reg = monitoredRegs_[id];
	monitoredRegs_.erase(id);
	delete reg;
      }

      COUT("Removing register " << id << " map now contains: " << monitoredRegs_.size());

      // Now recalculate the size of the monitored byte stream

      resize();

      // Lastly, send an acknowledgement.  This is safe to do now
      // because we can't service any new data frames until we return
      // from this call, by which time the acknowledgement will be
      // queued to be sent before the new frame is queued

      server->sendRemRegAck(client);
    }
    break;
  default:
    CERR("Got an unrecognized message from a client");
    break;
  }
}

/**.......................................................................
 * Pack data for this client
 */

/**.......................................................................
 * Resize the underlying byte array for the register map
 */
void RtdServer::ClientData::resize()
{
  unsigned nByte = 0;
  for(std::map<unsigned, MonitoredReg*>::iterator iter=monitoredRegs_.begin(); iter != monitoredRegs_.end(); iter++) {
    MonitoredReg* reg = iter->second;
    nByte += reg->nByte_;
  }

  // Resize the underlying byte array

  clientData_.dataBytes_.resize(nByte);
}

/**.......................................................................
 * Pack all requested data for this client
 */
void RtdServer::ClientData::pack()
{
  unsigned char* destPtr = &clientData_.dataBytes_[0];

  for(std::map<unsigned, MonitoredReg*>::iterator iter=monitoredRegs_.begin(); iter != monitoredRegs_.end(); iter++) {
    MonitoredReg* reg = iter->second;
    destPtr += reg->pack(destPtr);
  }
}

/**.......................................................................
 * Destructor deletes any allocated regs
 */
RtdServer::ClientData::~ClientData()
{
  for(std::map<unsigned, MonitoredReg*>::iterator iter=monitoredRegs_.begin(); iter != monitoredRegs_.end(); iter++) {
    MonitoredReg* reg = iter->second;
    delete reg;
  }
}

/**.......................................................................
 * Construct a map of register blocks.  We will use this for quick
 * lookup of requested registers for monitoring
 */
void RtdServer::constructMapOfRegisters()
{
  ArrayMap* arrayMap = nmf_->nadfm_.arrayMap();
  ArrayDataFrameManager& adfm = nmf_->nadfm_;

    // Now iterate over regtemplates, serializing each one

  unsigned iReg=0;
  //  COUTCOLOR("Inside constructMapOfregisters() regmaps = " << arrayMap->regmaps.size(), "red");

  for(unsigned iRegMap=0; iRegMap < arrayMap->regmaps.size(); iRegMap++) {
    ArrRegMap* arrRegMap = arrayMap->regmaps[iRegMap];

    //    COUTCOLOR("Inside constructMapOfregisters() regmap = " << arrRegMap->name << " boards = " << arrRegMap->regmap->boards_.size(), "red");
    for(unsigned iBoard=0; iBoard < arrRegMap->regmap->boards_.size(); iBoard++) {
      RegMapBoard* regMapBrd = arrRegMap->regmap->boards_[iBoard];

      //      COUTCOLOR("Inside constructMapOfregisters() board = " << regMapBrd->name << " blocks = " << regMapBrd->blocks.size(), "red");
      for(int iBlock = 0; iBlock < regMapBrd->blocks.size(); iBlock++) {
	RegMapBlock* regMapBlk = regMapBrd->blocks[iBlock];
	registerBlockMap_[iReg] = regMapBlk;
	srcPtrMap_[iReg] = (unsigned char*)
	  adfm.frame()->getPtr(adfm.byteOffsetInFrameOf(arrRegMap->name, regMapBrd->name, regMapBlk->name_), 
			       DataType::typeOf(regMapBlk));

	//	COUTCOLOR("Block = " << regMapBlk->name_ << " " << iReg, "red");

	++iReg;

	
      }
    }
  }
}

RtdServer::MonitoredReg::MonitoredReg(RegMapBlock* block, unsigned char* srcPtr) 
{
  block_ = block;
  
  if(block->isShort() || block->isUshort()) {
    convFn_ = MonitoredReg::convertHtons;
  } else if(block->isInt() || block->isUint()) {
    convFn_ = MonitoredReg::convertHtonl;
  } else {
    convFn_ = MonitoredReg::convertNone;
  }
  
  nEl_   = block->nEl();
  nByte_ = block->nByte();

  // Finally, store a pointer to the start of the data for this
  // register

  srcPtr_ = srcPtr;
}

REG_DATA_FN(RtdServer::MonitoredReg::convertNone)
{
  for(unsigned i=0; i < nByte; i++)
    *dest++ = *src++;
}

REG_DATA_FN(RtdServer::MonitoredReg::convertHtons)
{
  unsigned short hs, ns;
  unsigned short* hptr = (unsigned short*)src;

  for(unsigned iEl=0; iEl < nEl; iEl++) {
    hs = *(hptr + iEl);
    ns = htons(hs);
    unsigned char* nptr = (unsigned char*) &ns;
    *dest++ = *nptr++;
    *dest++ = *nptr++;
  }
}

REG_DATA_FN(RtdServer::MonitoredReg::convertHtonl)
{
  unsigned int hl, nl;
  unsigned int* hptr = (unsigned int*)src;

  for(unsigned iEl=0; iEl < nEl; iEl++) {
    hl = hptr[iEl];
    nl = htonl(hl);

    unsigned char* nptr = (unsigned char*) &nl;
    *dest++ = *nptr++;
    *dest++ = *nptr++;
    *dest++ = *nptr++;
    *dest++ = *nptr++;
  }
}

/**.......................................................................
 * Method to pack data into a destination pointer
 */
unsigned RtdServer::MonitoredReg::pack(unsigned char* destPtr)
{
  convFn_(nByte_, nEl_, srcPtr_, destPtr);
  return nByte_;
}
