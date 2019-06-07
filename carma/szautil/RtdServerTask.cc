#include "carma/szautil/NetMonitorFrame.h"
#include "carma/szautil/RtdServerTask.h"

#include <arpa/inet.h>

using namespace std;
using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
RtdServerTask::RtdServerTask(bool spawnThread, unsigned port, NetMonitorFrame* nmf) :
  ServerTask<RtdServerMsg>(spawnThread, port, 8, 8)
{
  nmf_        = nmf;
  fdRead_     = -1;
  haveRegMap_ = false;
  vRegPtr_    = 0;
  vRegBlk_    = 0;
}

RtdServerTask::RtdServerTask(bool spawnThread, unsigned port, NetMonitorFrame* nmf, int fdRead) :
  ServerTask<RtdServerMsg>(spawnThread, port, 8, 8)
{
  nmf_        = nmf;
  fdRead_     = fdRead;
  haveRegMap_ = false;
  vRegPtr_    = 0;
  vRegBlk_    = 0;
}

/**.......................................................................
 * Destructor
 */
RtdServerTask::~RtdServerTask() {}

/**.......................................................................
 * Method called when a new client connects
 */
void RtdServerTask::acceptClientAction(ServerConnection* conn)
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
void RtdServerTask::run()
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
void RtdServerTask::checkForServerData()
{
  if(fdRead_ != -1) {
    if(fdSet_.isSetInRead(fdRead_)) {
      
      unsigned byte;
      ::read(fdRead_, &byte, 1);

      //------------------------------------------------------------
      // If a template from the server, store the compressed version
      // that we will send to clients
      //------------------------------------------------------------

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

	//------------------------------------------------------------
	// Else we got a frame -- send connected clients whatever the
	// server sent us
	//------------------------------------------------------------
	
	sendDataRegisters();
      }
    }
  }

}

/**.......................................................................
 * Method will be called whenever a data frame arrives -- send to all
 * connected clients
 */
void RtdServerTask::sendDataRegisters()
{
  //------------------------------------------------------------
  // Fill the validity bitmask with new values
  //------------------------------------------------------------

  if(vRegPtr_) {
    COUT("Just filled validity bitmask... 0 nel = " << vRegBlk_->nEl());
    validityBitMask_.fillFrom(vRegPtr_, vRegBlk_->nEl());
    COUT("Just filled validity bitmask... 1");
  }

  //------------------------------------------------------------
  // And mail off registers to clients
  //------------------------------------------------------------

  for(std::list<ServerConnection*>::iterator iclient=clients_.begin();
      iclient != clients_.end(); iclient++)
    sendDataRegisters(*iclient);
}

/**.......................................................................
 * Method will be called whenever a new frame arrives
 */
void RtdServerTask::sendDataRegisters(ServerConnection* client)
{
  ClientData* clientData = (ClientData*)client->data_;

  // If this client is monitoring any registers, send them now

  if(clientData->monitoredRegsList_.size() > 0) {
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
void RtdServerTask::sendRegisterMap()
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
void RtdServerTask::sendRegisterMap(ServerConnection* client)
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
void RtdServerTask::sendAddRegAck(ServerConnection* client)
{
  ClientData* clientData = (ClientData*)client->data_;

  clientData->clientData_.setTo(RtdClientData::MEM_ADDREGACK);
  clientData->clientData_.resize();

  //  COUTCOLOR("Sending addreg ACK", "magenta");
  sendClientData(clientData->clientData_, client);
}

/**.......................................................................
 * Method will be called to acknowledge a REMREG command
 */
void RtdServerTask::sendRemRegAck(ServerConnection* client)
{
  ClientData* clientData = (ClientData*)client->data_;

  clientData->clientData_.setTo(RtdClientData::MEM_REMREGACK);
  clientData->clientData_.resize();

  //  COUTCOLOR("Sending remdreg ACK", "magenta");
  sendClientData(clientData->clientData_, client);
}

/**.......................................................................
 * Read data received from a client
 */
void RtdServerTask::readClientData(ServerConnection* client)
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
void RtdServerTask::processClientData(ServerConnection* client)
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
void RtdServerTask::ClientData::processMessage(RtdServerTask* server,
					   ServerConnection* client,
					   std::map<unsigned, RegMapBlock*>& regMap,
					   std::map<unsigned, unsigned char*>& srcPtrMap)
{
  switch(clientData_.id_) {
  case RtdClientData::MEM_ADDREG:
    {
      unsigned id = clientData_.addReg_;

      // Only add this reg if it isn't already in our list

      if(monitoredRegsMap_.find(id) == monitoredRegsMap_.end()) {

	// Add it to the map

	MonitoredReg* reg = new MonitoredReg(regMap[id], srcPtrMap[id], &server->validityBitMask_);

	monitoredRegsMap_[id] = reg;

	// And push it onto the end of our list

	monitoredRegsList_.push_back(reg);

	// Now recalculate the size of the monitored byte stream
	
	resize();
      }

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

      if(monitoredRegsMap_.find(id) != monitoredRegsMap_.end()) {

	// Remove it from the map

	MonitoredReg* reg = monitoredRegsMap_[id];
	monitoredRegsMap_.erase(id);

	// And remove it from our list also

	monitoredRegsList_.remove(reg);

	// Finally, delete the memory for it

	delete reg;
	
	// Now recalculate the size of the monitored byte stream
	
	resize();
      }

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
void RtdServerTask::ClientData::resize()
{
  unsigned nByte = 0;
  for(std::list<MonitoredReg*>::iterator iter=monitoredRegsList_.begin(); iter != monitoredRegsList_.end(); iter++) {
    MonitoredReg* reg = *iter;

    // Add the bytes for this reg's data

    nByte += reg->nByte_;

    // Add an additional byte for this reg's validity

    nByte += 1;
  }

  // Resize the underlying byte array

  clientData_.dataBytes_.resize(nByte);
}

/**.......................................................................
 * Pack all requested data for this client
 */
void RtdServerTask::ClientData::pack()
{
  unsigned char* destPtr = &clientData_.dataBytes_[0];

  for(std::list<MonitoredReg*>::iterator iter=monitoredRegsList_.begin(); iter != monitoredRegsList_.end(); iter++) {
    MonitoredReg* reg = *iter;
    destPtr += reg->pack(destPtr);
  }
}

/**.......................................................................
 * Destructor deletes any allocated regs
 */
RtdServerTask::ClientData::~ClientData()
{
  for(std::list<MonitoredReg*>::iterator iter=monitoredRegsList_.begin(); iter != monitoredRegsList_.end(); iter++) {
    MonitoredReg* reg = *iter;
    delete reg;
  }
}

/**.......................................................................
 * Construct a map of register blocks.  We will use this for quick
 * lookup of requested registers for monitoring
 */
void RtdServerTask::constructMapOfRegisters()
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

	if(strcmp(arrRegMap->name, "array") == 0 && strcmp(regMapBrd->name, "frame") == 0 && strcmp(regMapBlk->name_, "validity") == 0) {
	  vRegPtr_ = srcPtrMap_[iReg];
	  vRegBlk_ = registerBlockMap_[iReg];
	  COUT("Validity reg is char: " << vRegBlk_->isUchar() << " of size = " << vRegBlk_->nEl());
	}

	++iReg;
      }
    }
  }
}

RtdServerTask::MonitoredReg::MonitoredReg(RegMapBlock* block, unsigned char* srcPtr, BitMask* validityBitMask) 
{
  validityBitMask_ = validityBitMask;
  block_ = block;
  
  // Java wants all 2-byte types as net shorts

  if(block->isShort() || block->isUshort()) {
    convFn_ = MonitoredReg::convertHtons;

    // Java converts all 4-byte types (including floats) to net longs

  } else if(block->isInt() || block->isUint() || (block->isFloat() && !block->isComplex())) {
    convFn_ = MonitoredReg::convertHtonl;

    // Java converts all doubles to pairs of net longs

  } else if(block->isDouble()) {
    convFn_ = MonitoredReg::convertDouble;

    // Convert paired 4-byte types to consecutive net longs

  } else if(block->isUtc()) {
    convFn_ = MonitoredReg::convertPairedFourByteType;
  } else if(block->isFloat() && block->isComplex()) {
    convFn_ = MonitoredReg::convertPairedFourByteType;
  } else {
    convFn_ = MonitoredReg::convertNone;
  }
  
  nEl_   = block->nEl();
  nByte_ = block->nByte();

  // Finally, store a pointer to the start of the data for this
  // register

  srcPtr_ = srcPtr;
}

REG_DATA_FN(RtdServerTask::MonitoredReg::convertNone)
{
  for(unsigned i=0; i < nByte; i++)
    *dest++ = *src++;
}

REG_DATA_FN(RtdServerTask::MonitoredReg::convertHtons)
{
  unsigned short hs, ns;
  unsigned short* hptr = (unsigned short*)src;

  for(unsigned iEl=0; iEl < nEl; iEl++) {
    hs = *(hptr + iEl);
    ns = htons(hs);
    unsigned char* nptr = (unsigned char*) &ns;
    for(unsigned i=0; i < 2; i++) 
      *dest++ = *nptr++;
  }
}

REG_DATA_FN(RtdServerTask::MonitoredReg::convertHtonl)
{
  unsigned int hl, nl;
  unsigned int* hptr = (unsigned int*)src;

  for(unsigned iEl=0; iEl < nEl; iEl++) {
    hl = hptr[iEl];
    nl = htonl(hl);

    unsigned char* nptr = (unsigned char*) &nl;
    for(unsigned i=0; i < 4; i++) 
      *dest++ = *nptr++;
  }
}

REG_DATA_FN(RtdServerTask::MonitoredReg::convertDouble)
{
  unsigned int hl1, hl2, nl1, nl2;
  unsigned int* iptr = (unsigned int*)src;

  for(unsigned iEl=0; iEl < nEl; iEl++) {

    hl1 = iptr[2*iEl];
    hl2 = iptr[2*iEl+1];

    nl1 = htonl(hl1);
    nl2 = htonl(hl2);

    unsigned char* nptr1 = (unsigned char*) &nl1;
    unsigned char* nptr2 = (unsigned char*) &nl2;

    for(unsigned i=0; i < 4; i++) 
      *dest++ = *nptr2++;

    for(unsigned i=0; i < 4; i++) 
      *dest++ = *nptr1++;
  }
}

REG_DATA_FN(RtdServerTask::MonitoredReg::convertPairedFourByteType)
{
  unsigned int hl, nl;
  unsigned int* iptr = (unsigned int*)src;

  for(unsigned iEl=0; iEl < 2*nEl; iEl++) {

    hl = iptr[iEl];
    nl = htonl(hl);

    unsigned char* nptr = (unsigned char*) &nl;

    for(unsigned i=0; i < 4; i++) 
      *dest++ = *nptr++;
  }
}


/**.......................................................................
 * Method to pack data into a destination pointer
 */
unsigned RtdServerTask::MonitoredReg::pack(unsigned char* destPtr)
{
  // Pack the data for this register

  convFn_(nByte_, nEl_, srcPtr_, destPtr);

  // Finally, pack the validity byte for this register

  destPtr[nByte_] = (unsigned char) validityBitMask_->bitIsLow(block_->carmaValidityBitIndex_);

  // Return the total number of bytes packed

  return nByte_+1;
}
