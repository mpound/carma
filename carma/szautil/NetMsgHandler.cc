#include "carma/szautil/NetMsg.h"
#include "carma/szautil/NetMsgHandler.h"

#include "carma/szaarrayutils/netobj.h"
#include "carma/szaarrayutils/rtcnetcoms.h"
#include "carma/szaarrayutils/szaregs.h"
#include "carma/szaarrayutils/arraymaprev.h"

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
NetMsgHandler::NetMsgHandler() :
  NetHandler()
{
  arraymap_ = 0;

  if((arraymap_ = new_SzaArrayMap())==0) {
    LogStream errStr;
    errStr.appendMessage(true, "Unable to allocate arrmap_.\n");
    throw Error(errStr);
  }

  // Set the user handlers to NULL

  userReadHandler_  = 0;
  userReadArg_      = 0;
  userSendHandler_  = 0;
  userSendArg_      = 0;
  userErrorHandler_ = 0;
  userErrorArg_     = 0;
  
  // Set up the network buffers

  sza::array::RtcNetMsg netMsg;
  
  // Get the minimum buffer size

  unsigned bufferSize = NET_PREFIX_LEN + netMsg.size();

  setReadBuffer(NULL, bufferSize);
  setSendBuffer(NULL, bufferSize);

  // Install our handlers as the default handlers

  NetHandler::installReadHandler(readHandler, this);
  NetHandler::installReadErrorHandler(errorHandler, this);
  NetHandler::installSendHandler(sendHandler, this);
  NetHandler::installSendErrorHandler(errorHandler, this);
}

/**.......................................................................
 * Destructor.
 */
NetMsgHandler::~NetMsgHandler() 
{
  if(arraymap_ != 0)
    arraymap_ = del_SzaArrayMap(arraymap_);
  arraymap_ = 0;
}

/**.......................................................................
 * Send a message to a socket described by a previously attached fd.
 */
NetSendStr::NetSendId NetMsgHandler::sendNetMsg(NetMsg* msg)
{
  LogStream ls;

  if(nss_->state() != NetSendStr::NET_SEND_DONE) {
    ls.appendMessage(true, "Last message wasn't completely sent");
    throw Error(ls);
  }

  // Pack the message in our buffer and send it.

  packNetMsg(msg);
  return NetHandler::send();
}

/**.......................................................................
 * Send a message to a socket
 */
NetSendStr::NetSendId NetMsgHandler::sendNetMsg(int fd, NetMsg* msg)
{
  LogStream ls;

  if(nss_->state() != NetSendStr::NET_SEND_DONE) {
    ls.appendMessage(true, "Last message wasn't completely sent");
    throw Error(ls);
  }

  // Pack the message in our buffer and send it.

  packNetMsg(msg);
  return NetHandler::send(fd);
}

/**.......................................................................
 * Pack a network message into our send buffer.
 */
void NetMsgHandler::packNetMsg(NetMsg* msg)
{
  DBPRINT(true, Debug::DEBUG6, "Packing: type = " << msg->type
	  << " antenna = " << msg->body.antenna);

  // Copy it into our last sent buffer

  lastSentNetMsg_ = *msg;

  // And pack the message into the network buffer.

  nss_->startPut(msg->type);
  nss_->putLong(1, &msg->body.antenna);
  nss_->putObj(&rtc_msg_table, msg->type, (void*)&msg->body.msg);
  nss_->endPut();
}

/**.......................................................................
 * Pack a greeting message into our send buffer.
 */
void NetMsgHandler::packGreetingMsg(unsigned int antenna)
{
  AntNum antNum(antenna);
  std::string regMapName = antNum.getAntennaName();

  RegMap* regmap = arraymap_->findRegMap(regMapName);

  if(regmap==0) {
    LogStream errStr;
    errStr.initMessage(true);
    errStr << "No such register map: " << antNum.getAntennaName() << std::endl;
    throw Error(errStr);
  }

  NetMsg netMsg;
  netMsg.packGreetingMsg(antenna, ARRAYMAP_REVISION, 
			 regmap->nreg_, regmap->nByte_);

  packNetMsg(&netMsg);
}

/**.......................................................................
 * Pack an antenna ID message into our send buffer.
 */
void NetMsgHandler::packAntennaIdMsg(unsigned int antenna)
{
  NetMsg netMsg;
  netMsg.packAntennaIdMsg(antenna);
  packNetMsg(&netMsg);
}

/**.......................................................................
 * Return the last message read
 */
sza::util::NetMsg* NetMsgHandler::getLastReadNetMsg()
{
  return &lastReadNetMsg_;
}

/**.......................................................................
 * Return the last message sent
 */
sza::util::NetMsg* NetMsgHandler::getLastSentNetMsg()
{
  return &lastSentNetMsg_;
}

/**.......................................................................
 * Read a net message out of the network buffer
 */
void NetMsgHandler::readNetMsg()
{
  // Read the message type and unpack the corresponding message

  nrs_->startGet((int*)&lastReadNetMsg_.type);
  nrs_->getLong(1, &lastReadNetMsg_.body.antenna);
  nrs_->getObj(&rtc_msg_table, lastReadNetMsg_.type, 
	      &lastReadNetMsg_.body.msg);
  nrs_->endGet();

  DBPRINT(true, Debug::DEBUG5, "Read message: " 
	  << " type = " << lastReadNetMsg_.type
	  << " antenna = " << lastReadNetMsg_.body.antenna);
}

/**.......................................................................
 * A handler to be called when a message has been completely read.
 */
NET_READ_HANDLER(NetMsgHandler::readHandler)
{
  NetMsgHandler* netHandler = (NetMsgHandler*) arg;

  netHandler->readNetMsg();

  if(netHandler->userReadHandler_ != 0)
    netHandler->userReadHandler_(netHandler->userReadArg_);
}

/**.......................................................................
 * A handler to be called when a message has been completely send.
 */
NET_SEND_HANDLER(NetMsgHandler::sendHandler)
{
  NetMsgHandler* netHandler = (NetMsgHandler*) arg;

  if(netHandler->userSendHandler_ != 0)
    netHandler->userSendHandler_(netHandler->userSendArg_);
}

/**.......................................................................
 * A handler to be called when an error has occurred
 */
NET_ERROR_HANDLER(NetMsgHandler::errorHandler)
{
  NetMsgHandler* netHandler = (NetMsgHandler*) arg;

  if(netHandler->userErrorHandler_ != 0)
    netHandler->userErrorHandler_(netHandler->userErrorArg_);
}

/**.......................................................................
 * Methods to install user-defined handlers
 */
void NetMsgHandler::installReadHandler(NET_READ_HANDLER(*handler), void* arg)
{
  userReadHandler_ = handler;
  userReadArg_ = arg;
}

/**.......................................................................
 * Methods to install user-defined handlers
 */
void NetMsgHandler::installSendHandler(NET_SEND_HANDLER(*handler), void* arg)
{
  userSendHandler_ = handler;
  userSendArg_ = arg;
}

/**.......................................................................
 * Methods to install user-defined handlers
 */
void NetMsgHandler::installErrorHandler(NET_READ_HANDLER(*handler), void* arg)
{
  userErrorHandler_ = handler;
  userErrorArg_ = arg;
}


