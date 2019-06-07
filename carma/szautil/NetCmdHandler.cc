#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"

#include "carma/szautil/NetCmdHandler.h"

using namespace std;
using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
NetCmdHandler::NetCmdHandler() :
  NetHandler()
{
  userReadHandler_  = 0;
  userReadArg_      = 0;
  userSendHandler_  = 0;
  userSendArg_      = 0;
  userErrorHandler_ = 0;
  userErrorArg_     = 0;
  
  // Create unassigned TCP/IP network streams for control.  This
  // should be long enough to accomodate the network prefix, the
  // antennas bitmask, and the command itself
  
  nrs_->setBuffer(NULL, NET_PREFIX_LEN + sizeof(unsigned) + net_max_obj_size(&rtc_cmd_table));
  nss_->setBuffer(NULL, NET_PREFIX_LEN + sizeof(unsigned) + net_max_obj_size(&rtc_cmd_table));

  // Install our handlers as the default handlers

  NetHandler::installReadHandler(readHandler, this);
  NetHandler::installReadErrorHandler(errorHandler, this);
  NetHandler::installSendHandler(sendHandler, this);
  NetHandler::installSendErrorHandler(errorHandler, this);
}

/**.......................................................................
 * Destructor.
 */
NetCmdHandler::~NetCmdHandler() {};

/**.......................................................................
 * Get the last read network command.
 */
sza::util::NetCmd* NetCmdHandler::getLastReadNetCmd()
{ 
  return &lastReadNetCmd_;
}

/**.......................................................................
 * Get the last sent network command.
 */
sza::util::NetCmd* NetCmdHandler::getLastSentNetCmd()
{ 
  return &lastSentNetCmd_;
}

/**.......................................................................
 * Pack a network command.
 */
void NetCmdHandler::packNetCmd(sza::util::NetCmd* netCmd)
{ 
  // Pack the command into our send buffer.
   
  packNetCmd(&netCmd->rtc_, netCmd->opcode_);
}

/**.......................................................................
 * Pack a network command.
 */
void NetCmdHandler::packNetCmd(sza::array::RtcNetCmd* rtc, 
			       sza::array::NetCmdId opcode)
{ 
  // Copy it into our last sent buffer

  lastSentNetCmd_.opcode_ = opcode;
  lastSentNetCmd_.rtc_    = *rtc;

  // Pack the command into our send buffer.
   
  nss_->startPut((int)opcode);
  nss_->putLong(1, &rtc->antennas);
  nss_->putObj(&rtc_cmd_table, opcode, &rtc->cmd);
  nss_->endPut();
}

/**.......................................................................
 * Read a NetCmd out of the network buffer
 */
void NetCmdHandler::readNetCmd()
{
  // Read the command type and unpack the corresponding command
  // context.
  
  nrs_->startGet((int*)&lastReadNetCmd_.opcode_);
  
  if(Debug::debugging(Debug::DEBUG8)) {
    if(lastReadNetCmd_.opcode_ == sza::array::NET_INIT_CMD)
      DBPRINT(true, Debug::DEBUG8, "Got an init command");
  }
  
  nrs_->getLong(1, &lastReadNetCmd_.rtc_.antennas);
  nrs_->getObj(&rtc_cmd_table, lastReadNetCmd_.opcode_, 
	      &lastReadNetCmd_.rtc_.cmd);
  nrs_->endGet();
}

/**.......................................................................
 * A handler to be called when a message has been completely read.
 */
NET_READ_HANDLER(NetCmdHandler::readHandler)
{
  NetCmdHandler* netHandler = (NetCmdHandler*) arg;

  netHandler->readNetCmd();

  if(netHandler->userReadHandler_ != 0)
    netHandler->userReadHandler_(netHandler->userReadArg_);
}

/**.......................................................................
 * A handler to be called when a message has been completely send.
 */
NET_SEND_HANDLER(NetCmdHandler::sendHandler)
{
  NetCmdHandler* netHandler = (NetCmdHandler*) arg;

  if(netHandler->userSendHandler_ != 0)
    netHandler->userSendHandler_(netHandler->userSendArg_);
}

/**.......................................................................
 * A handler to be called when an error has occurred
 */
NET_ERROR_HANDLER(NetCmdHandler::errorHandler)
{
  NetCmdHandler* netHandler = (NetCmdHandler*) arg;

  if(netHandler->userErrorHandler_ != 0)
    netHandler->userErrorHandler_(netHandler->userErrorArg_);
}

/**.......................................................................
 * Methods to install user-defined handlers
 */
void NetCmdHandler::installReadHandler(NET_READ_HANDLER(*handler), void* arg)
{
  userReadHandler_ = handler;
  userReadArg_ = arg;
}

/**.......................................................................
 * Methods to install user-defined handlers
 */
void NetCmdHandler::installSendHandler(NET_SEND_HANDLER(*handler), void* arg)
{
  userSendHandler_ = handler;
  userSendArg_ = arg;
}

/**.......................................................................
 * Methods to install user-defined handlers
 */
void NetCmdHandler::installErrorHandler(NET_READ_HANDLER(*handler), void* arg)
{
  userErrorHandler_ = handler;
  userErrorArg_ = arg;
}










