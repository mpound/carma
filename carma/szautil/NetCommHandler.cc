#include "carma/szautil/Debug.h"
#include "carma/szautil/NetCommHandler.h"

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
NetCommHandler::NetCommHandler() 
{
  DBPRINT(false, Debug::DEBUG6, "Inside NetCommHandler constructor ");
  antNum_.setId(AntNum::ANTNONE);
}

/**.......................................................................
 * Destructor.
 */
NetCommHandler::~NetCommHandler() {}

/**.......................................................................
 * Attach all network I/O streams to the same socket.
 */
void NetCommHandler::attach(int fd)
{
  netCmdHandler_.attachReadStream(fd);
  netMsgHandler_.attachReadStream(fd);

  netCmdHandler_.attachSendStream(fd);
  netMsgHandler_.attachSendStream(fd);
}

/**.......................................................................
 * Attach the network I/O streams to a socket.
 */
void NetCommHandler::attachReadStream(int fd)
{
  netCmdHandler_.attachReadStream(fd);
  netMsgHandler_.attachReadStream(fd);
}

/**.......................................................................
 * Attach the network I/O streams to a socket.
 */
void NetCommHandler::attachSendStream(int fd)
{
  netCmdHandler_.attachSendStream(fd);
  netMsgHandler_.attachSendStream(fd);
}

/**.......................................................................
 * Read all or part of a control-program command from the network.
 * If the command is completely received, interpret it and prepare
 * for the next command. Otherwise postpone interpreting the message
 * until a future call to this function receives its completion.
 */
NetReadStr::NetReadId NetCommHandler::readNetCmd()
{
  return netCmdHandler_.read();
}

/**.......................................................................
 * Read all or part of a control-program message from the network.
 * If the message is completely received, interpret it and prepare
 * for the next command. Otherwise postpone interpreting the message
 * until a future call to this function receives its completion.
 */
NetReadStr::NetReadId NetCommHandler::readNetMsg()
{
  DBPRINT(true, Debug::DEBUG6, "Reading message");

  return netMsgHandler_.read();
}

/**.......................................................................
 * Send a command previously packed into our network buffer to a
 * socket described by a previously attached fd.
 */
NetSendStr::NetSendId NetCommHandler::sendNetCmd()
{
  return  netCmdHandler_.send();
}

/**.......................................................................
 * Send a message previously packed into our network buffer to a
 * socket described by a previously attached fd.
 */
NetSendStr::NetSendId NetCommHandler::sendNetMsg()
{
  DBPRINT(true, Debug::DEBUG6, "Sending message");

  return netMsgHandler_.send();
}

/**.......................................................................
 * Return a single fd.  It is assumed that all streams in this handler
 * are attached to the same socket.
 */
int NetCommHandler::getFd()
{
  int readFd, sendFd;

  // Either of these might throw if the call is ambiguous, which is
  // ok.

  readFd = getReadFd();
  sendFd = getSendFd();

  // Otherwise throw if there is ambiguity betwen the read and send
  // fds, else return someting sensible.
 
  if(readFd >= 0 && sendFd >= 0 && readFd != sendFd) {
    LogStream errStr;
    errStr.appendMessage(true, "Call is ambiguous\n");
    throw Error(errStr);
  } else if(readFd >= 0) {
    return readFd;
  } else {
    return sendFd;
  }
}

/**.......................................................................
 * Return read fd.
 */
int NetCommHandler::getReadFd()
{
  int msgFd = netMsgHandler_.getReadFd();
  int cmdFd = netCmdHandler_.getReadFd();

  if(msgFd >= 0 && cmdFd >= 0 && msgFd != cmdFd) {
    LogStream errStr;
    errStr.appendMessage(true, "Call is ambiguous\n");
    throw Error(errStr);
  } else if(msgFd >= 0) {
    return msgFd;
  } else {
    return cmdFd;
  }
}

/**.......................................................................
 * Return send fd.
 */
int NetCommHandler::getSendFd()
{
  int msgFd = netMsgHandler_.getSendFd();
  int cmdFd = netCmdHandler_.getSendFd();

  if(msgFd >= 0 && cmdFd >= 0 && msgFd != cmdFd) {
    LogStream errStr;
    errStr.appendMessage(true, "Call is ambiguous\n");
    throw Error(errStr);
  } else if(msgFd >= 0) {
    return msgFd;
  } else {
    return cmdFd;
  }
}

/**.......................................................................
 * Return the last read command
 */
sza::util::NetCmd* NetCommHandler::getLastReadNetCmd()
{
  return netCmdHandler_.getLastReadNetCmd();
}

/**.......................................................................
 * Return the last sent command
 */
sza::util::NetCmd* NetCommHandler::getLastSentNetCmd()
{
  return netCmdHandler_.getLastSentNetCmd();
}

/**.......................................................................
 * Return the last read message
 */
sza::util::NetMsg* NetCommHandler::getLastReadNetMsg()
{
  return netMsgHandler_.getLastReadNetMsg();
}

/**.......................................................................
 * Return the last sent message
 */
sza::util::NetMsg* NetCommHandler::getLastSentNetMsg()
{
  return netMsgHandler_.getLastSentNetMsg();
}

/**.......................................................................
 * Pack a network message into our send buffer
 */
void NetCommHandler::packNetMsg(sza::util::NetMsg* netMsg)
{
  netMsgHandler_.packNetMsg(netMsg);
}

/**.......................................................................
 * Pack a network command into our send buffer
 */
void NetCommHandler::packRtcNetCmd(sza::array::RtcNetCmd* rtc, 
				   sza::array::NetCmdId opcode)
{
  netCmdHandler_.packNetCmd(rtc, opcode);
}

/**.......................................................................
 * Pack a network command into our send buffer
 */
void NetCommHandler::packNetCmd(sza::util::NetCmd* netCmd)
{
  netCmdHandler_.packNetCmd(netCmd);
}

/**.......................................................................
 * Return a pointer to our NetMsg handler.
 */
NetMsgHandler* NetCommHandler::getNetMsgHandler()
{
  return &netMsgHandler_;
}

/**.......................................................................
 * Return a pointer to our NetCmd handler
 */
NetCmdHandler* NetCommHandler::getNetCmdHandler()
{
  return &netCmdHandler_;
}


