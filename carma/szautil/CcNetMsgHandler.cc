#include "carma/szautil/CcNetMsg.h"
#include "carma/szautil/CcNetMsgHandler.h"

#include "carma/szaarrayutils/netobj.h"
#include "carma/szaarrayutils/rtcnetcoms.h"

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
CcNetMsgHandler::CcNetMsgHandler() 
{
  nss_ = 0;
  nrs_ = 0;

  nrs_ = new NetReadStr(-1, NET_PREFIX_LEN + net_max_obj_size(&rtc_msg_table));
  nss_ = new NetSendStr(-1, NET_PREFIX_LEN + net_max_obj_size(&rtc_msg_table));
}

/**.......................................................................
 * Destructor.
 */
CcNetMsgHandler::~CcNetMsgHandler() 
{
  if(nrs_ != 0)
    delete nrs_;

  if(nss_ != 0)
    delete nss_;
}

/**.......................................................................
 * Attach the network read stream to a socket.
 */
void CcNetMsgHandler::attachReadStream(int fd)
{
  (void) nrs_->attach(fd);
}

/**.......................................................................
 * Attach the network send stream to a socket.
 */
void CcNetMsgHandler::attachSendStream(int fd)
{
  (void) nss_->attach(fd);
}

/**.......................................................................
 * Read a message packed into our network buffer from a socket
 * described by a previously attached fd.
 */
NetReadStr::NetReadId CcNetMsgHandler::read()
{
  NetReadStr::NetReadId state;

  state = nrs_->read();

  if(state == NetReadStr::NET_READ_DONE) {

    // Read the message type and unpack the corresponding message

    nrs_->startGet((int*)&netMsg_.type);
    nrs_->getObj(&cc_msg_table, netMsg_.type, &netMsg_.body);
    nrs_->endGet();
  }
  return state;
}

/**.......................................................................
 * Read a message packed into our network buffer from the specified
 * socket.
 */
NetReadStr::NetReadId CcNetMsgHandler::read(int fd)
{
  NetReadStr::NetReadId state;

  state = nrs_->read(fd);

  if(state == NetReadStr::NET_READ_DONE) {

    // Read the message type and unpack the corresponding message

    nrs_->startGet((int*)&netMsg_.type);
    nrs_->getObj(&cc_msg_table, netMsg_.type, &netMsg_.body);
    nrs_->endGet();
  }
  return state;
}

/**.......................................................................
 * Send a message previously packed into our network buffer to a
 * socket described by a previously attached fd.
 */
NetSendStr::NetSendId CcNetMsgHandler::send()
{
  return nss_->send();
}

/**.......................................................................
 * Send a message packed into our network buffer to the
 * specified socket.  
 */
NetSendStr::NetSendId CcNetMsgHandler::send(int fd)
{
  return nss_->send(fd);
}

/**.......................................................................
 * Send a message to a socket described by a previously attached fd.
 */
NetSendStr::NetSendId CcNetMsgHandler::send(CcNetMsg* msg)
{
  LogStream ls;

  if(nss_->state() != NetSendStr::NET_SEND_DONE) {
    ls.appendMessage(true, "Last message wasn't completely sent");
    throw Error(ls);
  }

  // Pack the message in our buffer and send it.

  packCcNetMsg(msg);
  return nss_->send();
}

/**.......................................................................
 * Send a message to a socket
 */
NetSendStr::NetSendId CcNetMsgHandler::send(int fd, CcNetMsg* msg)
{
  LogStream ls;

  if(nss_->state() != NetSendStr::NET_SEND_DONE) {
    ls.appendMessage(true, "Last message wasn't completely sent");
    throw Error(ls);
  }

  // Pack the message in our buffer and send it.

  packCcNetMsg(msg);
  return nss_->send(fd);
}

/**.......................................................................
 * Pack a network message into our send buffer.
 */
void CcNetMsgHandler::packCcNetMsg(CcNetMsg* msg)
{
  nss_->startPut(msg->type);
  nss_->putObj(&cc_msg_table, msg->type, (void*)&msg->body);
  nss_->endPut();
}

/**.......................................................................
 * Return the last message read
 */
sza::util::CcNetMsg* CcNetMsgHandler::getCcNetMsg()
{
  return &netMsg_;
}
