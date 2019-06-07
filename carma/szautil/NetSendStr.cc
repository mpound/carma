#include "carma/szaarrayutils/netbuf.h"

#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"

#include "carma/szautil/NetSendStr.h"

using namespace sza::util;

#define THROW_ERROR(fn) \
  { \
    ThrowError("Error occurred in: " << fn);\
  }

/**.......................................................................
 * Constructor.
 */
NetSendStr::NetSendStr() 
{
  privateConstructor(-1, 0);
};

/**.......................................................................
 * Constructor with file descriptor and size.
 */
NetSendStr::NetSendStr(int fd, unsigned long size) 
{
  privateConstructor(fd, size);
}

/**.......................................................................
 * The substance of any constructor for this class.
 */
void NetSendStr::privateConstructor(int fd, unsigned long size)
{
  LogStream errStr;

  sendHandler_  = 0;
  sendArg_      = 0;
  errorHandler_ = 0;
  errorArg_     = 0;

  netStream_ = 0;
  netBufAllocated_ = false;
  attached_        = false;

  if((netStream_ = new_NetSendStr(fd, size)) == 0) {
    errStr.appendMessage(true, "Allocate failed in new_NetSendStr()");
    throw Error(errStr);
  }

  if(fd >= 0)
    attached_ = true;

  if(size > 0)
    netBufAllocated_ = true;
}

/**.......................................................................
 * Install a send handler
 */
void NetSendStr::installSendHandler(NET_SEND_HANDLER(*handler), void* arg)
{
  sendHandler_ = handler;
  sendArg_     = arg;
}

/**.......................................................................
 * Install a error handler
 */
void NetSendStr::installErrorHandler(NET_SEND_HANDLER(*handler), void* arg)
{
  errorHandler_ = handler;
  errorArg_     = arg;
}

/**.......................................................................
 * Destructor.
 */
NetSendStr::~NetSendStr() 
{
  netStream_ = del_NetSendStr(netStream_);
};


/**.......................................................................
 * Attach this network buffer to a file descriptor.
 */
void NetSendStr::attach(int fd)
{
  attach_NetSendStr(netStream_, fd);
  attached_ = fd >= 0 ? false : true;
}

/**.......................................................................
 * Attach this network buffer to a file descriptor.
 */
void NetSendStr::setBuffer(void* buffer, unsigned int size)
{
  LogStream errStr;

  if(net_set_buffer(netStream_->net, buffer, size)==0) {
    errStr.appendMessage(true, "Error in net_set_buffer()");
    throw Error(errStr);
  }

  netBufAllocated_ = true;
}

/**.......................................................................
 * Write the message in our network buffer to a socket.
 */
NetSendStr::NetSendId NetSendStr::send()
{
  return privateSend(-1);
}

/**.......................................................................
 * Write the message in our network buffer to a socket.
 */
NetSendStr::NetSendId NetSendStr::send(int fd)
{
  return privateSend(fd);
}

/**.......................................................................
 * Private substance of the above read commands.
 */
NetSendStr::NetSendId NetSendStr::privateSend(int fd)
{
  LogStream errStr;
  NetSendId sendState;

  // If a file descriptor was specified, attach to it first.

  if(fd >= 0)
    attach(fd);

  // Send as much of the message as possible.

  switch (state()) {
  case NET_SEND_DONE:
  case NET_SEND_DATA:

    // If the message in our buffer hasn't been sent at all yet, or is
    // in the process of being sent, continue sending.

    sendState = privateState((sza::array::NetSendStr::NetSendId)
			     nss_send_msg(netStream_));
    
    // Check the state after the last read

    if(sendState == NET_SEND_ERROR || sendState == NET_SEND_CLOSED) {

      DBPRINT(true, Debug::DEBUG5, "An error occurred while sending ("
	      << sendState << "). "
	      << "errorHandler is: "
	      << (errorHandler_ ? "not NULL" : "NULL"));

      // If an error occurred, call our error handler, if we have one.
      
      if(errorHandler_ != 0)
	errorHandler_(errorArg_);

    } else if(sendState == NET_SEND_DONE) {

      DBPRINT(true, Debug::DEBUG5, "Send is done.  sendHandler is: "
	      << (sendHandler_ ? "not NULL" : "NULL"));

      // If a send finished, call our send handler, if we have one.
      
      if(sendHandler_ != 0)
	sendHandler_(sendArg_);
    }
    break;

    // Else an error occurred.

  default:

    DBPRINT(true, Debug::DEBUG7, "An error occurred while sending");

    // If an error occurred, call our error handler, if we have one.
      
    if(errorHandler_ != 0)
      errorHandler_(errorArg_);

    break;
  }

  return sendState;
}

/**.......................................................................
 * Private method to return the state.
 */
NetSendStr::NetSendId 
NetSendStr::privateState(sza::array::NetSendStr::NetSendId id)
{
  LogStream errStr;

  switch (id) {
  case sza::array::NetSendStr::NET_SEND_DATA:
    return NET_SEND_DATA;
    break;
  case sza::array::NetSendStr::NET_SEND_DONE:
    return NET_SEND_DONE;
    break;
  case sza::array::NetSendStr::NET_SEND_CLOSED:
    return NET_SEND_CLOSED;
    break;
  case sza::array::NetSendStr::NET_SEND_ERROR:
    return NET_SEND_ERROR;
    break;
  default:
    errStr.appendMessage(true, "Unrecognized return code in nss_send_msg()");
    throw Error(errStr);
  }
}

/**.......................................................................
 * Public method to return the state.
 */
NetSendStr::NetSendId NetSendStr::state()
{
  return privateState(netStream_->state);
}

/**.......................................................................
 * Return the file descriptor to which we're currently attached.
 */
int NetSendStr::getFd()
{
  return netStream_->fd;
}

/**.......................................................................
 * Start packing a message into a network buffer.
 */
void NetSendStr::startPut(int opcode)
{
  if(net_start_put(netStream_->net, opcode))
    THROW_ERROR("net_start_put");
}

/**.......................................................................
 * Finish packing a message into a network buffer.
 */
void NetSendStr::endPut()
{
  if(net_end_put(netStream_->net))
    THROW_ERROR("net_end_put");
}

/**.......................................................................
 * Pack a byte into a network buffer.
 */
void NetSendStr::putChar(long ndata, unsigned char *data)
{
  if(net_put_char(netStream_->net, ndata, data))
    THROW_ERROR("net_put_char");
}

/**.......................................................................
 * Pack a short into a network buffer
 */
void NetSendStr::putShort(long ndata, unsigned short *data)
{
  if(net_put_short(netStream_->net, ndata, data))
    THROW_ERROR("net_put_short");
}

/**.......................................................................
 * Pack a long into a network buffer.
 */
void NetSendStr::putLong(long ndata, unsigned long *data)
{
  if(net_put_long(netStream_->net, ndata, data))
    THROW_ERROR("net_put_long");
}

/**.......................................................................
 * Pack a float into a network buffer.
 */
void NetSendStr::putFloat(long ndata, float *data)
{
  if(net_put_float(netStream_->net, ndata, data))
    THROW_ERROR("net_put_float");
}

/**.......................................................................
 * Pack a double into a network buffer.
 */
void NetSendStr::putDouble(long ndata, double *data)
{
  if(net_put_double(netStream_->net, ndata, data))
    THROW_ERROR("net_put_double");
}

/**.......................................................................
 * Pack an object to a network buffer.
 */
void NetSendStr::putObj(const NetObjTable* types, int id, void *obj)
{
  if(obj_to_net(types, netStream_->net, id, obj))
    THROW_ERROR("obj_to_net");
}

/**.......................................................................
 * Incrementally put bytes into a network buffer.
 */
void NetSendStr::incNput(long nbytes)
{
  if(net_inc_nput(netStream_->net, nbytes) < 0)
    THROW_ERROR("net_inc_nput");
}
