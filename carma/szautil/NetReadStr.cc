#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/NetReadStr.h"

using namespace sza::util;

#define THROW_ERROR(fn) \
  { \
    ThrowError("Error occurred in: " << fn);\
  }

/**.......................................................................
 * Constructor.
 */
NetReadStr::NetReadStr() 
{
  privateConstructor(-1, 0);
};

/**.......................................................................
 * Constructor with file descriptor and size.
 */
NetReadStr::NetReadStr(int fd, unsigned long size) 
{
  privateConstructor(fd, size);
}

/**.......................................................................
 * The substance of any constructor for this class.
 */
void NetReadStr::privateConstructor(int fd, unsigned long size)
{
  LogStream errStr;

  readHandler_  = 0;
  readArg_      = 0;
  errorHandler_ = 0;
  errorArg_     = 0;

  netStream_ = 0;
  netBufAllocated_ = false;
  attached_        = false;

  if((netStream_ = new_NetReadStr(fd, size)) == 0) {
    errStr.appendMessage(true, "Allocate failed in new_NetReadStr()");
    throw Error(errStr);
  }

  if(fd >= 0)
    attached_ = true;

  if(size > 0)
    netBufAllocated_ = true;
}

/**.......................................................................
 * Install a read handler
 */
void NetReadStr::installReadHandler(NET_READ_HANDLER(*handler), void* arg)
{
  readHandler_ = handler;
  readArg_     = arg;
}

/**.......................................................................
 * Install a error handler
 */
void NetReadStr::installErrorHandler(NET_READ_HANDLER(*handler), void* arg)
{
  errorHandler_ = handler;
  errorArg_     = arg;
}

/**.......................................................................
 * Destructor.
 */
NetReadStr::~NetReadStr() 
{
  netStream_ = del_NetReadStr(netStream_);
};


/**.......................................................................
 * Attach this network buffer to a file descriptor.
 */
void NetReadStr::attach(int fd)
{
  attach_NetReadStr(netStream_, fd);
  attached_ = fd >= 0 ? false : true;
}

/**.......................................................................
 * Attach this network buffer to a file descriptor.
 */
void NetReadStr::setBuffer(void* buffer, unsigned int size)
{
  LogStream errStr;

  if(net_set_buffer(netStream_->net, buffer, size)==0) {
    errStr.appendMessage(true, "Error in net_set_buffer()");
    throw Error(errStr);
  }

  netBufAllocated_ = true;
}

/**.......................................................................
 * Read a message into our network buffer from a socket.
 */
NetReadStr::NetReadId NetReadStr::read()
{
  return privateRead(-1);
}

/**.......................................................................
 * Read a message into our network buffer from a socket.
 */
NetReadStr::NetReadId NetReadStr::read(int fd)
{
  return privateRead(fd);
}

/**.......................................................................
 * Private substance of the above read commands.
 */
NetReadStr::NetReadId NetReadStr::privateRead(int fd)
{
  LogStream errStr;
  NetReadId readState;

  // If a file descriptor was specified, attach to it first.

  if(fd >= 0)
    attach(fd);

  // Read as much of the message as possible.

  switch (state()) {
  case NET_READ_DONE:
  case NET_READ_SIZE:
  case NET_READ_DATA:

    // If the message hasn't been read at all yet, or is
    // in the process of being read, continue reading.

    readState = privateState((sza::array::NetReadStr::NetReadId)
			     nrs_read_msg(netStream_));

    // Check the state after the last read

    if(readState == NET_READ_ERROR || readState == NET_READ_CLOSED) {

      // If an error occurred, call our error handler, if we have one.
      
      if(errorHandler_ != 0)
	errorHandler_(errorArg_);

    } else if(readState == NET_READ_DONE) {

      // If a read finished, call our read handler, if we have one.
      
      if(readHandler_ != 0)
	readHandler_(readArg_);
    }

    break;

    // Else an error occurred.

  default:

    // If an error occurred, call our error handler, if we have one.
      
    if(errorHandler_ != 0)
      errorHandler_(errorArg_);

    break;
  }
  return readState;
}

/**.......................................................................
 * Private method to return the state.
 */
NetReadStr::NetReadId 
NetReadStr::privateState(sza::array::NetReadStr::NetReadId id)
{
  LogStream errStr;

  switch (id) {
  case sza::array::NetReadStr::NET_READ_SIZE:
    return NET_READ_SIZE;
    break;
  case sza::array::NetReadStr::NET_READ_DATA:
    return NET_READ_DATA;
    break;
  case sza::array::NetReadStr::NET_READ_DONE:
    return NET_READ_DONE;
    break;
  case sza::array::NetReadStr::NET_READ_CLOSED:
    return NET_READ_CLOSED;
    break;
  case sza::array::NetReadStr::NET_READ_ERROR:
    return NET_READ_ERROR;
    break;
  default:
    errStr.appendMessage(true, "Unrecognized return code in nrs_read_msg()");
    throw Error(errStr);
  }
}

/**.......................................................................
 * Public method to return the state.
 */
NetReadStr::NetReadId NetReadStr::state()
{
  return privateState(netStream_->state);
}

/**.......................................................................
 * Return the file descriptor to which we're currently attached.
 */
int NetReadStr::getFd()
{
  return netStream_->fd;
}

/**.......................................................................
 * Start unpacking a message from a network buffer.
 */
void NetReadStr::startGet(int *opcode)
{
  if(net_start_get(netStream_->net, opcode))
    THROW_ERROR("net_start_get");
}

/**.......................................................................
 * Finish unpacking a message from a network buffer.
 */
void NetReadStr::endGet()
{
  if(net_end_get(netStream_->net))
    THROW_ERROR("net_end_get");
}

/**.......................................................................
 * Get a char from a network buffer.
 */
void NetReadStr::getChar(long ndata, unsigned char *data)
{
  if(net_get_char(netStream_->net, ndata, data))
    THROW_ERROR("net_get_char");
}

/**.......................................................................
 * Get a short from a network buffer.
 */
void NetReadStr::getShort(long ndata, unsigned short *data)
{
  if(net_get_short(netStream_->net, ndata, data))
    THROW_ERROR("net_get_short");
}

/**.......................................................................
 * Get a long from a network buffer.
 */
void NetReadStr::getLong(long ndata, unsigned long *data)
{
  if(net_get_long(netStream_->net, ndata, data))
    THROW_ERROR("net_get_long");
}

/**.......................................................................
 * Get a float from a network buffer.
 */
void NetReadStr::getFloat(long ndata, float *data)
{
  if(net_get_float(netStream_->net, ndata, data))
    THROW_ERROR("net_get_float");
}

/**.......................................................................
 * Get a double from a network buffer.
 */
void NetReadStr::getDouble(long ndata, double *data)
{
  if(net_get_double(netStream_->net, ndata, data))
    THROW_ERROR("net_get_double");
}

/**.......................................................................
 * Pack an object to a network buffer.
 */
void NetReadStr::getObj(const NetObjTable* types, int id, void *obj)
{
  if(net_to_obj(types, netStream_->net, id, obj))
    THROW_ERROR("net_to_obj");
}

/**.......................................................................
 * Incrementally read bytes from a network buffer.
 */
void NetReadStr::incNget(long nbytes)
{
  if(net_inc_nget(netStream_->net, nbytes))
    THROW_ERROR("net_inc_nget");
}

