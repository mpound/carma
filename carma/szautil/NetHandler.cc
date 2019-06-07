#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/NetHandler.h"

using namespace sza::util;

void NetHandler::initialize()
{
  fd_ = -1;
  nrs_ = 0;
  nss_ = 0;

  nrs_ = new NetReadStr(-1, 0);
  nss_ = new NetSendStr(-1, 0);
}

/**.......................................................................
 * Constructor.
 */
NetHandler::NetHandler() 
{
  initialize();
}

/**.......................................................................
 * Constructor.
 */
NetHandler::NetHandler(int fd) 
{
  initialize();
  attach(fd);
}

/**.......................................................................
 * Destructor.
 */
NetHandler::~NetHandler() {}

/**.......................................................................
 * Attach the network read stream to a socket.
 */
void NetHandler::attachReadStream(int fd)
{
  (void) nrs_->attach(fd);
}

/**.......................................................................
 * Attach the network send stream to a socket.
 */
void NetHandler::attachSendStream(int fd)
{
  (void) nss_->attach(fd);
}

/**.......................................................................
 * Attach all streams to the same socket.
 */
void NetHandler::attach(int fd)
{
  fd_ = fd;
  nrs_->attach(fd);
  nss_->attach(fd);
}

/**.......................................................................
 * Return the file descriptor the read stream is attached to.
 */
int NetHandler::getReadFd()
{
  return nrs_->getFd();
}

/**.......................................................................
 * Return the file descriptor the send stream is attached to.
 */
int NetHandler::getSendFd()
{
  return nss_->getFd();
}

/**.......................................................................
 * Return a single file descriptor to which all streams are attached.
 */
int NetHandler::getFd()
{
  return fd_;
}

/**.......................................................................
 * Read a message packed into our network buffer from a socket
 * described by a previously attached fd.
 */
NetReadStr::NetReadId NetHandler::read()
{
  return nrs_->read();
}

/**.......................................................................
 * Read a message packed into our network buffer from the specified
 * socket.
 */
NetReadStr::NetReadId NetHandler::read(int fd)
{
  return  nrs_->read(fd);
}

/**.......................................................................
 * Send a message previously packed into our network buffer to a
 * socket described by a previously attached fd.
 */
NetSendStr::NetSendId NetHandler::send()
{
  return nss_->send();
}

/**.......................................................................
 * Send a message packed into our network buffer to the
 * specified socket.  
 */
NetSendStr::NetSendId NetHandler::send(int fd)
{
  return nss_->send(fd);
}

/**.......................................................................
 * Methods to install user-defined handlers
 */
void NetHandler::installReadHandler(NET_READ_HANDLER(*handler), void* arg)
{
  nrs_->installReadHandler(handler, arg);
}

/**.......................................................................
 * Methods to install user-defined handlers
 */
void NetHandler::installReadErrorHandler(NET_READ_HANDLER(*handler), void* arg)
{
  nrs_->installErrorHandler(handler, arg);
}

/**.......................................................................
 * Methods to install user-defined handlers
 */
void NetHandler::installSendHandler(NET_SEND_HANDLER(*handler), void* arg)
{
  nss_->installSendHandler(handler, arg);
}

/**.......................................................................
 * Methods to install user-defined handlers
 */
void NetHandler::installSendErrorHandler(NET_ERROR_HANDLER(*handler), void* arg)
{
  nss_->installErrorHandler(handler, arg);
}

/**.......................................................................
 * Install the same error handler for both streams
 */
void NetHandler::installErrorHandler(NET_READ_HANDLER(*handler), void* arg)
{
  nrs_->installErrorHandler(handler, arg);
  nss_->installErrorHandler(handler, arg);
}

/**.......................................................................
 * Set the network buffer pointing to an external buffer, or
 * pass buffer=NULL to dynamically allocate it.
 */
void NetHandler::setReadBuffer(void* buffer, unsigned int size)
{
  nrs_->setBuffer(buffer, size);
}

/**.......................................................................
 * Set the network buffer pointing to an external buffer, or
 * pass buffer=NULL to dynamically allocate it.
 */
void NetHandler::setSendBuffer(void* buffer, unsigned int size)
{
  nss_->setBuffer(buffer, size);
}
