#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/NetStr.h"

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
NetStr::NetStr() 
{
  fd_ = -1;
  netReadStr_ = 0;
  netSendStr_ = 0;

  netReadStr_ = new NetReadStr(-1, 0);
  netSendStr_ = new NetSendStr(-1, 0);
};

/**.......................................................................
 * Constructor.
 */
NetStr::NetStr(int fd, unsigned long readSize, unsigned long sendSize) 
{
  LogStream errStr;

  fd_ = fd;
  netReadStr_ = 0;
  netSendStr_ = 0;

  netReadStr_ = new NetReadStr(fd, readSize);
  netSendStr_ = new NetSendStr(fd, sendSize);

  if(netReadStr_ == 0 || netSendStr_ == 0) {
    errStr.appendMessage(true, "Failed to allocate streams");
    throw Error(errStr);
  }
};

/**.......................................................................
 * Destructor.
 */
NetStr::~NetStr() 
{
  if(netReadStr_ == 0) {
    delete netReadStr_;
    netReadStr_ = 0;
  }

  if(netSendStr_ == 0) {
    delete netSendStr_;
    netSendStr_ = 0;
  }
};

/**.......................................................................
 * Get a reference to our NetReadStr object.
 */
NetReadStr* NetStr::getReadStr()
{
  return netReadStr_;
}

/**.......................................................................
 * Get a reference to our NetSendStr object.
 */
NetSendStr* NetStr::getSendStr()
{
  return netSendStr_;
}

/**.......................................................................
 * Attach our network buffers to a file descriptor.
 */
void NetStr::attach(int fd)
{
  fd_ = fd;
  netReadStr_->attach(fd);
  netSendStr_->attach(fd);
}

/**.......................................................................
 * Attach this network read buffer to a file descriptor.
 */
void NetStr::setReadBuffer(unsigned int* buffer, unsigned int size)
{
  netReadStr_->setBuffer(buffer, size);
}

/**.......................................................................
 * Attach this network send buffer to a file descriptor.
 */
void NetStr::setSendBuffer(unsigned int* buffer, unsigned int size)
{
  netSendStr_->setBuffer(buffer, size);
}

/**.......................................................................
 * Return the fd to which we are currently attached.
 */
int NetStr::getFd()
{
  return fd_;
}

/**.......................................................................
 * Send a message.
 */
NetSendStr::NetSendId NetStr::send()
{
  netSendStr_->send();
}

/**.......................................................................
 * Read a message.
 */
NetReadStr::NetReadId NetStr::read()
{
  netReadStr_->read();
}
