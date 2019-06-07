#include <iostream>
#include <sstream>

#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <fcntl.h>
#include <time.h>
#include <sys/time.h> // gettimeofday() 
#include <signal.h>

#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/PipeQueue.h"
#include "carma/szautil/Thread.h"

using namespace sza::util;
using namespace std;

/**.......................................................................
 * PipeQueue constructor function
 */
PipeQueue::PipeQueue() : Pipe() {}

/**.......................................................................
 * PipeQueue destructor function
 */
PipeQueue::~PipeQueue() {}

/**.......................................................................
 * Read from the PipeQueue.  This should only be called after select()
 * has returned because new data are available. 
 */
PipeState PipeQueue::read(void *buffer, size_t nbyte, long timeout)
{
  try {

    queueGuard_.lock();
    
    PipeState state=PIPE_ERROR;
    
    // In this class, a single byte is written/read for each message.
    // This allows us to use select() as with the baseclass Pipe, but
    // doesn't restrict us to messages of any particular length.
  
    // If we successfully read a single byte, then get the last
    // message off the queue
  
    unsigned char byte;
    ssize_t status = ::read(readFd(), &byte, 1);

    if(status == 1) {
      messages_.pop(buffer, nbyte);
      state = PIPE_OK;
    }

    queueGuard_.unlock();

    return state;

  } catch(Exception& err) {

    queueGuard_.unlock();
    COUT(err.what());
    throw err;

  } catch(...) {

    queueGuard_.unlock();
    ThrowError("Caught unknown exception");

  }
}

/**.......................................................................
 * Write a new message to the PipeQueue
 */
PipeState PipeQueue::write(void *buffer, size_t nbyte, long timeout)
{
  try {

    queueGuard_.lock();

    PipeState state = PIPE_ERROR;

    // In this class, a single byte is written/read for each message.
    // This allows us to use select() as with the baseclass Pipe, but
    // doesn't restrict us to messages of any particular length.
    
    // If we successfully wrote a single byte, then get the last
    // message off the queue
  
    unsigned char byte=1;
    ssize_t status = ::write(writeFd(), &byte, 1);
  
    if(status == 1) {
      messages_.push(buffer, nbyte);
      state = PIPE_OK;
    }

    queueGuard_.unlock();

    return state;

  } catch(Exception& err) {

    queueGuard_.unlock();
    COUT(err.what());
    throw err;

  } catch(...) {

    queueGuard_.unlock();
    ThrowError("Caught unknown exception");

  }
}

//-----------------------------------------------------------------------
// Methods of QueueNode
//-----------------------------------------------------------------------

/**.......................................................................
 * Constructor
 */
PipeQueue::QueueNode::QueueNode()
{
  nbyte_  = 0;
  buffer_ = 0;
}

PipeQueue::QueueNode::QueueNode(void* buffer, size_t nbyte) 
{
  nbyte_   = nbyte;
  
  buffer_ = 0;
  buffer_ = new unsigned char[nbyte_];

  ::memcpy(buffer_, buffer, nbyte);
}

/**.......................................................................
 * Copy constructor
 */
PipeQueue::QueueNode::QueueNode(const QueueNode& node) 
{
  *this = node;
}

/**.......................................................................
 * Copy constructor
 */
PipeQueue::QueueNode::QueueNode(QueueNode& node) 
{
  *this = node;
}

/**.......................................................................
 * Copy constructor
 */
void PipeQueue::QueueNode::operator=(const QueueNode& node) 
{
  *this = (QueueNode&) node;
}

/**.......................................................................
 * Copy constructor
 */
void PipeQueue::QueueNode::operator=(QueueNode& node) 
{
  nbyte_   = node.nbyte_;
  
  buffer_ = 0;

  buffer_ = new unsigned char[nbyte_];
  
  ::memcpy(buffer_, node.buffer_, nbyte_);
}

/**.......................................................................
 * Destructor
 */
PipeQueue::QueueNode::~QueueNode() 
{
  if(buffer_) {
    delete buffer_;
    buffer_ = 0;
  }
}

//-----------------------------------------------------------------------
// Methods of MsgQueue
//-----------------------------------------------------------------------

bool PipeQueue::MsgQueue::empty()
{
  return queue_.empty();
}

/**.......................................................................
 * Push a new message onto the queue
 */
void PipeQueue::MsgQueue::push(void* buffer, size_t nbyte) 
{
  PipeQueue::QueueNode newNode(buffer, nbyte);
  queue_.push(newNode);
}

/**.......................................................................
 * Get the last message from the queue
 */
void PipeQueue::MsgQueue::pop(void* buffer, size_t nbyte) 
{
  if(queue_.empty())
    ThrowError("Queue is empty");

  PipeQueue::QueueNode& node = queue_.front();
  
  if(nbyte != node.nbyte_)
    ThrowError("Number of bytes in target (" << nbyte << ") and source (" 
	       << node.nbyte_ << ") don't match");
  
  ::memcpy(buffer, node.buffer_, nbyte);
  
  // And remove the front of the list
    
  queue_.pop();
}

PipeQueue::QueueNode& PipeQueue::MsgQueue::front()
{
  return queue_.front();
}

void PipeQueue::MsgQueue::pop()
{
  queue_.pop();
}
