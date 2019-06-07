#include "carma/szautil/Communicator.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/IoLock.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/TimeVal.h"

using namespace std;
using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
Communicator::Communicator()
{
  client_ = 0;
}

/**.......................................................................
 * Destructor.
 */
Communicator::~Communicator() 
{
  // Delete the client if it was successfully allocated
  
  if(client_ != 0) {
    delete client_;
    client_ = 0;
  }
}

/**.......................................................................
 * Terminate a command sequence to the power strip.
 */
void Communicator::terminateCommSequence(bool error)
{
  // Disconnect the client

  client_->disconnect();
}

/**.......................................................................
 * Read a line from the power strip and determine what to do
 */
void Communicator::processClientMessage()
{
  // Concatenate the last line read from the serial port
  
  concatenateString(os_);
  
  // If the line just read contains the expected response string, send
  // the next command in our command stack
  
  checkLine();
}

void Communicator::checkLine()
{
  String line = os_.str();
  

  // If we are done with commands, terminate the communication
  // sequence
  
  if(sentStringIter_ == sentStrings_.end() &&
     rcvdStringIter_ == rcvdStrings_.end()) {
    return terminateCommSequence(false);
  }

  // If this line contains the string we are searching for, or if we
  // are matching any string, proceed:

  if(line.contains(rcvdStringIter_->start_) || rcvdStringIter_->matchAny_) {
    

    // Call any parser which was registered to be notified when new
    // data arrived
    
    if(rcvdStringIter_->parser_ != 0) {
      rcvdStringIter_->parser_(line, rcvdStringIter_->arg_);
    }
    
    // If we matched the string we're looking for, increment to the
    // next received string we want to watch for.
    
    if(!rcvdStringIter_->matchAny_) {
      
      advanceIterator(true);

      // If the current command was to match any string, we should
      // catch the case where the last response contains the next
      // string we want to search for.  Do this by incrementing the
      // iterator to the next string without reading the next line
      // from the client
      
    }  else if(line.contains(rcvdStringIter_->stop_)) {
      // Call the termination fn, if any

      if(rcvdStringIter_->endParser_ != 0) {
	rcvdStringIter_->endParser_(rcvdStringIter_->endArg_);
      }

      // And increment to the next string
      
      advanceIterator(false);
      checkLine();
    }
  }
}

void Communicator::checkIterators()
{
  if(sentStringIter_ == sentStrings_.end() &&
     rcvdStringIter_ == rcvdStrings_.end()) {
    terminateCommSequence(false);
  }
} 

/**.......................................................................
 * Reset in preparation for searching for the next string
 */
void Communicator::advanceIterator(bool bufferReset)
{
  // Advance to the next string
  
  rcvdStringIter_++;
  checkIterators();

  // Reset the timeout timer
  
  timer_.setToCurrentTime();
  
  // And clear the buffer
  
  if(bufferReset)
    os_.str("");
}


/**.......................................................................,
 * React to a failure to reply
 */
void Communicator::registerTimeOut()
{
  // And terminate the communication sequence
  
  terminateCommSequence(true);
}

/**.......................................................................
 * Return true if we timed out waiting for a command
 */
bool Communicator::timedOut()
{
  TimeVal current, diff;
  current.setToCurrentTime();
  
  diff = current - timer_;
  
  bool timedOut = (diff.getSeconds() > COMMAND_TIMEOUT_SEC);
  
  if(timedOut)
    DBPRINT(true, Debug::DEBUG9, "Communicator " << this << " timed out timer_ = " << timer_
	    << " current = " << current);
  
  return timedOut;
}

/**.......................................................................
 * Return the fd
 */
int Communicator::getFd()
{
  return client_->getFd();
}

Communicator::RcvdStr::RcvdStr(const RcvdStr& str)
{
  initialize();
  
  start_     = str.start_;
  stop_      = str.stop_;
  parser_    = str.parser_;
  arg_       = str.arg_;
  endParser_ = str.endParser_;
  endArg_    = str.endArg_;
  matchAny_  = str.matchAny_;
}

Communicator::RcvdStr::RcvdStr()
{
  initialize();
}

void Communicator::RcvdStr::initialize() 
{
  parser_    = 0;
  arg_       = 0;
  endParser_ = 0;
  endArg_    = 0;
  matchAny_  = false;
}

Communicator::RcvdStr::RcvdStr(std::string str, COMM_PARSER_FN(*parser), 
			       void* arg)
{
  initialize();
  
  start_     = str;
  parser_    = parser;
  arg_       = arg;
}

Communicator::RcvdStr::RcvdStr(std::string start, std::string stop, 
			       COMM_PARSER_FN(*parser), void* arg,
			       COMM_END_PARSER_FN(*endParser), void* endArg)
{
  initialize();
  
  start_     = start;
  stop_      = stop;
  parser_    = parser;
  arg_       = arg;
  endParser_ = endParser;
  endArg_    = endArg;
  matchAny_  = false;
}

void Communicator::sendNextString()
{
  writeString(*sentStringIter_);

  // Set the current time
  
  timer_.setToCurrentTime();
  
  // And increment to the next send str
  
  sentStringIter_++;
  checkIterators();
}

COMM_PARSER_FN(Communicator::sendNextString)
{
  Communicator* comm = (Communicator*) arg;
  comm->sendNextString();
}

void Communicator::writeString(std::string str)
{
  client_->writeString(str);
}

/**.......................................................................
 * Overloaded function to concatenate a string
 */
void Communicator::concatenateString(std::ostringstream& os)
{
  client_->concatenateString(os);
}

void Communicator::execSendNextString()
{
  writeString(*sentStringIter_);
  
  // Set the current time
  
  timer_.setToCurrentTime();
  
  // And increment to the next send str
  
  sentStringIter_++;
  checkIterators();
}

/**.......................................................................
 * If we are connected to the device, this method executes the
 * command sequence 
 */
void Communicator::run() {}

