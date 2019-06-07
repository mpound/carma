#include "carma/szautil/LogMsgHandler.h"
#include "carma/szautil/Exception.h"

#include<iostream>

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
LogMsgHandler::LogMsgHandler() 
{
  seq_ = 0;
}

/**.......................................................................
 * Destructor.
 */
LogMsgHandler::~LogMsgHandler() {}

unsigned LogMsgHandler::nextSeq()
{
  unsigned seq;
  seqLock_.lock();
  seq = ++seq_;
  seqLock_.unlock();
  return seq;
}

void LogMsgHandler::append(unsigned seq, std::string text, LogMsg::Type type)
{
  std::map<unsigned, LogMsg*>::iterator 
    mess = messages_.find(seq);

  LogMsg* msg=0;

  if(mess != messages_.end()) {
    msg = messages_[seq];
  } else {
    msg = new LogMsg(seq);
    messages_[seq] = msg;
  }

  // And add the text

  msg->os_ << text;

  if(type != LogMsg::TYPE_UNSPEC)
    msg->type_ = type;
}

void LogMsgHandler::append(unsigned seq, std::string text, 
			   sza::array::LogStream nature)
{
  append(seq, text, nature==sza::array::LOG_STDERR ? 
	 LogMsg::TYPE_ERR : LogMsg::TYPE_MESS); 
}

void LogMsgHandler::appendWithSpace(unsigned seq, std::string text, LogMsg::Type type)
{
  std::map<unsigned, LogMsg*>::iterator 
    mess = messages_.find(seq);

  LogMsg* msg=0;

  if(mess != messages_.end()) {
    msg = messages_[seq];
  } else {
    msg = new LogMsg(seq);
    messages_[seq] = msg;
  }

  // And add the text

  for(unsigned iChar=0; iChar < text.size(); iChar++) {
    msg->os_ << text[iChar];
    
    if(text[iChar] == '\n') {
      msg->os_ << "                ";
    }
  }

  if(type != LogMsg::TYPE_UNSPEC)
    msg->type_ = type;
}

void LogMsgHandler::appendWithSpace(unsigned seq, std::string text, 
				    sza::array::LogStream nature)
{
  appendWithSpace(seq, text, nature==sza::array::LOG_STDERR ? 
		  LogMsg::TYPE_ERR : LogMsg::TYPE_MESS); 
}

/**.......................................................................
 * Return the next message
 */
std::string LogMsgHandler::getMessage(unsigned seq)
{
  std::map<unsigned, LogMsg*>::iterator 
    mess = messages_.find(seq);

  if(mess == messages_.end()) {
    ThrowError("No such message: " << seq);
  }

  // Copy the string

  std::string message = mess->second->os_.str();

  eraseMessage(seq);

  return message;
}

/**.......................................................................
 * Return the next message
 */
LogMsgHandler::LogMsg* LogMsgHandler::findMessage(unsigned seq)
{
  std::map<unsigned, LogMsg*>::iterator 
    mess = messages_.find(seq);

  if(mess == messages_.end()) {
    ThrowError("No such message: " << seq);
  }

  // return the message:
  
  return mess->second;
}

/**.......................................................................
 * Return the next message
 */
void LogMsgHandler::eraseMessage(unsigned seq)
{
  std::map<unsigned, LogMsg*>::iterator 
    mess = messages_.find(seq);

  if(mess != messages_.end()) {
    delete mess->second;
    messages_.erase(mess);
  }
}

/**.......................................................................
 * Get the next substring of a message
 */
std::string LogMsgHandler::readMessage(unsigned seq)
{
  LogMsg* msg = findMessage(seq);
  return msg->os_.str();
} 

/**.......................................................................
 * Get the next substring of a message
 */
std::string LogMsgHandler::getNextMessageSubstr(unsigned seq, unsigned maxChars,
						bool& isLast)
{
  LogMsg* msg = findMessage(seq);

  // Check the remaining portion of the string.  If it is greater than
  // maxChars, just return maxChars of it.  If it is < maxChars,
  // return the whole thing, and set isLast to true:

  std::string retval;
  unsigned size = msg->os_.str().size();
  unsigned& last = msg->lastReadIndex_;

  if(size - last <= maxChars) {
    retval = msg->os_.str().substr(last);
    isLast = true;

    eraseMessage(seq);

    // Else just return the next portion of it

  } else {
    retval = msg->os_.str().substr(last, maxChars);
    last += maxChars;
    isLast = false;
  }

  return retval;
}
