#include <errno.h>

#include "carma/szautil/IoLock.h"
#include "carma/szautil/Logger.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/TimeVal.h"
#include <cstring>

using namespace sza::util;
using namespace std;

#define ADD_PREFIX() \
{\
  if(usePrefix)\
    addPrefix(lineNo, functionName);\
}

/**.......................................................................
 * Constructor.
 */
LogStream::LogStream() 
{
  isError_ = false;
};

/**.......................................................................
 * Constructor.
 */
LogStream::LogStream(string message, string fileName, 
		     int lineNo, string functionName,
		     bool isError, bool usePrefix)
{
  setLogStreamMessage(message, fileName, lineNo, functionName, isError, 
		      usePrefix);
};

/**.......................................................................
 * Constructor.
 */
LogStream::LogStream(ostringstream os, string fileName, 
		     int lineNo, string functionName,
		     bool isError, bool usePrefix)
{
  setLogStreamMessage(os, fileName, lineNo, functionName, isError, usePrefix);
};

/**.......................................................................
 * Destructor.
 */
LogStream::~LogStream() {};

/**.......................................................................
 * Report if an error has been pushed onto the stream
 */
bool LogStream::isError()
{
  return isError_;
}

/**.......................................................................
 * Cap off an accumulating error message.
 */
void LogStream::finish()
{
  *this << ends;
}

/**.......................................................................
 * Set the message.
 */
void LogStream::setLogStreamMessage(string message, string fileName, 
				    int lineNo, string functionName, 
				    bool isError, bool usePrefix)
{
  clear();
  
  ADD_PREFIX();

  *this << message << ".";
  
  isError_ = isError;
}

/**.......................................................................
 * Append a message to the stream.
 */
void LogStream::appendLogStreamMessage(string message, string fileName, 
				       int lineNo, string functionName,
				       bool isError, bool usePrefix)
{
  ADD_PREFIX();

  *this << message << ".";
  
  isError_ = isError;
}

/**.......................................................................
 * Initializea message to the stream.
 */
void LogStream::initLogStreamMessage(string fileName, int lineNo,
				     string functionName,
				     bool isError, bool usePrefix)
{
  ADD_PREFIX();

  isError_ = isError;
}

/**.......................................................................
 * Set the message.
 */
void LogStream::setLogStreamMessage(ostringstream& os, string fileName, 
				    int lineNo, string functionName,
				    bool isError, bool usePrefix)
{
  clear();
  
  ADD_PREFIX();

  *this << os.str() << ".";
  
  isError_ = isError;
}

/**.......................................................................
 * Append a message to the stream.
 */
void LogStream::appendLogStreamMessage(ostringstream& os, string fileName, 
				       int lineNo, string functionName,
				       bool isError, bool usePrefix)
{
  ADD_PREFIX();

  *this << os.str() << ".";
  
  isError_ = isError;
}

/**.......................................................................
 * Set the message to the last system error message.
 */
void LogStream::setSysLogStreamError(string sysFunction, string fileName, 
				     int lineNo, string functionName,
				     bool isError, bool usePrefix)
{
  clear();
  
  ADD_PREFIX();

  *this << "In " << sysFunction << ": "
	<< strerror(errno) << ".";
  
  isError_ = isError;
}

/**.......................................................................
 * Append a system error message to the stream.
 */
void LogStream::appendSysLogStreamError(string sysFunction, string fileName,
					int lineNo, string functionName,
					bool isError, bool usePrefix)
{
  ADD_PREFIX();

  *this << "In " << sysFunction << ": "
	<< strerror(errno) << ".";
  
  isError_ = isError;
}

/**.......................................................................
 * Cap off any message which has been accumulating in our stream
 * buffer & return it as a string.
 */
string LogStream::getMessage()
{
  finish();
  return str();
}

/**.......................................................................
 * Clear the error status and any messages in our stream buffer.
 */
void LogStream::clear()
{
  isError_ = false;
  str("");
}

/**.......................................................................
 * Log a message
 */
void LogStream::report()
{
  TimeVal timeVal;
  timeVal.setToCurrentTime();

  // Push this message onto the appropriate stream.

  if(!isError_) {
    sza::util::IoLock::lockCout();
    cout << timeVal << ": " << this->str() << endl;
    sza::util::IoLock::unlockCout();
  } else {
    sza::util::IoLock::lockCerr();
    cout << timeVal << ": " << this->str() << endl;
    sza::util::IoLock::unlockCerr();
  }

  Logger::report(this);
}

/**.......................................................................
 * Log a message
 */
void LogStream::log()
{
  // And report it to the global Logger

  Logger::log(this);
}


/**.......................................................................
 * Add a prefix to the log stream
 */
void LogStream::addPrefix(int lineNo, string functionName)
{
    *this << "In "
	  << functionName << ": ";
}
