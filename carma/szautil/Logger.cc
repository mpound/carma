#include <sstream>

#include "carma/szaarrayutils/rtcnetcoms.h"

#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/Logger.h"
#include "carma/szautil/LogFile.h"
#include "carma/szautil/LogStream.h"

using namespace std;
using namespace sza::util;

LogFile Logger::logFile_;
bool Logger::haveLogFile_;

/**.......................................................................
 * Constructor.
 */
Logger::Logger() 
{
  // Initialize the log and err handlers to NULL

  logHandler_ = 0;
  errHandler_ = 0;

  // Initialize the print functions to default values.  These can be
  // changed globally by a call to Logger::installStdoutPrintFn() and
  // Logger::installStderrPrintFn()

  stdoutPrintFn_ = 0;
  stderrPrintFn_ = 0;

  haveLogFile_ = false;
}

// Initialize static variables

LOG_HANDLER_FN(*Logger::logHandler_);
LOG_HANDLER_FN(*Logger::errHandler_);
LOG_HANDLER_FN(*Logger::stdoutPrintFn_);
LOG_HANDLER_FN(*Logger::stderrPrintFn_);
std::string Logger::prefix_("");
Mutex Logger::guard_;

/**.......................................................................
 * Destructor.
 */
Logger::~Logger() {}

/**.......................................................................
 * Log a message (write to a logfile, if any)
 */
void Logger::log(string message, bool isError)
{
  write(message, isError, false, true);
}

/**.......................................................................
 * Log a message (write to a logfile, if any)
 */
void Logger::log(LogStream* ls)
{
  log(ls->str(), ls->isError());
}

/**.......................................................................
 * Dispatch a message (call any handlers set to handle this message type)
 */
void Logger::dispatch(string message, bool isError)
{
  write(message, isError, true, false);
}

/**.......................................................................
* Dispatch a message (call any handlers set to handle this message type)
 */
void Logger::dispatch(LogStream* ls)
{
  dispatch(ls->str(), ls->isError());
}

/**.......................................................................
 * Report a message (equivalent to log + dispatch)
 */
void Logger::report(string message, bool isError)
{
  write(message, isError, true, true);
}

/**.......................................................................
 * Report a message (equivalent to log + dispatch)
 */
void Logger::report(LogStream* ls)
{
  report(ls->str(), ls->isError());
}

/**.......................................................................
 * Write a message to any destination
 */
void Logger::write(const std::string& message, bool isError, bool dispatch, bool log)
{
  // If the string is longer than can be atomically sent, break it up
  // into substrings of length NET_LOG_MAX - prefix_.length()

  ostringstream os;
  int istart, istop, len;
  int length = message.length();
  int incr   = sza::array::NET_LOG_MAX - (prefix_.length()+1);

  // Lock the mutex that will ensure that interleaved calls to this
  // method by different threads cannot create garbled log messages
  // for long messages that require multiple handler calls, below.

  guard_.lock();

  try {

    // Break the message into network-sendable chunks, if necessary,
    // invoking installed handlers on each chunk.
    
    istart = 0;
    do {
      
      istop = (istart + incr) > length ? length : (istart + incr);
      len = (istop - istart);
      
      // Zero the stream and insert the prefix before each line
      
      os.str("");
      
      os << prefix_ << message.substr(istart, len);

      if(dispatch) {
	if(!isError && logHandler_ != 0)
	  logHandler_(os.str());
	else if(isError && errHandler_ != 0)
	  errHandler_(os.str());
      }
      
      // If we are using a logfile, write it to the logfile too
      
      if(log && haveLogFile_) {
	os << endl;

	logFile_.append(os.str());
      }
      
      istart += len;
    } while(istop < length);

    logFile_.flush();

  } catch(...) {
    guard_.unlock();
  }

  guard_.unlock();
}

/**.......................................................................
 * Install a log-message handler.
 */
void Logger::installLogHandler(LOG_HANDLER_FN(*logHandler))
{
  logHandler_ = logHandler;
}

/**.......................................................................
 * Install an error-message handler.
 */
void Logger::installErrHandler(LOG_HANDLER_FN(*errHandler))
{
  errHandler_ = errHandler;
}

/**.......................................................................
 * Install a default stdout print fn
 */
void Logger::installStdoutPrintFn(LOG_HANDLER_FN(*stdoutPrintFn))
{
  stdoutPrintFn_ = stdoutPrintFn;
}

/**.......................................................................
 * Install a default stderr print fn
 */
void Logger::installStderrPrintFn(LOG_HANDLER_FN(*stderrPrintFn))
{
  stderrPrintFn_ = stderrPrintFn;
}

/**.......................................................................
 * Set a string to be prepended to every message we log
 */
void Logger::setPrefix(std::string prefix)
{
  if((prefix.length()) >= sza::array::NET_LOG_MAX)
    throw Error("Logger::setPrefix(): Prefix is too long");

  prefix_ = prefix;
}

/**.......................................................................
 * Set the prefix for a logfile
 */
void Logger::setLogFilePrefix(const std::string& prefix)
{
  logFile_.setPrefix(prefix);
}

/**.......................................................................
 * Set the directory for a logfile
 */
void Logger::setLogFileDirectory(const std::string& dir)
{
  logFile_.setDirectory(dir);
}

/**.......................................................................
 * Open the logfile
 */
void Logger::openLogFile()
{
    logFile_.open();
    haveLogFile_ = true;
}

/**.......................................................................
 * Open the logfile
 */
void Logger::closeLogFile()
{
  if(haveLogFile_) {
    logFile_.close();
    haveLogFile_ = false;
  }
}

/**.......................................................................
 * Print a message to stdout.  If a user-defined print function has
 * been installed, use that, else use the default print method
 */
void Logger::printToStdout(std::string message)
{
  if(stdoutPrintFn_ == 0)
    defaultStdoutPrintFn(message);
  else
    stdoutPrintFn_(message);
}

/**.......................................................................
 * Print a message to stderr.  If a user-defined print function has
 * been installed, use that, else use the default print method
 */
void Logger::printToStderr(std::string message)
{
  if(stderrPrintFn_ == 0)
    defaultStderrPrintFn(message);
  else
    stderrPrintFn_(message);
}

LOG_HANDLER_FN(Logger::defaultStdoutPrintFn) 
{
  sza::util::IoLock::lockCout(); 
  std::cout << logStr;
  fflush(stdout);
  sza::util::IoLock::unlockCout(); 
  LogMessage(false, logStr);
}

LOG_HANDLER_FN(Logger::defaultStderrPrintFn) 
{
  sza::util::IoLock::lockCerr(); 
  std::cerr << logStr;
  fflush(stderr);
  sza::util::IoLock::unlockCerr(); 
  LogMessage(true, logStr);
}
