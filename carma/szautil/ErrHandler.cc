#include "carma/szautil/Exception.h"
#include "carma/szautil/ErrHandler.h"
#include "carma/szautil/LogStream.h"

using namespace std;
using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
ErrHandler::ErrHandler() 
{
  // Initialize the throw function to NULL

  throwFn_  = 0;
  reportFn_ = 0;
  logFn_    = 0;
}

// Initialize static variables

ERR_HANDLER_FN(*ErrHandler::throwFn_);
ERR_HANDLER_FN(*ErrHandler::reportFn_);
ERR_HANDLER_FN(*ErrHandler::logFn_);

/**.......................................................................
 * Destructor.
 */
ErrHandler::~ErrHandler() {}

/**.......................................................................
 * Install a user-defined throw function
 */
void ErrHandler::installThrowFn(ERR_HANDLER_FN(*throwFn))
{
  throwFn_ = throwFn;
}

/**.......................................................................
 * Install a user-defined report function
 */
void ErrHandler::installReportFn(ERR_HANDLER_FN(*reportFn))
{
  reportFn_ = reportFn;
}

/**.......................................................................
 * Install a user-defined log function
 */
void ErrHandler::installLogFn(ERR_HANDLER_FN(*logFn))
{
  logFn_ = logFn;
}

/**.......................................................................
 * Throw an error
 */
ERR_HANDLER_FN(ErrHandler::throwError)
{
  if(throwFn_ == 0)
    defaultThrowFn(os, fileName, lineNumber, functionName, isErr, isSimple, isSysErr);
  else
    throwFn_(os, fileName, lineNumber, functionName, isErr, isSimple, isSysErr);
}

/**.......................................................................
 * Report a message
 */
ERR_HANDLER_FN(ErrHandler::report)
{
  if(reportFn_ == 0)
    defaultReportFn(os, fileName, lineNumber, functionName, isErr, isSimple, isSysErr);
  else
    reportFn_(os, fileName, lineNumber, functionName, isErr, isSimple, isSysErr);
}

/**.......................................................................
 * Log a message
 */
ERR_HANDLER_FN(ErrHandler::log)
{
  if(logFn_ == 0)
    defaultLogFn(os, fileName, lineNumber, functionName, isErr, isSimple, isSysErr);
  else
    logFn_(os, fileName, lineNumber, functionName, isErr, isSimple, isSysErr);
}

/**.......................................................................
 * Default throw function
 */
ERR_HANDLER_FN(ErrHandler::defaultThrowFn)
{
  sza::util::LogStream errStr;

  // Init the message according to type

  errStr.initLogStreamMessage(fileName, lineNumber, functionName, true, !isSimple);

  // If this is a system error, append the system message to it

  if(isSysErr)
    errStr.appendSysLogStreamError(os.str(), fileName, lineNumber, functionName, true, true);
  else
    errStr << std::endl << os.str();

  // Log the error

  errStr.log();

  // Finally, throw it

  throw sza::util::Exception(errStr, fileName, lineNumber, true);
}

/**.......................................................................
 * Default report function
 */
ERR_HANDLER_FN(ErrHandler::defaultReportFn)
{
  sza::util::LogStream errStr;

  // Init the message according to type

  errStr.initLogStreamMessage(fileName, lineNumber, functionName, isErr, !isSimple);

  // If this is a system error, append the system message to it

  if(isSysErr)
    errStr.appendSysLogStreamError(os.str(), fileName, lineNumber, functionName, isErr, true);
  else
    errStr << os.str();

  // Report the error

  errStr.report();
}

/**.......................................................................
 * Default log function
 */
ERR_HANDLER_FN(ErrHandler::defaultLogFn)
{
  sza::util::LogStream errStr;

  // Init the message according to type

  errStr.initLogStreamMessage(fileName, lineNumber, functionName, isErr, !isSimple);

  // If this is a system error, append the system message to it

  if(isSysErr)
    errStr.appendSysLogStreamError(os.str(), fileName, lineNumber, functionName, isErr, true);
  else
    errStr << os.str();

  // Report the error

  errStr.log();
}
