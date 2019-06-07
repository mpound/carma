#ifndef CARMA_UTIL_SEMAPHORETIMEDOUTEXCEPTION_H
#define CARMA_UTIL_SEMAPHORETIMEDOUTEXCEPTION_H

/**
 * @file
 *
 * SemaphoreTimedOutException class.
 *
 * @author: Colby Gutierrez-Kraybill
 *
 * $CarmaCopyright$
 *
 */
#include "carma/util/ErrorException.h"

namespace carma
{
  namespace util
  {

    /**
     * An exception indicating that a file you wish to create already exists.
     */
    class SemaphoreTimedOutException : public carma::util::ErrorException {
      public:

	/**
	 * create a SemaphoreTimedOutException 
	 * Suggested usage CARMA_EXCEPTION(SemaphoreTimedOutException, string msg);
	 * @param msgstr   an output string containing the message.
	 * @param filename The source file containing the code throwing the
	 *                 exception. Can be set using the
	 *                 cpp macro '__FILE__'.
	 * @param lineNo   The line number in the source file where the
	 *                 exception is created. Can be set using the
	 *                 cpp macro '__LINE__'.
	 */
	SemaphoreTimedOutException(const std::string & msgstr, 
	    const char *        filename,
	    int                 lineNo);

	/**
	 * create a SemaphoreTimedOutException 
	 * Suggested usage CARMA_EXCEPTION(SemaphoreTimedOutException, ostringstream msg);
	 * @param msgstr   an output string stream contining the message
	 * @param filename The source file containing the code throwing the
	 *                 exception. Can be set using the
	 *                 cpp macro '__FILE__'.
	 * @param lineNo   The line number in the source file where the
	 *                 exception is created. Can be set using the
	 *                 cpp macro '__LINE__'.
	 */
	SemaphoreTimedOutException(const std::ostringstream & msgstr, 
	    const char *               filename,
	    int                        lineNo);

	/**
	 * copy a SemaphoreTimedOutException
	 * @param ex   the exception to copy
	 */
	SemaphoreTimedOutException(const SemaphoreTimedOutException & ex);

    };

  }
}

#endif // CARMA_UTIL_SEMAPHORETIMEDOUTEXCEPTION_H
