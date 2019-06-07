#ifndef CARMA_UTIL_SEMAPHOREEXCEPTION_H
#define CARMA_UTIL_SEMAPHOREEXCEPTION_H

/**
 * @file
 *
 * SemaphoreException class.
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
    class SemaphoreException : public carma::util::ErrorException {
      public:

	/**
	 * create a SemaphoreException 
	 * Suggested usage CARMA_EXCEPTION(SemaphoreException, string msg);
	 * @param msgstr   an output string containing the message.
	 * @param filename The source file containing the code throwing the
	 *                 exception. Can be set using the
	 *                 cpp macro '__FILE__'.
	 * @param lineNo   The line number in the source file where the
	 *                 exception is created. Can be set using the
	 *                 cpp macro '__LINE__'.
	 */
	SemaphoreException(const std::string & msgstr, 
	    const char *        filename,
	    int                 lineNo);

	/**
	 * create a SemaphoreException 
	 * Suggested usage CARMA_EXCEPTION(SemaphoreException, ostringstream msg);
	 * @param msgstr   an output string stream contining the message
	 * @param filename The source file containing the code throwing the
	 *                 exception. Can be set using the
	 *                 cpp macro '__FILE__'.
	 * @param lineNo   The line number in the source file where the
	 *                 exception is created. Can be set using the
	 *                 cpp macro '__LINE__'.
	 */
	SemaphoreException(const std::ostringstream & msgstr, 
	    const char *               filename,
	    int                        lineNo);

	/**
	 * copy a SemaphoreException
	 * @param ex   the exception to copy
	 */
	SemaphoreException(const SemaphoreException & ex);

    };

  }
}

#endif // CARMA_UTIL_SEMAPHOREEXCEPTION_H
