#ifndef CARMA_UTIL_MALFORMEDSYSLOGLINEEXCEPTION_H
#define CARMA_UTIL_MALFORMEDSYSLOGLINEEXCEPTION_H

/**
 * @file
 *
 * MalformedSyslogLineException class.
 *
 * @author: Colby Gutierrez-Kraybill
 *
 * $CarmaCopyright$
 *
 */
#include "carma/util/ErrorException.h"

namespace carma
{
  namespace dbms
  {

    /**
     * An exception indicating that a file you wish to create already exists.
     */
    class MalformedSyslogLineException : public carma::util::ErrorException {
      public:

	/**
	 * create a MalformedSyslogLineException 
	 * Suggested usage CARMA_EXCEPTION(MalformedSyslogLineException, string msg);
	 * @param msgstr   an output string containing the message.
	 * @param filename The source file containing the code throwing the
	 *                 exception. Can be set using the
	 *                 cpp macro '__FILE__'.
	 * @param lineNo   The line number in the source file where the
	 *                 exception is created. Can be set using the
	 *                 cpp macro '__LINE__'.
	 */
	MalformedSyslogLineException(const std::string & msgstr, 
	    const char *        filename,
	    int                 lineNo);

	/**
	 * create a MalformedSyslogLineException 
	 * Suggested usage CARMA_EXCEPTION(MalformedSyslogLineException, ostringstream msg);
	 * @param msgstr   an output string stream contining the message
	 * @param filename The source file containing the code throwing the
	 *                 exception. Can be set using the
	 *                 cpp macro '__FILE__'.
	 * @param lineNo   The line number in the source file where the
	 *                 exception is created. Can be set using the
	 *                 cpp macro '__LINE__'.
	 */
	MalformedSyslogLineException(const std::ostringstream & msgstr, 
	    const char *               filename,
	    int                        lineNo);

	/**
	 * copy a MalformedSyslogLineException
	 * @param ex   the exception to copy
	 */
	MalformedSyslogLineException(const MalformedSyslogLineException & ex);

    };

  }
}

#endif // CARMA_UTIL_MALFORMEDSYSLOGLINEEXCEPTION_H
