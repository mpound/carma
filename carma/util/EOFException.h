#ifndef CARMA_UTIL_EOFEXCEPTION_H
#define CARMA_UTIL_EOFEXCEPTION_H

/**
 * @file
 *
 * EOFException class.
 *
 * @author: Marc Pound
 *
 * $CarmaCopyright$
 *
 */
#include "carma/util/ErrorException.h"

namespace carma {
namespace util {

/**
 * An exception indicating that the end of file marker has been reached.
 */
class EOFException : public ErrorException {
public:

    /**
     * create a EOFException 
     * Suggested usage CARMA_EXCEPTION(EOFException, string msg);
     * @param msgstr   an output string containing the message.
     * @param filename The source file containing the code throwing the
     *                 exception. Can be set using the
     *                 cpp macro '__FILE__'.
     * @param lineNo   The line number in the source file where the
     *                 exception is created. Can be set using the
     *                 cpp macro '__LINE__'.
     */
    EOFException(const std::string & msgstr, 
                 const char *        filename,
                 int                 lineNo);

    /**
     * create a EOFException 
     * Suggested usage CARMA_EXCEPTION(EOFException, ostringstream msg);
     * @param msgstr   an output string stream contining the message
     * @param filename The source file containing the code throwing the
     *                 exception. Can be set using the
     *                 cpp macro '__FILE__'.
     * @param lineNo   The line number in the source file where the
     *                 exception is created. Can be set using the
     *                 cpp macro '__LINE__'.
     */
    EOFException(const std::ostringstream & msgstr, 
                 const char *               filename,
                 int                        lineNo);

    /**
     * copy a EOFException
     * @param ex   the exception to copy
     */
    EOFException(const EOFException & ex);

};

}}

#endif // CARMA_UTIL_EOFEXCEPTION_H
