#ifndef CARMA_UTIL_NUMBER_FORMAT_EXCEPTION_H
#define CARMA_UTIL_NUMBER_FORMAT_EXCEPTION_H

/**
 * @file
 *
 * NumberFormatException class.
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
 * An exception that a numeric conversion from character or string to 
 * number failed.
 */
class NumberFormatException : public ErrorException {
public:

    /**
     * create a NumberFormatException 
     * Suggested usage CARMA_EXCEPTION(NumberFormatException, string msg);
     * @param msgstr   an output string containing the message.
     * @param filename The source file containing the code throwing the
     *                 exception. Can be set using the
     *                 cpp macro '__FILE__'.
     * @param lineNo   The line number in the source file where the
     *                 exception is created. Can be set using the
     *                 cpp macro '__LINE__'.
     */
    NumberFormatException(const std::string & msgstr, 
                        const char *        filename,
                        int                 lineNo);

    /**
     * create a NumberFormatException 
     * Suggested usage CARMA_EXCEPTION(NumberFormatException, ostringstream msg);
     * @param msgstr   an output string stream contining the message
     * @param filename The source file containing the code throwing the
     *                 exception. Can be set using the
     *                 cpp macro '__FILE__'.
     * @param lineNo   The line number in the source file where the
     *                 exception is created. Can be set using the
     *                 cpp macro '__LINE__'.
     */
    NumberFormatException(const std::ostringstream & msgstr, 
                        const char *               filename,
                        int                        lineNo);

    /**
     * copy a NumberFormatException
     * @param ex   the exception to copy
     */
    NumberFormatException(const NumberFormatException & ex);

};

}}

#endif // CARMA_UTIL_NUMBER_FORMAT_EXCEPTION_H
