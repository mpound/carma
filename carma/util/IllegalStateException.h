#ifndef CARMA_UTIL_ILLEGALSTATEEXCEPTION_H
#define CARMA_UTIL_ILLEGALSTATEEXCEPTION_H

/**
 * @file
 *
 * IllegalStateException class.
 *
 * @author: Ray Plante
 *
 * $CarmaCopyright$
 *
 */
#include "carma/util/ErrorException.h"

namespace carma {
namespace util {

/**
 * an exception indicating that an object is in an illegal state for the
 * operation that was called on or with it
 */
class IllegalStateException : public ErrorException {
public:

    /**
     * create an IllegalStateException
     * Recommended usage CARMA_EXCEPTION(IllegalStateException, string msg);
     * @param msgstr   an output string containing the message.
     * @param filename The source file containing the code throwing the
     *                 exception. Can be set using the
     *                 cpp macro '__FILE__'.
     * @param lineNo   The line number in the source file where the
     *                 exception is created. Can be set using the
     *                 cpp macro '__LINE__'.
     */
    IllegalStateException(const std::string & msgstr, 
                          const char *        filename,
                          int                 lineNo);

    /**
     * create an IllegalStateException 
     * Recommended usage: 
     * CARMA_EXCEPTION(IllegalStateException, ostringstream msg);
     * @param msgstr   an output string stream contining the message
     * @param filename The source file containing the code throwing the
     *                 exception. Can be set using the
     *                 cpp macro '__FILE__'.
     * @param lineNo   The line number in the source file where the
     *                 exception is created. Can be set using the
     *                 cpp macro '__LINE__'.
     */
    IllegalStateException(const std::ostringstream & msgstr, 
                          const char *               filename,
                          int                        lineNo);

    /**
     * copy an IllegalStateException
     */
    IllegalStateException(const IllegalStateException & ex);

};

}}

#endif // CARMA_UTIL_ILLEGALSTATEEXCEPTION_H
