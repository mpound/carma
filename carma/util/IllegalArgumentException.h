#ifndef CARMA_UTIL_ILLEGALARGUMENTEXCEPTION_H
#define CARMA_UTIL_ILLEGALARGUMENTEXCEPTION_H

/**
 * @file
 *
 * IllegalArgumentException class.
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
class IllegalArgumentException : public ErrorException {
public:

    /**
     * create an IllegalArgumentException
     * Recommended usage CARMA_EXCEPTION(IllegalArgumentException, string msg);
     * @param msgstr   an output string containing the message.
     * @param filename The source file containing the code throwing the
     *                 exception. Can be set using the
     *                 cpp macro '__FILE__'.
     * @param lineNo   The line number in the source file where the
     *                 exception is created. Can be set using the
     *                 cpp macro '__LINE__'.
     */
    IllegalArgumentException(const std::string & msgstr, 
                             const char *        filename,
                             int                 lineNo);

    /**
     * create an IllegalArgumentException 
     * Recommended usage: 
     * CARMA_EXCEPTION(IllegalArgumentException, ostringstream msg);
     * @param msgstr   an output string stream contining the message
     * @param filename The source file containing the code throwing the
     *                 exception. Can be set using the
     *                 cpp macro '__FILE__'.
     * @param lineNo   The line number in the source file where the
     *                 exception is created. Can be set using the
     *                 cpp macro '__LINE__'.
     */
    IllegalArgumentException(const std::ostringstream & msgstr, 
                             const char *               filename,
                             int                        lineNo);

    /**
     * copy an IllegalArgumentException
     */
    IllegalArgumentException(const IllegalArgumentException & ex);

};

}}

#endif // CARMA_UTIL_ILLEGALARGUMENTEXCEPTION_H
