#ifndef CARMA_UTIL_NOTFOUNDEXCEPTION_H
#define CARMA_UTIL_NOTFOUNDEXCEPTION_H

/**
 * @file
 *
 * NotFoundException class.
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
 * an exception indicating that a look-up operation failed find a match
 * to the inputs.
 */
class NotFoundException : public ErrorException {
public:

    /**
     * create a NotFoundException 
     * Suggested usage CARMA_EXCEPTION(NotFoundException, string msg);
     * @param msgstr   an output string containing the message.
     * @param filename The source file containing the code throwing the
     *                 exception. Can be set using the
     *                 cpp macro '__FILE__'.
     * @param lineNo   The line number in the source file where the
     *                 exception is created. Can be set using the
     *                 cpp macro '__LINE__'.
     */
    NotFoundException(const std::string & msgstr, 
                      const char *        filename,
                      int                 lineNo);

    /**
     * create a NotFoundException 
     * Suggested usage CARMA_EXCEPTION(NotFoundException, ostringstream msg);
     * @param msgstr   an output string stream contining the message
     * @param filename The source file containing the code throwing the
     *                 exception. Can be set using the
     *                 cpp macro '__FILE__'.
     * @param lineNo   The line number in the source file where the
     *                 exception is created. Can be set using the
     *                 cpp macro '__LINE__'.
     */
    NotFoundException(const std::ostringstream & msgstr, 
                      const char *               filename,
                      int                        lineNo);

    /**
     * create a copy of a NotFoundException
     * @param ex   the exception to copy
     */
    NotFoundException(const NotFoundException & ex);

};

}}

#endif // CARMA_UTIL_NOTFOUNDEXCEPTION_H
