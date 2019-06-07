#ifndef CARMA_UTIL_FILEEXISTSEXCEPTION_H
#define CARMA_UTIL_FILEEXISTSEXCEPTION_H

/**
 * @file
 *
 * FileExistsException class.
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
 * An exception indicating that a file you wish to create already exists.
 */
class FileExistsException : public ErrorException {
public:

    /**
     * create a FileExistsException 
     * Suggested usage CARMA_EXCEPTION(FileExistsException, string msg);
     * @param msgstr   an output string containing the message.
     * @param filename The source file containing the code throwing the
     *                 exception. Can be set using the
     *                 cpp macro '__FILE__'.
     * @param lineNo   The line number in the source file where the
     *                 exception is created. Can be set using the
     *                 cpp macro '__LINE__'.
     */
    FileExistsException(const std::string & msgstr, 
                        const char *        filename,
                        int                 lineNo);

    /**
     * create a FileExistsException 
     * Suggested usage CARMA_EXCEPTION(FileExistsException, ostringstream msg);
     * @param msgstr   an output string stream contining the message
     * @param filename The source file containing the code throwing the
     *                 exception. Can be set using the
     *                 cpp macro '__FILE__'.
     * @param lineNo   The line number in the source file where the
     *                 exception is created. Can be set using the
     *                 cpp macro '__LINE__'.
     */
    FileExistsException(const std::ostringstream & msgstr, 
                        const char *               filename,
                        int                        lineNo);

    /**
     * copy a FileExistsException
     * @param ex   the exception to copy
     */
    FileExistsException(const FileExistsException & ex);

};

}}

#endif // CARMA_UTIL_FILEEXISTSEXCEPTION_H
