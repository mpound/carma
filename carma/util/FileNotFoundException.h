#ifndef CARMA_UTIL_FILENOTFOUNDEXCEPTION_H
#define CARMA_UTIL_FILENOTFOUNDEXCEPTION_H

/**
 * @file
 *
 * FileNotFoundException class.  This exception should be thrown when
 * when a request file does not exist.
 *
 * @author: Marc Pound
 *
 * $Id: FileNotFoundException.h,v 1.1 2005/03/02 17:19:00 mpound Exp $
 * $CarmaCopyright$
 *
 */
#include "carma/util/NotFoundException.h"

namespace carma {
namespace util {

/**
 * An exception indicating that a requested file does not exist.
 */
class FileNotFoundException : public carma::util::NotFoundException {
public:

    /**
     * create an FileNotFoundException
     * Recommended usage 
     * CARMA_EXCEPTION(FileNotFoundException, string msg);
     * @param msgstr   an output string containing the message.
     * @param filename The source file containing the code throwing the
     *                 exception. Can be set using the
     *                 cpp macro '__FILE__'.
     * @param lineNo   The line number in the source file where the
     *                 exception is created. Can be set using the
     *                 cpp macro '__LINE__'.
     */
    FileNotFoundException(const std::string & msgstr, 
                                  const char *        filename,
                                  int                 lineNo);

    /**
     * create an FileNotFoundException 
     * Recommended usage: 
     * CARMA_EXCEPTION(FileNotFoundException, ostringstream msg);
     * @param msgstr   an output string stream contining the message
     * @param filename The source file containing the code throwing the
     *                 exception. Can be set using the
     *                 cpp macro '__FILE__'.
     * @param lineNo   The line number in the source file where the
     *                 exception is created. Can be set using the
     *                 cpp macro '__LINE__'.
     */
    FileNotFoundException(const std::ostringstream & msgstr, 
                                  const char *               filename,
                                  int                        lineNo);

    /**
     * copy an FileNotFoundException
     */
    FileNotFoundException(const FileNotFoundException & ex);

};

}
}

#endif // CARMA_UTIL_FILENOTFOUNDEXCEPTION_H
