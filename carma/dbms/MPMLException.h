#ifndef CARMA_DBMS_MPMLEXCEPTION_H
#define CARMA_DBMS_MPMLEXCEPTION_H

/**
 * @file
 *
 * MPML Exception class.
 *
 * @author Original: Dave Mehringer
 *
 * $CarmaCopyright$
 *
 */
#include "carma/util/ErrorException.h"

namespace carma {
namespace dbms {

/**
 * an exception indicating that there is a problem with the structure of an
 * mpml document
 */
class MPMLException : public carma::util::ErrorException {
public:

    /**
     * create an MPMLException
     * Suggested usage CARMA_EXCEPTION(MPMLException, string msg);
     * @param msgstr   an output string containing the message.
     * @param filename The source file containing the code throwing the
     *                 exception. Can be set using the
     *                 cpp macro '__FILE__'.
     * @param lineNo   The line number in the source file where the
     *                 exception is created. Can be set using the
     *                 cpp macro '__LINE__'.
     */
    MPMLException(const std::string & msgstr,
                  const char *        filename,
                  int                 lineNo);

    /**
     * create an MPMLException 
     * Suggested usage CARMA_EXCEPTION(MPMLException, ostringstream msg);
     * @param msgstr   an output string stream contining the message
     * @param filename The source file containing the code throwing the
     *                 exception. Can be set using the
     *                 cpp macro '__FILE__'.
     * @param lineNo   The line number in the source file where the
     *                 exception is created. Can be set using the
     *                 cpp macro '__LINE__'.
     */
    MPMLException(const std::ostringstream & msgstr, 
                  const char *               filename,
                  int                        lineNo);

    /**
     * create a copy of an MPMLException
     * @param ex   the exception to copy
     */
    MPMLException(const MPMLException & ex);

};

}}

#endif // CARMA_DBMS_MPMLEXCEPTION_H
