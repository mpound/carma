
#ifndef CARMA_SERVICES_UNSUPPORTEDCOORDSYSEXCEPTION_H
#define CARMA_SERVICES_UNSUPPORTEDCOORDSYSEXCEPTION_H

/**
 * @file
 *
 * UnsupportedCoordSysException class.  This exception should be thrown when
 * trying to use an coordinate system enum value that is not supported
 * in the code in question.
 * @see carma::services::coordSysType
 *
 * @author: Marc Pound
 *
 * $Id: UnsupportedCoordSysException.h,v 1.1 2005/07/22 05:17:29 mpound Exp $
 * $CarmaCopyright$
 *
 */
#include "carma/util/ErrorException.h"

namespace carma {
namespace services {

/**
 * An exception indicating that a unit conversion failed because
 * the units were not conformable.
 */
class UnsupportedCoordSysException : public carma::util::ErrorException {
public:

    /**
     * create an UnsupportedCoordSysException
     * Recommended usage 
     * CARMA_EXCEPTION(UnsupportedCoordSysException, string msg);
     * @param msgstr   an output string containing the message.
     * @param filename The source file containing the code throwing the
     *                 exception. Can be set using the
     *                 cpp macro '__FILE__'.
     * @param lineNo   The line number in the source file where the
     *                 exception is created. Can be set using the
     *                 cpp macro '__LINE__'.
     */
    UnsupportedCoordSysException(const std::string & msgstr, 
                       const char *        filename,
                       int                 lineNo);

    /**
     * create an UnsupportedCoordSysException 
     * Recommended usage: 
     * CARMA_EXCEPTION(UnsupportedCoordSysException, ostringstream msg);
     * @param msgstr   an output string stream contining the message
     * @param filename The source file containing the code throwing the
     *                 exception. Can be set using the
     *                 cpp macro '__FILE__'.
     * @param lineNo   The line number in the source file where the
     *                 exception is created. Can be set using the
     *                 cpp macro '__LINE__'.
     */
    UnsupportedCoordSysException(const std::ostringstream & msgstr, 
                       const char *               filename,
                       int                        lineNo);

    /**
     * copy an UnsupportedCoordSysException
     */
    UnsupportedCoordSysException(const UnsupportedCoordSysException & ex);

};

}}

#endif // CARMA_SERVICES_UNSUPPORTEDCOORDSYSEXCEPTION_H
