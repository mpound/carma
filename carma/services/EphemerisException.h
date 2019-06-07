
#ifndef CARMA_SERVICES_EPHEMERISEXCEPTION_H
#define CARMA_SERVICES_EPHEMERISEXCEPTION_H

/**
 * @file
 *
 * EphemerisException class.  This exception should be thrown when
 * NOVAS returns a bad value.
 *
 * @author: Marc Pound
 *
 * $Id: EphemerisException.h,v 1.1 2005/07/17 02:10:10 mpound Exp $
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
class EphemerisException : public carma::util::ErrorException {
public:

    /**
     * create an EphemerisException
     * Recommended usage CARMA_EXCEPTION(EphemerisException, string msg);
     * @param msgstr   an output string containing the message.
     * @param filename The source file containing the code throwing the
     *                 exception. Can be set using the
     *                 cpp macro '__FILE__'.
     * @param lineNo   The line number in the source file where the
     *                 exception is created. Can be set using the
     *                 cpp macro '__LINE__'.
     */
    EphemerisException(const std::string & msgstr, 
                       const char *        filename,
                       int                 lineNo);

    /**
     * create an EphemerisException 
     * Recommended usage: 
     * CARMA_EXCEPTION(EphemerisException, ostringstream msg);
     * @param msgstr   an output string stream contining the message
     * @param filename The source file containing the code throwing the
     *                 exception. Can be set using the
     *                 cpp macro '__FILE__'.
     * @param lineNo   The line number in the source file where the
     *                 exception is created. Can be set using the
     *                 cpp macro '__LINE__'.
     */
    EphemerisException(const std::ostringstream & msgstr, 
                       const char *               filename,
                       int                        lineNo);

    /**
     * copy an EphemerisException
     */
    EphemerisException(const EphemerisException & ex);

};

}}

#endif // CARMA_SERVICES_EPHEMERISEXCEPTION_H
