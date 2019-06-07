#ifndef CARMA_SERVICES_CONFORMABILITYEXCEPTION_H
#define CARMA_SERVICES_CONFORMABILITYEXCEPTION_H

/**
 * @file
 *
 * ConformabilityException class.  This exception should be thrown when
 * a unit conversion fails because the units are not conformable.
 * E.g. if you tried to convert meters to joules.
 *
 * @author: Marc Pound
 *
 * $Id: ConformabilityException.h,v 1.3 2005/03/02 17:19:00 mpound Exp $
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
class ConformabilityException : public carma::util::ErrorException {
public:

    /**
     * create an ConformabilityException
     * Recommended usage CARMA_EXCEPTION(ConformabilityException, string msg);
     * @param msgstr   an output string containing the message.
     * @param filename The source file containing the code throwing the
     *                 exception. Can be set using the
     *                 cpp macro '__FILE__'.
     * @param lineNo   The line number in the source file where the
     *                 exception is created. Can be set using the
     *                 cpp macro '__LINE__'.
     */
    ConformabilityException(const std::string & msgstr, 
                            const char *        filename,
                            int                 lineNo);

    /**
     * create an ConformabilityException 
     * Recommended usage: 
     * CARMA_EXCEPTION(ConformabilityException, ostringstream msg);
     * @param msgstr   an output string stream contining the message
     * @param filename The source file containing the code throwing the
     *                 exception. Can be set using the
     *                 cpp macro '__FILE__'.
     * @param lineNo   The line number in the source file where the
     *                 exception is created. Can be set using the
     *                 cpp macro '__LINE__'.
     */
    ConformabilityException(const std::ostringstream & msgstr, 
                            const char *               filename,
                            int                        lineNo);

    /**
     * copy an ConformabilityException
     */
    ConformabilityException(const ConformabilityException & ex);

};

}}

#endif // CARMA_SERVICES_CONFORMABILITYEXCEPTION_H
