#ifndef CARMA_SERVICES_SOURCENOTFOUNDEXCEPTION_H
#define CARMA_SERVICES_SOURCENOTFOUNDEXCEPTION_H

/**
 * @file
 *
 * SourceNotFoundException class.  This exception should be thrown when
 * when the Ephemeris, SourceCatalog, or FluxCatalog class cannot 
 * locate the requested source.
 *
 * @author: Marc Pound
 *
 * $Id: SourceNotFoundException.h,v 1.4 2005/12/08 18:53:15 mpound Exp $
 * $CarmaCopyright$
 *
 */
#include "carma/services/CatalogEntryNotFoundException.h"

namespace carma {
namespace services {

/**
 * An exception indicating that a source could not be found in
 * the catalog nor as solar system body.  This method should 
 * be thrown by SourceCatalog.lookup().
 */
class SourceNotFoundException : 
    public carma::services::CatalogEntryNotFoundException {
public:

    /**
     * create an SourceNotFoundException
     * Recommended usage CARMA_EXCEPTION(SourceNotFoundException, string msg);
     * @param msgstr   an output string containing the message.
     * @param filename The source file containing the code throwing the
     *                 exception. Can be set using the
     *                 cpp macro '__FILE__'.
     * @param lineNo   The line number in the source file where the
     *                 exception is created. Can be set using the
     *                 cpp macro '__LINE__'.
     */
    SourceNotFoundException(const std::string & msgstr, 
                            const char *        filename,
                            int                 lineNo);

    /**
     * create an SourceNotFoundException 
     * Recommended usage: 
     * CARMA_EXCEPTION(SourceNotFoundException, ostringstream msg);
     * @param msgstr   an output string stream contining the message
     * @param filename The source file containing the code throwing the
     *                 exception. Can be set using the
     *                 cpp macro '__FILE__'.
     * @param lineNo   The line number in the source file where the
     *                 exception is created. Can be set using the
     *                 cpp macro '__LINE__'.
     */
    SourceNotFoundException(const std::ostringstream & msgstr, 
                            const char *               filename,
                            int                        lineNo);

    /**
     * copy an SourceNotFoundException
     */
    SourceNotFoundException(const SourceNotFoundException & ex);

};

}
}

#endif // CARMA_SERVICES_SOURCENOTFOUNDEXCEPTION_H
