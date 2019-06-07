#ifndef CARMA_SERVICES_SPECTRALLINENOTFOUNDEXCEPTION_H
#define CARMA_SERVICES_SPECTRALLINENOTFOUNDEXCEPTION_H

/**
 * @file
 *
 * SpectralLineNotFoundException class.  This exception should be thrown when
 * when the SpectralLineCatalog class cannot locate the requested source.
 *
 * @author: Marc Pound
 *
 * $Id: SpectralLineNotFoundException.h,v 1.3 2005/03/02 17:19:00 mpound Exp $
 * $CarmaCopyright$
 *
 */
#include "carma/services/CatalogEntryNotFoundException.h"

namespace carma {
namespace services {

/**
 * An exception indicating that a spectral line could not be
 * found in the catalog.  This method should 
 * be thrown by SpectralLineCatalog.lookup().
 */
class SpectralLineNotFoundException : 
    public carma::services::CatalogEntryNotFoundException {
public:

    /**
     * create an SpectralLineNotFoundException
     * Recommended usage 
     * CARMA_EXCEPTION(SpectralLineNotFoundException, string msg);
     * @param msgstr   an output string containing the message.
     * @param filename The source file containing the code throwing the
     *                 exception. Can be set using the
     *                 cpp macro '__FILE__'.
     * @param lineNo   The line number in the source file where the
     *                 exception is created. Can be set using the
     *                 cpp macro '__LINE__'.
     */
    SpectralLineNotFoundException(const std::string & msgstr, 
                                  const char *        filename,
                                  int                 lineNo);

    /**
     * create an SpectralLineNotFoundException 
     * Recommended usage: 
     * CARMA_EXCEPTION(SpectralLineNotFoundException, ostringstream msg);
     * @param msgstr   an output string stream contining the message
     * @param filename The source file containing the code throwing the
     *                 exception. Can be set using the
     *                 cpp macro '__FILE__'.
     * @param lineNo   The line number in the source file where the
     *                 exception is created. Can be set using the
     *                 cpp macro '__LINE__'.
     */
    SpectralLineNotFoundException(const std::ostringstream & msgstr, 
                                  const char *               filename,
                                  int                        lineNo);

    /**
     * copy an SpectralLineNotFoundException
     */
    SpectralLineNotFoundException(const SpectralLineNotFoundException & ex);

};

}
}

#endif // CARMA_SERVICES_SPECTRALLINENOTFOUNDEXCEPTION_H
