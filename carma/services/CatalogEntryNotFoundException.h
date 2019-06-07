#ifndef CARMA_SERVICES_CATALOGENTRYNOTFOUNDEXCEPTION_H
#define CARMA_SERVICES_CATALOGENTRYNOTFOUNDEXCEPTION_H

/**
 * @file
 *
 * CatalogEntryNotFoundException class.  This exception should be thrown when
 * when a class derived from Catalog cannot locate the requested entry.
 *
 * @author: Marc Pound
 *
 * $Id: CatalogEntryNotFoundException.h,v 1.4 2005/03/02 17:19:00 mpound Exp $
 * $CarmaCopyright$
 *
 */
#include "carma/util/NotFoundException.h"

namespace carma {
namespace services {

/**
 * An exception indicating that an entry could not be found
 * in the given catalog. Should be thrown by the lookup(string name)
 * method of Catalog classes.
 */
class CatalogEntryNotFoundException : public carma::util::NotFoundException {
public:

    /**
     * create an CatalogEntryNotFoundException
     * Recommended usage 
     * CARMA_EXCEPTION(CatalogEntryNotFoundException, string msg);
     * @param msgstr   an output string containing the message.
     * @param filename The source file containing the code throwing the
     *                 exception. Can be set using the
     *                 cpp macro '__FILE__'.
     * @param lineNo   The line number in the source file where the
     *                 exception is created. Can be set using the
     *                 cpp macro '__LINE__'.
     */
    CatalogEntryNotFoundException(const std::string & msgstr, 
                                  const char *        filename,
                                  int                 lineNo);

    /**
     * create an CatalogEntryNotFoundException 
     * Recommended usage: 
     * CARMA_EXCEPTION(CatalogEntryNotFoundException, ostringstream msg);
     * @param msgstr   an output string stream contining the message
     * @param filename The source file containing the code throwing the
     *                 exception. Can be set using the
     *                 cpp macro '__FILE__'.
     * @param lineNo   The line number in the source file where the
     *                 exception is created. Can be set using the
     *                 cpp macro '__LINE__'.
     */
    CatalogEntryNotFoundException(const std::ostringstream & msgstr, 
                                  const char *               filename,
                                  int                        lineNo);

    /**
     * copy an CatalogEntryNotFoundException
     */
    CatalogEntryNotFoundException(const CatalogEntryNotFoundException & ex);

};

}
}

#endif // CARMA_SERVICES_CATALOGENTRYNOTFOUNDEXCEPTION_H
