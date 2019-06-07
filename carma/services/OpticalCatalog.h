/**
 * @file 
 * $Id: OpticalCatalog.h,v 1.1 2008/11/24 14:22:06 mpound Exp $
 *
 * @author Marc Pound
 *
 */

#ifndef CARMA_SERVICES_OPTICALCATALOG_H
#define CARMA_SERVICES_OPTICALCATALOG_H

#include "carma/services/Catalog.h"
#include "carma/services/Star.h"
#include "carma/util/Program.h"
#include <string>
#include <map>

namespace carma {
namespace services {

  class OpticalCatalog : public carma::services::Catalog {
  public:
    /** Constructor */
    OpticalCatalog();
    /** Destructor */
    virtual ~OpticalCatalog();

    /**
     * Open a file containing a stellar catalog (columns: Source, V)
     * @param fileName The file to open
     * @throw carma::util::NotFoundException if the file is not found
     */
    void open(const std::string & fileName);
    
    /**
     * Look up a stellar name magnitude
     * <b>This look-up is case insensitive.</b>
     *
     * @param sourceName The star name
     * @return A carma::services::Star representation of the named entry
     *
     * @throw carma::services::SourceNotFoundException if the source name cannot
     * be found in the opened file.
     */
    const Star & lookup(const std::string & sourceName);

    /**
     * @return the default CARMA optical catalog
     * NB: could move this to Catalog.h if we want all
     * Catalogs to provide this.
     */
    static const ::std::string defaultCatalog() {
        return carma::util::Program::getConfFile(DEFAULT_CATALOG);
    }

    /**
     * @return the underlying map of stars.  The map key is the
     * upper case source name.
     */
    ::std::map< ::std::string, carma::services::Star > getSourceMap() const 
    {
        return stars_;
    }

  private:

    static const ::std::string DEFAULT_CATALOG;

    // catalog entries.  key is star name
    ::std::map< ::std::string, services::Star > stars_;


  };
} // end namespace services
} // end namespace carma

#endif
