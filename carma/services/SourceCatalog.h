#ifndef CARMA_SERVICES_SOURCECATALOG_H
#define CARMA_SERVICES_SOURCECATALOG_H

#include <map>

#include "carma/services/Catalog.h"
#include "carma/services/Source.h"
#include "carma/services/SourceNotFoundException.h"
#include "carma/services/Table.h"
#include "carma/services/Types.h"
#include "carma/util/Program.h"

/**
 * $Id: SourceCatalog.h,v 1.16 2008/11/24 14:22:06 mpound Exp $
 * 
 * @file SourceCatalog.h
 * @author Chul Gwon
 */

namespace carma {
  namespace services {
    
    /**
     * The SourceCatalog class is used to hold information 
     * regarding a particular
     * source catalog file as retrieved from a Catalog
     */
    class SourceCatalog : public carma::services::Catalog {
    public:

      SourceCatalog() {}
      virtual ~SourceCatalog() {}

      /**
       * Open a file containing a source catalog
       * @param fileName The file to open
       * @throw carma::util::FileNotFoundException if the file is not found
       */
      void open(const std::string& fileName);

      /**
       * Select a source from the file of input sources.
       * <b>This look-up is case insensitive.</b>
       *
       * @param sourceName The Source name
       * @return A carma::services::Source representation of the named entry
       *
       * @throw SourceNotFoundException if the source name cannot
       * be found in the opened file.
       */
      const carma::services::Source& lookup(const std::string& sourceName);

      /**
       * @return the default CARMA Source catalog
       * NB: could move this to Catalog.h if we want all
       * Catalogs to provide this.
       */
      static const std::string defaultCatalog() {
	  return carma::util::Program::getConfFile(DEFAULT_CATALOG);
      }

      /**
       * @return an iterator over all sources in this catalog, initially 
       * pointing to the first source in the underlying source map.
       */
      carma::services::SourceIterator catalogBegin() const;

      /**
       * @return an iterator over all sources in this catalog, pointing
       * to the end of the underlying source map. For use in loops.
       */
      carma::services::SourceIterator catalogEnd() const;

      /**
       * @return the underlyin map of sources.  The map key is the
       * upper case source name.
       */
      ::std::map< ::std::string, carma::services::Source > getSourceMap() const 
      {
	  return sources_;
      }

      /**
       * Return list of sources within delta degrees of a position (X,Y)
       * @param x X coordinate
       * @param y Y coordinate
       * @param delta angular target size
      std::vector<Sources> getSources(Angle x, Angle y, Angle delta);
       */

    private:
      // map of sources
      std::map<std::string, carma::services::Source> sources_;
      static const std::string DEFAULT_CATALOG;
      static const unsigned short SOURCE_NAME_CHAR_LIMIT;

    }; // end class SourceCatalog

  }
}

#endif // CARMA_SERVICES_SOURCECATALOG_H
