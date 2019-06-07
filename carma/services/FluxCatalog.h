/**
 * @file FluxCatalog.h
 * $Id: FluxCatalog.h,v 1.7 2008/11/24 14:22:06 mpound Exp $
 *
 * @author Chul Gwon
 *
 */

#ifndef CARMA_SERVICES_FLUXCATALOG_H
#define CARMA_SERVICES_FLUXCATALOG_H

#include "carma/services/Catalog.h"
#include "carma/services/FluxSource.h"
#include "carma/util/Program.h"
#include <map>

namespace carma {
namespace services {
  class Frequency; // forward decl
  class FluxCatalog : public carma::services::Catalog {
  public:
    /** Constructor */
    FluxCatalog();
    /** Destructor */
    virtual ~FluxCatalog();

    /**
     * Open a file containing a flux catalog
     * @param fileName The file to open
     * @throw carma::util::NotFoundException if the file is not found
     */
    void open(const std::string& fileName);
    
    /**
     * Look up a flux calibration source from a catalog.
     * <b>This look-up is case insensitive.</b>
     * The most recent flux entry for the named source is returned,
     * regardless of the frequency at which the flux was measured.
     *
     * @param sourceName The flux source name
     * @return A carma::services::Flux representation of the named entry
     *
     * @throw carma::services::SourceNotFoundException if the source name cannot
     * be found in the opened file.
     */
    const carma::services::FluxSource& lookup(const std::string& sourceName);

    /**
     * Look up a flux calibration source from a catalog.
     * <b>This look-up is case insensitive.</b>
     * The most recent flux entry for the named source is returned.
     *
     * @param sourceName The flux source name
     * @param freq The frequency at which to search
     * @param deltaFreq A range of frequency to bracket the freq
     *   parameter.  If <code>freq</code> is 0.0, then this term is ignored.
     *   The value of <code>deltaFreq</code> is used to range matching
     *   frequencies such that a match occurs when
     *   <br>
     *   [freq-deltaFreq <= freq[table] <= freq+deltaFreq]. 
     *   <br>
     * @param deltaTime The range of time, in days, to search. The time
     * of the observations matches in the range 
     * [now-deltaTime <= time <= now]
     * @return A carma::services::Flux representation of the named entry
     *
     * @throw carma::services::SourceNotFoundException if the source 
     * name cannot be found in the opened file, or there is no
     * flux measurement for the source within the specified
     * time and frequency ranges.
     */
    const carma::services::FluxSource& lookup(
	    const std::string& sourceName,
	    const Frequency& freq,
	    const Frequency& deltaFreq,
	    float deltaDays);
    /**
     * @return the default CARMA Flux catalog
     * NB: could move this to Catalog.h if we want all
     * Catalogs to provide this.
     */
    static const std::string defaultCatalog() {
        return carma::util::Program::getConfFile(DEFAULT_CATALOG);
    }

    /**
     * @return the underlying multimap map of flux sources.  The map key is the
     * upper case source name.
     */
    ::std::multimap< ::std::string, carma::services::FluxSource > 
	getSourceMap() const 
    {
       return fluxSources_;
    }



  private:
    // this is a multimap because we have multiple entries for
    // the same source name key.
    std::multimap<std::string, carma::services::FluxSource> fluxSources_;
    std::vector<carma::services::FluxSource> finalMatches_;
    static const std::string DEFAULT_CATALOG;
    const Frequency zeroFreq_;
  };
} // end namespace services
} // end namespace carma

#endif
