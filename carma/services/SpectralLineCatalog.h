/**
 * @file 
 *
 * $Id: SpectralLineCatalog.h,v 1.11 2006/10/03 21:16:30 mpound Exp $
 *
 * @author Chul Gwon
 * 
 */


#ifndef CARMA_UTIL_SPECTRALLINECATALOG_H
#define CARMA_UTIL_SPECTRALLINECATALOG_H


#include <iostream>
#include <iomanip>
#include <sstream>
#include <string>
#include <map>

#include "carma/services/Table.h"
#include "carma/services/Frequency.h"
#include "carma/services/SpectralLine.h"
#include "carma/services/Catalog.h"

namespace carma {
  namespace services {
    
    /**
     * This class extracts information from a spectral line catalog
     * file and puts it into a multimap for later searches
     */

    class SpectralLineCatalog : public carma::services::Catalog {
    public:

      /**
       ** Default constructor
       */
      SpectralLineCatalog();
      
      /**
       ** Destructor
       */
      ~SpectralLineCatalog();
      

      /**
       *  Open a table for read access
       *  @param fileName the file to open.
       */
      void open(const std::string& fileName);

      /**
       *  Look up spectral line from a catalog.
       * <b>This look-up is case insensitive.</b>
       *  @param lineName The identifying name of the spectral line 
       *  @param freqLo lower limit on the frequency of the spectral
       *  lines
       *  @param freqHi upper limit on the frequency of the spectral
       *  lines
       *  @return A vector of carma::services::SpectralLine which fall
       *  within the limits defined by the frequency parameters
       *  @throw SpectraLineNotFoundException if the spectral line 
       *  cannot be found in the opened catalog.
       */
      std::vector<carma::services::SpectralLine> 
	  lookup(const std::string & lineName,
		 const carma::services::Frequency & freqLo,
		 const carma::services::Frequency & freqHi);


      /**
       *  Look up spectral line from a catalog.
       * <b>This look-up is case insensitive.</b>
       *  @param name The spectral line name
       *  @param transition The specific transition for the spectral line
       *  @return A carma::services::SpectralLine representation of 
       *  the named entry
       *  @throw SpectraLineNotFoundException if the spectral line 
       *  cannot be found in the opened catalog.
       */
      const carma::services::SpectralLine& lookup(const std::string &name,
						  const std::string &transition);

    private:
      /**
       *  Look up spectral line from a catalog.
       * <b>This look-up is case insensitive.</b>
       *  @param name The identifying name of the line concatenated
       *  with the transition, separated with a ':'
       *  @return A carma::services::SpectralLine representation of 
       *  the named entry
       *  @throw SpectraLineNotFoundException if the spectral line 
       *  cannot be found in the opened catalog.
       */
      const carma::services::SpectralLine& lookup(const std::string &name);

      // use a multimap since a line can have more than one freq
      // (caused by different transitions of the same molecule)
      std::multimap<std::string, carma::services::SpectralLine> lines_;
    }; // end SpectralLineCatalog

  }
}

#endif  // CARMA_UTIL_SPECTRALLINECATALOG_H
