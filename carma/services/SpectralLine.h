/**
 * $Id: SpectralLine.h,v 1.6 2005/10/05 17:57:37 davidm Exp $
 *
 * @file 
 * @author Chul Gwon
 */

#ifndef CARMA_SERVICES_SPECTRALLINE_H
#define CARMA_SERVICES_SPECTRALLINE_H

#include "carma/services/CatalogEntry.h"
#include "carma/services/Frequency.h"

namespace carma {
namespace services {

 /**
  * SpectralLine is derived from CatalogEntry and is used to hold information
  * for a particular entry from a line catalog
  */
  class SpectralLine : public carma::services::CatalogEntry {
  public:
    SpectralLine();
    virtual ~SpectralLine();
    
    void setFrequency(Frequency frequency);
    void setTransition(std::string transition);

    Frequency getFrequency() const;
    std::string getTransition() const;

  private:
    Frequency frequency_;
    std::string transition_;
    // always include additional one for name
    static const int NELEMENTS = 2;

  }; // end class SpectralLine
}
}

#endif // CARMA_SERVICES_SPECTRALLINE_H
