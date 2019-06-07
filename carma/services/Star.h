/**
 * @file 
 * $Id: Star.h,v 1.1 2008/11/24 14:22:06 mpound Exp $
 *
 * @author Marc Pound
 *
 */

#ifndef CARMA_SERVICES_STAR_H
#define CARMA_SERVICES_STAR_H

#include "carma/services/CatalogEntry.h"

namespace carma {
namespace services {

  /**
   * Star holds the name and magnitude of a star.
   * Used by OpticalCatalog.
   */
  class Star : public carma::services::CatalogEntry {
  public:

    /** Default constructor */
    Star();

    /** Destructor */
    virtual ~Star();

    /**
     * @return the (visual) magnitude of this star
     */
    double getMagnitude() const {
	return magnitude_;
    }

    /**
     * Set the magnitude of this star
     * @param magnitude the (visual) magnitude of this star
     */
    void setMagnitude(double magnitude ) {
	magnitude_ = magnitude;
    }

  private:
    double magnitude_;

  };

} // end namespace services
} // end namespace carma

#endif // CARMA_SERVICES_STAR_H
