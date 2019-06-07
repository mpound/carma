/**
 * @file 
 * $Id: FluxDensity.h,v 1.2 2014/04/02 23:11:12 iws Exp $
 *
 * @author Chul Gwon
 */

#ifndef CARMA_SERVICES_FLUXDENSITY_H
#define CARMA_SERVICES_FLUXDENSITY_H

#include "carma/services/ConformableQuantity.h"

namespace carma {
namespace services {

  /**
   * The FluxDensity class is used to represent a flux density in any units.  
   * It uses the Units class to internally deal with conversions.
   */
  class FluxDensity : public ConformableQuantity {
   public:
    /**
     * Construct a FluxDensity object
     * @param value flux value
     * @param units flux units
     * @throws carma::util::IllegalArgumentException
     */
    FluxDensity(double value, const std::string &units);

    virtual ~FluxDensity();

    /**
     * Convenience method to return the value of this FluxDensity in
     * Janskys.
     * @return the flux value in Janskys
     */
    double jansky() const;

    /**
     * @return the flux value in mJy
     */
    double millijansky() const;

    /**
     * @return the flux value in megaJy
     */
    double megajansky() const;


    /**
     * Add two FluxDensities
     * @return FluxDensity object in units Jy
     * @throws ConformabilityException
     */
    const FluxDensity operator+(const FluxDensity& flux) const;

    /**
     * Increment FluxDensity
     * @return FluxDensity object in units Jy
     * @throws ConformabilityException
     */
    const FluxDensity operator+=(const FluxDensity& flux);

    /**
     * Subtract two FluxDensities
     * @return FluxDensity object in units Jy
     * @throws ConformabilityException
     */
    const FluxDensity operator-(const FluxDensity& flux) const;

    /**
     * Decrement FluxDensity
     * @return FluxDensity object in units Jy
     * @throws ConformabilityException
     */
    const FluxDensity operator-=(const FluxDensity& flux);

    /**
     * Compare two FluxDensityes
     * @throws ConformabilityException
     */
    bool operator<(const FluxDensity& flux) const;

  };

  /**
   * Define << operator for easy output
   */
  std::ostream& operator<<(std::ostream& os, const carma::services::FluxDensity& flux);

} // end namespace services
} // end namespace carma

#endif
