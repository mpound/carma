
/**
 * @file 
 * $Id: Observatory.h,v 1.9 2014/04/02 23:11:12 iws Exp $
 *
 * @author Marc Pound
 *
 */

#ifndef CARMA_SERVICES_OBSERVATORY_H
#define CARMA_SERVICES_OBSERVATORY_H

#include "carma/services/AntennaCoordinates.h"
#include "carma/services/Location.h"
#include "carma/services/Pad.h"
#include "carma/services/stringConstants.h"
#include "carma/services/Types.h"
#include <map>
#include <vector>

namespace carma {
namespace services {

  /**
   * Observatory specifies the parameters of a given observatory: name,
   * reference location (longitude, latitude, altitude), optional pad names and
   * locations, and optional array configurations.  The class is 
   * instantiated from the file conf/catalogs/observatory/Observatory.cat.
   * Once instantiated, the object can be queried for any of its
   * properties, and pads can be accessed individually or in groups.
   * All properties, including Pads, are read-only.
   */

  class Observatory {
  public:
    
    /**
     * Constructor using a name from the Observatory.cat catalog
     * that should live in CARMA/conf/catalogs/Observatory.cat
     * Examples are "carma", "ovro", "wsrt" etc.etc.
     * The match is case-insensitive.
     * @param observatory   Name of the observatory; default is "carma".
     * @throws carma::util::NotFoundException if the an observatory
     * matching the requested name is not found.
     * @throws carma::util::NotFoundException if the a position
     * called "reference" for the named observatory is not found.
     */
    Observatory(const std::string& observatory = CARMA_OBSERVATORY );

    /**
     * Destructor
     */
    virtual ~Observatory();

    /**
     * @return the name of this observatory
     */
    std::string getName() const 
    {
	return name_;
    }

    /**
     * @return The reference location (longitude, latitude, altitude)
     * of this observatory, i.e. the "array center".
     */
    carma::services::Location getReference() const 
    {
	return reference_;
    }

    /**
     * @return The reference location of this observatory
     * in an AntennaCoordinates container, which supports various
     * types of calculations, e.g. coordinate offsets between 
     * station locations.
     */
    carma::services::AntennaCoordinates getReferenceCoordinates() const ;

    /**
     * @return The reference location of this observatory
     * in an Pad container.
     */
    carma::services::Pad getReferencePad() const ;

    /**
     * Request a specific Pad by name.
     *
     * @param The name of the requested pad
     * @return The Pad object representing the request pad, if the
     * name is matched.
     * @throws carma::util::NotFoundException if the pad is not found.
     */
    const carma::services::Pad getPad(const std::string& name);

    /**
     * @return a map of all Pads in this Observatory, where the
     * key is the pad name.
     */
    std::map<std::string, carma::services::Pad> getPadMap() const
    {
	return padMap_;
    }

    /**
     * @return an iterator over all Pads in this observatory, initially
     * pointing to the first pad in the list.
     */
    carma::services::PadIterator mapBegin() const;

    /**
     * @return an iterator over all Pads in this observatory, 
     * pointing to the end of the map.  For use in loops.
     */
    carma::services::PadIterator mapEnd() const;

    /**
     * Get all the pads in a specific array configuration.
     * @param arrayConfig The name of the array configuration to retrieve.
     * @return a vector containing the pads that are in requested
     * array configuration.
     * @throws carma::util::NotFoundException if no matching Pads are found
     * (the requested array configuration is not in this Observatory).
     */
    const std::vector<carma::services::Pad> getPadsInConfig( 
	                                      const std::string& arrayConfig);
    
    /**
     * @return The total number of array configurations in this
     * observatory.
     */
    int numConfigs() const 
    {
	return numConfigs_ ;
    }

    /**
     * @return The total number of pads in this Observatory.
     */
    int numPads() const
    {
	return padMap_.size();
    }
    /**
     * @param arrayConfig The name of the array configuration to retrieve.
     * @return The total number of pads in a given array configuration.
     * This method will return zero if the requested array configuration
     * is not in this Observatory.
     */
    int numPadsInConfig(const std::string& arrayConfig);

    /**
     * @param arrayConfig The name of the array configuration to retrieve.
     * @return The total number of baselines in a given array configuration.
     * This method will return zero if the requested array configuration
     * is not in this Observatory.
     */
    int numBaselinesInConfig(const std::string& arrayConfig);

    /**
     * @return a string description of this observatory.
     */
    std::string toString() const;

  private:
    std::string                                 name_;
    int                                         numConfigs_;
    std::map<std::string, carma::services::Pad> padMap_;
    carma::services::Location                   reference_;

  }; // end class Observatory

 }
}

#endif
