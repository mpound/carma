
/**
 * @file 
 * $Id: Pad.h,v 1.6 2012/06/27 18:05:05 eml Exp $
 *
 * @author Marc Pound
 *
 */

#ifndef CARMA_SERVICES_PAD_H
#define CARMA_SERVICES_PAD_H

#include "carma/services/Length.h"
#include "carma/services/Location.h"
#include "carma/services/stringConstants.h"

namespace carma {
namespace services {

  /**
   * This class specifies an antenna pad or station with 
   * coordinates, name, and array configuration membership.
   */

  class Pad {
  public:

    /**
     * Constructor using observatory name and pad designation from the 
     * catalog that should live in CARMA/conf/catalogs/Observatory.cat
     * @param observatory   Case-<b>in</b>sensitive name of the observatory.
     * Examples are "CARMA", "ovro", "wsrt" etc.
     * @param pad  Name of the pad/station. Examples are "pad#29", "Area-51".
     * <i>The pad names are case-sensitive.</i>
     */
    Pad(const std::string& observatory = CARMA_OBSERVATORY , 
	const std::string& pad = REFERENCE );


    /**
     * Constructor using observatory name and pad designation from the 
     * catalog that should live in CARMA/conf/catalogs/Observatory.cat
     * @param observatory   Name of the observatory
     * @param observatory   Case-<b>in</b>sensitive name of the observatory.
     * Examples are "CARMA", "ovro", "wsrt" etc.
     * @param pad  Name of the pad/station. Examples are "pad#29", "Area-51".
     * <i>The pad names are case-sensitive.</i>
     * @param padLocation  Location object indicating 
     * longitude, latitude,and altitude of this pad.
     * @param arrayReference Location object indicating 
     * longitude, latitude,and altitude of the array reference position 
     * for this pad.
     * @param arrayConfigs string representing the array configuration(s)
     * in which this pad is a member.  Individual configurations must be 
     * comma-separated, e.g.  "A,B,BnC",  and are case-sensitive.
     */
    Pad(const std::string& observatory, const std::string& pad,
        const carma::services::Location& padLocation,
        const carma::services::Location& arrayReference,
        const std::string& arrayConfigs
        );

    virtual ~Pad();

    /**
     * @return The fixed Location of this pad.
     */
    carma::services::Location getLocation() const
    {
	return location_;
    }

    /**
     * @return The array reference position for this pad.
     */
    carma::services::Location getReference() const
    {
	return reference_;
    }

    /**
     * @return vector of carma::services::Length containing East, North,
     * Up (in that order) offsets of this pad, with respect to its array
     * reference position.
     */
    std::vector<carma::services::Length*> getEnu() const
    {
      return enu_;
    }

    /**
     * @return the name/designation of this pad
     */
    std::string getName() const 
    {
	return name_;
    }

    /**
     * @return The pad number of this Pad.  The pad name is searched
     * for the substring "#", and everything after the # sign is
     * assumed to be the pad number.  If substring "#" does not exist
     * or the string-to-number conversion fails, an NotFoundException is
     * thrown.
     */

    unsigned short getPadNo() const;

    /**
     * @return a representation of the array configurations in which
     * this pad is a member.  The pad<->array configuration information is 
     * located in Observatory.cat.
     */
    std::vector<std::string> getArrayConfigs() const 
    {
	return arrayConfigs_;
    }

    /**
     * @return true if this pad is in the specified array configuration,
     * false, otherwise. This comparison is case-sensitive.
     */
    bool isInArrayConfig(const std::string& array);
    
    /**
     * @return a string description of this pad
     */
    std::string toString() const;

  private:

    /**
     * The location of this pad
     */
    carma::services::Location location_;

    /** 
     * The array reference position
     */
    carma::services::Location reference_;

    /**
     * The name/designation of this pad
     */
    std::string name_;

    /**
     * The name of the observatory with which this pad is associated.
     */
    std::string observatory_;

    /**
     * ENU coordinates of this Pad with respect to its reference
     * position, in meters
     */
    std::vector<carma::services::Length*> enu_;

    /**
     * The configuration membership of this pad
     */
    std::vector<std::string> arrayConfigs_;


  }; // end class Pad

 } // end namespace services
} // end namespace carma


#endif
