/**
 * @file 
 * $Id: Location.h,v 1.11 2006/11/09 16:43:39 mpound Exp $
 *
 * Representation of a geographic location on the Earth.
 *
 * @author Chul Gwon
 *
 */

#ifndef CARMA_SERVICES_LOCATION_H
#define CARMA_SERVICES_LOCATION_H

#include "carma/services/Angle.h"
#include "carma/services/Length.h"
#include "carma/services/stringConstants.h"

namespace carma {
 namespace services {

   template <typename T> class Vector;

  /**
   * Location specifies a location (observatory if you wish) on planet earth,
   * as longitude, latitude, and altitude above sea-level.
   * Longitude increases from 0 at Greenwich, England in an
   * easterly direction, and decreases in a westerly direction.
   * East longitudes are positive numbers, between 0 and 180 degrees.
   * West longitudes are negative numbers, between 0 and -180 degrees.
   */

  class Location {
  public:
    /**
     * default constructor, longitude = 0, latitude = 0, altitude = 0.
     */
    Location();

    /**
     * Full constructor where longitude, latitude and altitude
     * are specified. Recall west (e.g. CARMA) has longitude < 0
     * @param longitude  Longitude of the observatory. (<0 means west)
     * @param latitude   Latitude 
     * @param altitude   Altitude
     */
    Location(const Angle& longitude, const Angle& latitude, 
	     const Length& altitude);

    /**
     * Constructor using a name from the Observatory.cat catalog
     * that should live in CARMA/conf/catalogs/Observatory.cat
     * Examples are "carma", "ovro", "wsrt" etc.etc.
     * The match is case-insensitive.
     * If this constructor were not explicit, Ephemeris(string)
     * constructor could be ambiguously interpreted as meaning a Location
     * or a Source.
     * @param observatory   Name of the observatory
     * @param position      Name of the station/pad position for this location,
     *                      defaults to "reference" meaning the array reference
     *                      position
     *
     * @see carma:services::Observatory
     */

    explicit Location(const std::string & observatory, 
	     const std::string& position = REFERENCE);

    virtual ~Location() {}
    
    /**
     * Specify the latitude.
     * @param latitude  The latitude represented as a carma::services::Angle
     */
    void setLatitude(Angle latitude);

    /**
     * Specify the latitude, as value and units which 
     * will be used to construct an Angle internally.
     * @param value The latitude value
     * @param units The latitude units
     */
    void setLatitude(double value, const std::string &units);

    /**
     * Specify the longitude.
     * @param longitude The longitude represented as a carma::services::Angle
     */
    void setLongitude(Angle longitude);

    /**
     * Specify the longitude, as value and units which 
     * will be used to construct an Angle internally.
     * @param value The longitude value
     * @param units The longitude units
     */
    void setLongitude(double value, const std::string &units);

    /**
     * Specify the altitude above sea level.
     * @param altitude  The altitude above sea level represented as 
     * a carma::services::Length (may be negative)
     */
    void setAltitude(Length altitude);

    /**
     * Specify the altitude above sea level, as value and units which 
     * will be used to construct a Length internally.
     *
     * @param value  The altitude value
     * @param units  The altitude units
     */
    void setAltitude(double value, const std::string &units);

    /**
     * @return The latitude, as an Angle
     */
    Angle getLatitude() const;

    /**
     * @return The longitude, as an Angle
     */
    Angle getLongitude() const;

    /**
     * @return The altitude, as a Length
     */
    Length getAltitude() const;

    /**
     * @return the name of this location, "unnamed" if it has no name
     */
    std::string getName() const;

    /**
     * @Return the longitude, latitude and altitude (in that order!!!) as a
     * carma:services::Vector. Note the units will be 
     * standard CARMA units, i.e., radians
     * for the two angles, and meters for altitude.
     */
    carma::services::Vector<double> vector();

  private:
    Angle longitude_;
    Angle latitude_;
    Length altitude_;
    std::string name_;

  }; // end class Location

  /**
   *  Define the << operator to allow, e.g. cout << Location
   */
  std::ostream& operator<<(std::ostream& os, 
                           const carma::services::Location& location);

 }
}

#endif // CARMA_SERVICES_LOCATION_H
