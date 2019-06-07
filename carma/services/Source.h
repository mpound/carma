/**
 * $Id: Source.h,v 1.15 2008/11/24 14:22:06 mpound Exp $
 * 
 * @file Source.h
 * @author Chul Gwon
 *
 */

#ifndef CARMA_SERVICES_SOURCE_H
#define CARMA_SERVICES_SOURCE_H

#include "carma/services/CatalogEntry.h"
#include "carma/services/Angle.h"
#include "carma/services/Distance.h"
#include "carma/services/Velocity.h"

namespace carma {
namespace services {
  /**
   * Source is derived from CatalogEntry and is used to hold information
   * for a particular entry from a source catalog.
   */
  class Source : public carma::services::CatalogEntry {
  public:
      /**
       * Default Constructor. We need this because this class
       * is used in a map in SourceCatalog
       */
      Source();

      /**
       * Simple Constructor 
       * @param name The name of the source object.
       */
      Source(const std::string& name);

      /**
       * Full Constructor 
       *
       * @param name The name of the source
       *
       * @param xCoord The X coordinate of the object 
       * (e.g. RA, Galactic Longitude), represented by an Angle
       *
       * @param yCoord The Y coordinate of the object 
       * (e.g. DEC, Galactic Latitude), represented by an Angle
       *
       * @param velocity The radial velocity of the source, represented 
       * by a Velocity
       *
       * @param parallax The parallax of the source, represented by an Angle. 
       *
       * @param coordSys the coordinate system type. (COORDSYS_RADEC,
       * COORDSYS_GALACTIC)
       *
       * @param xProperMotion The proper motion in the X direction 
       * (mas/yr)
       *
       * @param yProperMotion The proper motion in the Y direction 
       * (mas/yr)
       *
       * @param catalogFormat Catalog format (FK5, Hipparcos)
       *
       * @param idNo An ID number for this source.
       *
       * @param pntType Indicates if this source can be used for
       * radio and/or optical pointing. Default PNT_RADIO.
       *
       * @param comments Comments about this source
       */
      Source(const std::string& name,
	     const Angle& xCoord,
	     const Angle& yCoord,
	     const Velocity& velocity,
	     const Angle& parallax,
	     coordSysType coordSys = COORDSYS_RADEC,
	     double xProperMotion  = 0.0,
	     double yProperMotion  = 0.0,
	     const std::string& catalogFormat   = "CARMA",
	     unsigned long idNo = 0,
	     sourcePntType pntType = PNT_RADIO,
	     const std::string& comments = "This Space For Rent"
	    );

      /**
       * Full Constructor 
       *
       * @param name The name of the source
       *
       * @param xCoord The X coordinate of the object 
       * (e.g. RA, Galactic Longitude), represented by an Angle
       *
       * @param yCoord The Y coordinate of the object 
       * (e.g. DEC, Galactic Latitude), represented by an Angle
       *
       * @param velocity The radial velocity of the source, represented 
       * by a Velocity
       *
       * @param distance The distance to the source, represented by 
       * a Distance. CARMA convention is that a zero distance 
       * (Distance::value()) means the infinite distance (parallax=0).
       * This quantity is stored internally as a parallax Angle.
       *
       * @param coordSys the coordinate system type. 
       * (COORDSYS_RADEC, COORDSYS_GALACTIC)
       *
       * @param xProperMotion The proper motion in the X direction 
       * (mas/yr)
       *
       * @param yProperMotion The proper motion in the Y direction 
       * (mas/yr)
       *
       * @param catalogFormat Catalog format (FK5, Hipparcos)
       *
       * @param idNo An ID number for this source.
       *
       * @param comments Comments about this source
       */
      Source(const std::string& name,
	     const Angle& xCoord,
	     const Angle& yCoord,
	     const Velocity& velocity,
	     const Distance& distance,
	     coordSysType coordSys = COORDSYS_RADEC,
	     double xProperMotion  = 0.0,
	     double yProperMotion  = 0.0,
	     const std::string& catalogFormat = "CARMA",
	     unsigned long idNo = 0,
	     sourcePntType pntType = PNT_RADIO,
	     const std::string& comments = "This Space For Rent"
             );


    /** Destructor*/
    virtual ~Source();

    /**
     * Set the X coordinate of this Source
     * @param xcoord The X coordinate of the object 
     * (e.g. RA, Galactic Longitude), represented by an Angle
     */
    void setXCoordinate(const Angle& xcoord);

    /**
     * Set the Y coordinate of this Source
     * @param ycoord The Y coordinate of the object 
     * (e.g. DEC, Galactic Latitude), represented by an Angle
     */
    void setYCoordinate(const Angle& ycoord);

    /**
     * Set the X proper motion of this Source
     * @param xProperMotion The proper motion in the X direction 
     * (mas/yr)
     */
    void setXProperMotion(double xProperMotion);

    /**
     * Set the Y proper motion of this Source
     * @param yProperMotion The proper motion in the Y direction 
     * (mas/yr)
     */
    void setYProperMotion(double yProperMotion);

    /**
     * Set the radial velocity of this Source
     * @param velocity The radial velocity of the source, represented 
     * by a Velocity
     */ 
    void setVelocity(const Velocity& velocity);

    /**
     * Set the parallax of this Source
     * @param parallax The parallax of the source, represented by an Angle. 
     */
    void setParallax(const Angle& parallax);

    /**
     * Set the catalog format style (this is used by NOVAS).
     * @param catalog Catalog format (FK5, Hipparcos)
     */
    void setCatalogFormat(const std::string& catalogFormat);

    /**
     * Set an ID number for this Source
     * @param idNo An ID number for this source.
     */
    void setIdNo(unsigned long idNo);

    /**
     * Set the pointing type for this source.
     * @param the pointing type (PNT_RADIO,PNT_OPTICAL,PNT_BOTH)
     */
    void setPntType(sourcePntType type) {
	pntType_ = type;
    }

    /**
     * Set any additional comments about this Source
     * @param comments Additional comments about the source.
     */
    void setComments(const std::string& comments);

    // inline all the accessor methods

    /**
     * @return The X coordinate of the object 
     * (e.g. RA, Galactic Longitude), represented by an Angle
     */
    Angle  getXCoordinate() const {
	return x_;
    }

    /**
     * @return The Y coordinate of the object 
     * (e.g. DEC, Galactic Latitude), represented by an Angle
     */
    Angle  getYCoordinate() const {
	return y_;
    }

    /**
     * @return The proper motion in the X direction (mas/yr)
     */
    double getXProperMotion() const {
	return xProperMotion_;
    }

    /**
     * @return the coordinate system enum, as a string
     */
    coordSysType getCoordSysType() const {
	return coordSys_;
    }

    /**
     * @return The proper motion in the Y direction (mas/yr)
     */
    double getYProperMotion() const {
	return yProperMotion_;
    }

    /**
     * @return The radial velocity of the source, represented 
     * by a Velocity
     */ 
    Velocity getVelocity() const {
	return velocity_;
    }

    /**
     * @return The parallax of the source, represented by an Angle. 
     */
    Angle  getParallax() const {
	return parallax_;
    }

    /**
     * @return Additional comments about the source.
     */
    std::string getComments() const {
	return comments_;
    }

    /**
     * @param catalog Catalog format (FK5, Hipparcos)
     */
    std::string getCatalogFormat() const {
	return catalogFormat_;
    }

    /**
     * @return ID number for this source.
     */
    unsigned long getIdNo() const {
	return idNo_;
    }

    /**
     * @return the pointing type for this source 
     * (PNT_RADIO,PNT_OPTICAL,PNT_BOTH)
     */
    sourcePntType getPntType() const {
	return pntType_;
    }

    bool isOptical() const
    {
	return ( pntType_ == PNT_OPTICAL || pntType_ == PNT_BOTH );
    }

    bool isRadio() const {
	return ( pntType_ == PNT_RADIO   || pntType_ == PNT_BOTH );
    }

    /**
     * @return True if this Source's name matches a Solar System 
     * body (including the Moon), false otherwise. Match is case-insensitive.
     */
    bool isPlanet() const
    {
	return Source::isPlanet(getName());
    }

    /**
     * @return An equivalent distance (pc) for this Source's
     * parallax
     */
    Distance getDistance() const {
	return Distance::getDistance(parallax_);
    }

    /**
     * @param sourceName Name of source to check.
     * @return true if the source name matches a Solar System 
     * body (including the Moon), false otherwise. Match is case-insensitive.
     */
    static bool isPlanet(const std::string& sourceName);

  private:
    Angle        x_;             // x coordinate
    Angle        y_;             // y coordinate
    Velocity     velocity_;      // velocity value, frame, definition
    Angle        parallax_;      // trigonometric parallax     
    coordSysType coordSys_;      // coordinate system
    double       xProperMotion_; // milli arcsec (mas) per year
    double       yProperMotion_; // milli arcsec (mas) per year
    std::string  catalogFormat_; // catalog format (for NOVAS)
    unsigned long idNo_;         // source ID number (for NOVAS)
    sourcePntType   pntType_;    // This is a RADIO and/or OPTICAL source.
    std::string  comments_;      // additional comments

    // number of elements in this CatalogEntry
    static const unsigned int NELEMENTS = 11;

  }; // end class Source

/**
 *  Define the << operator to allow, e.g. cout << Source
 */
std::ostream& operator<<(std::ostream& os, carma::services::Source& source);

} // end services
} // end carma

#endif // CARMA_SERVICES_SOURCE_H
