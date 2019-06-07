/**
 * $Id: Source.cc,v 1.10 2008/11/24 14:22:06 mpound Exp $
 * 
 * @file Source.cc
 * @author Chul Gwon
 */

#include "carma/services/Astro.h"
#include "carma/services/Source.h"
#include "carma/services/stringConstants.h"
#include "carma/util/StringUtils.h"
#include <iomanip>
#include <iostream>

using namespace carma::services;
using namespace carma::services::constants;
using namespace carma::util;
using namespace std;

Source::Source() : CatalogEntry(),
		   x_(Angle(0.0, "radians")),
		   y_(Angle(0.0, "radians")),
		   velocity_(Velocity(0.0, "km/s")),
		   parallax_(Angle(0.0, "arcsec"))		   
{
  CatalogEntry::setName( services::NO_SOURCE );
  coordSys_ = COORDSYS_RADEC;
  xProperMotion_ = 0.0;
  yProperMotion_ = 0.0;
  catalogFormat_ = "CARMA";
  idNo_ = 0;
  pntType_ = PNT_RADIO;
  comments_ = "default values";
}

Source::Source(const string& name) : CatalogEntry(),
					  x_(Angle(0.0, "radians")),
					  y_(Angle(0.0, "radians")),
					  velocity_(Velocity(0.0, "km/s")),
					  parallax_(Angle(0.0, "arcsec"))
  
{
    CatalogEntry::setName(name);
    coordSys_ = COORDSYS_RADEC;
    xProperMotion_ = 0.0;
    yProperMotion_ = 0.0;
    catalogFormat_ = "CARMA";
    idNo_ = 0;
    pntType_ = PNT_RADIO;
    comments_ = "default values";
}

Source::Source(const string& name,
	       const Angle& xCoord,
	       const Angle& yCoord,
	       const Velocity& velocity,
	       const Angle& parallax,
	       coordSysType coordSys,
	       double xProperMotion,
	       double yProperMotion,
	       const string& catalogFormat,
	       unsigned long idNo,
	       sourcePntType pntType,
	       const string& comments
	      ) : CatalogEntry(),
	x_(xCoord),
	y_(yCoord),
	velocity_(velocity),
	parallax_(parallax),
	coordSys_(coordSys),
	xProperMotion_(xProperMotion),
	yProperMotion_(yProperMotion),
	catalogFormat_(catalogFormat),
	idNo_(idNo),
	pntType_(pntType),
	comments_(comments)
{
    CatalogEntry::setName(name);
}

Source::Source(const string& name,
	       const Angle& xCoord,
	       const Angle& yCoord,
	       const Velocity& velocity,
	       const Distance& distance,
	       coordSysType coordSys,
	       double xProperMotion,
	       double yProperMotion,
	       const string& catalogFormat,
	       unsigned long idNo,
	       sourcePntType pntType,
	       const string& comments
	      ) : CatalogEntry(),
	x_(xCoord),
	y_(yCoord),
	velocity_(velocity),
	parallax_( distance.getParallax() ),
	coordSys_(coordSys),
	xProperMotion_(xProperMotion),
	yProperMotion_(yProperMotion),
	catalogFormat_(catalogFormat),
	idNo_(idNo),
	pntType_(pntType),
	comments_(comments)
{
    CatalogEntry::setName(name);
}

Source::~Source() {}

void Source::setXCoordinate(const Angle& xcoord) {
    x_ = xcoord;
}

void Source::setYCoordinate(const Angle& ycoord) {
    y_ = ycoord;
}

void Source::setXProperMotion(double xProperMotion) {
    xProperMotion_ = xProperMotion;
}

void Source::setYProperMotion(double yProperMotion)
{
    yProperMotion_ = yProperMotion;
}

void Source::setVelocity(const Velocity& velocity) {
    velocity_ = velocity;
}

void Source::setParallax(const Angle& parallax) {
    parallax_ = parallax;
}

void Source::setCatalogFormat(const string& catalogFormat)
{
    catalogFormat_ = catalogFormat;
}

void Source::setIdNo(unsigned long idNo) {
    idNo_ = idNo;
}

void Source::setComments(const string& comments) {
  comments_ = comments;
}

bool Source::isPlanet(const string& sourceName)
{

    return Astro::isPlanet(sourceName);
}

/**.......................................................................
 * Write the contents of this object to an ostream
 */
ostream& carma::services::operator<<(ostream& os, 
	carma::services::Source& source)
{
  os << source.getName();
  string first;
  switch ( source.getCoordSysType() ) {
    default:
    case COORDSYS_RADEC:
	os << " (RA,DEC)=(";
        first  = source.getXCoordinate().dms(true, 1);
      break;
    case COORDSYS_GALACTIC:
	os << " (L,B)=(";
        first  = source.getXCoordinate().dms(true, 1);
      break;
    case COORDSYS_AZEL:
	os << " (AZ,EL)=(";
        first  = source.getXCoordinate().dms(false, 1);
      break;
  }
  os << first
     << " , "
     << source.getYCoordinate().dms(false, 1)
     << " )  VEL="
     << source.getVelocity().kms()
     << " km/s";
  return os;
}
