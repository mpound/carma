#include "carma/services/Neighbor.h"

using namespace ::std;
using namespace carma;
using namespace carma::services;

Neighbor::Neighbor() :
      name_(""),
      reference_(""),
      distance_(0),
      azimuth_(0),
      elevation_(0),
      brightness_(0),
      isOptical_(false)
{ 

}

Neighbor::~Neighbor() { }

const ::std::string 
Neighbor::getName() const
{
    return name_;
}

void 
Neighbor::setName( ::std::string name )
{
    name_ = name;
}

const ::std::string 
Neighbor::getReference() const
{
    return reference_;
}

void 
Neighbor::setReference( ::std::string reference )
{
    reference_ = reference;
}

float
Neighbor::getDistance() const
{
    return distance_;
}

void 
Neighbor::setDistance( const float distance )
{
    distance_ = distance;
}

float
Neighbor::getAzimuth() const
{
    return azimuth_;
}

void 
Neighbor::setAzimuth( const float azimuth )
{
    azimuth_ = azimuth;
}

float
Neighbor::getElevation() const
{
    return elevation_;
}

void 
Neighbor::setElevation( const float elevation)
{
    elevation_ = elevation;
}

float
Neighbor::getBrightness() const
{
    return brightness_;
}

void 
Neighbor::setBrightness( const float brightness) 
{
    brightness_ = brightness;
}

bool
Neighbor::isOptical() const
{
    return isOptical_;
}


void 
Neighbor::setOptical( const bool optical )
{
    isOptical_ = optical;
}

double
Neighbor::getMJD() const 
{
    return mjd_;
}

void
Neighbor::setMJD( const double mjd )
{
    mjd_ = mjd;
}


bool
Neighbor::operator<( const Neighbor & rhs ) const
{
    return ( this->distance_ < rhs.distance_ );
}

bool
Neighbor::operator==( const Neighbor & rhs ) 
{
    return (
      this->name_       == rhs.name_        &&
      this->reference_  == rhs.reference_   &&
      this->distance_   == rhs.distance_    &&
      this->azimuth_    == rhs.azimuth_     &&
      this->elevation_  == rhs.elevation_   &&
      this->brightness_ == rhs.brightness_  && 
      this->isOptical_  == rhs.isOptical_   &&
      this->mjd_        == rhs.mjd_
      );
}

/**.......................................................................
 * Write the contents of this object to an ostream
 */
::std::ostream & 
carma::services::operator<<( ::std::ostream & os, 
	                     const carma::services::Neighbor & neighbor )
{
  os << "Name: " << neighbor.getName() 
     << " " 
     << "Ref: " << neighbor.getReference()
     << " " 
     << "Dist: " << neighbor.getDistance()
     << " " 
     << "Az:   " << neighbor.getAzimuth()
     << " " 
     << "El:   " << neighbor.getElevation()
     << " "
     << "Bright: " << neighbor.getBrightness()
     << " "
     << "Optical? " << boolalpha << neighbor.isOptical()
     << " "
     << "MJD: " << neighbor.getMJD()
     ;
  return os;
}
