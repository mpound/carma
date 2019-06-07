#include "carma/szautil/Site.h"

using namespace std;
using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
Site::Site() 
{
  initialize();
}

void Site::initialize()
{
  latInit_  = false;
  latitude_.initialize();

  altInit_  = false;
  altitude_.initialize();

  longInit_ = false;
  longitude_.initialize();
}

Site::Site(Angle& longitude, Angle& latitude, Length& altitude)
{
  setTo(longitude, latitude, altitude);
}

/**.......................................................................
 * Destructor.
 */
Site::~Site() {}

void Site::setTo(Angle& longitude, Angle& latitude, Length& altitude)
{
  setLongitude(longitude);
  setLatitude(latitude);
  setAltitude(altitude);
}

void Site::setLongitude(Angle& longitude)
{
  longitude_ = longitude;
  longInit_  = true;
}

void Site::setLatitude(Angle& latitude)
{
  latitude_  = latitude;
  latInit_   = true;
}

void Site::setAltitude(Length& altitude)
{
  altitude_  = altitude;
  altInit_   = true;
}
