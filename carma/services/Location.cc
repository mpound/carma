/**
 * @file Location.cc
 * $Id: Location.cc,v 1.10 2006/01/13 14:37:09 cgwon Exp $
 *
 * @author Chul Gwon
 * 
 */

#include "carma/services/Location.h"
#include "carma/services/Table.h"
#include "carma/services/Vector.h"
#include "carma/util/NotFoundException.h"
#include "carma/util/Program.h"
#include "carma/util/StringUtils.h"
#include "carma/util/Trace.h"

using namespace carma::services;
using namespace carma::util;
using namespace std;

Location::Location() :
  longitude_(0.0, "radians"),
  latitude_(0.0, "radians"),
  altitude_(0.0, "meters"),
  name_("unnamed") {}

Location::Location(const Angle& longitude, 
	           const Angle& latitude, 
		   const Length& altitude) :
  longitude_(longitude),
  latitude_(latitude),
  altitude_(altitude),
  name_("unnamed") {}


Location::Location(const std::string& observatory,
	           const std::string& position) :
  longitude_(0.0, "radians"),
  latitude_(0.0, "radians"),
  altitude_(0.0, "meters"),
  name_(observatory+"/"+position)
{
  Table t;
  string catalog = Program::getConfFile("catalogs/Observatory.cat");
  t.open(catalog);

  std::vector<std::string> name = t.getColumn(0);
  // these return radians!
  std::vector<double>       lon = t.getDMSColumn(1);
  std::vector<double>       lat = t.getDMSColumn(2);
  std::vector<double>       alt = t.getDoubleColumn(3);
  std::vector<std::string>  pos = t.getColumn(4);

  // loop over the list until observatory found
  int stop=name.size();
  for (int i = 0; i < stop; i++) {      
    if (   StringUtils::equalsIgnoreCase(name[i], observatory)
        && pos[i] == position) 
    {  
	ostringstream oos;
	oos << " setting pad " << pos[i]
	    << " coords lon[rad],lat[rad],alt[m] = " 
	    << lon[i] << ", " << lat[i] << ", " << alt[i];
	CARMA_CPTRACE(carma::util::Trace::TRACE5, oos.str());
	    

      longitude_.reset(lon[i],"radians");
      latitude_.reset(lat[i],"radians");
      altitude_.reset(alt[i],"meters");
      return;
    }
  }
  // Unable to find requested location, throw a descriptive message.
  std::ostringstream os;
  os << " Could not find observatory/position " 
     << observatory << "/" << position
     << " in " << catalog;
  throw CARMA_EXCEPTION(carma::util::NotFoundException,os.str());
}

void Location::setLatitude(Angle latitude) {
  latitude_ = latitude;
}

void Location::setLatitude(double value, const std::string &units) {
  latitude_.reset(value, units);
}

void Location::setLongitude(Angle longitude) {
  longitude_ = longitude;
}

void Location::setLongitude(double value, const std::string &units) {
  longitude_.reset(value, units);
}

void Location::setAltitude(Length altitude) {
  altitude_ = altitude;
}

void Location::setAltitude(double value, const std::string &units) {
  altitude_.reset(value, units);
}

Angle Location::getLatitude() const {
  return latitude_;
}

Angle Location::getLongitude() const {
  return longitude_;
}

Length Location::getAltitude() const {
  return altitude_;
}

std::string Location::getName() const 
{
    return name_;
}

carma::services::Vector<double> Location::vector() {
  return carma::services::Vector<double>(longitude_.getValue(), 
					 latitude_.getValue(),
					 altitude_.getValue());

}

std::ostream& carma::services::operator<<(
	std::ostream& os, 
	const carma::services::Location& location
        )
{
    os << "Location: " << location.getName()
       << " (longitude,latitude,altitude) [deg,deg,m] = (" 
       << setprecision(6) 
       << location.getLongitude().degrees() << ","
       << location.getLatitude().degrees() << ","
       << location.getAltitude().meters() << ")";
    return os;
}
