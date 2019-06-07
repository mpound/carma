#include <iomanip>
#include <iostream>
#include <fstream>

#include "carma/szautil/Angle.h"
#include "carma/szautil/Astrometry.h"
#include "carma/szautil/ArchiveReader.h"
#include "carma/szautil/BitMask.h"
#include "carma/szautil/Date.h"
#include "carma/szautil/Declination.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/Geoid.h"
#include "carma/szautil/HourAngle.h"
#include "carma/szautil/Length.h"
#include "carma/szautil/Program.h"
#include "carma/szautil/RegDate.h"
#include "carma/szautil/String.h"
#include "carma/szautil/TimeVal.h"

#include "carma/szaslalib/slalib.h"

#include "carma/szapgutil/PgUtil.h"

#include "pgplot/cpgplot.h"

#include <fstream>
#include <string>
#include <vector>

using namespace std;
using namespace sza::util;

static const double eps = 1e-12;

PROGRAM_KEYWORDS = {
  { "source",       "",    "s", USAGE "Source"},
  { "ra",           "",    "s", USAGE "Ra (hours)"},
  { "dec",          "",    "s", USAGE "Dec (degrees)"},
  { "mjd",          "",    "d", USAGE "Mjd (unspecified = now)"},
  { "elmin",        "20",  "d", USAGE "Minimum EL (degrees)"},
  { "printPrimary", "t",   "b", USAGE "True to print primary calibrator"},         
  { "printMjd",     "f",   "b", USAGE "True to print current MJD"},
  { "printLstMin",  "t",   "b", USAGE "True to print minimum LST"},
  { "printLstMax",  "t",   "b", USAGE "True to print maximum LST"},

  { END_OF_KEYWORDS}
};

PROGRAM_INITIALIZE_USAGE {};

//------------------------------------------------------------
// Test programs
//------------------------------------------------------------

enum Type {
  TYPE_UNKNOWN,
  TYPE_PLANET,
  TYPE_FIXED
};

class Source {
public:

  Type type_;
  std::string name_;
  HourAngle ra_;
  Declination dec_;
  int id_;

  void setTo(std::string name) {
    name_ = name;
    type_ = TYPE_PLANET;

    String str = String::toLower(name);
    if(str == "mercury") {
      id_ = 1;
    } else if(str == "venus") {
      id_ = 2;
    } else if(str == "moon") {
      id_ = 3;
    } else if(str == "mars") {
      id_ = 4;
    } else if(str == "jupiter") {
      id_ = 5;
    } else if(str == "saturn") {
      id_ = 6;
    } else if(str == "uranus") {
      id_ = 7;
    } else if(str == "neptune") {
      id_ = 8;
    } else {
      ThrowError("Unrecognized planet: " << name);
    }
  };

  Source() {
    id_   = 0;
    type_ = TYPE_UNKNOWN;
  }

  Source(std::string name) {
    setTo(name);
  };

  Source(std::string name, HourAngle ra, Declination dec) {
    setTo(name, ra, dec);
  };

  friend std::ostream& operator<<(std::ostream& os, Source& src);

  void setTo(std::string name, HourAngle ra, Declination dec) {
    name_ = name;
    type_ = TYPE_FIXED;
    ra_   = ra;
    dec_  = dec;
    id_   = 0;
  };

  Source(const Source& src) {
    name_ = src.name_;
    type_ = src.type_;
    ra_   = src.ra_;
    dec_  = src.dec_;
    id_   = src.id_;
  };
  
};

std::ostream& operator<<(std::ostream& os, Source& src)
{
  os << "Name = " << src.name_ << " RA = " << src.ra_ << " DEC = " << src.dec_;
  return os;
}

/**.......................................................................
 * Construct the list primary sources
 */
std::vector<Source> getPrimarySources()
{
  Source src;
  std::vector<Source> sources;

  src.setTo("Uranus");
  sources.push_back(src);

  src.setTo("Neptune");
  sources.push_back(src);

  src.setTo("Mars");
  sources.push_back(src);

  HourAngle ra;
  ra.setDegrees("20:32:45.5");
  Declination dec;
  dec.setDegrees("+40:39:36.6");
  src.setTo("MWC349", ra, dec);

  return sources;
}

/**.......................................................................
 * Return fixed site information
 */
Lla getSiteInformation()
{
  Lla lla;

  lla.longitude_.setDegrees("-118:08:29.91261");
  lla.latitude_.setDegrees("37:16:49.36981");
  lla.altitude_.setMeters(2196.265);

  return lla;
}

/**.......................................................................
 * Update positions of planets
 */
void updatePositions(std::vector<Source>& sources, Lla& lla, double mjd)
{
  for(unsigned i=0; i < sources.size(); i++) {

    Source& src = sources[i];

    if(src.type_ == TYPE_PLANET) {

      double raRad, decRad, diam;

      slaRdplan(mjd, src.id_, lla.longitude_.radians(), lla.latitude_.radians(),
		&raRad, &decRad, &diam);

      src.ra_.setRadians(raRad);
      src.dec_.setRadians(decRad);
    }
  }
}

/**.......................................................................
 * Return the closest source to the requested position
 */
Source& getClosestSource(HourAngle& sourceRa, Declination& sourceDec, std::vector<Source>& sources)
{
  //  Angle minSep;
  double minSep;
  Source* minSrc=0;

  for(unsigned i=0; i < sources.size(); i++) {

    Source& src = sources[i];

    // For this purpose we don't actually care about absolute
    // separation -- we really care about minimizing the RA separation.

    //    Angle sep = Astrometry::angularSeparation(sourceRa, sourceDec, src.ra_, src.dec_);
    double sep = abs(sourceRa.hours() - src.ra_.hours());

    //    COUT("Current source is: " << src << " sep = " << sep);

    if(i==0 || sep < minSep) {
      minSep = sep;
      minSrc = &src;
    }
  }

  return *minSrc;
}

/**.......................................................................
 * Main -- return the valid LST range for observation of the requested
 * source
 */
int Program::main(void)
{
  //------------------------------------------------------------
  // Parse arguments
  //------------------------------------------------------------

  TimeVal tval;
  tval.setToCurrentTime();

  if(Program::getBoolParameter("printMjd")) {
    COUT("Mjd = " << tval.getMjd());
  }

  double mjd = Program::parameterWasSpecified("mjd") ? 
    Program::getDoubleParameter("mjd") : tval.getMjd();

  double elMinDeg = Program::getDoubleParameter("elmin");

  Source secondary;
  secondary.ra_.setHours(Program::getStringParameter("ra"));
  secondary.dec_.setDegrees(Program::getStringParameter("dec"));

  bool printPrimary = Program::getBoolParameter("printPrimary");
  bool printLstMin  = Program::getBoolParameter("printLstMin");
  bool printLstMax  = Program::getBoolParameter("printLstMax");

  //------------------------------------------------------------
  // Update positions for the requested time
  //------------------------------------------------------------

  std::vector<Source> sources = getPrimarySources();
  Lla lla = getSiteInformation();

  //  COUT("Updating positions for lla = " << lla);

  updatePositions(sources, lla, mjd);
  
  //------------------------------------------------------------
  // Now find the closest primary source for the requested mjd
  //------------------------------------------------------------

  Source& primary = getClosestSource(secondary.ra_, secondary.dec_, sources);

  if(printPrimary)
    COUT(primary);

  //------------------------------------------------------------
  // Now determine when that source is above the requested elevation
  // limit
  //------------------------------------------------------------

  Geoid geoid;

  double DeltaHaHours = 6.0;
  double deltaHaHours = 2*DeltaHaHours/100;
  HourAngle delta, hamin, hamax;

  // Sanity check that these sources are ever above the requested
  // elevation limit

  if(90 - (lla.latitude_.degrees() - primary.dec_.degrees()) < elMinDeg) {
    COUT("Primary never gets higher than " << elMinDeg);
    return 1;
  }

  if(90 - (lla.latitude_.degrees() - secondary.dec_.degrees()) < elMinDeg) {
    COUT("Secondary never gets higher than " << elMinDeg);
    return 1;
  }

  // Determine the LST boundary between which both primary and
  // secondary are above the EL limit

  //  COUT("Secondary = " << secondary);

  double lstMinHours, lstMaxHours;

  HourAngle lstMin, haPrimary, haSecondary;

  lstMin.setHours(secondary.ra_.hours() - DeltaHaHours);

  bool hasMin = false;
  bool hasMax = false;

  for(unsigned i=0; i < 100; i++) {

    // Set the current LST for this iteration

    haPrimary.setHours(  lstMin.hours() + deltaHaHours * i - primary.ra_.hours());
    haSecondary.setHours(lstMin.hours() + deltaHaHours * i - secondary.ra_.hours());

    //    COUT("HA primary   = " << haPrimary);
    //    COUT("HA secondary = " << haSecondary);

    PolarLengthVector azelPrimary   = geoid.geodeticLlaAndHaDecToAzEl(lla, haPrimary,   primary.dec_);
    PolarLengthVector azelSecondary = geoid.geodeticLlaAndHaDecToAzEl(lla, haSecondary, secondary.dec_);
    
    if(!hasMin && azelPrimary.el_.degrees() > elMinDeg && azelSecondary.el_.degrees() > elMinDeg) {

      hasMin = true;
      lstMinHours = lstMin.hours() + deltaHaHours * i;

      if(lstMinHours < 0)
	lstMinHours += 24;

      if(lstMinHours >= 24)
	lstMinHours -= 24;
      
      if(printLstMin) {
	COUT("Lst min = " << right << fixed << setw(5) << setprecision(2) << lstMinHours 
	     << " EL (sec) = " << azelSecondary.el_
	     << " EL (prim) = " << azelPrimary.el_);
      }
    }

    if(hasMin && (azelPrimary.el_.degrees() <= elMinDeg || azelSecondary.el_.degrees() <= elMinDeg)) {

      hasMax = true;
      lstMaxHours = lstMin.hours() + deltaHaHours * (i-1);

      haSecondary.setHours(lstMaxHours - secondary.ra_.hours());
      azelSecondary = geoid.geodeticLlaAndHaDecToAzEl(lla, haSecondary, secondary.dec_);

      haPrimary.setHours(lstMaxHours - primary.ra_.hours());
      azelPrimary = geoid.geodeticLlaAndHaDecToAzEl(lla, haPrimary, primary.dec_);

      if(lstMaxHours < 0)
	lstMaxHours += 24;

      if(lstMaxHours >= 24)
	lstMaxHours -= 24;
      
      if(printLstMax) {
	COUT("Lst max = " << right << fixed << setw(5) << setprecision(2) << lstMaxHours 
	     << " EL (sec) = " << azelSecondary.el_
	     << " EL (prim) = " << azelPrimary.el_);
      }

      return 0;
    }
  }

  COUTCOLOR("There is no suitable LST range when both the secondary and a primary source are above the elevation limit", "red");

  return 1;
}

