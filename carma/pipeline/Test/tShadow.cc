#include <iostream>

#include "carma/szautil/Exception.h"
#include "carma/szautil/Program.h"

#include "carma/monitor/MonitorSystem.h"

#include "carma/pipeline/ShadowingCalculator.h"

#include "carma/util/ErrorException.h"

using namespace std;
using namespace sza::util;

PROGRAM_KEYWORDS = {
  { "run",      "f",  "b", USAGE "True to calculate the full range"},
  { "az",       "0.0", "d", USAGE "az to calculate for (if run=f)"},
  { "el",       "0.0", "d", USAGE "el to calculate for (if run=f)"},
  { "azmin",    "0", "d", USAGE "azmin (if run=f)"},
  { "azmax",    "360", "d", USAGE "azmax (if run=f)"},
  { "elmin",    "0", "d", USAGE "elmin (if run=f)"},
  { "elmax",    "90", "d", USAGE "elmax (if run=f)"},
  { "n",      "100",   "i", USAGE "Which ant"},
  { "ant",      "1",   "i", USAGE "Which ant"},
  { "percmax1", "0.0", "d", USAGE "Percent shadowing"},
  { "diam",       "t", "b", USAGE "True if percmax1 should be interpreted as a percentage of the diameter (false = area)"},
  { END_OF_KEYWORDS}
};

PROGRAM_INITIALIZE_USAGE {};

int Program::main() 
{
  Angle az(Angle::Degrees(), Program::getDoubleParameter("az"));
  Angle el(Angle::Degrees(), Program::getDoubleParameter("el"));
  unsigned antNo = Program::getIntParameter("ant");

  CarmaConfig::PadLocation pad1;
  CarmaConfig::PadLocation pad2;

  pad1.east_.setMeters(-473.64069);
  pad1.north_.setMeters(-148.21182);
  pad1.up_.setMeters(32.06175);
  pad1.ant_ = CarmaConfig::getAntennaInfo(CarmaConfig::OVRO);

#if 0
  pad2.east_.setMeters(-497.82925);
  pad2.north_.setMeters(-151.54646);
  pad2.up_.setMeters(25.83712);
#else
  pad2.east_.setMeters(-497.82925);
  pad2.north_.setMeters(-150.04646);
  pad2.up_.setMeters(29.83712);
#endif

  pad2.ant_ = CarmaConfig::getAntennaInfo(CarmaConfig::SZA);

  //  COUT("Pad 1 = " << pad1);
  //  COUT("Pad 2 = " << pad2);

  Percent perc;
  perc.setPercentMax1(0.0);

  bool run = Program::getBoolParameter("run");

  if(run) {

    double azmin = Program::getDoubleParameter("azmin");
    double azmax = Program::getDoubleParameter("azmax");

    double elmin = Program::getDoubleParameter("elmin");
    double elmax = Program::getDoubleParameter("elmax");

    unsigned n = Program::getIntParameter("n");
    
    double dAz = (azmax - azmin)/(n-1);
    double dEl = (elmax - elmin)/(n-1);
    
    for(unsigned iAz=0; iAz < n; iAz++) {
      az.setDegrees(azmin + iAz*dAz);
      for(unsigned iEl=0; iEl < n; iEl++) {
	el.setDegrees(elmin + iEl*dEl);
	
	if(antNo == 1) {
	  COUT(az.degrees() << " " << el.degrees() << " " << pad1.isShadowed(az, el, pad2, true, perc, true));
	} else {
	  COUT(az.degrees() << " " << el.degrees() << " " << pad2.isShadowed(az, el, pad1, true, perc, true));
	}
	
      }
    }
  } else {
    if(antNo == 1) {
      COUT(az.degrees() << " " << el.degrees() << " " << pad1.isShadowed(az, el, pad2, true, perc, true, true));
    } else {
      COUT(az.degrees() << " " << el.degrees() << " " << pad2.isShadowed(az, el, pad1, true, perc, true, true));
    }
  }

  return 0;
}
