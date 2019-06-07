#include <iostream>

#include "carma/szautil/CarmaConfig.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/Program.h"

using namespace std;
using namespace sza::util;

PROGRAM_KEYWORDS = {
  { "az",       "0",         "d", USAGE "Az in degrees"},
  { "el",       "0",         "d", USAGE "El in degrees"},
  { "east",     "10",        "d", USAGE "east in meters"},
  { "north",    "0",         "d", USAGE "north in meters"},
  { "shadowedType",     "sza",       "s", USAGE "type of shadowed antenna"},
  { "shadowingType",    "sza",       "s", USAGE "type of shadowing antenna"},
  { "percmax1", "0",         "d", USAGE "percent shadowing (max 1)"},
  { "swept",    "f",         "b", USAGE "true if swept volume, false if internal"},
  { "diam",     "t",         "b", USAGE "true if percent of diameter, false if percent of area"},
  { END_OF_KEYWORDS}
};

PROGRAM_INITIALIZE_USAGE {};

int Program::main(void)
{
  COUT("Hello world!");

  Percent perc;
  perc.setPercentMax1(Program::getDoubleParameter("percmax1"));

  bool useSweptVolume = Program::getBoolParameter("swept");
  bool diam           = Program::getBoolParameter("diam");

  Angle az;
  az.setDegrees(Program::getDoubleParameter("az"));

  Angle el;
  el.setDegrees(Program::getDoubleParameter("el"));

  double east  = Program::getDoubleParameter("east");
  double north = Program::getDoubleParameter("north");

  // Configure the 'shadowingd' antenna

  CarmaConfig::PadLocation pad1(1, east, north, 0.0, CarmaConfig::SZA);
  std::string type1 = Program::getParameter("shadowingType");
  if(type1 == "sza")
    pad1.ant_ = CarmaConfig::getAntennaInfo(CarmaConfig::SZA);
  else if(type1 == "bima")
    pad1.ant_ = CarmaConfig::getAntennaInfo(CarmaConfig::BIMA);
  else
    pad1.ant_ = CarmaConfig::getAntennaInfo(CarmaConfig::OVRO);

  // Configure the 'shadowed' antenna

  CarmaConfig::PadLocation pad2(2,  0.0,   0.0, 0.0, CarmaConfig::SZA);
  std::string type2 = Program::getParameter("shadowedType");
  if(type2 == "sza")
    pad2.ant_ = CarmaConfig::getAntennaInfo(CarmaConfig::SZA);
  else if(type2 == "bima")
    pad2.ant_ = CarmaConfig::getAntennaInfo(CarmaConfig::BIMA);
  else
    pad2.ant_ = CarmaConfig::getAntennaInfo(CarmaConfig::OVRO);

  COUT("Antenna at: " << pad2 << " is shadowed by antenna at " << pad1 
       << ": " << pad2.isShadowed(az, el, pad1, useSweptVolume, perc, diam));

  return 0;
}
