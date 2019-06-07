#include <iostream>

#include "carma/szautil/Exception.h"
#include "carma/szautil/Program.h"

#include "carma/monitor/MonitorSystem.h"

#include "carma/pipeline/ShadowingCalculator.h"

#include "carma/util/ErrorException.h"

using namespace std;
using namespace sza::util;

PROGRAM_KEYWORDS = {
  { "percmax1", "0.0", "d", USAGE "Percent shadowing"},
  { "diam",       "t", "b", USAGE "True if percmax1 should be interpreted as a percentage of the diameter (false = area)"},
  { "ha",        "0.0", "d", USAGE "Hour angle of the source (hours)"},
  { "dec",        "35", "d", USAGE "Declination of the source (degrees)"},
  { END_OF_KEYWORDS}
};

PROGRAM_INITIALIZE_USAGE {};

void testIter(carma::monitor::MonitorSystem* ms, Percent& percent, bool diam);
void testHaDec(carma::monitor::MonitorSystem* ms, Percent& percent, bool diam,
	       HourAngle& ha, Declination& dec);

int Program::main() 
{
  carma::monitor::MonitorSystem* ms = 0;
  ms = new carma::monitor::CarmaMonitorSystem();

  if(ms == 0) {
    ThrowError("Unable to instantiate Carma Monitor system");
  }

  sza::util::HourAngle ha;
  ha.setHours(Program::getDoubleParameter("ha"));

  sza::util::Declination dec;
  dec.setDegrees(Program::getDoubleParameter("dec"));

  sza::util::Percent percent;
  percent.setPercentMax1(Program::getDoubleParameter("percmax1"));

  bool diam = Program::getBoolParameter("diam");

  //  testIter(ms, percent, diam);

  testHaDec(ms, percent, diam, ha, dec);

  if(ms) {
    delete ms;
    ms = 0;
  }

  return 0;
}

void testHaDec(carma::monitor::MonitorSystem* ms, Percent& percent, bool diam,
	       HourAngle& ha, Declination& dec)
{
  try {

    // Instantiate shadowing calculator with the monitor system
    // pointer we just created

    carma::pipeline::ShadowingCalculator sc(ms);

    // Specify shadowing limit, as a percentage of the diameter or area

    if(diam) {
      sc.setInternalShadowingDiameterPercentage(percent);
      sc.setSweptVolumeShadowingDiameterPercentage(percent);
    } else {
      sc.setInternalShadowingAreaPercentage(percent);
      sc.setSweptVolumeShadowingAreaPercentage(percent);
    }

    // Iterate 10 times, calculating shadowing flags

    ms->resetQueue();
    ms->read();

    sc.updateConfigurationInformation();
    sc.setHaDec(ha, dec);
    sc.updateShadowFlags();

    std::vector<bool> internallyShadowed = sc.getInternalShadowing();
    std::vector<bool> externallyShadowed = sc.getSweptVolumeShadowing();

    for(unsigned iAnt=0; iAnt < internallyShadowed.size(); iAnt++) {
      COUT("Ant " << iAnt+1 << " is internally shadowed: " << internallyShadowed[iAnt]);
    }
    
    for(unsigned iAnt=0; iAnt < externallyShadowed.size(); iAnt++) {
      COUT("Ant " << iAnt+1 << " is externally shadowed: " << externallyShadowed[iAnt]);
    }

  } catch(carma::util::ErrorException& err) {
    COUT("Caught a CARMA exception: " << err.what());
  } catch(Exception& err) {
    COUT("Caught an SZA exception: " << err.what());
  } catch(...) {
    COUT("Caught an exception");
  }  
}

void testIter(carma::monitor::MonitorSystem* ms, Percent& percent, bool diam)
{
  try {

    // Instantiate shadowing calculator with the monitor system
    // pointer we just created

    carma::pipeline::ShadowingCalculator sc(ms);

    // Specify shadowing limit, as a percentage of the diameter or area

    if(diam) {
      sc.setInternalShadowingDiameterPercentage(percent);
      sc.setSweptVolumeShadowingDiameterPercentage(percent);
    } else {
      sc.setInternalShadowingAreaPercentage(percent);
      sc.setSweptVolumeShadowingAreaPercentage(percent);
    }

    // Iterate 10 times, calculating shadowing flags

    ms->resetQueue();

    for(unsigned i=0; i < 10; i++) {

      ms->read();

      sc.update();

      std::vector<bool> internallyShadowed = sc.getInternalShadowing();
      std::vector<bool> externallyShadowed = sc.getSweptVolumeShadowing();

      for(unsigned iAnt=0; iAnt < internallyShadowed.size(); iAnt++) {
	COUT("Ant " << iAnt+1 << " is internally shadowed: " << internallyShadowed[iAnt]);
      }

      for(unsigned iAnt=0; iAnt < externallyShadowed.size(); iAnt++) {
	COUT("Ant " << iAnt+1 << " is externally shadowed: " << externallyShadowed[iAnt]);
      }

      sleep(5);
    }

  } catch(carma::util::ErrorException& err) {
    COUT("Caught a CARMA exception: " << err.what());
  } catch(Exception& err) {
    COUT("Caught an SZA exception: " << err.what());
  } catch(...) {
    COUT("Caught an exception");
  }
}
