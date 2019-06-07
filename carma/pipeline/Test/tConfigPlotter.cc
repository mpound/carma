#include <iostream>

#include "carma/szautil/Exception.h"
#include "carma/szautil/Program.h"

#include "carma/szapgutil/ConfigPlotter.h"
#include "carma/szapgutil/PgUtil.h"

#include "carma/monitor/MonitorSystem.h"

#include "carma/services/Pad.h"

#include "carma/pipeline/ShadowingCalculator.h"

#include "carma/util/ErrorException.h"

using namespace std;
using namespace sza::util;

PROGRAM_KEYWORDS = {
  { "percmax1",  "0.0", "d", USAGE "Percent shadowing"},

  { "diam",        "t", "b", USAGE "True if percmax1 should be interpreted as a percentage of the diameter (false = area)"},
  { "now",         "t", "b", USAGE "True if we want to plot the current shadowing status. False to use the HA/DEC from the command line"},

  { "hadec",       "t", "b", USAGE "If now=f, use the HA/DEC from the command line.  Else use the AZ/EL"},

  { "ha",        "0.0", "d", USAGE "Hour angle of the source (hours)"},
  { "dec",        "35", "d", USAGE "Declination of the source (degrees)"},

  { "az",        "0.0", "d", USAGE "AZ to check"},
  { "el",       "45.0", "d", USAGE "EL to check"},

  {"swept",      "f",  "b",  USAGE "If true, default to swept volume calculation for all antennas (only if now=f)"},

  { "xmin",     "0.0", "d", USAGE "Xmin "},
  { "xmax",     "0.0", "d", USAGE "Xmax "},
  { "ymin",     "0.0", "d", USAGE "Ymin "},
  { "ymax",     "0.0", "d", USAGE "Ymax "},

  { END_OF_KEYWORDS}
};

PROGRAM_INITIALIZE_USAGE {};

void testPlotter(carma::pipeline::ShadowingCalculator& sc);
bool containsPad(std::vector<CarmaConfig::PadLocation>& pads, CarmaConfig::PadLocation& pad);
std::vector<CarmaConfig::PadLocation> getMergedPadVector(carma::pipeline::ShadowingCalculator& sc);

int Program::main() 
{
  try {
  
    // Inhibit warnings about this damn unused global var from color_table.h
    n_std_cmap += 0;

    carma::monitor::MonitorSystem* ms = 0;
    ms = new carma::monitor::CarmaMonitorSystem();

    if(ms == 0) {
      ThrowError("Unable to instantiate Carma Monitor system");
    }

    //------------------------------------------------------------
    // Parse command-line arguments
    //------------------------------------------------------------

    sza::util::Angle az;
    az.setDegrees(Program::getDoubleParameter("az"));

    sza::util::Angle el;
    el.setDegrees(Program::getDoubleParameter("el"));

    sza::util::HourAngle ha;
    ha.setHours(Program::getDoubleParameter("ha"));

    sza::util::Declination dec;
    dec.setDegrees(Program::getDoubleParameter("dec"));

    sza::util::Percent percent;
    percent.setPercentMax1(Program::getDoubleParameter("percmax1"));

    bool diam  = Program::getBoolParameter("diam");
    bool now   = Program::getBoolParameter("now");
    bool hadec = Program::getBoolParameter("hadec");
    bool swept = Program::getBoolParameter("swept");

    //------------------------------------------------------------
    // Now instantiate a shadowing calculator and update it with the
    // specified parameters
    //------------------------------------------------------------

    carma::pipeline::ShadowingCalculator sc(ms);
  
    if(swept) {
      if(now) {
	swept = false;
      } else {
	sc.defaultToSweptVolumeForAllAntennas(swept);
      }
    }

    // Specify shadowing limit, as a percentage of the diameter or area

    if(diam) {
      sc.setInternalShadowingDiameterPercentage(percent);
      sc.setSweptVolumeShadowingDiameterPercentage(percent);
    } else {
      sc.setInternalShadowingAreaPercentage(percent);
      sc.setSweptVolumeShadowingAreaPercentage(percent);
    }
  
    ms->resetQueue();
    ms->read();
  
    // If we want the shadowing status now, just update from the monitor
    // stream

    if(now) {
      sc.update();

      // Else update configuration info, and set the requested HA/DEC
      // before recalculating shadowing flags
    
    } else {

      sc.updateConfigurationInformation();

      if(hadec) {
	sc.setHaDec(ha, dec);
      } else {
	sc.setAzEl(az, el);
      }

      sc.updateShadowFlags();
    }

    //------------------------------------------------------------
    // Now plot the array with shadowing status displayed
    //------------------------------------------------------------

    if(Program::parameterWasSpecified("xmin") && Program::parameterWasSpecified("xmax") &&
       Program::parameterWasSpecified("ymin") && Program::parameterWasSpecified("ymax")) {
      PgUtil::setXmin(Program::getDoubleParameter("xmin"));
      PgUtil::setXmax(Program::getDoubleParameter("xmax"));
      PgUtil::setYmin(Program::getDoubleParameter("ymin"));
      PgUtil::setYmax(Program::getDoubleParameter("ymax"));
      PgUtil::setUsedefs(true);
    }

    testPlotter(sc);

    if(ms) {
      delete ms;
      ms = 0;
    }
  } catch(carma::util::ErrorException& err) {
    COUT("Caught a CARMA exception: " << err.what());
    return 1;
  } catch(Exception& err) {
    COUT("Caught an SZA exception: " << err.what());
    return 1;
  } catch(...) {
    COUT("Caught an exception");
    return 1;
  }  

  return 0;
}

void testPlotter(carma::pipeline::ShadowingCalculator& sc)
{
  // Now get the configuration and plot it.  First plot occupied
  // pads to establish the plot range

  ConfigPlotter plotter;
  plotter.openDevice("/xs");

  plotter.overplot(false);
  plotter.useLabels(false);
  plotter.plotConfiguration(sc.getPadLocations());

  // Next overplot the merged pad array (including nearby unoccupied
  // pads), overplotting so that we don't change the range

  std::vector<CarmaConfig::PadLocation> mergedPads = getMergedPadVector(sc);

  plotter.useLabels(true);
  plotter.overplot(true);
  plotter.plotConfiguration(mergedPads);

  // Now replot with shadowing status indicated

  std::vector<bool> internalShadowFlags    = sc.getInternalShadowing();
  std::vector<bool> sweptVolumeShadowFlags = sc.getSweptVolumeShadowing();

  plotter.useLabels(false);
  plotter.overplot(true);
  plotter.useFillFlags(true);

  plotter.setFillFlags(sweptVolumeShadowFlags);
  plotter.setFillStyle(FILL_HATCHED);
  plotter.plotConfiguration(sc.getPadLocations());

  plotter.setAz(sc.azimuths_);
  plotter.setEl(sc.elevations_);
  plotter.setFillFlags(internalShadowFlags);
  plotter.setFillStyle(FILL_SOLID);
  plotter.plotConfiguration(sc.getPadLocations());

}

std::vector<CarmaConfig::PadLocation> getMergedPadVector(carma::pipeline::ShadowingCalculator& sc)
{
  std::vector<CarmaConfig::PadLocation> mergedPads;
  std::vector<CarmaConfig::PadLocation> occupiedPads = sc.getPadLocations();

  CarmaConfig config;
  std::vector<CarmaConfig::PadLocation> allPads = config.getAllPads();
  for(unsigned iPad=0;iPad < allPads.size(); iPad++) {
    allPads[iPad].ant_.antFlag_ = CarmaConfig::UNKNOWN;
    allPads[iPad].ant_.diameter_.setMeters(sqrt(2.0) * 3.0);
  }
  
  // Now merge the two arrays.  First add any pads that are not
  // currently occupied

  std::ostringstream os;
  for(unsigned iPad=0; iPad < allPads.size(); iPad++) {
    if(!containsPad(occupiedPads, allPads[iPad])) {

#if 1
      CarmaConfig::PadLocation& szaPad = allPads[iPad];

      // Get the actual ENU from the CARMA observatory catalog file

      os.str("");
      os << "pad#" << allPads[iPad].padNumber_;
      carma::services::Pad carmaPad("CARMA", os.str());
      std::vector<carma::services::Length*> enu = carmaPad.getEnu();
      
      szaPad.east_.setMeters(enu[0]->meters());
      szaPad.north_.setMeters(enu[1]->meters());
      szaPad.up_.setMeters(enu[2]->meters());
#endif

      mergedPads.push_back(allPads[iPad]);
    }
  }

  // Now add any occupied pads

  for(unsigned iPad=0; iPad < occupiedPads.size(); iPad++) {
    mergedPads.push_back(occupiedPads[iPad]);
  }
  
  return mergedPads;
}

bool containsPad(std::vector<CarmaConfig::PadLocation>& pads, CarmaConfig::PadLocation& pad)
{
  for(unsigned iPad=0; iPad < pads.size(); iPad++) {
    if(pad.padNumber_ == pads[iPad].padNumber_) {
      return true;
    }
  }
  
  return false;
}
