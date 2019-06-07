/*
 * @file
 * 
 * Gets data from the SZA calterts and displays
 *
 * @author Erik Leitch
 */
#include <sstream>
#include <vector>

#include "carma/ui/rtd/common/AntennaMapper.h"

#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorCell.h"
#include "carma/ui/rtd/common/MonitorCellScaled.h"
#include "carma/ui/rtd/common/MonitorCellMapped.h"

#include "carma/ui/rtd/windows/SzaRtdUtils.h"

#include "carma/monitor/AntennaCommon.h"
#include "carma/monitor/AstroSubsystemExt.h"
#include "carma/monitor/BimaSubsystem.h"
#include "carma/monitor/MonitorPointIterator.h"
#include "carma/monitor/OvroSubsystem.h"
#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/monitor/SzaSubsystem.h"

#include "carma/services/Global.h"

#include "carma/util/Program.h"
#include "carma/util/programLogging.h"

using namespace std;
using namespace carma::monitor;
using namespace carma::services;
using namespace carma::util;
using namespace carma::ui::rtd;
using namespace sza::util;

std::string makeHelp()
{
  ostringstream ost;
  ost << "       3.5M ANTENNA CALTERT HELP\n\n" 
      << "Status of the 3.5m antenna caltert subsystem. "
      << "Fill in more info here... ";
  return ost.str();
}

MonitorCellScaled *castScaled(MonitorCellPtr cell)
{
    return dynamic_cast<MonitorCellScaled *>(cell.get());
}

MonitorCellMapped *castMapped(MonitorCellPtr cell)
{
    return dynamic_cast<MonitorCellMapped *>(cell.get());
}

//------------------------------------------------------------
// Define a class for managing Coherence displays
//------------------------------------------------------------

namespace carma {
  namespace ui {
    namespace rtd {

      class CoherenceDisplay : public MonitorDisplay {
      public:

	AntennaMapper* antMapper_;

	CoherenceDisplay(const std::string& name) :
	  MonitorDisplay(name) 
	{
	  antMapper_ = new AntennaMapper(cms());
	}

	~CoherenceDisplay() {
	  if(antMapper_) {
	    delete antMapper_;
	    antMapper_ = 0;
	  }
	}

	void internalUpdate() {
	  antMapper_->rebuildSubarrayMaps();
	}
      };

      class MpManager {
      public:
  
	void initialize(CoherenceDisplay& display);

	std::vector<MonitorCellScaled::ScaledMp> ifSwitchStateVec_;
	std::vector<MonitorCellScaled::ScaledMp> freqVec3mm_;
	std::vector<MonitorCellScaled::ScaledMp> freqVec1mm_;
	std::vector<MonitorCellScaled::ScaledMp> yigFreqVec_;
	std::vector<MonitorCellScaled::ScaledMp> azVec_;
	std::vector<MonitorCellScaled::ScaledMp> elVec_;
	std::vector<MonitorCellScaled::ScaledMp> dewarTempVec_;
      };

    };
  };
};


RtTablePtr getSzaTable(CoherenceDisplay& display, MpManager& mpm);
RtTablePtr getOvroTable(CoherenceDisplay& display, MpManager& mpm);
RtTablePtr getBimaTable(CoherenceDisplay& display, MpManager& mpm);

/**.......................................................................
 * Create an RTD displaying coherence monitor points for all antenna types
 */
int Program::main() 
{
  CoherenceDisplay display("Outlier test");
  display.setLayout(EOL_LEFT_JUSTIFIED_LAYOUT);
  display.setSpecificHelp("Outlier test help", makeHelp());

  RtFolderPtr folder(new RtFolder("Folder"));
  folder->setLayout(EOL_LEFT_JUSTIFIED_LAYOUT);
  display.add(folder);

  MpManager mpm;
  mpm.initialize(display);

  // Add a table for the OVRO antennas

  folder->add(getOvroTable(display, mpm));

  // Add a table for the BIMA antennas

  folder->add(getBimaTable(display, mpm));

  // Add a table for the SZA antennas

  folder->add(getSzaTable(display, mpm));

  // Loop forever serving data to the client
  
  try {
    while (display.serveData()) {}
  } catch(carma::util::ErrorException& err) {
    COUT("Caught an error: " << err.what());
  } catch(Exception& err) {
    COUT("Caught an exception: " << err.what());
  } catch(...) {
    COUT("Caught an unknown exception");
  }

  COUT("Exiting for no explained cause");

  return 0;
}

/**.......................................................................
 * Initialize this monitor-point manager from our display object
 */
void MpManager::initialize(CoherenceDisplay& display)
{
  //------------------------------------------------------------
  // Construct a vector of dewar temps
  //------------------------------------------------------------

  for(unsigned iAnt=0; iAnt < AntennaMapper::nOvro_; iAnt++) {
    MonitorPoint& mp = display.cms().ovro(iAnt).antennaCommon().receivers().dewarTemp();
    mp.setPrecision(2);
    dewarTempVec_.push_back(MonitorCellScaled::ScaledMp(&mp));
  }

  for(unsigned iAnt=0; iAnt < AntennaMapper::nBima_; iAnt++) {
    MonitorPoint& mp = display.cms().bima(iAnt).antennaCommon().receivers().dewarTemp();
    mp.setPrecision(2);
    dewarTempVec_.push_back(MonitorCellScaled::ScaledMp(&mp));
  }

  for(unsigned iAnt=0; iAnt < AntennaMapper::nSza_; iAnt++) {
    MonitorPoint& mp = display.cms().sza(iAnt).antennaCommon().receivers().dewarTemp();
    mp.setPrecision(2);
    dewarTempVec_.push_back(MonitorCellScaled::ScaledMp(&mp));
  }

  //------------------------------------------------------------
  // Construct a vector of all IF switch states.  This will
  // be used to check if any antenna is an outlier from other antennas
  // in the same subarray
  //------------------------------------------------------------

  for(unsigned iAnt=0; iAnt < AntennaMapper::nOvro_; iAnt++) {
    MonitorPoint& mp = display.cms().ovro(iAnt).antennaIfContainer(0).antennaIF().ifSwitchStat();
    ifSwitchStateVec_.push_back(MonitorCellScaled::ScaledMp(&mp));
  }

  for(unsigned iAnt=0; iAnt < AntennaMapper::nBima_; iAnt++) {
    MonitorPoint& mp = display.cms().bima(iAnt).antennaIfContainer(0).antennaIF().ifSwitchStat();
    ifSwitchStateVec_.push_back(MonitorCellScaled::ScaledMp(&mp));
  }

  for(unsigned iAnt=0; iAnt < AntennaMapper::nSza_; iAnt++) {
    MonitorPoint& mp = display.cms().sza(0).ifmod().ifSwitchState();
    ifSwitchStateVec_.push_back(MonitorCellScaled::ScaledMp(&mp));
  }

  //------------------------------------------------------------
  // Construct a vector of all 3mm frequencies.  This will
  // be used to check if any antenna is an outlier from other antennas
  // in the same subarray
  //------------------------------------------------------------

  for(unsigned iAnt=0; iAnt < AntennaMapper::nOvro_; iAnt++) {
    MonitorPoint& mp1 = display.cms().ovro(iAnt).gunn3mm().gunnPll().gunnFreq();
    MonitorPoint& mp2 = display.cms().ovro(iAnt).gunn3mm().gunnPll().multiplier();
    freqVec3mm_.push_back(MonitorCellScaled::ScaledMp(&mp1, OPER_MULT, &mp2));
  }

  for(unsigned iAnt=0; iAnt < AntennaMapper::nBima_; iAnt++) {
    MonitorPoint& mp1 = display.cms().bima(iAnt).antennaCommon().lO().yigFreq();
    MonitorPoint& mp2 = display.cms().control().antenna(iAnt+6).harmonic();
    freqVec3mm_.push_back(MonitorCellScaled::ScaledMp(&mp1, OPER_MULT, &mp2));
  }

  for(unsigned iAnt=0; iAnt < AntennaMapper::nSza_; iAnt++) {
    MonitorPoint& mp = display.cms().sza(iAnt).bias().gunnFrequency();
    freqVec3mm_.push_back(MonitorCellScaled::ScaledMp(&mp, 0.01, 17.5));
  }

  //------------------------------------------------------------
  // Construct a vector of all 1mm frequencies.  This will
  // be used to check if any antenna is an outlier from other antennas
  // in the same subarray
  //------------------------------------------------------------

  for(unsigned iAnt=0; iAnt < AntennaMapper::nOvro_; iAnt++) {
    MonitorPoint& mp1 = display.cms().ovro(iAnt).gunn1mm().gunnPll().gunnFreq();
    MonitorPoint& mp2 = display.cms().ovro(iAnt).gunn1mm().gunnPll().multiplier();
    freqVec1mm_.push_back(MonitorCellScaled::ScaledMp(&mp1, OPER_MULT, &mp2));
  }

  for(unsigned iAnt=0; iAnt < AntennaMapper::nBima_; iAnt++) {
    MonitorPoint& mp1 = display.cms().bima(iAnt).antennaCommon().lO().yigFreq();
    MonitorPoint& mp2 = display.cms().control().antenna(iAnt+6).harmonic();
    freqVec1mm_.push_back(MonitorCellScaled::ScaledMp(&mp1, OPER_MULT, &mp2, 3.0));
  }

  // SZA ants don't have 1mm receivers, but I require the comparison
  // vectors to be 23 elements long, so fill in with bogus frequencies
  // here.  Doesn't matter, since these will bever be used in the same
  // subarray at 1mm.

  for(unsigned iAnt=0; iAnt < AntennaMapper::nSza_; iAnt++) {
    MonitorPoint& mp = display.cms().sza(iAnt).bias().gunnFrequency();
    freqVec1mm_.push_back(MonitorCellScaled::ScaledMp(&mp, 0.01, 17.5));
  }

  //------------------------------------------------------------
  // Construct a vector of all YIG frequencies.  This will be used to
  // check if any antenna is an outlier from other antennas in the
  // same subarray
  //------------------------------------------------------------

  for(unsigned iAnt=0; iAnt < AntennaMapper::nOvro_; iAnt++) {
    MonitorPoint& mp = display.cms().ovro(iAnt).antennaCommon().lO().yigFreq();
    yigFreqVec_.push_back(MonitorCellScaled::ScaledMp(&mp));
  }

  for(unsigned iAnt=0; iAnt < AntennaMapper::nBima_; iAnt++) {
    MonitorPoint& mp = display.cms().bima(iAnt).antennaCommon().lO().yigFreq();
    yigFreqVec_.push_back(MonitorCellScaled::ScaledMp(&mp));
  }

  for(unsigned iAnt=0; iAnt < AntennaMapper::nSza_; iAnt++) {
    MonitorPoint& mp = display.cms().sza(iAnt).antennaCommon().lO().yigFreq();
    yigFreqVec_.push_back(MonitorCellScaled::ScaledMp(&mp));
  }

  //------------------------------------------------------------
  // Construct a vector of all azimuths.  This will be used to
  // check if any antenna is an outlier from other antennas in the
  // same subarray
  //------------------------------------------------------------

  for(unsigned iAnt=0; iAnt < AntennaMapper::nOvro_; iAnt++) {
    MonitorPoint& mp = display.cms().ovro(iAnt).antennaCommon().drive().track().actualAzimuth();
    mp.setPrecision(1);
    azVec_.push_back(MonitorCellScaled::ScaledMp(&mp));
  }

  for(unsigned iAnt=0; iAnt < AntennaMapper::nBima_; iAnt++) {
    MonitorPoint& mp = display.cms().bima(iAnt).antennaCommon().drive().track().actualAzimuth();
    mp.setPrecision(1);
    azVec_.push_back(MonitorCellScaled::ScaledMp(&mp));
  }

  for(unsigned iAnt=0; iAnt < AntennaMapper::nSza_; iAnt++) {
    MonitorPoint& mp = display.cms().sza(iAnt).antennaCommon().drive().track().actualAzimuth();
    mp.setPrecision(1);
    azVec_.push_back(MonitorCellScaled::ScaledMp(&mp));
  }

  for(unsigned iAnt=0; iAnt < AntennaMapper::nOvro_; iAnt++) {
    MonitorPoint& mp = display.cms().ovro(iAnt).antennaCommon().drive().track().actualElevation();
    mp.setPrecision(1);
    elVec_.push_back(MonitorCellScaled::ScaledMp(&mp));
  }

  for(unsigned iAnt=0; iAnt < AntennaMapper::nBima_; iAnt++) {
    MonitorPoint& mp = display.cms().bima(iAnt).antennaCommon().drive().track().actualElevation();
    mp.setPrecision(1);
    elVec_.push_back(MonitorCellScaled::ScaledMp(&mp));
  }

  for(unsigned iAnt=0; iAnt < AntennaMapper::nSza_; iAnt++) {
    MonitorPoint& mp = display.cms().sza(iAnt).antennaCommon().drive().track().actualElevation();
    mp.setPrecision(1);
    elVec_.push_back(MonitorCellScaled::ScaledMp(&mp));
  }
}  

/**.......................................................................
 * Get the SZA Table
 */
RtTablePtr getSzaTable(CoherenceDisplay& display, MpManager& mpm)
{
  RtTablePtr table(new RtTable("Table"));
  table->setLayout(EOL_LEFT_JUSTIFIED_LAYOUT);

  unsigned nAnt = 8;

  // Now iterate over SZA antennas, creating a column for each one

  std::ostringstream os;
  for(unsigned iAnt=0; iAnt < nAnt; iAnt++) {
    os.str("");
    os << "C" << (iAnt + 16);
    table->addCol(RtColumn::makeColumn(os.str()));
  }

  //------------------------------------------------------------
  // Add labels for all rows
  //------------------------------------------------------------

  table->addRow(RtRow::makeRow("Coherence"));
  table->addRow(RtRow::makeRow("Drive State"));
  table->addRow(RtRow::makeRow("Az (deg)"));
  table->addRow(RtRow::makeRow("El (deg)"));
  table->addRow(RtRow::makeRow("Az Err (arcsec)"));
  table->addRow(RtRow::makeRow("El Err (arcsec)"));
  table->addRow(RtRow::makeRow("IF Switch State Correct for 1cm?"));
  table->addRow(RtRow::makeRow("IF Switch State Correct for 3mm?"));
  table->addRow(RtRow::makeRow("Rx State"));
  table->addRow(RtRow::makeRow("Dewar Temp"));
  table->addRow(RtRow::makeRow("Yig Lock State"));
  table->addRow(RtRow::makeRow("Yig Frequency (GHz)"));
  table->addRow(RtRow::makeRow("Bias Ref Lock State"));
  table->addRow(RtRow::makeRow("Bias HW Lock State"));
  table->addRow(RtRow::makeRow("3mm LO Frequency (GHz)"));
  table->addRow(RtRow::makeRow("10 MHz Lock"));
  table->addRow(RtRow::makeRow("Tertiary State Correct for 1cm?"));
  table->addRow(RtRow::makeRow("Tertiary State Correct for 3mm?"));
  table->addRow(RtRow::makeRow("Tertiary State Correct for 1mm?"));
  table->addRow(RtRow::makeRow("Calibrator State"));
  table->addRow(RtRow::makeRow("Mount Az Offset (% of beam FWHM)"));
  table->addRow(RtRow::makeRow("Mount El Offset (% of beam FWHM)"));
  table->addRow(RtRow::makeRow("Shadowed"));

  //------------------------------------------------------------
  // Now add columns of monitor points
  //------------------------------------------------------------

  unsigned colWidth = 12;
  std::vector<MonitorCellPtr> col;
  std::map<std::string, CellColor> colorMap;
  std::map<std::string, std::string> stateMap;

  MonitorCellPtr scell;
  MonitorCellPtr mcell;

  sza::util::DataType cohLimit = (float)0.2;
  sza::util::MonitorCondition lowCoherence(DataTypeTruthFn::greaterThan, cohLimit);

  sza::util::DataType freq1cmLimitGHz = (double)50.0;
  sza::util::DataType freq3mmLimitGHz = (double)150.0;
  sza::util::MonitorCondition not1cm(DataTypeTruthFn::greaterThan, freq1cmLimitGHz);
  sza::util::MonitorCondition is1cm(DataTypeTruthFn::lessThan, freq1cmLimitGHz);
  sza::util::MonitorCondition is3mm(DataTypeTruthFn::greaterThanAndLessThan, freq1cmLimitGHz, freq3mmLimitGHz);
  sza::util::MonitorCondition is1mm(DataTypeTruthFn::greaterThan, freq3mmLimitGHz);

  for(unsigned iAnt=0; iAnt < 8; iAnt++) {

    // Get the descriptor for this antenna

    AntennaMapper::Antenna* ant = display.antMapper_->getAntenna(AntennaMapper::ANT_SZA, iAnt);
    Frequency freqNorm(Frequency::GigaHz(), 30.0);
    Angle fwhm = ant->beamFwhm(freqNorm);

    //------------------------------------------------------------
    // Coherence
    //------------------------------------------------------------
    const double mul    = 1.0;
    const double offset = 0.0;
    int precision       = 2;
    scell = MonitorCellScaled::makeCell(colWidth, 
                display.cms().astro().antenna(iAnt+15).maxCoherence(),
                mul, offset, precision);
    castScaled(scell)->errorOnAbsDeviationFromVal(1.0, 0.8);
    col.push_back(scell);

    //------------------------------------------------------------
    // Drive state
    //------------------------------------------------------------

    stateMap.clear();
    stateMap["TRACK"]   = "TRACK";
    stateMap["CLOSE"]   = "CLOSE";
    stateMap["SLEW"]    = "SLEW";
    stateMap["STOW"]    = "STOW";
    stateMap["SNOW"]    = "SNOW";
    stateMap["STOP"]    = "STOP";
    stateMap["DISABLE"] = "DISABLE";
    stateMap["HWLIMIT"] = "HWLIMIT";
    stateMap["ERROR"]   = "ERROR";
    stateMap["LOCAL"]   = "LOCAL";
    stateMap["SERVICE"] = "SERVICE";
    stateMap["TEST"]    = "TEST";
    stateMap["FATAL"]   = "FATAL";

    colorMap.clear();
    colorMap["TRACK"]   = WHITE_CELL_COLOR;
    colorMap["CLOSE"]   = RED_CELL_COLOR;
    colorMap["SLEW"]    = RED_CELL_COLOR;
    colorMap["STOW"]    = RED_CELL_COLOR;
    colorMap["SNOW"]    = RED_CELL_COLOR;
    colorMap["STOP"]    = RED_CELL_COLOR;
    colorMap["DISABLE"] = RED_CELL_COLOR;
    colorMap["HWLIMIT"] = RED_CELL_COLOR;
    colorMap["ERROR"]   = RED_CELL_COLOR;
    colorMap["LOCAL"]   = RED_CELL_COLOR;
    colorMap["SERVICE"] = RED_CELL_COLOR;
    colorMap["TEST"]    = RED_CELL_COLOR;
    colorMap["FATAL"]   = RED_CELL_COLOR;

    mcell = MonitorCellMapped::makeCell(colWidth, 
        display.cms().sza(iAnt).antennaCommon().drive().state(), stateMap);
    castMapped(mcell)->setAntenna(ant);
    castMapped(mcell)->setColorMap(colorMap);
    col.push_back(mcell);

    //------------------------------------------------------------
    // Position
    //------------------------------------------------------------

    // Set the allowable delta to be the FWHM at the current
    // frequency.  For AZ, this is worst-case scenario, since we can
    // tolerate much larger deltas at higher elevation

    MonitorCellScaled::Delta positionDelta(fwhm.degrees(), OPER_DIV, freqNorm);

    scell = MonitorCellScaled::makeCell(colWidth, 
        display.cms().sza(iAnt).antennaCommon().drive().track().actualAzimuth(),
        mul, offset, precision);
    castScaled(scell)->setAntenna(ant);
    castScaled(scell)->colorIf(display.cms().astro().antenna(iAnt+15).maxCoherence(), lowCoherence);
    castScaled(scell)->errorOnAbsDeviationFromMean(mpm.azVec_, positionDelta, GROUP_SUBARRAY);

    col.push_back(scell);

    scell = MonitorCellScaled::makeCell(colWidth, 
        display.cms().sza(iAnt).antennaCommon().drive().track().actualElevation(),
        mul, offset, precision);
    castScaled(scell)->setAntenna(ant);
    castScaled(scell)->colorIf(display.cms().astro().antenna(iAnt+15).maxCoherence(), lowCoherence);
    castScaled(scell)->errorOnAbsDeviationFromMean(mpm.elVec_, positionDelta, GROUP_SUBARRAY);

    col.push_back(scell);

    //------------------------------------------------------------
    // Tracking error
    //------------------------------------------------------------

    // Set tracking errors to be 1/4 of the FWHM at any frequency

    MonitorCellScaled::Delta trackDelta(fwhm.arcsec()/4, OPER_DIV, freqNorm);

    scell = MonitorCellScaled::makeCell(colWidth, 
        display.cms().sza(iAnt).antennaCommon().drive().track().errorAzimuthSky(),
        mul, offset, precision);
    castScaled(scell)->setAntenna(ant);
    castScaled(scell)->colorIf(display.cms().astro().antenna(iAnt+15).maxCoherence(), lowCoherence);
    castScaled(scell)->errorOnAbsDeviationFromVal(0.0, trackDelta);

    col.push_back(scell);

    scell = MonitorCellScaled::makeCell(colWidth, 
        display.cms().sza(iAnt).antennaCommon().drive().track().errorElevation(),
        mul, offset, precision);
    castScaled(scell)->setAntenna(ant);
    castScaled(scell)->colorIf(display.cms().astro().antenna(iAnt+15).maxCoherence(), lowCoherence);
    castScaled(scell)->errorOnAbsDeviationFromVal(0.0, trackDelta);

    col.push_back(scell);

    //------------------------------------------------------------
    // IF switch states
    //------------------------------------------------------------

    stateMap.clear();
    stateMap["0"] = "FALSE";
    stateMap["1"] = "TRUE";
    stateMap["2"] = "FALSE";
    stateMap["3"] = "FALSE";

    colorMap.clear();
    colorMap["TRUE"]  = WHITE_CELL_COLOR;
    colorMap["FALSE"] = RED_CELL_COLOR;

    mcell = MonitorCellMapped::makeCell(colWidth, display.cms().sza(iAnt).ifmod().ifSwitchState(), stateMap);
    castMapped(mcell)->setColorMap(colorMap);
    castMapped(mcell)->setAntenna(ant);
    castMapped(mcell)->colorIfFrequencyInGHz(is1cm);

    col.push_back(mcell);

    stateMap["2"]   = "TRUE";
    stateMap["1"]   = "FALSE";

    mcell = MonitorCellMapped::makeCell(colWidth, display.cms().sza(iAnt).ifmod().ifSwitchState(), stateMap);
    castMapped(mcell)->setColorMap(colorMap);
    castMapped(mcell)->setAntenna(ant);
    castMapped(mcell)->colorIfFrequencyInGHz(is3mm);

    col.push_back(mcell);

    //------------------------------------------------------------
    // Rx state
    //------------------------------------------------------------
    
    stateMap.clear();
    stateMap["GOOD"]         = "GOOD";
    stateMap["BAD"]          = "BAD";
    stateMap["TUNE"]         = "TUNE";
    stateMap["YIG_BAD"]      = "YIG_BAD";
    stateMap["GUNN_BAD"]     = "GUNN_BAD";
    stateMap["IFSEL_BAD"]    = "IFSEL_BAD";
    stateMap["RXSEL_BAD"]    = "RXSEL_BAD";
    stateMap["RX_BAD"]       = "RX_BAD";
    stateMap["TERTIARY_BAD"] = "TERTIARY_BAD";
    stateMap["OPTIMIZE"]     = "OPTIMIZE";
    stateMap["SEARCH"]       = "SEARCH";

    colorMap.clear();
    colorMap["GOOD"]         = WHITE_CELL_COLOR;
    colorMap["BAD"]          = RED_CELL_COLOR;
    colorMap["TUNE"]         = RED_CELL_COLOR;
    colorMap["YIG_BAD"]      = RED_CELL_COLOR;
    colorMap["GUNN_BAD"]     = RED_CELL_COLOR;
    colorMap["IFSEL_BAD"]    = RED_CELL_COLOR;
    colorMap["RXSEL_BAD"]    = RED_CELL_COLOR;
    colorMap["RX_BAD"]       = RED_CELL_COLOR;
    colorMap["TERTIARY_BAD"] = RED_CELL_COLOR;
    colorMap["OPTIMIZE"]     = RED_CELL_COLOR;
    colorMap["SEARCH"]       = RED_CELL_COLOR;
    
    mcell = MonitorCellMapped::makeCell(colWidth, display.cms().sza(iAnt).antennaCommon().receivers().rxState(), stateMap);
    castMapped(mcell)->setColorMap(colorMap);
    col.push_back(mcell);

    //------------------------------------------------------------
    // Set up the dewar temperature to highlight if more than 5K from
    // the mean for this antenna type
    //------------------------------------------------------------
    precision=1;
    scell = MonitorCellScaled::makeCell(colWidth, 
        display.cms().sza(iAnt).antennaCommon().receivers().dewarTemp(),
        mul, offset, precision);
    castScaled(scell)->setAntenna(ant);
    castScaled(scell)->errorOnAbsDeviationFromMean(mpm.dewarTempVec_, 5, GROUP_ANTTYPE);

    col.push_back(scell);

    //------------------------------------------------------------
    // Yig lock status
    //------------------------------------------------------------
    
    mcell = MonitorCellMapped::makeCell(colWidth, display.cms().sza(iAnt).yig().lockState(),
							      "UNLOCKED", "SEARCHING", "REFINING", "LOCKED", "RELOCK");
    colorMap.clear();
    colorMap["UNLOCKED"]  = RED_CELL_COLOR;
    colorMap["SEARCHING"] = RED_CELL_COLOR;
    colorMap["REFINING"]  = RED_CELL_COLOR;
    colorMap["LOCKED"]    = WHITE_CELL_COLOR;
    colorMap["RELOCK"]    = RED_CELL_COLOR;
    castMapped(mcell)->setColorMap(colorMap);

    col.push_back(mcell);

    //------------------------------------------------------------
    // Set up the YIG frequency to highlight cells that are more than
    // 0.2 GHz away from the mean for the subarray that each antenna
    // belongs to
    //------------------------------------------------------------

    scell = MonitorCellScaled::makeCell(colWidth, display.cms().sza(iAnt).antennaCommon().lO().yigFreq());
    castScaled(scell)->setAntenna(ant);
    castScaled(scell)->errorOnAbsDeviationFromMean(mpm.yigFreqVec_, 0.2, GROUP_SUBARRAY);
    
    col.push_back(scell);
    
    //------------------------------------------------------------
    // Bias lock status and reflock status
    //------------------------------------------------------------
    
    colorMap.clear();
    colorMap["false"] = RED_CELL_COLOR;  
    colorMap["true"]  = WHITE_CELL_COLOR;

    mcell = MonitorCellMapped::makeCell(colWidth, display.cms().sza(iAnt).bias().refLockStatus(), "UNLOCKED", "LOCKED");
    castMapped(mcell)->setColorMap(colorMap);
    castMapped(mcell)->setAntenna(ant);
    castMapped(mcell)->colorIfFrequencyInGHz(not1cm);
    col.push_back(mcell);

    mcell = MonitorCellMapped::makeCell(colWidth, display.cms().sza(iAnt).bias().hwLockStatus(), "UNLOCKED", "LOCKED");
    castMapped(mcell)->setColorMap(colorMap);
    castMapped(mcell)->setAntenna(ant);
    castMapped(mcell)->colorIfFrequencyInGHz(not1cm);
    col.push_back(mcell);

    //------------------------------------------------------------
    // Set up the 3mm LO frequencyto check
    //------------------------------------------------------------

    scell = MonitorCellScaled::makeCell(colWidth, display.cms().sza(iAnt).bias().gunnFrequency(), 0.01, 17.5);
    castScaled(scell)->errorOnAbsDeviationFromMean(mpm.freqVec3mm_, 0.7, GROUP_SUBARRAY);
    castScaled(scell)->setAntenna(ant);
    castScaled(scell)->colorIfFrequencyInGHz(not1cm);
    col.push_back(scell);

    //------------------------------------------------------------
    // Interface module 10 MHz reference state
    //------------------------------------------------------------

    BINARY_MAPPED_MP(display.cms().sza(iAnt).intmod().lockStatus10MHz(),   "BAD", "GOOD", RED_CELL_COLOR, WHITE_CELL_COLOR);

    //------------------------------------------------------------
    // Is the caltert in a valid state?
    //------------------------------------------------------------

    stateMap.clear();
    stateMap["0"] = "FALSE"; // "Idle";
    stateMap["1"] = "FALSE"; // "Homing";
    stateMap["2"] = "FALSE"; // "Home";
    stateMap["3"] = "FALSE"; // "Home error";
    stateMap["4"] = "FALSE"; // "Moving";
    stateMap["5"] = "TRUE";  // "1-cm Rx selected";
    stateMap["6"] = "FALSE"; // "3-mm Rx selected";
    stateMap["7"] = "FALSE"; // "1-mm Rx selected";
    stateMap["8"] = "FALSE"; // "Manual position";
    stateMap["9"] = "FALSE"; // "Stuck";

    colorMap.clear();
    colorMap["FALSE"] = RED_CELL_COLOR;
    colorMap["TRUE"]  = WHITE_CELL_COLOR;

    // 1cm state
    
    mcell = MonitorCellMapped::makeCell(colWidth, display.cms().sza(iAnt).caltert().tertState(), stateMap);
    castMapped(mcell)->setColorMap(colorMap);
    castMapped(mcell)->setAntenna(ant);
    castMapped(mcell)->colorIfFrequencyInGHz(is1cm);
    col.push_back(mcell);

    // 3mm state

    stateMap["5"] = "FALSE";  // "1-cm Rx selected";
    stateMap["6"] = "TRUE";   // "3-mm Rx selected";

    mcell = MonitorCellMapped::makeCell(colWidth, display.cms().sza(iAnt).caltert().tertState(), stateMap);
    castMapped(mcell)->setColorMap(colorMap);
    castMapped(mcell)->setAntenna(ant);
    castMapped(mcell)->colorIfFrequencyInGHz(is3mm);
    col.push_back(mcell);

    // 1mm state

    stateMap["6"] = "FALSE";  // "3-mm Rx selected";
    stateMap["7"] = "TRUE";   // "1-mm Rx selected";

    mcell = MonitorCellMapped::makeCell(colWidth, display.cms().sza(iAnt).caltert().tertState(), stateMap);
    castMapped(mcell)->setColorMap(colorMap);
    castMapped(mcell)->setAntenna(ant);
    castMapped(mcell)->colorIfFrequencyInGHz(is1mm);
    col.push_back(mcell);

    //------------------------------------------------------------
    // Calibrator state
    //------------------------------------------------------------

    stateMap.clear();
    colorMap.clear();

    stateMap["0"] = "Idle";
    stateMap["1"] = "Moving";
    stateMap["2"] = "Sky";
    stateMap["3"] = "Ambient load";
    stateMap["4"] = "Stuck";
    
    colorMap["Idle"]              = RED_CELL_COLOR;
    colorMap["Moving"]            = RED_CELL_COLOR;
    colorMap["Sky"]               = WHITE_CELL_COLOR;
    colorMap["Ambient load"]      = RED_CELL_COLOR;
    colorMap["Stuck"]             = RED_CELL_COLOR;
    
    mcell = MonitorCellMapped::makeCell(colWidth, display.cms().sza(iAnt).caltert().calibState(), stateMap);
    castMapped(mcell)->setColorMap(colorMap);
    col.push_back(mcell);

    //------------------------------------------------------------
    // Mount offsets
    //------------------------------------------------------------

    // Convert AZ offset to percent of beamwidth at the normalization frequency
    precision = 0;
    scell = MonitorCellScaled::makeCell(colWidth, 
				  display.cms().sza(iAnt).antennaCommon().drive().point().mountOffsetAz(), 
				  100 * 1.0/fwhm.arcmin(), offset, precision);

    // Fractional beamwidth scales as the frequency

    castScaled(scell)->scaleByFrequency(OPER_MULT, freqNorm);

    // Flag if this becomes greater than 25% of the beamwidth

    castScaled(scell)->errorOnAbsDeviationFromVal(0.0, 25);
    castScaled(scell)->setAntenna(ant);
    col.push_back(scell);

    // Convert EL offset to percent of beamwidth at the normalization frequency

    scell = MonitorCellScaled::makeCell(colWidth, 
				  display.cms().sza(iAnt).antennaCommon().drive().point().mountOffsetEl(),
				  100 * 1.0/fwhm.arcmin(), offset, precision);
    // Fractional beamwidth scales as the frequency

    castScaled(scell)->scaleByFrequency(OPER_MULT, freqNorm);

    // Flag if this becomes greater than 25% of the beamwidth

    castScaled(scell)->errorOnAbsDeviationFromVal(0.0, 25.0);
    castScaled(scell)->setAntenna(ant);
    col.push_back(scell);

    //------------------------------------------------------------
    // Shadowed state
    //------------------------------------------------------------

    BOOLEAN_MAPPED_MP(display.cms().astro().antenna(iAnt+15).shadowed(),   "FALSE", "TRUE", 
		      WHITE_CELL_COLOR, RED_CELL_COLOR);
  }

  //------------------------------------------------------------
  // Finally, add all columns
  //------------------------------------------------------------

  for(unsigned iCol=0; iCol < col.size(); iCol++) {
    table->add(col[iCol]);
  }

  return table;
}

/**.......................................................................
 * Get the OVRO Table
 */
RtTablePtr getOvroTable(CoherenceDisplay& display, MpManager& mpm)
{
  RtTablePtr table(new RtTable("Table"));
  table->setLayout(EOL_LEFT_JUSTIFIED_LAYOUT);

  unsigned nAnt = 6;

  // Now iterate over OVRO antennas, creating a column for each one

  std::ostringstream os;
  for(unsigned iAnt=0; iAnt < nAnt; iAnt++) {
    os.str("");
    os << "C" << (iAnt + 1);
    table->addCol(RtColumn::makeColumn(os.str()));
  }

  //------------------------------------------------------------
  // Add labels for all rows
  //------------------------------------------------------------

  RtRowPtr row = RtRow::makeRow("Coherence");
  row->setLayout(EOL_LEFT_JUSTIFIED_LAYOUT);
  table->addRow(row);

  table->addRow(RtRow::makeRow("Drive State"));
  table->addRow(RtRow::makeRow("Az (deg)"));

  table->addRow(RtRow::makeRow("El (deg)"));
  table->addRow(RtRow::makeRow("Az Err (arcsec)"));
  table->addRow(RtRow::makeRow("El Err (arcsec)"));
  table->addRow(RtRow::makeRow("IF Switch State Correct for 1cm?"));
  table->addRow(RtRow::makeRow("IF Switch State Correct for 3mm?"));
  table->addRow(RtRow::makeRow("IF Switch State Correct for 1mm?"));
  table->addRow(RtRow::makeRow("Rx State"));
  table->addRow(RtRow::makeRow("Dewar Temperature"));
  table->addRow(RtRow::makeRow("Yig Lock State"));
  table->addRow(RtRow::makeRow("Yig Frequency (GHz)"));

  table->addRow(RtRow::makeRow("3mm Bias Ref Lock State"));
  table->addRow(RtRow::makeRow("3mm Bias HW Lock State"));
  table->addRow(RtRow::makeRow("3mm LO Frequency (GHz)"));

  table->addRow(RtRow::makeRow("1mm Bias Ref Lock State"));
  table->addRow(RtRow::makeRow("1mm Bias HW Lock State"));
  table->addRow(RtRow::makeRow("1mm LO Frequency (GHz)"));

  table->addRow(RtRow::makeRow("10 MHz Lock"));

  table->addRow(RtRow::makeRow("Optics State Correct for 1cm?"));
  table->addRow(RtRow::makeRow("Optics State Correct for 3mm?"));
  table->addRow(RtRow::makeRow("Optics State Correct for 1mm?"));
  table->addRow(RtRow::makeRow("Calibrator State"));
  table->addRow(RtRow::makeRow("Mount Az Offset (% of beam FWHM)"));
  table->addRow(RtRow::makeRow("Mount El Offset (% of beam FWHM)"));
  table->addRow(RtRow::makeRow("Shadowed"));

  //------------------------------------------------------------
  // Now add columns of monitor points
  //------------------------------------------------------------

  unsigned colWidth = 12;
  std::vector<MonitorCellPtr> col;
  std::map<std::string, CellColor> colorMap;
  std::map<std::string, std::string> stateMap;
  
  MonitorCellPtr scell;
  MonitorCellPtr mcell;

  sza::util::DataType cohLimit = (float)0.2;
  sza::util::MonitorCondition lowCoherence(DataTypeTruthFn::greaterThan, cohLimit);

  sza::util::DataType freq1cmLimitGHz = (double)50.0;
  sza::util::DataType freq3mmLimitGHz = (double)150.0;
  sza::util::MonitorCondition not1cm(DataTypeTruthFn::greaterThan, freq1cmLimitGHz);
  sza::util::MonitorCondition is1cm(DataTypeTruthFn::lessThan, freq1cmLimitGHz);
  sza::util::MonitorCondition is3mm(DataTypeTruthFn::greaterThanAndLessThan, freq1cmLimitGHz, freq3mmLimitGHz);
  sza::util::MonitorCondition is1mm(DataTypeTruthFn::greaterThan, freq3mmLimitGHz);

  for(unsigned iAnt=0; iAnt < nAnt; iAnt++) {

    // Get the descriptor for this antenna

    AntennaMapper::Antenna* ant = display.antMapper_->getAntenna(AntennaMapper::ANT_OVRO, iAnt);
    Frequency freqNorm(Frequency::GigaHz(), 30.0);
    Angle fwhm = ant->beamFwhm(freqNorm);

    //------------------------------------------------------------
    // Coherence
    //------------------------------------------------------------
    const double mul       = 1.0;
    const double offset    = 0.0;
    int          precision = 2;
    scell = MonitorCellScaled::makeCell(colWidth, 
        display.cms().astro().antenna(iAnt).maxCoherence(),
        mul, offset, precision);
    castScaled(scell)->errorOnAbsDeviationFromVal(1.0, 0.8);

    col.push_back(scell);

    //------------------------------------------------------------
    // Drive state
    //------------------------------------------------------------
    
    stateMap.clear();
    stateMap["TRACK"]   = "TRACK";
    stateMap["CLOSE"]   = "CLOSE";
    stateMap["SLEW"]    = "SLEW";
    stateMap["STOW"]    = "STOW";
    stateMap["SNOW"]    = "SNOW";
    stateMap["STOP"]    = "STOP";
    stateMap["DISABLE"] = "DISABLE";
    stateMap["HWLIMIT"] = "HWLIMIT";
    stateMap["ERROR"]   = "ERROR";
    stateMap["LOCAL"]   = "LOCAL";
    stateMap["SERVICE"] = "SERVICE";
    stateMap["TEST"]    = "TEST";
    stateMap["FATAL"]   = "FATAL";

    colorMap.clear();
    colorMap["TRACK"]   = WHITE_CELL_COLOR;
    colorMap["CLOSE"]   = RED_CELL_COLOR;
    colorMap["SLEW"]    = RED_CELL_COLOR;
    colorMap["STOW"]    = RED_CELL_COLOR;
    colorMap["SNOW"]    = RED_CELL_COLOR;
    colorMap["STOP"]    = RED_CELL_COLOR;
    colorMap["DISABLE"] = RED_CELL_COLOR;
    colorMap["HWLIMIT"] = RED_CELL_COLOR;
    colorMap["ERROR"]   = RED_CELL_COLOR;
    colorMap["LOCAL"]   = RED_CELL_COLOR;
    colorMap["SERVICE"] = RED_CELL_COLOR;
    colorMap["TEST"]    = RED_CELL_COLOR;
    colorMap["FATAL"]   = RED_CELL_COLOR;
    
    mcell = MonitorCellMapped::makeCell(colWidth, 
        display.cms().ovro(iAnt).antennaCommon().drive().state(), stateMap);
    castMapped(mcell)->setAntenna(ant);
    castMapped(mcell)->setColorMap(colorMap);
    col.push_back(mcell);

    //------------------------------------------------------------
    // Position
    //------------------------------------------------------------

    // Set the allowable delta to be the FWHM at the current
    // frequency.  For AZ, this is worst-case scenario, since we can
    // tolerate much larger deltas at higher elevation

    MonitorCellScaled::Delta positionDelta(fwhm.degrees(), OPER_DIV, freqNorm);
    
    precision = 1;
    scell = MonitorCellScaled::makeCell(colWidth, 
        display.cms().ovro(iAnt).antennaCommon().drive().track().actualAzimuth(),
        mul, offset, precision);
    castScaled(scell)->setAntenna(ant);
    castScaled(scell)->colorIf(display.cms().astro().antenna(iAnt).maxCoherence(), lowCoherence);
    castScaled(scell)->errorOnAbsDeviationFromMean(mpm.azVec_, positionDelta, GROUP_SUBARRAY);

    col.push_back(scell);

    scell = MonitorCellScaled::makeCell(colWidth, 
        display.cms().ovro(iAnt).antennaCommon().drive().track().actualElevation(),
        mul, offset, precision);
    castScaled(scell)->setAntenna(ant);
    castScaled(scell)->colorIf(display.cms().astro().antenna(iAnt).maxCoherence(), lowCoherence);
    castScaled(scell)->errorOnAbsDeviationFromMean(mpm.elVec_, positionDelta, GROUP_SUBARRAY);

    col.push_back(scell);

    //------------------------------------------------------------
    // Tracking error
    //------------------------------------------------------------

    // Set tracking errors to be 1/4 of the FWHM at any frequency

    MonitorCellScaled::Delta trackDelta(fwhm.arcsec()/4, OPER_DIV, freqNorm);

    scell = MonitorCellScaled::makeCell(colWidth, 
        display.cms().ovro(iAnt).antennaCommon().drive().track().errorAzimuthSky(),
        mul, offset, precision);
    castScaled(scell)->setAntenna(ant);
    castScaled(scell)->colorIf(display.cms().astro().antenna(iAnt).maxCoherence(), lowCoherence);
    castScaled(scell)->errorOnAbsDeviationFromVal(0.0, trackDelta);

    col.push_back(scell);

    scell = MonitorCellScaled::makeCell(colWidth, 
        display.cms().ovro(iAnt).antennaCommon().drive().track().errorElevation(),
        mul, offset, precision);
    castScaled(scell)->setAntenna(ant);
    castScaled(scell)->colorIf(display.cms().astro().antenna(iAnt).maxCoherence(), lowCoherence);
    castScaled(scell)->errorOnAbsDeviationFromVal(0.0, trackDelta);

    col.push_back(scell);

    //------------------------------------------------------------
    // IF switch states
    //------------------------------------------------------------

    stateMap.clear();
    stateMap["CHANGED"] = "FALSE";
    stateMap["POS_1"]   = "TRUE";
    stateMap["POS_2"]   = "FALSE";
    stateMap["POS_3"]   = "FALSE";
    stateMap["POS_4"]   = "FALSE";
    stateMap["STUCK"]   = "FALSE";

    colorMap.clear();
    colorMap["TRUE"]  = WHITE_CELL_COLOR;
    colorMap["FALSE"] = RED_CELL_COLOR;

    mcell = MonitorCellMapped::makeCell(colWidth, display.cms().ovro(iAnt).antennaIfContainer(0).antennaIF().ifSwitchStat(), stateMap);
    castMapped(mcell)->setColorMap(colorMap);
    castMapped(mcell)->setAntenna(ant);
    castMapped(mcell)->colorIfFrequencyInGHz(is1cm);

    col.push_back(mcell);

    stateMap["POS_2"]   = "TRUE";
    stateMap["POS_1"]   = "FALSE";

    mcell = MonitorCellMapped::makeCell(colWidth, display.cms().ovro(iAnt).antennaIfContainer(0).antennaIF().ifSwitchStat(), stateMap);
    castMapped(mcell)->setColorMap(colorMap);
    castMapped(mcell)->setAntenna(ant);
    castMapped(mcell)->colorIfFrequencyInGHz(is3mm);

    col.push_back(mcell);

    stateMap["POS_3"]   = "TRUE";
    stateMap["POS_2"]   = "FALSE";

    mcell = MonitorCellMapped::makeCell(colWidth, display.cms().ovro(iAnt).antennaIfContainer(0).antennaIF().ifSwitchStat(), stateMap);
    castMapped(mcell)->setColorMap(colorMap);
    castMapped(mcell)->setAntenna(ant);
    castMapped(mcell)->colorIfFrequencyInGHz(is1mm);

    col.push_back(mcell);

    //------------------------------------------------------------
    // Rx state
    //------------------------------------------------------------
    
    stateMap.clear();
    stateMap["GOOD"]         = "GOOD";
    stateMap["BAD"]          = "BAD";
    stateMap["TUNE"]         = "TUNE";
    stateMap["YIG_BAD"]      = "YIG_BAD";
    stateMap["GUNN_BAD"]     = "GUNN_BAD";
    stateMap["IFSEL_BAD"]    = "IFSEL_BAD";
    stateMap["RXSEL_BAD"]    = "RXSEL_BAD";
    stateMap["RX_BAD"]       = "RX_BAD";
    stateMap["TERTIARY_BAD"] = "TERTIARY_BAD";
    stateMap["OPTIMIZE"]     = "OPTIMIZE";
    stateMap["SEARCH"]       = "SEARCH";

    colorMap.clear();
    colorMap["GOOD"]         = WHITE_CELL_COLOR;
    colorMap["BAD"]          = RED_CELL_COLOR;
    colorMap["TUNE"]         = RED_CELL_COLOR;
    colorMap["YIG_BAD"]      = RED_CELL_COLOR;
    colorMap["GUNN_BAD"]     = RED_CELL_COLOR;
    colorMap["IFSEL_BAD"]    = RED_CELL_COLOR;
    colorMap["RXSEL_BAD"]    = RED_CELL_COLOR;
    colorMap["RX_BAD"]       = RED_CELL_COLOR;
    colorMap["TERTIARY_BAD"] = RED_CELL_COLOR;
    colorMap["OPTIMIZE"]     = RED_CELL_COLOR;
    colorMap["SEARCH"]       = RED_CELL_COLOR;
    
    mcell = MonitorCellMapped::makeCell(colWidth, display.cms().ovro(iAnt).antennaCommon().receivers().rxState(), stateMap);
    castMapped(mcell)->setColorMap(colorMap);
    col.push_back(mcell);

    //------------------------------------------------------------
    // Set up the dewar temperature to highlight if more than 5K from
    // the mean for this antenna type
    //------------------------------------------------------------

    scell = MonitorCellScaled::makeCell(colWidth, display.cms().ovro(iAnt).antennaCommon().receivers().dewarTemp());
    castScaled(scell)->setAntenna(ant);
    castScaled(scell)->errorOnAbsDeviationFromMean(mpm.dewarTempVec_, 5, GROUP_ANTTYPE);
    
    col.push_back(scell);

    //------------------------------------------------------------
    // Yig lock status
    //------------------------------------------------------------
    
    stateMap.clear();
    stateMap["UNLOCKED"]  = "UNLOCKED";
    stateMap["SEARCHING"] = "SEARCHING";
    stateMap["REFINING"]  = "REFINING";
    stateMap["LOCKED"]    = "LOCKED";
    stateMap["RELOCK"]    = "RELOCK";
    mcell =   MonitorCellMapped::makeCell(colWidth, display.cms().ovro(iAnt).yig().lockState(), stateMap);

    colorMap.clear();
    colorMap["UNLOCKED"]  = RED_CELL_COLOR;
    colorMap["SEARCHING"] = RED_CELL_COLOR;
    colorMap["REFINING"]  = RED_CELL_COLOR;
    colorMap["LOCKED"]    = WHITE_CELL_COLOR;
    colorMap["RELOCK"]    = RED_CELL_COLOR;
    castMapped(mcell)->setColorMap(colorMap);

    col.push_back(mcell);

    //------------------------------------------------------------
    // Set up the YIG frequency to highlight cells that are more than
    // 0.2 GHz away from the mean for the subarray that each antenna
    // belongs to
    //------------------------------------------------------------

    scell = MonitorCellScaled::makeCell(colWidth, display.cms().ovro(iAnt).antennaCommon().lO().yigFreq());
    castScaled(scell)->setAntenna(ant);
    castScaled(scell)->errorOnAbsDeviationFromMean(mpm.yigFreqVec_, 0.2, GROUP_SUBARRAY);

    col.push_back(scell);

    //------------------------------------------------------------
    // 3mm Bias lock status and reflock status
    //------------------------------------------------------------
    
    stateMap.clear();
    stateMap["LEVEL_LOW"] = "BAD";
    stateMap["LEVEL_OK"]  = "OK";

    colorMap.clear();
    colorMap["BAD"] = RED_CELL_COLOR;  
    colorMap["OK"]  = WHITE_CELL_COLOR;

    mcell = MonitorCellMapped::makeCell(colWidth, display.cms().ovro(iAnt).gunn3mm().gunnPll().refStat(), stateMap);
    castMapped(mcell)->setColorMap(colorMap);
    castMapped(mcell)->setAntenna(ant);
    castMapped(mcell)->colorIfFrequencyInGHz(is3mm);
    col.push_back(mcell);

    stateMap.clear();
    stateMap["UNLOCKED"] = "UNLOCKED";
    stateMap["LOCKED"]   = "LOCKED";

    colorMap.clear();
    colorMap["UNLOCKED"] = RED_CELL_COLOR;  
    colorMap["LOCKED"]  = WHITE_CELL_COLOR;

    mcell = MonitorCellMapped::makeCell(colWidth, display.cms().ovro(iAnt).gunn3mm().gunnPll().lockBit(), stateMap);
    castMapped(mcell)->setColorMap(colorMap);
    castMapped(mcell)->setAntenna(ant);
    castMapped(mcell)->colorIfFrequencyInGHz(is3mm);
    col.push_back(mcell);

    //------------------------------------------------------------
    // Set up the bias frequency
    //------------------------------------------------------------

    {
      MonitorPoint& mp1 = display.cms().ovro(iAnt).gunn3mm().gunnPll().gunnFreq();
      MonitorPoint& mp2 = display.cms().ovro(iAnt).gunn3mm().gunnPll().multiplier();
      scell = MonitorCellScaled::makeCell(colWidth, mp1, OPER_MULT, mp2);
    }

    castScaled(scell)->errorOnAbsDeviationFromMean(mpm.freqVec3mm_, 0.7, GROUP_SUBARRAY);
    castScaled(scell)->setAntenna(ant);
    castScaled(scell)->colorIfFrequencyInGHz(is3mm);
    col.push_back(scell);

    //------------------------------------------------------------
    // 1mm Bias lock status and reflock status
    //------------------------------------------------------------
    
    stateMap.clear();
    stateMap["LEVEL_LOW"] = "BAD";
    stateMap["LEVEL_OK"]  = "OK";

    colorMap.clear();
    colorMap["BAD"] = RED_CELL_COLOR;  
    colorMap["OK"]  = WHITE_CELL_COLOR;

    mcell = MonitorCellMapped::makeCell(colWidth, display.cms().ovro(iAnt).gunn1mm().gunnPll().refStat(), stateMap);
    castMapped(mcell)->setColorMap(colorMap);
    castMapped(mcell)->setAntenna(ant);
    castMapped(mcell)->colorIfFrequencyInGHz(is1mm);
    col.push_back(mcell);

    stateMap.clear();
    stateMap["UNLOCKED"] = "UNLOCKED";
    stateMap["LOCKED"]   = "LOCKED";

    colorMap.clear();
    colorMap["UNLOCKED"] = RED_CELL_COLOR;  
    colorMap["LOCKED"]  = WHITE_CELL_COLOR;

    mcell = MonitorCellMapped::makeCell(colWidth, display.cms().ovro(iAnt).gunn1mm().gunnPll().lockBit(), stateMap);
    castMapped(mcell)->setColorMap(colorMap);
    castMapped(mcell)->setAntenna(ant);
    castMapped(mcell)->colorIfFrequencyInGHz(is1mm);
    col.push_back(mcell);

    //------------------------------------------------------------
    // Set up the bias frequency
    //------------------------------------------------------------

    {
      MonitorPoint& mp1 = display.cms().ovro(iAnt).gunn1mm().gunnPll().gunnFreq();
      MonitorPoint& mp2 = display.cms().ovro(iAnt).gunn1mm().gunnPll().multiplier();
      scell = MonitorCellScaled::makeCell(colWidth, mp1, OPER_MULT, mp2);
    }

    castScaled(scell)->errorOnAbsDeviationFromMean(mpm.freqVec1mm_, 0.7, GROUP_SUBARRAY);
    castScaled(scell)->setAntenna(ant);
    castScaled(scell)->colorIfFrequencyInGHz(is1mm);
    col.push_back(scell);

    //------------------------------------------------------------
    // Interface module 10 MHz reference state
    //------------------------------------------------------------

    stateMap.clear();
    stateMap["BAD"]  = "BAD";
    stateMap["GOOD"] = "OK";

    colorMap.clear();
    colorMap["BAD"] = RED_CELL_COLOR;  
    colorMap["OK"]  = WHITE_CELL_COLOR;

    mcell = MonitorCellMapped::makeCell(colWidth, display.cms().ovro(iAnt).loReferenceContainer().loReference().lockStatus10Mhz(),  stateMap);
    castMapped(mcell)->setColorMap(colorMap);
    col.push_back(mcell);

    //------------------------------------------------------------
    // Is the caltert in a valid state?
    //------------------------------------------------------------

    stateMap.clear();
    stateMap["UNKNOWN_OR_BAD"] = "FALSE"; 
    stateMap["MOVING"]         = "FALSE"; 
    stateMap["RX_CM"]          = "TRUE"; 
    stateMap["RX_3MM"]         = "FALSE";
    stateMap["RX_1MM"]         = "FALSE";
    stateMap["GRID"]           = "FALSE";
    stateMap["MANUAL"]         = "FALSE";
    stateMap["STUCK"]          = "FALSE";

    colorMap.clear();
    colorMap["FALSE"] = RED_CELL_COLOR;
    colorMap["TRUE"]  = WHITE_CELL_COLOR;

    // 1cm state
    
    mcell = MonitorCellMapped::makeCell(colWidth, display.cms().ovro(iAnt).optics().rxSelectState(), stateMap);
    castMapped(mcell)->setColorMap(colorMap);
    castMapped(mcell)->setAntenna(ant);
    castMapped(mcell)->colorIfFrequencyInGHz(is1cm);
    col.push_back(mcell);

    // 3mm state

    stateMap["RX_CM"]  = "FALSE";  // "1-cm Rx selected";
    stateMap["RX_3MM"] = "TRUE";   // "3-mm Rx selected";

    mcell = MonitorCellMapped::makeCell(colWidth, display.cms().ovro(iAnt).optics().rxSelectState(), stateMap);
    castMapped(mcell)->setColorMap(colorMap);
    castMapped(mcell)->setAntenna(ant);
    castMapped(mcell)->colorIfFrequencyInGHz(is3mm);
    col.push_back(mcell);

    // 1mm state

    stateMap["RX_3MM"] = "FALSE";  // "3-mm Rx selected";
    stateMap["RX_1MM"] = "TRUE";   // "1-mm Rx selected";

    mcell = MonitorCellMapped::makeCell(colWidth, display.cms().ovro(iAnt).optics().rxSelectState(), stateMap);
    castMapped(mcell)->setColorMap(colorMap);
    castMapped(mcell)->setAntenna(ant);
    castMapped(mcell)->colorIfFrequencyInGHz(is1mm);
    col.push_back(mcell);

    //------------------------------------------------------------
    // Calibrator state
    //------------------------------------------------------------

    stateMap.clear();
    colorMap.clear();

    stateMap["UNKNOWN"]      = "BAD";
    stateMap["MOVING"]       = "BAD";
    stateMap["SKY"]          = "GOOD";
    stateMap["AMBIENT_LOAD"] = "BAD";
    stateMap["PARTIAL"]      = "BAD";
    stateMap["HOT_LOAD"]     = "BAD";
    stateMap["STUCK"]        = "BAD";
    
    colorMap["BAD"]          = RED_CELL_COLOR;
    colorMap["GOOD"]         = WHITE_CELL_COLOR;
    
    mcell = MonitorCellMapped::makeCell(colWidth, display.cms().ovro(iAnt).optics().calState(), stateMap);
    castMapped(mcell)->setColorMap(colorMap);
    col.push_back(mcell);

    //------------------------------------------------------------
    // Mount offsets
    //------------------------------------------------------------

    // Convert AZ offset to percent of beamwidth at the normalization frequency
    precision = 0;
    scell = MonitorCellScaled::makeCell(colWidth, 
				  display.cms().ovro(iAnt).antennaCommon().drive().point().mountOffsetAz(), 
				  100 * 1.0/fwhm.arcmin(), offset, precision);

    // Fractional beamwidth scales as the frequency

    castScaled(scell)->scaleByFrequency(OPER_MULT, freqNorm);

    // Flag if this becomes greater than 25% of the beamwidth

    castScaled(scell)->errorOnAbsDeviationFromVal(0.0, 25);
    castScaled(scell)->setAntenna(ant);
    col.push_back(scell);

    // Convert EL offset to percent of beamwidth at the normalization frequency

    scell = MonitorCellScaled::makeCell(colWidth, 
				  display.cms().ovro(iAnt).antennaCommon().drive().point().mountOffsetEl(),
				  100 * 1.0/fwhm.arcmin(), offset, precision);
    // Fractional beamwidth scales as the frequency

    castScaled(scell)->scaleByFrequency(OPER_MULT, freqNorm);

    // Flag if this becomes greater than 25% of the beamwidth

    castScaled(scell)->errorOnAbsDeviationFromVal(0.0, 25.0);
    castScaled(scell)->setAntenna(ant);
    col.push_back(scell);

    //------------------------------------------------------------
    // Shadowed state
    //------------------------------------------------------------

    BOOLEAN_MAPPED_MP(display.cms().astro().antenna(iAnt).shadowed(),   "FALSE", "TRUE", 
		      WHITE_CELL_COLOR, RED_CELL_COLOR);
  }

  //------------------------------------------------------------
  // Finally, add all columns
  //------------------------------------------------------------

  for(unsigned iCol=0; iCol < col.size(); iCol++) {
    table->add(col[iCol]);
  }

  return table;
}


/**.......................................................................
 * Get the BIMA Table
 */
RtTablePtr getBimaTable(CoherenceDisplay& display, MpManager& mpm)
{
  RtTablePtr table(new RtTable("Table"));
  table->setLayout(EOL_LEFT_JUSTIFIED_LAYOUT);

  unsigned nAnt = 9;

  // Now iterate over BIMA antennas, creating a column for each one

  std::ostringstream os;
  for(unsigned iAnt=0; iAnt < nAnt; iAnt++) {
    os.str("");
    os << "C" << (iAnt + 7);
    table->addCol(RtColumn::makeColumn(os.str()));
  }

  //------------------------------------------------------------
  // Add labels for all rows
  //------------------------------------------------------------

  RtRowPtr row = RtRow::makeRow("Coherence");
  row->setLayout(EOL_LEFT_JUSTIFIED_LAYOUT);
  table->addRow(row);

  table->addRow(RtRow::makeRow("Drive State"));
  table->addRow(RtRow::makeRow("Az (deg)"));

  table->addRow(RtRow::makeRow("El (deg)"));
  table->addRow(RtRow::makeRow("Az Err (arcsec)"));
  table->addRow(RtRow::makeRow("El Err (arcsec)"));
  table->addRow(RtRow::makeRow("IF Switch State Correct for 1cm?"));
  table->addRow(RtRow::makeRow("IF Switch State Correct for 3mm?"));
  table->addRow(RtRow::makeRow("IF Switch State Correct for 1mm?"));
  table->addRow(RtRow::makeRow("Rx State"));
  table->addRow(RtRow::makeRow("Dewar Temperature"));
  table->addRow(RtRow::makeRow("Yig Lock State"));
  table->addRow(RtRow::makeRow("Yig Frequency (GHz)"));

  table->addRow(RtRow::makeRow("mm Bias Ref Lock State"));
  table->addRow(RtRow::makeRow("mm Bias HW Lock State"));
  table->addRow(RtRow::makeRow("3mm LO frequency (GHz)"));
  table->addRow(RtRow::makeRow("1mm LO frequency (GHz)"));

  table->addRow(RtRow::makeRow("Optics State Correct for cm?"));
  table->addRow(RtRow::makeRow("Optics State Correct for mm?"));
  table->addRow(RtRow::makeRow("Calibrator State"));
  table->addRow(RtRow::makeRow("Mount Az Offset (% of beam FWHM)"));
  table->addRow(RtRow::makeRow("Mount El Offset (% of beam FWHM)"));
  table->addRow(RtRow::makeRow("Shadowed"));

  //------------------------------------------------------------
  // Now add columns of monitor points
  //------------------------------------------------------------

  unsigned colWidth = 12;
  std::vector<MonitorCellPtr> col;
  std::map<std::string, CellColor> colorMap;
  std::map<std::string, std::string> stateMap;
  
  MonitorCellPtr scell;
  MonitorCellPtr mcell;

  sza::util::DataType cohLimit = (float)0.2;
  sza::util::MonitorCondition lowCoherence(DataTypeTruthFn::greaterThan, cohLimit);

  sza::util::DataType freq1cmLimitGHz = (double)50.0;
  sza::util::DataType freq3mmLimitGHz = (double)150.0;
  sza::util::MonitorCondition not1cm(DataTypeTruthFn::greaterThan, freq1cmLimitGHz);
  sza::util::MonitorCondition is1cm(DataTypeTruthFn::lessThan, freq1cmLimitGHz);
  sza::util::MonitorCondition is3mm(DataTypeTruthFn::greaterThanAndLessThan, freq1cmLimitGHz, freq3mmLimitGHz);
  sza::util::MonitorCondition is1mm(DataTypeTruthFn::greaterThan, freq3mmLimitGHz);

  for(unsigned iAnt=0; iAnt < nAnt; iAnt++) {

    // Get the descriptor for this antenna

    AntennaMapper::Antenna* ant = display.antMapper_->getAntenna(AntennaMapper::ANT_BIMA, iAnt);
    Frequency freqNorm(Frequency::GigaHz(), 30.0);
    Angle fwhm = ant->beamFwhm(freqNorm);

    //------------------------------------------------------------
    // Coherence
    //------------------------------------------------------------
    const double mul       = 1.0;
    const double offset    = 0.0;
    int          precision = 2;

    scell = MonitorCellScaled::makeCell(colWidth, 
        display.cms().astro().antenna(iAnt+6).maxCoherence(),
        mul, offset, precision);
    castScaled(scell)->errorOnAbsDeviationFromVal(1.0, 0.8);

    col.push_back(scell);

    //------------------------------------------------------------
    // Drive state
    //------------------------------------------------------------
    
    stateMap.clear();
    stateMap["TRACK"]   = "TRACK";
    stateMap["CLOSE"]   = "CLOSE";
    stateMap["SLEW"]    = "SLEW";
    stateMap["STOW"]    = "STOW";
    stateMap["SNOW"]    = "SNOW";
    stateMap["STOP"]    = "STOP";
    stateMap["DISABLE"] = "DISABLE";
    stateMap["HWLIMIT"] = "HWLIMIT";
    stateMap["ERROR"]   = "ERROR";
    stateMap["LOCAL"]   = "LOCAL";
    stateMap["SERVICE"] = "SERVICE";
    stateMap["TEST"]    = "TEST";
    stateMap["FATAL"]   = "FATAL";

    colorMap.clear();
    colorMap["TRACK"]   = WHITE_CELL_COLOR;
    colorMap["CLOSE"]   = RED_CELL_COLOR;
    colorMap["SLEW"]    = RED_CELL_COLOR;
    colorMap["STOW"]    = RED_CELL_COLOR;
    colorMap["SNOW"]    = RED_CELL_COLOR;
    colorMap["STOP"]    = RED_CELL_COLOR;
    colorMap["DISABLE"] = RED_CELL_COLOR;
    colorMap["HWLIMIT"] = RED_CELL_COLOR;
    colorMap["ERROR"]   = RED_CELL_COLOR;
    colorMap["LOCAL"]   = RED_CELL_COLOR;
    colorMap["SERVICE"] = RED_CELL_COLOR;
    colorMap["TEST"]    = RED_CELL_COLOR;
    colorMap["FATAL"]   = RED_CELL_COLOR;
    
    mcell = MonitorCellMapped::makeCell(colWidth, display.cms().bima(iAnt).antennaCommon().drive().state(), stateMap);
    castMapped(mcell)->setAntenna(ant);
    castMapped(mcell)->setColorMap(colorMap);
    col.push_back(mcell);

    //------------------------------------------------------------
    // Position
    //------------------------------------------------------------

    // Set the allowable delta to be the FWHM at the current
    // frequency.  For AZ, this is worst-case scenario, since we can
    // tolerate much larger deltas at higher elevation
    precision = 1;
    MonitorCellScaled::Delta positionDelta(fwhm.degrees(), OPER_DIV, freqNorm);

    scell = MonitorCellScaled::makeCell(colWidth, 
                display.cms().bima(iAnt).antennaCommon().drive().track().actualAzimuth(),
                mul, offset, precision);
    castScaled(scell)->setAntenna(ant);
    castScaled(scell)->colorIf(display.cms().astro().antenna(iAnt+6).maxCoherence(), lowCoherence);
    castScaled(scell)->errorOnAbsDeviationFromMean(mpm.azVec_, positionDelta, GROUP_SUBARRAY);

    col.push_back(scell);

    scell = MonitorCellScaled::makeCell(colWidth, 
                display.cms().bima(iAnt).antennaCommon().drive().track().actualElevation(),
                mul, offset, precision);
    castScaled(scell)->setAntenna(ant);
    castScaled(scell)->colorIf(display.cms().astro().antenna(iAnt+6).maxCoherence(), lowCoherence);
    castScaled(scell)->errorOnAbsDeviationFromMean(mpm.elVec_, positionDelta, GROUP_SUBARRAY);

    col.push_back(scell);

    //------------------------------------------------------------
    // Tracking error
    //------------------------------------------------------------

    // Set tracking errors to be 1/4 of the FWHM at any frequency

    MonitorCellScaled::Delta trackDelta(fwhm.arcsec()/4, OPER_DIV, freqNorm);

    scell = MonitorCellScaled::makeCell(colWidth, 
                display.cms().bima(iAnt).antennaCommon().drive().track().errorAzimuthSky(),
                mul, offset, precision);
    castScaled(scell)->setAntenna(ant);
    castScaled(scell)->colorIf(display.cms().astro().antenna(iAnt+6).maxCoherence(), lowCoherence);
    castScaled(scell)->errorOnAbsDeviationFromVal(0.0, trackDelta);

    col.push_back(scell);

    scell = MonitorCellScaled::makeCell(colWidth, 
                display.cms().bima(iAnt).antennaCommon().drive().track().errorElevation(),
                mul, offset, precision);
    castScaled(scell)->setAntenna(ant);
    castScaled(scell)->colorIf(display.cms().astro().antenna(iAnt+6).maxCoherence(), lowCoherence);
    castScaled(scell)->errorOnAbsDeviationFromVal(0.0, trackDelta);

    col.push_back(scell);

    //------------------------------------------------------------
    // IF switch states
    //------------------------------------------------------------

    stateMap.clear();
    stateMap["CHANGED"] = "FALSE";
    stateMap["POS_1"]   = "TRUE";
    stateMap["POS_2"]   = "FALSE";
    stateMap["POS_3"]   = "FALSE";
    stateMap["POS_4"]   = "FALSE";
    stateMap["STUCK"]   = "FALSE";

    colorMap.clear();
    colorMap["TRUE"]  = WHITE_CELL_COLOR;
    colorMap["FALSE"] = RED_CELL_COLOR;

    mcell = MonitorCellMapped::makeCell(colWidth, display.cms().bima(iAnt).antennaIfContainer(0).antennaIF().ifSwitchStat(), stateMap);
    castMapped(mcell)->setColorMap(colorMap);
    castMapped(mcell)->setAntenna(ant);
    castMapped(mcell)->colorIfFrequencyInGHz(is1cm);

    col.push_back(mcell);

    stateMap["POS_2"]   = "TRUE";
    stateMap["POS_1"]   = "FALSE";

    mcell = MonitorCellMapped::makeCell(colWidth, display.cms().bima(iAnt).antennaIfContainer(0).antennaIF().ifSwitchStat(), stateMap);
    castMapped(mcell)->setColorMap(colorMap);
    castMapped(mcell)->setAntenna(ant);
    castMapped(mcell)->colorIfFrequencyInGHz(is3mm);

    col.push_back(mcell);

    stateMap["POS_3"]   = "TRUE";
    stateMap["POS_2"]   = "FALSE";

    mcell = MonitorCellMapped::makeCell(colWidth, display.cms().bima(iAnt).antennaIfContainer(0).antennaIF().ifSwitchStat(), stateMap);
    castMapped(mcell)->setColorMap(colorMap);
    castMapped(mcell)->setAntenna(ant);
    castMapped(mcell)->colorIfFrequencyInGHz(is1mm);

    col.push_back(mcell);

    //------------------------------------------------------------
    // Rx state
    //------------------------------------------------------------
    
    stateMap.clear();
    stateMap["GOOD"]         = "GOOD";
    stateMap["BAD"]          = "BAD";
    stateMap["TUNE"]         = "TUNE";
    stateMap["YIG_BAD"]      = "YIG_BAD";
    stateMap["GUNN_BAD"]     = "GUNN_BAD";
    stateMap["IFSEL_BAD"]    = "IFSEL_BAD";
    stateMap["RXSEL_BAD"]    = "RXSEL_BAD";
    stateMap["RX_BAD"]       = "RX_BAD";
    stateMap["TERTIARY_BAD"] = "TERTIARY_BAD";
    stateMap["OPTIMIZE"]     = "OPTIMIZE";
    stateMap["SEARCH"]       = "SEARCH";

    colorMap.clear();
    colorMap["GOOD"]         = WHITE_CELL_COLOR;
    colorMap["BAD"]          = RED_CELL_COLOR;
    colorMap["TUNE"]         = RED_CELL_COLOR;
    colorMap["YIG_BAD"]      = RED_CELL_COLOR;
    colorMap["GUNN_BAD"]     = RED_CELL_COLOR;
    colorMap["IFSEL_BAD"]    = RED_CELL_COLOR;
    colorMap["RXSEL_BAD"]    = RED_CELL_COLOR;
    colorMap["RX_BAD"]       = RED_CELL_COLOR;
    colorMap["TERTIARY_BAD"] = RED_CELL_COLOR;
    colorMap["OPTIMIZE"]     = RED_CELL_COLOR;
    colorMap["SEARCH"]       = RED_CELL_COLOR;
    
    mcell = MonitorCellMapped::makeCell(colWidth, display.cms().bima(iAnt).antennaCommon().receivers().rxState(), stateMap);
    castMapped(mcell)->setColorMap(colorMap);
    col.push_back(mcell);

    //------------------------------------------------------------
    // Set up the dewar temperature to highlight if more than 5K from
    // the mean for this antenna type
    //------------------------------------------------------------

    scell = MonitorCellScaled::makeCell(colWidth, 
                display.cms().bima(iAnt).antennaCommon().receivers().dewarTemp(),
                 mul, offset, precision);
    castScaled(scell)->setAntenna(ant);
    castScaled(scell)->errorOnAbsDeviationFromMean(mpm.dewarTempVec_, 5, GROUP_ANTTYPE);
    
    col.push_back(scell);

    //------------------------------------------------------------
    // Yig lock status
    //------------------------------------------------------------
    
    stateMap.clear();
    stateMap["LOCK"]        = "LOCKED";
    stateMap["SEARCH"]      = "SEARCHING";
    stateMap["OPTIMIZING"]  = "REFINING";
    stateMap["FAILED"]      = "UNLOCKED";

    mcell =   MonitorCellMapped::makeCell(colWidth, display.cms().bima(iAnt).antennaCommon().lO().yigState(), stateMap);

    colorMap.clear();
    colorMap["UNLOCKED"]  = RED_CELL_COLOR;
    colorMap["SEARCHING"] = RED_CELL_COLOR;
    colorMap["REFINING"]  = RED_CELL_COLOR;
    colorMap["LOCKED"]    = WHITE_CELL_COLOR;

    castMapped(mcell)->setColorMap(colorMap);

    col.push_back(mcell);

    //------------------------------------------------------------
    // Set up the YIG frequency to highlight cells that are more than
    // 0.2 GHz away from the mean for the subarray that each antenna
    // belongs to
    //------------------------------------------------------------

    scell = MonitorCellScaled::makeCell(colWidth, display.cms().bima(iAnt).antennaCommon().lO().yigFreq());
    castScaled(scell)->setAntenna(ant);
    castScaled(scell)->errorOnAbsDeviationFromMean(mpm.yigFreqVec_, 0.2, GROUP_SUBARRAY);
    
    col.push_back(scell);

    //------------------------------------------------------------
    // 3mm Bias lock status and reflock status
    //------------------------------------------------------------
    
    stateMap.clear();
    stateMap["NOTOK"] = "BAD";
    stateMap["OK"]    = "OK";

    colorMap.clear();
    colorMap["BAD"] = RED_CELL_COLOR;  
    colorMap["OK"]  = WHITE_CELL_COLOR;

    mcell = MonitorCellMapped::makeCell(colWidth, display.cms().bima(iAnt).bimaSpecific().mMlock().fiftyMHzRef(), stateMap);
    castMapped(mcell)->setColorMap(colorMap);
    castMapped(mcell)->setAntenna(ant);
    castMapped(mcell)->colorIfFrequencyInGHz(not1cm);
    col.push_back(mcell);

    stateMap.clear();
    stateMap["LOCK"]       = "LOCKED";
    stateMap["SEARCH"]     = "SEARCHING";
    stateMap["OPTIMIZING"] = "REFINING";
    stateMap["FAILED"]     = "UNLOCKED";

    colorMap.clear();
    colorMap["UNLOCKED"]  = RED_CELL_COLOR;
    colorMap["SEARCHING"] = RED_CELL_COLOR;
    colorMap["REFINING"]  = RED_CELL_COLOR;
    colorMap["LOCKED"]    = WHITE_CELL_COLOR;

    mcell = MonitorCellMapped::makeCell(colWidth, display.cms().bima(iAnt).bimaSpecific().mMlock().state(), stateMap);
    castMapped(mcell)->setColorMap(colorMap);
    castMapped(mcell)->setAntenna(ant);
    castMapped(mcell)->colorIfFrequencyInGHz(not1cm);
    col.push_back(mcell);

    //------------------------------------------------------------
    // Set up the bias frequencies to check
    //------------------------------------------------------------

    {
      MonitorPoint& mp1 = display.cms().bima(iAnt).antennaCommon().lO().yigFreq();
      MonitorPoint& mp2 = display.cms().control().antenna(iAnt+6).harmonic();
      scell = MonitorCellScaled::makeCell(colWidth, mp1, OPER_MULT, mp2);
    }

    castScaled(scell)->errorOnAbsDeviationFromMean(mpm.freqVec3mm_, 0.7, GROUP_SUBARRAY);
    castScaled(scell)->setAntenna(ant);
    castScaled(scell)->colorIfFrequencyInGHz(is3mm);
    col.push_back(scell);

    {
      MonitorPoint& mp1 = display.cms().bima(iAnt).antennaCommon().lO().yigFreq();
      MonitorPoint& mp2 = display.cms().control().antenna(iAnt+6).harmonic();
      scell = MonitorCellScaled::makeCell(colWidth, mp1, OPER_MULT, mp2, 3.0);
    }

    castScaled(scell)->errorOnAbsDeviationFromMean(mpm.freqVec1mm_, 4.0, GROUP_SUBARRAY);
    castScaled(scell)->setAntenna(ant);
    castScaled(scell)->colorIfFrequencyInGHz(is1mm);
    col.push_back(scell);

    //------------------------------------------------------------
    // Is the caltert in a valid state?
    //------------------------------------------------------------

    stateMap.clear();
    stateMap["MM"]     = "FALSE"; 
    stateMap["CM"]     = "TRUE"; 
    stateMap["MOVING"] = "FALSE"; 
    stateMap["ERROR"]  = "FALSE";
    stateMap["NONE"]   = "FALSE";

    colorMap.clear();
    colorMap["FALSE"] = RED_CELL_COLOR;
    colorMap["TRUE"]  = WHITE_CELL_COLOR;

    // 1cm state
    
    mcell = MonitorCellMapped::makeCell(colWidth, display.cms().bima(iAnt).bimaSpecific().optics().beamSelect(), stateMap);
    castMapped(mcell)->setColorMap(colorMap);
    castMapped(mcell)->setAntenna(ant);
    castMapped(mcell)->colorIfFrequencyInGHz(is1cm);
    col.push_back(mcell);

    // mm state

    stateMap["CM"] = "FALSE";  // "1-cm Rx selected";
    stateMap["MM"] = "TRUE";   // "mm Rx selected";

    mcell = MonitorCellMapped::makeCell(colWidth, display.cms().bima(iAnt).bimaSpecific().optics().beamSelect(), stateMap);
    castMapped(mcell)->setColorMap(colorMap);
    castMapped(mcell)->setAntenna(ant);
    castMapped(mcell)->colorIfFrequencyInGHz(not1cm);
    col.push_back(mcell);

    //------------------------------------------------------------
    // Calibrator state
    //------------------------------------------------------------

    stateMap.clear();
    colorMap.clear();

    stateMap["SKY"]    = "GOOD";
    stateMap["AMB"]    = "BAD";
    stateMap["FIXED"]  = "BAD";
    stateMap["REFLEC"] = "BAD";
    stateMap["MOVING"] = "BAD";
    stateMap["ERROR"]  = "BAD";
    
    colorMap["BAD"]          = RED_CELL_COLOR;
    colorMap["GOOD"]         = WHITE_CELL_COLOR;
    
    mcell = MonitorCellMapped::makeCell(colWidth, display.cms().bima(iAnt).antennaCommon().calibrator().calState(), stateMap);
    castMapped(mcell)->setColorMap(colorMap);
    col.push_back(mcell);

    //------------------------------------------------------------
    // Mount offsets
    //------------------------------------------------------------

    // Convert AZ offset to percent of beamwidth at the normalization frequency
    precision = 0;
    scell = MonitorCellScaled::makeCell(colWidth, 
				display.cms().bima(iAnt).antennaCommon().drive().point().mountOffsetAz(), 
				100 * 1.0/fwhm.arcmin(), offset, precision);

    // Fractional beamwidth scales as the frequency

    castScaled(scell)->scaleByFrequency(OPER_MULT, freqNorm);

    // Flag if this becomes greater than 25% of the beamwidth

    castScaled(scell)->errorOnAbsDeviationFromVal(0.0, 25);
    castScaled(scell)->setAntenna(ant);
    col.push_back(scell);

    // Convert EL offset to percent of beamwidth at the normalization frequency

    scell = MonitorCellScaled::makeCell(colWidth, 
			    display.cms().bima(iAnt).antennaCommon().drive().point().mountOffsetEl(),
				100 * 1.0/fwhm.arcmin(), offset, precision);
    // Fractional beamwidth scales as the frequency

    castScaled(scell)->scaleByFrequency(OPER_MULT, freqNorm);

    // Flag if this becomes greater than 25% of the beamwidth

    castScaled(scell)->errorOnAbsDeviationFromVal(0.0, 25.0);
    castScaled(scell)->setAntenna(ant);
    col.push_back(scell);

    //------------------------------------------------------------
    // Shadowed state
    //------------------------------------------------------------

    BOOLEAN_MAPPED_MP(display.cms().astro().antenna(iAnt).shadowed(),   "FALSE", "TRUE", 
		      WHITE_CELL_COLOR, RED_CELL_COLOR);

  }

  //------------------------------------------------------------
  // Finally, add all columns
  //------------------------------------------------------------

  for(unsigned iCol=0; iCol < col.size(); iCol++) {
    table->add(col[iCol]);
  }

  return table;
}

