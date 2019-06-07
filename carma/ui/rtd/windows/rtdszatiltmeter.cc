/*
 * @file
 * 
 * Gets data from the SZA tiltmeters and displays
 *
 * @author Erik Leitch
 */
#include <sstream>
#include <vector>

#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorCell.h"
#include "carma/ui/rtd/common/MonitorCellScaled.h"
#include "carma/ui/rtd/common/MonitorCellMapped.h"

#include "carma/ui/rtd/windows/SzaRtdUtils.h"

#include "carma/monitor/SzaSubsystem.h"
#include "carma/monitor/MonitorPointIterator.h"

#include "carma/services/Global.h"

#include "carma/util/Program.h"
#include "carma/util/programLogging.h"

using namespace std;
using namespace carma::monitor;
using namespace carma::services;
using namespace carma::util;
using namespace carma::ui::rtd;
   
static SZA_COL_ADD_FN(getTiltmeterColumn);
static SZA_LABEL_FN(getTiltmeterLabels);
static SZA_CONTAINER_FN(getTiltmeterContainer);
std::string makeHelp();

//------------------------------------------------------------
// Main -- serve tiltmeter data
//------------------------------------------------------------

int Program::main() 
{
  // Create a dislay

  MonitorDisplay display("3.5m Antenna Tiltmeter Status");
  display.setSpecificHelp("3.5m Tiltmeter Help", makeHelp());

  SzaRtdUtils::addCanModule(display, "tiltmeter", getTiltmeterContainer, getTiltmeterColumn,  getTiltmeterLabels, 12);

  // Loop forever serving data to the client

  while (display.serveData()) {}

  return 0;
}

//------------------------------------------------------------
// Define labels for the tiltmeter monitor points
//------------------------------------------------------------

SZA_LABEL_FN(getTiltmeterLabels)
{
  std::vector<std::string> labels;

  labels.push_back("State");
  labels.push_back("LR Tilt (arcmin)");
  labels.push_back("AF Tilt (arcmin)");
  labels.push_back("ModTemp (C)");
  labels.push_back("TiltTemp (C)");
  labels.push_back("StructTemp (C)");
  labels.push_back("HeatVoltage (V)");
  labels.push_back("HeatCurrent (A)");
  labels.push_back("LoopState");
  labels.push_back("FracMaxPower (%)");
  labels.push_back("TempDiff (K)");
  labels.push_back("IntegDiff (K s)");
  labels.push_back("LoopGain (%MaxPower/K)");
  labels.push_back("LoopIntegGain (%MaxPower x K/S)");
  labels.push_back("LoopDiffGain (%MaxPower x S/K)");
  labels.push_back("Loopbandwidth (Hz)");
  labels.push_back("Ps24V (V)");
  labels.push_back("PsTiltPos12V (V)");
  labels.push_back("PsTiltNeg12V (V)");
  labels.push_back("PsTilt5V (V)");
  labels.push_back("PsThermalPos12V (V)");
  labels.push_back("PsThermalNeg12V (V)");
  labels.push_back("PsTherm5V (V)");
  labels.push_back("TeepeeTemp (C)");

  return labels;
}

//------------------------------------------------------------
// Return the tiltmeter container
//------------------------------------------------------------

SZA_CONTAINER_FN(getTiltmeterContainer)
{
  return &subsystem.tiltmeter();
}

//------------------------------------------------------------
// Define Monitor points for the Tiltmeters
//------------------------------------------------------------

SZA_COL_ADD_FN(getTiltmeterColumn)
{
  std::vector<MonitorCellPtr> col;
  MonitorCellMappedPtr cell;
  std::map<std::string, CellColor> colorMap;

  carma::monitor::Tiltmeter* tiltmeter = (carma::monitor::Tiltmeter*)container;

  CAN_RECEIVED_MP(tiltmeter);

  col.push_back(MonitorCellScaled::makeCell(colWidth, tiltmeter->lrTilt(),                 0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, tiltmeter->afTilt(),                 0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, tiltmeter->boardTemperature(),       0.01));
  col.push_back(MonitorCellScaled::makeCell(colWidth, tiltmeter->tiltTemp(),               0.01));
  col.push_back(MonitorCellScaled::makeCell(colWidth, tiltmeter->structTemp(),             0.01));
  col.push_back(MonitorCellScaled::makeCell(colWidth, tiltmeter->heaterVoltage(),          0.01));
  col.push_back(MonitorCellScaled::makeCell(colWidth, tiltmeter->heaterCurrent(),          0.01));

  // Loop state MP

  cell = MonitorCellMapped::makeCell(colWidth, tiltmeter->loopState(), 
							 "HEATER_OFF", "NOMINAL", 
							 "TEMP TOO HIGH (HEATER OUT OF RANGE)",
							 "TEMP TOO HIGH (HEATER IN RANGE)",
							 "TEMP TOO LOW (HEATER OUT OF RANGE)",
							 "TEMP TOO LOW (HEATER IN RANGE)");
  colorMap.clear();
  colorMap["HEATER_OFF"] = RED_CELL_COLOR;
  colorMap["NOMINAL"]    = WHITE_CELL_COLOR;
  colorMap["TEMP TOO HIGH (HEATER OUT OF RANGE)"] = RED_CELL_COLOR;
  colorMap["TEMP TOO HIGH (HEATER IN RANGE)"]     = RED_CELL_COLOR;
  colorMap["TEMP TOO LOW (HEATER OUT OF RANGE)"]  = RED_CELL_COLOR;
  colorMap["TEMP TOO LOW (HEATER IN RANGE)"]      = RED_CELL_COLOR;
  cell->setColorMap(colorMap);
  col.push_back(cell);

  // And the rest

  col.push_back(MonitorCellScaled::makeCell(colWidth, tiltmeter->pwrFract(),               0.01));
  col.push_back(MonitorCellScaled::makeCell(colWidth, tiltmeter->tempDiff(),               0.01));
  col.push_back(MonitorCellScaled::makeCell(colWidth, tiltmeter->integDiff(),              0.01));
  col.push_back(MonitorCellScaled::makeCell(colWidth, tiltmeter->loopGain(),               0.1));
  col.push_back(MonitorCellScaled::makeCell(colWidth, tiltmeter->loopIntegration(),        0.1));
  col.push_back(MonitorCellScaled::makeCell(colWidth, tiltmeter->loopDiffGain(),           0.1));
  col.push_back(MonitorCellScaled::makeCell(colWidth, tiltmeter->loopBw(),                 0.01));
  col.push_back(MonitorCellScaled::makeCell(colWidth, tiltmeter->pos24VPsVoltage(),        0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, tiltmeter->pos12VTiltPsVoltage(),    0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, tiltmeter->neg15VTiltPsVoltage(),    0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, tiltmeter->pos5VTiltPsVoltage(),     0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, tiltmeter->pos12VThermalPsVoltage(), 0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, tiltmeter->neg12VThermalPsVoltage(), 0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, tiltmeter->pos5VThermalPsVoltage(),  0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, tiltmeter->teePeeTemp(),             0.01));
  
  return col;
}

std::string makeHelp() 
{
  ostringstream ost;
  ost << "       3.5M ANTENNA TILTMETER HELP\n\n" 
      << "Status of the 3.5m antenna tiltmeter subsystem. "
      << "Fill in more info here... ";
  return ost.str();

}

