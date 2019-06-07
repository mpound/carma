/*
 * @file
 * 
 * Gets data from the SZA ifmods and displays
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
   
static SZA_COL_ADD_FN(getIfmodColumn);
static SZA_LABEL_FN(getIfmodLabels);
static SZA_CONTAINER_FN(getIfmodContainer);
std::string makeHelp();

//------------------------------------------------------------
// Main -- serve ifmod data
//------------------------------------------------------------

int Program::main() 
{
  // Create a dislay

  MonitorDisplay display("3.5m Antenna IF Module Status");
  display.setSpecificHelp("3.5m Antenna IF Module Help", makeHelp());

  SzaRtdUtils::addCanModule(display, "IF", getIfmodContainer, getIfmodColumn,  getIfmodLabels, 12);

  // Loop forever serving data to the client

  while (display.serveData()) {}

  return 0;
}

//------------------------------------------------------------
// Define labels for the ifmod monitor points
//------------------------------------------------------------

SZA_LABEL_FN(getIfmodLabels)
{
  std::vector<std::string> labels;

  labels.push_back("State");
  labels.push_back("IfOutTotalPower (mW)");
  labels.push_back("PamTemp (C)");
  labels.push_back("AttenSet (dB)");
  labels.push_back("PamStat");
  labels.push_back("IfSwitchStat");
  labels.push_back("LaserStat");
  labels.push_back("ErrorCount");
  labels.push_back("LaserOpticalPower");
  labels.push_back("LaserTemp (V)");
  labels.push_back("SetInputAtten (dB)");
  labels.push_back("SetOutputAtten (dB)");
  //  labels.push_back("LaserIdTop");
  //  labels.push_back("LaserIdBot");

  return labels;
}

//------------------------------------------------------------
// Return the ifmod container
//------------------------------------------------------------

SZA_CONTAINER_FN(getIfmodContainer)
{
  return &subsystem.ifmod();
}

//------------------------------------------------------------
// Define Monitor points for the Ifmods
//------------------------------------------------------------

SZA_COL_ADD_FN(getIfmodColumn)
{
  std::vector<MonitorCellPtr> col;
  MonitorCellMappedPtr cell;
  std::map<std::string, CellColor> colorMap;

  carma::monitor::Ifmod* ifmod = (carma::monitor::Ifmod*)container;

  CAN_RECEIVED_MP(ifmod);
  col.push_back(MonitorCellScaled::makeCell(colWidth, ifmod->ifTotalPower()));
  col.push_back(MonitorCellScaled::makeCell(colWidth, ifmod->pamTemperature(), 1.0, -273.15));
  col.push_back(MonitorCellScaled::makeCell(colWidth, ifmod->totalAtten()));

  // Pam status

  std::map<std::string, std::string> pamStatusMap;
  pamStatusMap["0"] = "VALID";
  pamStatusMap["1"] = "TEMP IN RANGE (ATTEN CHANGED)";
  pamStatusMap["2"] = "TEMP IN RANGE (IF TOO HIGH)";
  pamStatusMap["3"] = "TEMP IN RANGE (IF TOO LOW)";
  pamStatusMap["4"] = "TEMP OUT OF RANGE";
  pamStatusMap["5"] = "TEMP OUT OF RANGE (ATTEN CHANGED)";
  pamStatusMap["6"] = "TEMP OUT OF RANGE (IF TOO HIGH)";
  pamStatusMap["7"] = "TEMP OUT OF RANGE (IF TOO LOW)";    

  colorMap["VALID"]                             = WHITE_CELL_COLOR;
  colorMap["TEMP IN RANGE (ATTEN CHANGED)"]     = RED_CELL_COLOR;
  colorMap["TEMP IN RANGE (IF TOO HIGH)"]       = RED_CELL_COLOR;
  colorMap["TEMP IN RANGE (IF TOO LOW)"]        = RED_CELL_COLOR;
  colorMap["TEMP OUT OF RANGE"]                 = RED_CELL_COLOR;
  colorMap["TEMP OUT OF RANGE (ATTEN CHANGED)"] = RED_CELL_COLOR;
  colorMap["TEMP OUT OF RANGE (IF TOO HIGH)"]   = RED_CELL_COLOR;
  colorMap["TEMP OUT OF RANGE (IF TOO LOW)"]    = RED_CELL_COLOR;    

  cell = MonitorCellMapped::makeCell(colWidth, ifmod->pamStatus(), pamStatusMap);
  cell->setColorMap(colorMap);
  col.push_back(cell);

  // IF switch state

  cell = MonitorCellMapped::makeCell(colWidth, ifmod->ifSwitchState(),
							 "CHANGED", "POS_1", "POS_2", "POS_3", "POS_4", "STUCK");
  colorMap["CHANGED"] = WHITE_CELL_COLOR;
  colorMap["POS_1"]   = WHITE_CELL_COLOR;
  colorMap["POS_2"]   = WHITE_CELL_COLOR;
  colorMap["POS_3"]   = WHITE_CELL_COLOR;
  colorMap["POS_4"]   = WHITE_CELL_COLOR;
  colorMap["STUCK"]   = RED_CELL_COLOR;
  cell->setColorMap(colorMap);
  col.push_back(cell);

  // Laser status

  cell = MonitorCellMapped::makeCell(colWidth, ifmod->laserStatus(), 
							 "VALID", "POWBAD", "TBAD", "TBAD_POWBAD");
  colorMap["VALID"]       = WHITE_CELL_COLOR;
  colorMap["POWBAD"]      = RED_CELL_COLOR;
  colorMap["TBAD"]        = RED_CELL_COLOR;
  colorMap["TBAD_POWBAD"] = RED_CELL_COLOR;
  cell->setColorMap(colorMap);
  col.push_back(cell);

  // And the rest

  col.push_back(MonitorCellScaled::makeCell(colWidth, ifmod->errorCount()));
  col.push_back(MonitorCellScaled::makeCell(colWidth, ifmod->laserPower()));
  col.push_back(MonitorCellScaled::makeCell(colWidth, ifmod->laserRegError()));
  col.push_back(MonitorCellScaled::makeCell(colWidth, ifmod->inputAtten()));
  col.push_back(MonitorCellScaled::makeCell(colWidth, ifmod->outputAtten()));
  //  col.push_back(MonitorCellMapped::makeCell(colWidth, ifmod->laserIdTop()));
  //  col.push_back(MonitorCellMapped::makeCell(colWidth, ifmod->laserIdBot()));

  return col;
}

std::string makeHelp() 
{
  ostringstream ost;
  ost << "       3.5M ANTENNA IFMOD HELP\n\n" 
      << "Status of the 3.5m antenna ifmod subsystem. "
      << "Fill in more info here... ";
  return ost.str();

}

