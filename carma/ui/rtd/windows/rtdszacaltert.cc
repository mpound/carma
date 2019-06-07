/*
 * @file
 * 
 * Gets data from the SZA calterts and displays
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
   
static SZA_COL_ADD_FN(getCaltertColumn);
static SZA_LABEL_FN(getCaltertLabels);
static SZA_CONTAINER_FN(getCaltertContainer);
std::string makeHelp();

//------------------------------------------------------------
// Main -- serve caltert data
//------------------------------------------------------------

int Program::main() 
{
  // Create a dislay

  MonitorDisplay display("3.5m Antenna Caltert Status");
  display.setSpecificHelp("3.5m Caltert Help", makeHelp());

  SzaRtdUtils::addCanModule(display, "caltert", getCaltertContainer, getCaltertColumn,  getCaltertLabels, 15);

  // Loop forever serving data to the client

  while (display.serveData()) {}

  return 0;
}

//------------------------------------------------------------
// Define labels for the caltert monitor points
//------------------------------------------------------------

SZA_LABEL_FN(getCaltertLabels)
{
  std::vector<std::string> labels;

  labels.push_back("State");
  labels.push_back("Tertiary State");
  labels.push_back("Mirror State");
  labels.push_back("Position Code");
  labels.push_back("Encoder Position (counts)");
  labels.push_back("Calibrator State");
  labels.push_back("Requested calibrator position");
  labels.push_back("Calibrator temperature (K)");
  labels.push_back("Mirror stable?");
  labels.push_back("Cal load stable?");
  labels.push_back("Calibrator State");
  labels.push_back("Stepper State");
  labels.push_back("Encoder State");

  return labels;
}

//------------------------------------------------------------
// Return the caltert container
//------------------------------------------------------------

SZA_CONTAINER_FN(getCaltertContainer)
{
  return &subsystem.caltert();
}

//------------------------------------------------------------
// Define Monitor points for the Calterts
//------------------------------------------------------------

SZA_COL_ADD_FN(getCaltertColumn)
{
  std::vector<MonitorCellPtr> col;
  MonitorCellMappedPtr cell;
  std::map<std::string, CellColor> colorMap;

  std::map<std::string, std::string> tertStateMap;
  tertStateMap["0"] = "In position";
  tertStateMap["1"] = "Moving";
  tertStateMap["2"] = "Homing";
  tertStateMap["3"] = "Stopped";
  tertStateMap["4"] = "Soft Limit (+)";
  tertStateMap["5"] = "Soft Limit (-)";
  tertStateMap["6"] = "Hard Limit";
  tertStateMap["7"] = "Error";

  colorMap["In position"]    = WHITE_CELL_COLOR;
  colorMap["Moving"]         = RED_CELL_COLOR;
  colorMap["Homing"]         = RED_CELL_COLOR;
  colorMap["Stopped"]        = RED_CELL_COLOR;
  colorMap["Soft Limit (+)"] = RED_CELL_COLOR;
  colorMap["Soft Limit (-)"] = RED_CELL_COLOR;
  colorMap["Hard Limit"]     = RED_CELL_COLOR;
  colorMap["Error"]          = RED_CELL_COLOR;

  carma::monitor::Caltert* caltert = (carma::monitor::Caltert*)container;

  CAN_RECEIVED_MP(caltert);

  cell = MonitorCellMapped::makeCell(colWidth, caltert->tertState(), tertStateMap);
  cell->setColorMap(colorMap);
  col.push_back(cell);

  BINARY_MAPPED_MP(caltert->moveMirOk(), "Disabled", "Enabled", YELLOW_CELL_COLOR, WHITE_CELL_COLOR);
  col.push_back(MonitorCellMapped::makeCell(colWidth, caltert->posnCode(),   "30 GHz", "90 GHz", "230 GHz", "Requested position"));
  col.push_back(MonitorCellScaled::makeCell(colWidth, caltert->encPos()));

  cell = MonitorCellMapped::makeCell(colWidth, caltert->calibState(), "Acquired", "Moving", "Hard limit", "Error");
  colorMap.clear();
  colorMap["Acquired"]   = WHITE_CELL_COLOR;
  colorMap["Moving"]     = RED_CELL_COLOR;
  colorMap["Hard limit"] = RED_CELL_COLOR;
  colorMap["Error"]      = RED_CELL_COLOR;
  cell->setColorMap(colorMap);
  col.push_back(cell);

  BINARY_MAPPED_MP(caltert->calibPosReq(), "Sky",            "Ambient",          WHITE_CELL_COLOR, YELLOW_CELL_COLOR);
  col.push_back(MonitorCellScaled::makeCell(colWidth, caltert->calibTemp(), 0.01));
  BINARY_MAPPED_MP(caltert->mirStable(),   "true",           "false",            WHITE_CELL_COLOR, RED_CELL_COLOR);
  BINARY_MAPPED_MP(caltert->calibStable(), "true",           "false",            WHITE_CELL_COLOR, RED_CELL_COLOR);
  BINARY_MAPPED_MP(caltert->calFault(),    "Normal",         "Overheated",       WHITE_CELL_COLOR, RED_CELL_COLOR);
  BINARY_MAPPED_MP(caltert->stepFault(),   "Normal",         "Short curcuit",    WHITE_CELL_COLOR, RED_CELL_COLOR);
  BINARY_MAPPED_MP(caltert->encFault(),    "Valid position", "Invalid position", WHITE_CELL_COLOR, RED_CELL_COLOR);

  return col;
}

std::string makeHelp() 
{
  ostringstream ost;
  ost << "       3.5M ANTENNA CALTERT HELP\n\n" 
      << "Status of the 3.5m antenna caltert subsystem. "
      << "Fill in more info here... ";
  return ost.str();

}

