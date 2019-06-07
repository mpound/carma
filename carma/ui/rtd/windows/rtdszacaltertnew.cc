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

  programLogInfo("EML RTD CALTERT 0");
  SzaRtdUtils::addCanModule(display, "caltert", getCaltertContainer, getCaltertColumn,  getCaltertLabels, 20);
  programLogInfo("EML RTD CALTERT 1");

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

  // BFMP1

  labels.push_back("Tertiary State");
  labels.push_back("CCW Dir Limit");
  labels.push_back("CW  Dir Limit");
  labels.push_back("CCW Ult Limit");
  labels.push_back("CW  Ult Limit");
  labels.push_back("Stepper fault");
  labels.push_back("Stepper disabled");
  labels.push_back("Data Valid");

  // BFMP2

  labels.push_back("Tertiary Position (degrees)");
  labels.push_back("Encoder Index");
  labels.push_back("Encoder Fault");

  labels.push_back("Calibrator State");
  labels.push_back("Load temperature (C)");
  labels.push_back("Cal ID");

  // BFMP3

  labels.push_back("Cal Out");
  labels.push_back("Cal In");
  labels.push_back("Cal Out Lim");
  labels.push_back("Cal In Lim");
  labels.push_back("Cal Drive Fault");
  labels.push_back("Cal Disabled");
  labels.push_back("Cal Done");
  labels.push_back("Cal Fault");

  // BFMP4

  labels.push_back("Module Temp (C)");
  labels.push_back("24 V In (V)");
  labels.push_back("24 V Out (V)");
  labels.push_back("+5 V PSU (V)");

  // BFMP5

  labels.push_back("Pos Err (Degrees)");
  labels.push_back("FPGA Version");

  // Ctl register bits

  labels.push_back("Driver enabled bit");
  labels.push_back("Step enabled bit");
  labels.push_back("Direction bit");
  labels.push_back("Home bit");
  labels.push_back("Reset fault bit");
  labels.push_back("Set acquired LED bit");
  labels.push_back("Select stepper frequency bit 1");
  labels.push_back("Select stepper frequency bit 2");

  std::ostringstream os;
  os << "EML RTD CALTERT labels size = " << labels.size();
  programLogInfo(os.str());

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
  std::map<std::string, std::string> stateMap;

  carma::monitor::Caltert* caltert = (carma::monitor::Caltert*)container;

  // Module state

  CAN_RECEIVED_MP(caltert);

  // Tertiary state

  stateMap["0"] = "Idle";
  stateMap["1"] = "Homing";
  stateMap["2"] = "Home";
  stateMap["3"] = "Home error";
  stateMap["4"] = "Moving";
  stateMap["5"] = "1-cm Rx selected";
  stateMap["6"] = "3-mm Rx selected";
  stateMap["7"] = "1-mm Rx selected";
  stateMap["8"] = "Manual position";
  stateMap["9"] = "Stuck";

  colorMap["Idle"]             = WHITE_CELL_COLOR;
  colorMap["Homing"]           = WHITE_CELL_COLOR;
  colorMap["Home"]             = WHITE_CELL_COLOR;
  colorMap["Home error"]       = RED_CELL_COLOR;
  colorMap["Moving"]           = WHITE_CELL_COLOR;
  colorMap["1-cm Rx selected"] = WHITE_CELL_COLOR;
  colorMap["3-mm Rx selected"] = WHITE_CELL_COLOR;
  colorMap["1-mm Rx selected"] = WHITE_CELL_COLOR;
  colorMap["Manual position"]  = YELLOW_CELL_COLOR;
  colorMap["Stuck"]            = RED_CELL_COLOR;

  cell = MonitorCellMapped::makeCell(colWidth, caltert->tertState(), stateMap);
  cell->setColorMap(colorMap);
  col.push_back(cell);

  // CCW Dir Limit

  BINARY_MAPPED_MP(caltert->ccwDirLim(),"Not at limit", "Limit reached", WHITE_CELL_COLOR, RED_CELL_COLOR);

  // CW Dir Limit

  BINARY_MAPPED_MP(caltert->cwDirLim(), "Not at limit", "Limit reached", WHITE_CELL_COLOR, RED_CELL_COLOR);

  // CCW Ult Limit

  BINARY_MAPPED_MP(caltert->ccwUltLim(),"Not at limit", "Limit reached", WHITE_CELL_COLOR, RED_CELL_COLOR);

  // CW Ult Limit

  BINARY_MAPPED_MP(caltert->cwUltLim(),"Not at limit", "Limit reached", WHITE_CELL_COLOR, RED_CELL_COLOR);

  // Stepper fault

  BINARY_MAPPED_MP(caltert->stepFault(),"No stepper fault", "Stepper fault detected", WHITE_CELL_COLOR, RED_CELL_COLOR);

  // Stepper disabled flag

  BINARY_MAPPED_MP(caltert->stepDisabled(),"Stepper enabled", "Stepper disabled", WHITE_CELL_COLOR, RED_CELL_COLOR);

  // Data Valid

  BINARY_MAPPED_MP(caltert->dataValid(),"Data not valid", "Data valid", RED_CELL_COLOR, WHITE_CELL_COLOR);

  // Tertiary position (angle, in 0.01 degrees)

  col.push_back(MonitorCellScaled::makeCell(colWidth, caltert->tertPos(), 0.01));

  // Encoder index

  BINARY_MAPPED_MP(caltert->encIndex(),"Not at index", "At index mark", WHITE_CELL_COLOR, WHITE_CELL_COLOR);

  // Encoder fault flag

  BINARY_MAPPED_MP(caltert->encFault(),"No fault", "Fault", WHITE_CELL_COLOR, RED_CELL_COLOR);

  // Calibrator state

  stateMap.clear();
  colorMap.clear();

  stateMap["0"] = "Idle";
  stateMap["1"] = "Moving";
  stateMap["2"] = "Sky";
  stateMap["3"] = "Ambient load";
  stateMap["4"] = "Stuck";

  colorMap["Idle"]              = WHITE_CELL_COLOR;
  colorMap["Moving"]            = WHITE_CELL_COLOR;
  colorMap["Sky"]               = WHITE_CELL_COLOR;
  colorMap["Ambient load"]      = WHITE_CELL_COLOR;
  colorMap["Stuck"]             = RED_CELL_COLOR;

  cell = MonitorCellMapped::makeCell(colWidth, caltert->calibState(), stateMap);
  cell->setColorMap(colorMap);
  col.push_back(cell);

  // Cal temp

  col.push_back(MonitorCellScaled::makeCell(colWidth, caltert->calibTemp(), 0.01));

  // Cal ID

  col.push_back(MonitorCellScaled::makeCell(colWidth, caltert->calId()));

  // Cal out

  BINARY_MAPPED_MP(caltert->calOut(),   "Not in sky position", "In sky position", WHITE_CELL_COLOR, WHITE_CELL_COLOR);

  // Cal In 

  BINARY_MAPPED_MP(caltert->calIn(),   "Ambient load not in", "Ambient load in", WHITE_CELL_COLOR, WHITE_CELL_COLOR);

  // Cal out lim

  BINARY_MAPPED_MP(caltert->calOutLim(), "Ok",           "At limit",            WHITE_CELL_COLOR, RED_CELL_COLOR);

  // Cal in lim

  BINARY_MAPPED_MP(caltert->calInLim(), "Ok",           "At limit",            WHITE_CELL_COLOR, RED_CELL_COLOR);

  // Drive fault

  BINARY_MAPPED_MP(caltert->calDriveFault(), "Ok",           "Driver fault detected",  WHITE_CELL_COLOR, RED_CELL_COLOR);

  // Cal disable

  BINARY_MAPPED_MP(caltert->calDisable(),    "Normal operation",         "Disabled",   WHITE_CELL_COLOR, RED_CELL_COLOR);

  // Cal done

  BINARY_MAPPED_MP(caltert->calDone(),   "Motor running",         "Motor stopped",    YELLOW_CELL_COLOR, WHITE_CELL_COLOR);

  // Cal Fault

  BINARY_MAPPED_MP(caltert->calFault(),    "Ok", "Fault detected", WHITE_CELL_COLOR, RED_CELL_COLOR);

  //------------------------------------------------------------
  // BFMP4
  //------------------------------------------------------------

  col.push_back(MonitorCellScaled::makeCell(colWidth, caltert->modTemp(), 0.01));
  col.push_back(MonitorCellScaled::makeCell(colWidth, caltert->inPowSup24V(), 0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, caltert->outPowSup24V(), 0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, caltert->powSup5V(), 0.001));

  //------------------------------------------------------------
  // BFMP5
  //------------------------------------------------------------

  col.push_back(MonitorCellScaled::makeCell(colWidth, caltert->tertPosErr(), 0.01));

  col.push_back(MonitorCellScaled::makeCell(colWidth, caltert->fpgaVer(), 1.0, 0.0, 1));

  BINARY_BIT_MAPPED_MP(caltert->ctrlReg(), 0, "0", "1",  WHITE_CELL_COLOR, WHITE_CELL_COLOR); // Driver enabled
  BINARY_BIT_MAPPED_MP(caltert->ctrlReg(), 1, "0", "1",  WHITE_CELL_COLOR, WHITE_CELL_COLOR); // Stepper enabled
  BINARY_BIT_MAPPED_MP(caltert->ctrlReg(), 2, "0", "1",  WHITE_CELL_COLOR, WHITE_CELL_COLOR); // Direction
  BINARY_BIT_MAPPED_MP(caltert->ctrlReg(), 3, "0", "1",  WHITE_CELL_COLOR, WHITE_CELL_COLOR); // Home
  BINARY_BIT_MAPPED_MP(caltert->ctrlReg(), 4, "0", "1",  WHITE_CELL_COLOR, WHITE_CELL_COLOR); // Reset fault
  BINARY_BIT_MAPPED_MP(caltert->ctrlReg(), 5, "0", "1",  WHITE_CELL_COLOR, WHITE_CELL_COLOR); // Set acquired
  BINARY_BIT_MAPPED_MP(caltert->ctrlReg(), 6, "0", "1",  WHITE_CELL_COLOR, WHITE_CELL_COLOR); // Select stepper freq 1
  BINARY_BIT_MAPPED_MP(caltert->ctrlReg(), 7, "0", "1",  WHITE_CELL_COLOR, WHITE_CELL_COLOR); // Select stepper freq 2

  std::ostringstream os;
  os << "EML RTD CALTERT cols size = " << col.size();
  programLogInfo(os.str());

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

