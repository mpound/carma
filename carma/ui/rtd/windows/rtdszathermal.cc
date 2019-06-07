/*
 * @file
 * 
 * Gets data from the SZA thermals and displays
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
   
static SZA_COL_ADD_FN(getThermalColumn);
static SZA_LABEL_FN(getThermalLabels);
static SZA_CONTAINER_FN(getThermalContainer);
std::string makeHelp();

//------------------------------------------------------------
// Main -- serve thermal data
//------------------------------------------------------------

int Program::main() 
{
  // Create a dislay

  MonitorDisplay display("3.5m Antenna Thermal Status");
  display.setSpecificHelp("3.5m Thermal Help", makeHelp());

  SzaRtdUtils::addCanModule(display, "thermal", getThermalContainer, getThermalColumn,  getThermalLabels, 15);

  // Loop forever serving data to the client

  while (display.serveData()) {}

  return 0;
}

//------------------------------------------------------------
// Define labels for the thermal monitor points
//------------------------------------------------------------

SZA_LABEL_FN(getThermalLabels)
{
  std::vector<std::string> labels;

  labels.push_back("State");
  labels.push_back("Ebox Set Temperature (C)");
  labels.push_back("Ebox Temperature (C)");
  labels.push_back("Rbox Set Temperature (C)");
  labels.push_back("Rbox Top Temperature (C)");
  labels.push_back("Rbox Bottom Temperature (C)");
  labels.push_back("TEC Voltage (V)");
  labels.push_back("Airflow (%)");
  labels.push_back("Voltage Offset (V)");

  labels.push_back("Ebox control state");
  labels.push_back("Rbox control state");
  labels.push_back("Ebox Temperature Error (C)");
  labels.push_back("Rbox Temperature Error (C)");

  labels.push_back("Ebox Loop Gain");
  labels.push_back("Ebox Int Gain Constant");
  labels.push_back("Ebox Loop Rate Constant");
  labels.push_back("Ebox Int Temp Error (C)");
  labels.push_back("Ebox Bandwidth (Hz)");

  labels.push_back("Rbox Loop Gain");
  labels.push_back("Rbox Int Gain Constant");
  labels.push_back("Rbox Loop Rate Constant");
  labels.push_back("Rbox Int Temp Error (C)");
  labels.push_back("Rbox Bandwidth (Hz)");

  labels.push_back("Module Temperature (C)");
  labels.push_back("+24V Power Supply (V)");
  labels.push_back("PPS Error Message");
  labels.push_back("PPS Output Voltage");
  labels.push_back("PPS Output Current");
  labels.push_back("Circ Prop Constant");

  return labels;
}

//------------------------------------------------------------
// Return the thermal container
//------------------------------------------------------------

SZA_CONTAINER_FN(getThermalContainer)
{
  return &subsystem.thermal();
}

//------------------------------------------------------------
// Define Monitor points for the Thermals
//------------------------------------------------------------

SZA_COL_ADD_FN(getThermalColumn)
{
  std::vector<MonitorCellPtr> col;
  MonitorCellMappedPtr cell;
  std::map<std::string, CellColor> colorMap;

  carma::monitor::Thermal* thermal = (carma::monitor::Thermal*)container;

  //------------------------------------------------------------
  // Add the received state
  //------------------------------------------------------------

  CAN_RECEIVED_MP(thermal);

  col.push_back(MonitorCellScaled::makeCell(colWidth, thermal->eboxSetTemperature(),    0.01));
  col.push_back(MonitorCellScaled::makeCell(colWidth, thermal->eboxTemperature(),       0.01));
  col.push_back(MonitorCellScaled::makeCell(colWidth, thermal->rboxSetTemperature(),    0.01));
  col.push_back(MonitorCellScaled::makeCell(colWidth, thermal->rboxTopTemperature(),    0.01));
  col.push_back(MonitorCellScaled::makeCell(colWidth, thermal->rboxBottomTemperature(), 0.01));
  col.push_back(MonitorCellScaled::makeCell(colWidth, thermal->eboxVoltage(),          0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, thermal->rboxPwmFraction(),        0.1));
  col.push_back(MonitorCellScaled::makeCell(colWidth, thermal->voltageOffset()));

  //------------------------------------------------------------
  // EBOX loop state
  //------------------------------------------------------------

  cell = MonitorCellMapped::makeCell(colWidth, thermal->eboxLoopState(),
							 "OFF", 
							 "LOOP IN RANGE", 
							 "TEMP HIGH (TEC OOR)", 
							 "TEMP HIGH (TEC IR)", 
							 "TEMP LOW (TEC OOR)", 
							 "TEMP LOW (TEC IR)", 
							 "MANUAL");
  colorMap.clear();
  colorMap["OFF"]                 = RED_CELL_COLOR;
  colorMap["LOOP IN RANGE"]       = WHITE_CELL_COLOR;
  colorMap["TEMP HIGH (TEC OOR)"] = RED_CELL_COLOR;
  colorMap["TEMP HIGH (TEC IR)"]  = RED_CELL_COLOR;
  colorMap["TEMP LOW (TEC OOR)"]  = RED_CELL_COLOR;
  colorMap["TEMP LOW (TEC IR)"]   = RED_CELL_COLOR;
  colorMap["MANUAL"]              = YELLOW_CELL_COLOR;
  cell->setColorMap(colorMap);
  col.push_back(cell);

  //------------------------------------------------------------
  // RBOX loop state
  //------------------------------------------------------------

  cell = MonitorCellMapped::makeCell(colWidth, thermal->rboxLoopState(),
							 "OFF", 
							 "LOOP IN RANGE", 
							 "TEMP HIGH (FAN OOR)", 
							 "TEMP HIGH (FAN IR)", 
							 "TEMP LOW (FAN OOR)", 
							 "TEMP LOW (FAN IR)", 
							 "MANUAL");
  colorMap.clear();
  colorMap["OFF"]                 = RED_CELL_COLOR;
  colorMap["LOOP IN RANGE"]       = WHITE_CELL_COLOR;
  colorMap["TEMP HIGH (FAN OOR)"] = RED_CELL_COLOR;
  colorMap["TEMP HIGH (FAN IR)"]  = RED_CELL_COLOR;
  colorMap["TEMP LOW (FAN OOR)"]  = RED_CELL_COLOR;
  colorMap["TEMP LOW (FAN IR)"]   = RED_CELL_COLOR;
  colorMap["MANUAL"]              = YELLOW_CELL_COLOR;
  cell->setColorMap(colorMap);
  col.push_back(cell);

  //------------------------------------------------------------
  // Temperature errors
  //------------------------------------------------------------

  col.push_back(MonitorCellScaled::makeCell(colWidth, thermal->eboxTemperatureError(),     0.01));
  col.push_back(MonitorCellScaled::makeCell(colWidth, thermal->rboxTemperatureError(),     0.01));

  col.push_back(MonitorCellScaled::makeCell(colWidth, thermal->eboxLoopGain(),            0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, thermal->eboxIntGainConstant(),     0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, thermal->eboxLoopRateConstant(),    0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, thermal->eboxIntTemperatureError(), 0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, thermal->eboxLoopBandwidth(),       0.001));

  col.push_back(MonitorCellScaled::makeCell(colWidth, thermal->rboxLoopGain(),            0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, thermal->rboxIntGainConstant(),     0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, thermal->rboxLoopRateConstant(),    0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, thermal->rboxIntTemperatureError(), 0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, thermal->rboxLoopBandwidth(),       0.001));

  col.push_back(MonitorCellScaled::makeCell(colWidth, thermal->moduleTemperature(),        0.01));
  col.push_back(MonitorCellScaled::makeCell(colWidth, thermal->powSup24V(),               0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, thermal->powSupError()));
  col.push_back(MonitorCellScaled::makeCell(colWidth, thermal->powSupVoltage(),           0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, thermal->powSupCurrent(),           0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, thermal->circPropConst()));

  return col;
}

std::string makeHelp() 
{
  ostringstream ost;
  ost << "       3.5M ANTENNA THERMAL HELP\n\n" 
      << "Status of the 3.5m antenna thermal subsystem. "
      << "Fill in more info here... ";
  return ost.str();

}

