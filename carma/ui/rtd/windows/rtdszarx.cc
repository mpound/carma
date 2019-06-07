/*
 * @file
 * 
 * Gets data from the SZA rxs and displays
 *
 * @author Erik Leitch
 */
#include <sstream>
#include <vector>

#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorCell.h"
#include "carma/ui/rtd/common/MonitorCellAngle.h"
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
   
static SZA_COL_ADD_FN(getRx30GHzColumn);
static SZA_LABEL_FN(getRx30GHzLabels);

static SZA_COL_ADD_FN(getRx90GHzColumn);
static SZA_LABEL_FN(getRx90GHzLabels);

static SZA_CONTAINER_FN(getRxContainer);
std::string makeHelp();

//------------------------------------------------------------
// Main -- serve rx data
//------------------------------------------------------------

int Program::main() 
{
  // Create a dislay

  MonitorDisplay display("3.5m Antenna Rx Status");
  display.setSpecificHelp("3.5m Rx Help", makeHelp());

  SzaRtdUtils::addFolder(   display, "30 GHz", getRxContainer, getRx30GHzColumn,  getRx30GHzLabels, 15);
  SzaRtdUtils::addFolder(   display, "90 GHz", getRxContainer, getRx90GHzColumn,  getRx90GHzLabels, 15);
  SzaRtdUtils::addXacFolder(display, "rx",     getRxContainer);

  // Loop forever serving data to the client

  while (display.serveData()) {}

  return 0;
}

//------------------------------------------------------------
// Define labels for the rx monitor points
//------------------------------------------------------------

SZA_LABEL_FN(getRx30GHzLabels)
{
  std::vector<std::string> labels;

  labels.push_back("Stage 1 Drain Current (mA)");
  labels.push_back("Stage 2 Drain Current (mA)");
  labels.push_back("Stage 3 Drain Current (mA)");
  labels.push_back("Stage 4 Drain Current (mA)");

  labels.push_back("Stage 1 Gate Voltage (V)");
  labels.push_back("Stage 2 Gate Voltage (V)");
  labels.push_back("Stage 3 Gate Voltage (V)");
  labels.push_back("Stage 4 Gate Voltage (V)");

  labels.push_back("Stage 1 Gate Current (uA)");
  labels.push_back("Stage 2 Gate Current (uA)");
  labels.push_back("Stage 3 Gate Current (uA)");
  labels.push_back("Stage 4 Gate Current (uA)");

  labels.push_back("30 GHz Mixer Current (mA)");
  labels.push_back("30 GHz LED Current   (mA)");

  labels.push_back("Radiation Shield Temperature (K)");
  labels.push_back("Cold Plate Temperature (K)");
  labels.push_back("Cold Head Temperature (K)");
  labels.push_back("Board Temperature (C)");

  labels.push_back("-15V Analog Supply (V)");
  labels.push_back("+5V Digital Supply (V)");
  labels.push_back("-5V Digital Supply (V)");
  labels.push_back("+15V Analog Supply (V)");
  labels.push_back("+24V Analog Supply (V)");

  return labels;
}

SZA_LABEL_FN(getRx90GHzLabels)
{
  std::vector<std::string> labels;

  labels.push_back("Stage 1 Gate Current (uA)");
  labels.push_back("Stage 2 Gate Current (uA)");
  labels.push_back("Stage 3 Gate Current (uA)");
  labels.push_back("Stage 4 Gate Current (uA)");

  labels.push_back("Stage 1 Drain Current (mA)");
  labels.push_back("Stage 2 Drain Current (mA)");
  labels.push_back("IF Amp Drain Current  (mA)");
  labels.push_back("IF Amp Gate Current   (mA)");

  labels.push_back("Radiation Shield Temperature (K)");
  labels.push_back("Cold Plate Temperature (K)");
  labels.push_back("Cold Head Temperature (K)");
  labels.push_back("Board Temperature (C)");

  labels.push_back("+5V Analog Supply   (V)");
  labels.push_back("+5V Digital Supply  (V)");
  labels.push_back("+15V Analog Supply  (V)");
  labels.push_back("+28V Digital Supply (V)");

  return labels;
}

//------------------------------------------------------------
// Return the rx container
//------------------------------------------------------------

SZA_CONTAINER_FN(getRxContainer)
{
  return &subsystem.rx();
}

//------------------------------------------------------------
// Define Monitor points for the Rxs
//------------------------------------------------------------

SZA_COL_ADD_FN(getRx30GHzColumn)
{
  std::vector<MonitorCellPtr> col;

  carma::monitor::Rx* rx = (carma::monitor::Rx*)container;

  col.push_back(MonitorCellScaled::makeCell(colWidth, rx->drainCurrent30GHz(0), 0.01));
  col.push_back(MonitorCellScaled::makeCell(colWidth, rx->drainCurrent30GHz(1), 0.01));
  col.push_back(MonitorCellScaled::makeCell(colWidth, rx->drainCurrent30GHz(2), 0.01));
  col.push_back(MonitorCellScaled::makeCell(colWidth, rx->drainCurrent30GHz(3), 0.01));

  col.push_back(MonitorCellScaled::makeCell(colWidth, rx->gateVoltage30GHz(0), 0.0001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, rx->gateVoltage30GHz(1), 0.0001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, rx->gateVoltage30GHz(2), 0.0001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, rx->gateVoltage30GHz(3), 0.0001));

  col.push_back(MonitorCellScaled::makeCell(colWidth, rx->gateCurrent30GHz(0), 0.1));
  col.push_back(MonitorCellScaled::makeCell(colWidth, rx->gateCurrent30GHz(1), 0.1));
  col.push_back(MonitorCellScaled::makeCell(colWidth, rx->gateCurrent30GHz(2), 0.1));
  col.push_back(MonitorCellScaled::makeCell(colWidth, rx->gateCurrent30GHz(3), 0.1));

  col.push_back(MonitorCellScaled::makeCell(colWidth, rx->mixerCurrent30GHz(), 0.01));
  col.push_back(MonitorCellScaled::makeCell(colWidth, rx->ledCurrent30GHz(),   0.01));

  col.push_back(MonitorCellScaled::makeCell(colWidth, rx->tempRadShield(),          0.01));
  col.push_back(MonitorCellScaled::makeCell(colWidth, rx->tempStage2ColdHead(), 0.01));
  col.push_back(MonitorCellScaled::makeCell(colWidth, rx->temp90GHzIsolator(),      0.01));
  col.push_back(MonitorCellScaled::makeCell(colWidth, rx->boardTemperature(),       0.01));

  col.push_back(MonitorCellScaled::makeCell(colWidth, rx->neg15VAnalogVoltage(), 0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, rx->pos5VDigitalVoltage(), 0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, rx->neg5VDigitalVoltage(), 0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, rx->pos15VAnalogVoltage(), 0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, rx->pos24VAnalogVoltage(), 0.001));

  return col;
}

SZA_COL_ADD_FN(getRx90GHzColumn)
{
  std::vector<MonitorCellPtr> col;

  carma::monitor::Rx* rx = (carma::monitor::Rx*)container;

  col.push_back(MonitorCellScaled::makeCell(colWidth, rx->gateCurrent90GHz(0), 0.1));
  col.push_back(MonitorCellScaled::makeCell(colWidth, rx->gateCurrent90GHz(1), 0.1));
  col.push_back(MonitorCellScaled::makeCell(colWidth, rx->gateCurrent90GHz(2), 0.1));
  col.push_back(MonitorCellScaled::makeCell(colWidth, rx->gateCurrent90GHz(3), 0.1));

  col.push_back(MonitorCellScaled::makeCell(colWidth, rx->drainCurrent90GHz(0),     0.01));
  col.push_back(MonitorCellScaled::makeCell(colWidth, rx->drainCurrent90GHz(1),     0.01));
  col.push_back(MonitorCellScaled::makeCell(colWidth, rx->ifAmpDrainCurrent90GHz(), 0.01));
  col.push_back(MonitorCellScaled::makeCell(colWidth, rx->ifAmpGateCurrent90GHz(),  0.1));

  col.push_back(MonitorCellScaled::makeCell(colWidth, rx->tempSensor(0), 0.01));
  col.push_back(MonitorCellScaled::makeCell(colWidth, rx->tempSensor(1), 0.01));
  col.push_back(MonitorCellScaled::makeCell(colWidth, rx->tempSensor(2), 0.01));
  col.push_back(MonitorCellScaled::makeCell(colWidth, rx->boardTemperature(), 0.01));

  col.push_back(MonitorCellScaled::makeCell(colWidth, rx->pos5VAnalogVoltage(),   0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, rx->pos5VDigitalVoltage(),  0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, rx->pos15VAnalogVoltage(),  0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, rx->pos28VDigitalVoltage(), 0.001));

  return col;
}

std::string makeHelp() 
{
  ostringstream ost;
  ost << "       3.5M ANTENNA RX HELP\n\n" 
      << "Status of the 3.5m antenna rx subsystem. "
      << "Fill in more info here... ";
  return ost.str();

}

