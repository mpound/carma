/*
 * @file
 * 
 * Gets data from the SZA intmods and displays
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
   
std::string makeHelp();

//------------------------------------------------------------
// Main -- serve intmod data
//------------------------------------------------------------

int Program::main() 
{
  // Create a dislay

  MonitorDisplay display("3.5m Antenna Interface Module Status");
  display.setSpecificHelp("3.5m Antenna Interface Module Help", makeHelp());

  SzaRtdUtils::addCanModule(display, "interface", SzaRtdUtils::getIntmodContainer, SzaRtdUtils::getIntmodColumn,  SzaRtdUtils::getIntmodLabels, 12);

  // Loop forever serving data to the client

  while (display.serveData()) {}

  return 0;
}

std::string makeHelp() 
{
  ostringstream ost;
  ost << "       3.5M ANTENNA INTMOD HELP\n\n" 
      << "Status of the 3.5m antenna intmod subsystem. "
      << "Fill in more info here... ";
  return ost.str();

}

