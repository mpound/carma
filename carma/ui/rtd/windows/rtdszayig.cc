/*
 * @File
 * 
 * Gets data from the SZA yigs and displays
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
// Main -- serve yig data
//------------------------------------------------------------

int Program::main() 
{
  // Create a dislay

  MonitorDisplay display("3.5m Antenna Yig Status");
  display.setSpecificHelp("3.5m Yig Help", makeHelp());

  SzaRtdUtils::addCanModule(display, "yig", 
			    SzaRtdUtils::getYigContainer, 
			    SzaRtdUtils::getYigColumn,  
			    SzaRtdUtils::getYigLabels, 
			    15);

  // Loop forever serving data to the client

  while (display.serveData()) {}

  return 0;
}


std::string makeHelp() 
{
  ostringstream ost;
  ost << "       3.5M ANTENNA YIG HELP\n\n" 
      << "Status of the 3.5m antenna yig subsystem. "
      << "Fill in more info here... ";
  return ost.str();

}

