/*
 * @file
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

  MonitorDisplay display("3.5m Antenna LO Component Status");
  display.setSpecificHelp("3.5m Yig-Tuned Gunn Help", makeHelp());

  SzaRtdUtils::addFolder(display, "lo ref",   SzaRtdUtils::getIntmodContainer,   SzaRtdUtils::getIntmodColumn,   SzaRtdUtils::getIntmodLabels,   10);
  SzaRtdUtils::addFolder(display, "yig",      SzaRtdUtils::getYigContainer,      SzaRtdUtils::getYigColumn,      SzaRtdUtils::getYigLabels,      10);
  SzaRtdUtils::addFolder(display, "gunn 1cm", SzaRtdUtils::getVaractorContainer, SzaRtdUtils::getVaractorColumn, SzaRtdUtils::getVaractorLabels, 10);
  SzaRtdUtils::addFolder(display, "gunn 3mm", SzaRtdUtils::getBiasContainer,     SzaRtdUtils::getBiasColumn,     SzaRtdUtils::getBiasLabels,     10);

  SzaRtdUtils::addXacFolder(display, "lo ref",   SzaRtdUtils::getIntmodContainer);
  SzaRtdUtils::addXacFolder(display, "yig",      SzaRtdUtils::getYigContainer);
  SzaRtdUtils::addXacFolder(display, "gunn 1cm", SzaRtdUtils::getVaractorContainer);
  SzaRtdUtils::addXacFolder(display, "gunn 3mm", SzaRtdUtils::getBiasContainer);

  // Loop forever serving data to the client

  while (display.serveData()) {}

  return 0;
}

std::string makeHelp() 
{
  ostringstream ost;
  ost << "       3.5M ANTENNA YIG-TUNED GUNN HELP\n\n" 
      << "Status of the 3.5m antenna yig-tuned Gunn subsystem. "
      << "Fill in more info here... ";
  return ost.str();

}

