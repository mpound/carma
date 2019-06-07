/*
 * @file
 * 
 * Gets data from the SZA calterts and displays
 *
 * @author Erik Leitch
 */
#include <sstream>
#include <vector>
#include <iostream>

#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorCell.h"
#include "carma/ui/rtd/common/MonitorCellScaled.h"
#include "carma/ui/rtd/common/MonitorCellMapped.h"

#include "carma/ui/rtd/windows/SzaRtdUtils.h"

#include "carma/monitor/SystemStatus.h"
#include "carma/monitor/MonitorPointIterator.h"

#include "carma/services/Global.h"

#include "carma/util/Program.h"
#include "carma/util/programLogging.h"

using namespace std;
using namespace carma::monitor;
using namespace carma::services;
using namespace carma::util;
using namespace carma::ui::rtd;

static SZA_LABEL_FN(getPlaceRowLabels);
static SZA_LABEL_FN(getComputerRowLabels);
static SZA_LABEL_FN(getOverviewRowLabels);

static GENERIC_ROW_FN(getPlaceRows);
static SZA_ROW_ADD_FN(addPlaceRows);

static GENERIC_ROW_FN(getComputerRows);
static SZA_ROW_ADD_FN(addComputerRows);

static GENERIC_ROW_FN(getOverviewRows);
static SZA_ROW_ADD_FN(addOverviewRows);

#define STATUS_MP(mp) \
  {\
   std::map<std::string, std::string> labelMap; \
   labelMap["UNKNOWN"]      = "UNKNOWN";\
   labelMap["OK"]           = "OK";\
   labelMap["TOO_HIGH"]     = "TOO HIGH";\
   labelMap["TOO_LOW"]      = "TOO LOW";\
   labelMap["TOO_LONG"]     = "TOO LONG";\
   labelMap["STATIC_VALUE"] = "STATIC";\
   SzaRtdUtils::cell_ = MonitorCellMapped::makeCell(colWidth, mp, labelMap);\
   SzaRtdUtils::colorMap_.clear();\
   SzaRtdUtils::colorMap_["UNKNOWN"]  = RED_CELL_COLOR;\
   SzaRtdUtils::colorMap_["OK"]       = WHITE_CELL_COLOR;\
   SzaRtdUtils::colorMap_["TOO HIGH"] = RED_CELL_COLOR;\
   SzaRtdUtils::colorMap_["TOO LOW"]  = RED_CELL_COLOR;\
   SzaRtdUtils::colorMap_["TOO LONG"] = RED_CELL_COLOR;\
   SzaRtdUtils::colorMap_["STATIC"]   = RED_CELL_COLOR;\
   SzaRtdUtils::cell_->setColorMap(SzaRtdUtils::colorMap_);\
   col.push_back(SzaRtdUtils::cell_);\
  }
   
std::string makeHelp();

//-----------------------------------------------------------------------
// Macros for overview
//-----------------------------------------------------------------------

#define MAP_OVERVIEW_MPS(ptr)						\
  colWidth = 5;\
  BOOLEAN_MAPPED_MP(ptr->ok(),        "FALSE", "TRUE", RED_CELL_COLOR, WHITE_CELL_COLOR);

#define CHECK_OVERVIEW(SubsystemType)					\
  {\
    carma::monitor::SubsystemType* ptr = dynamic_cast<carma::monitor::SubsystemType*>(container); \
    if(ptr != 0) {							\
      MAP_OVERVIEW_MPS(ptr);						\
      return col;							\
    }\
  }

//-----------------------------------------------------------------------
// Macros for computers
//-----------------------------------------------------------------------

#define MAP_COMPUTER_MPS(ptr)						\
  colWidth = 5;\
  BOOLEAN_MAPPED_MP(ptr->ok(),        "FALSE", "TRUE", RED_CELL_COLOR, WHITE_CELL_COLOR); \
  colWidth = 16;\
  STATUS_MP(ptr->status()); \
  col.push_back(MonitorCellScaled::makeCell(colWidth, ptr->pingTime()));\
  col.push_back(MonitorCell::makeCell(colWidth, ptr->sampleTime()));

#define CHECK_COMPUTER(SubsystemType)					\
  {\
    carma::monitor::SystemStatusSubsystem::SubsystemType* ptr = dynamic_cast<carma::monitor::SystemStatusSubsystem::SubsystemType*>(container); \
    if(ptr != 0) {							\
      MAP_COMPUTER_MPS(ptr);						\
      return col;							\
    }\
  }

//-----------------------------------------------------------------------
// Macros for places
//-----------------------------------------------------------------------

#define MAP_PLACE_MPS(ptr) \
  colWidth = 5;\
  BOOLEAN_MAPPED_MP(ptr->ok(), "FALSE", "TRUE", RED_CELL_COLOR, WHITE_CELL_COLOR);\
  colWidth = 16;\
  STATUS_MP(ptr->status()); \
  colWidth = 25;\
  col.push_back(MonitorCellScaled::makeCell(colWidth, ptr->currentTemperature()));\
  colWidth = 25;\
  col.push_back(MonitorCellScaled::makeCell(colWidth, ptr->currentTemperature(), 9.0/5, 32)); \
  colWidth = 16;\
  col.push_back(MonitorCell::makeCell(colWidth, ptr->sampleTime()));

#define CHECK_PLACE(SubsystemType)					\
  {\
    carma::monitor::SystemStatusSubsystem::SubsystemType* ptr = dynamic_cast<carma::monitor::SystemStatusSubsystem::SubsystemType*>(container);	\
    if(ptr != 0) {							\
      MAP_PLACE_MPS(ptr);						\
      return col;							\
    }									\
  }

//------------------------------------------------------------
// Main -- serve SystemStatus subsystem data
//------------------------------------------------------------

int Program::main() 
{
  // Create a dislay

  MonitorDisplay display("System Status");

  display.setSpecificHelp("System Status Help", makeHelp());

  std::vector<unsigned> colWidths(1);

  colWidths[0] = 5;
  SzaRtdUtils::addFolder(display, "Overview",  getOverviewRows, addOverviewRows, getOverviewRowLabels, colWidths);

  colWidths.resize(4);
  colWidths[0] = 5;
  colWidths[1] = 15;
  colWidths[2] = 15;
  colWidths[3] = 15;
  SzaRtdUtils::addFolder(display, "Places",    getPlaceRows,    addPlaceRows,    getPlaceRowLabels,    colWidths);

  colWidths.resize(3);
  colWidths[0] = 5;
  colWidths[1] = 15;
  colWidths[2] = 15;
  SzaRtdUtils::addFolder(display, "Computers", getComputerRows, addComputerRows, getComputerRowLabels, colWidths);

  // Loop forever serving data to the client
  while (display.serveData()) {}
  return 0;
}

//-----------------------------------------------------------------------
// Functions returning vectors of rows
//-----------------------------------------------------------------------

GENERIC_ROW_FN(getOverviewRows)
{
  carma::monitor::SystemStatusSubsystem& ss = display.cms().systemStatus();

  std::vector<SzaRtdUtils::RtdRow*> rows;

  rows.push_back(new SzaRtdUtils::RtdRow("System Status", ss));
  rows.push_back(new SzaRtdUtils::RtdRow("Places",        ss.places()));
  rows.push_back(new SzaRtdUtils::RtdRow("Computers",     ss.devices().computers()));

  return rows;
}

GENERIC_ROW_FN(getPlaceRows)
{
  carma::monitor::SystemStatusSubsystem::Places& places = display.cms().systemStatus().places();

  std::vector<SzaRtdUtils::RtdRow*> rows;
  
  rows.push_back(new SzaRtdUtils::RtdRow("Correlator Room",          places.correlatorRoom()));
  rows.push_back(new SzaRtdUtils::RtdRow("CF Computer Room UPS",     places.computerRoomCF().upsTemp()));
  rows.push_back(new SzaRtdUtils::RtdRow("CF Computer Room Ambient", places.computerRoomCF().roomTemp()));
  rows.push_back(new SzaRtdUtils::RtdRow("Bldg12 Computer Room",     places.computerRoomBldg12()));
  rows.push_back(new SzaRtdUtils::RtdRow("Generator Room (1)",       places.generatorBldg1()));
  rows.push_back(new SzaRtdUtils::RtdRow("Generator Room (2)",       places.generatorBldg2()));

  return rows;
}

GENERIC_ROW_FN(getComputerRows)
{
  carma::monitor::SystemStatusSubsystem::Computers& cs = display.cms().systemStatus().devices().computers();

  std::vector<SzaRtdUtils::RtdRow*> rows;
  
  rows.push_back(new SzaRtdUtils::RtdRow("Acc",        cs.acc()));
  rows.push_back(new SzaRtdUtils::RtdRow("Boot1",      cs.boot1()));
  rows.push_back(new SzaRtdUtils::RtdRow("Cedarflat1", cs.cedarflat1()));
  rows.push_back(new SzaRtdUtils::RtdRow("Cedarflat3", cs.cedarflat3()));
  rows.push_back(new SzaRtdUtils::RtdRow("Cedarflat4", cs.cedarflat4()));
  rows.push_back(new SzaRtdUtils::RtdRow("Cedarflat5", cs.cedarflat5()));
  rows.push_back(new SzaRtdUtils::RtdRow("Db2",        cs.db2()));

  return rows;
}

//------------------------------------------------------------
// Functions returning Row labels
//------------------------------------------------------------

SZA_LABEL_FN(getOverviewRowLabels)
{
  std::vector<std::string> labels;

  labels.push_back("Ok");

  return labels;
}

SZA_LABEL_FN(getPlaceRowLabels)
{
  std::vector<std::string> labels;

  labels.push_back("Ok");
  labels.push_back("Status");
  labels.push_back("Current Temperature (C)");
  labels.push_back("Current Temperature (F)");
  labels.push_back("Sample Time");

  return labels;
}

SZA_LABEL_FN(getComputerRowLabels)
{
  std::vector<std::string> labels;

  labels.push_back("Ok");
  labels.push_back("Status");
  labels.push_back("Ping Time (s)");
  labels.push_back("Sample Time");

  return labels;
}

/**.......................................................................
 * More-or-less-templatized function for generating a row of monitor
 * points common to all SystemStatus Places containers
 *
 * This returns a row of cells for the specified container
 */
SZA_ROW_ADD_FN(addOverviewRows)
{
  std::vector<MonitorCellPtr> col;

  CHECK_OVERVIEW(SystemStatusSubsystem);
  CHECK_OVERVIEW(SystemStatusSubsystem::Places);
  CHECK_OVERVIEW(SystemStatusSubsystem::Computers);

  return col;
}

/**.......................................................................
 * Method to add rows for places
 */
SZA_ROW_ADD_FN(addPlaceRows)
{
  std::vector<MonitorCellPtr> col;

  CHECK_PLACE(CorrelatorRoom);
  CHECK_PLACE(UpsTemp);
  CHECK_PLACE(RoomTemp);
  CHECK_PLACE(ComputerRoomBldg12);
  CHECK_PLACE(GeneratorBldg1);
  CHECK_PLACE(GeneratorBldg2);

  return col;
}

SZA_ROW_ADD_FN(addComputerRows)
{
  std::vector<MonitorCellPtr> col;

  CHECK_COMPUTER(Acc);
  CHECK_COMPUTER(Boot1);
  CHECK_COMPUTER(Cedarflat1);
  CHECK_COMPUTER(Cedarflat3);
  CHECK_COMPUTER(Cedarflat4);
  CHECK_COMPUTER(Cedarflat5);
  CHECK_COMPUTER(Db2);

  return col;
}

std::string makeHelp() 
{
  ostringstream ost;
  ost << "       SYSTEM STATUS HELP\n\n" 
      << "The System Status subsystem. "
      << "Fill in more info here... ";
  return ost.str();

}
