/*
 * @file
 * 
 * Gets data from the SZA trackers and displays
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
   
static SZA_COL_ADD_FN(getTrackerLocationColumn);
static SZA_LABEL_FN(getTrackerLocationLabels);

static SZA_COL_ADD_FN(getTrackerPointingColumn);
static SZA_LABEL_FN(getTrackerPointingLabels);

static SZA_COL_ADD_FN(getTrackerTrackingColumn);
static SZA_LABEL_FN(getTrackerTrackingLabels);

static SZA_COL_ADD_FN(getTrackerLackingColumn);
static SZA_LABEL_FN(getTrackerLackingLabels);

static SZA_CONTAINER_FN(getTrackerContainer);
std::string makeHelp();

//------------------------------------------------------------
// Main -- serve tracker data
//------------------------------------------------------------

int Program::main() 
{
  // Create a dislay

  MonitorDisplay display("3.5m Antenna Tracker Status");
  display.setSpecificHelp("3.5m Tracker Help", makeHelp());

  SzaRtdUtils::addFolder(display, "tracking", getTrackerContainer, getTrackerTrackingColumn,  getTrackerTrackingLabels, 15);
  SzaRtdUtils::addFolder(display, "location", getTrackerContainer, getTrackerLocationColumn,  getTrackerLocationLabels, 15);
  SzaRtdUtils::addFolder(display, "pointing", getTrackerContainer, getTrackerPointingColumn,  getTrackerPointingLabels, 15);
  SzaRtdUtils::addFolder(display, "lacking",  getTrackerContainer, getTrackerLackingColumn,   getTrackerLackingLabels,  15);

  // Loop forever serving data to the client

  while (display.serveData()) {}

  return 0;
}

//------------------------------------------------------------
// Define labels for the tracker monitor points
//------------------------------------------------------------

SZA_LABEL_FN(getTrackerLocationLabels)
{
  std::vector<std::string> labels;

  labels.push_back("Reference latitude");
  labels.push_back("Reference longitude");
  labels.push_back("Reference altitude (m)");

  labels.push_back("East (m)");
  labels.push_back("North (m)");
  labels.push_back("Up (m)");

  labels.push_back("Actual latitude");
  labels.push_back("Actual longitude");
  labels.push_back("Actual altitude (m)");

  return labels;
}


SZA_LABEL_FN(getTrackerTrackingLabels)
{
  std::vector<std::string> labels;

  labels.push_back("Tracker Mode");
  labels.push_back("Source");
  labels.push_back("Pmac program");

  labels.push_back("Az encoder cal (cnt/revolution)");
  labels.push_back("El encoder cal (cnt/revolution)");

  labels.push_back("-Az encoder limit (degrees)");
  labels.push_back("+Az encoder limit (degrees)");

  labels.push_back("-El encoder limit (degrees)");
  labels.push_back("+El encoder limit (degrees)");

  labels.push_back("Az encoder offset (degrees)");
  labels.push_back("El encoder offset (degrees)");

  labels.push_back("Az rates (degrees/sec)");
  labels.push_back("El rates (degrees/sec)");

  labels.push_back("Az expected position");
  labels.push_back("Az actual position");
  labels.push_back("Az position error");

  labels.push_back("El expected position");
  labels.push_back("El actual position");
  labels.push_back("El position error");

  labels.push_back("Az tracking offset");
  labels.push_back("El tracking offset");

  labels.push_back("RA tracking offset");
  labels.push_back("Dec tracking offset");

  labels.push_back("Sky X offset");
  labels.push_back("Sky Y offset");

  return labels;
}

SZA_LABEL_FN(getTrackerPointingLabels)
{
  std::vector<std::string> labels;

  labels.push_back("Pointing model");

  labels.push_back("Hour-angle tilt");
  labels.push_back("Latitude tilt");
  labels.push_back("Elevation tilt");

  labels.push_back("Flexure cos coefficient (degrees)");

  labels.push_back("Az encoder offset (degrees)");
  labels.push_back("El encoder offset (degrees)");

  labels.push_back("Total X Offset (degrees)");
  labels.push_back("Total Y Offset (degrees)");

  // These three should never be populated -- monitoring only

  labels.push_back("Az collimation (degrees)");
  labels.push_back("El collimation (degrees)");
  labels.push_back("Flexure sin coefficient (degrees)");

  return labels;
}

SZA_LABEL_FN(getTrackerLackingLabels)
{
  std::vector<std::string> labels;

  labels.push_back("Site information lacking?");
  labels.push_back("Pad information lacking?");
  labels.push_back("Weather data information lacking?");
  labels.push_back("UT1-UTC ephemeris lacking?");
  labels.push_back("EQNEQX ephemeris lacking?");
  labels.push_back("Encoder calibration lacking?");
  labels.push_back("Encoder limits lacking?");
  labels.push_back("Encoder zeros lacking?");
  labels.push_back("Tilts lacking?");
  labels.push_back("Collimation lacking?");
  labels.push_back("Flexure lacking?");

  return labels;
}

//------------------------------------------------------------
// Return the tracker container
//------------------------------------------------------------

SZA_CONTAINER_FN(getTrackerContainer)
{
  return &subsystem;
}

//------------------------------------------------------------
// Define Monitor points for the Trackers
//------------------------------------------------------------

SZA_COL_ADD_FN(getTrackerTrackingColumn)
{
  std::vector<MonitorCellPtr> col;
  MonitorCellMappedPtr cell;
  std::map<std::string, CellColor> colorMap;

  SzaSubsystem* sza = (SzaSubsystem*)container;
  carma::monitor::Tracker* tracker = &sza->tracker();
  carma::monitor::Pmac*    pmac    = &sza->pmac();

  // Tracking status

  cell = MonitorCellMapped::makeCell(colWidth, tracker->mode(), "track", "slew", "halt", "sync");
  
  colorMap.clear();
  colorMap["track"] = WHITE_CELL_COLOR;
  colorMap["slew"]  = RED_CELL_COLOR;
  colorMap["halt"]  = RED_CELL_COLOR;
  colorMap["sync"]  = RED_CELL_COLOR;

  cell->setColorMap(colorMap);
  col.push_back(cell);

  // Tracking source

  col.push_back(MonitorCellScaled::makeCell(colWidth, tracker->source()));;

  // Drive status

  BINARY_BIT_MAPPED_MP(pmac->drive_status(), 0, "stopped", "running", RED_CELL_COLOR, WHITE_CELL_COLOR);

  // Encoder calibrations

  col.push_back(MonitorCellScaled::makeCell(colWidth, tracker->encoder_mul(0), 1.0, 0.0, 0));
  col.push_back(MonitorCellScaled::makeCell(colWidth, tracker->encoder_mul(1), 1.0, 0.0, 0));

  col.push_back(MonitorCellAngle::makeCell(colWidth, tracker->az_limits(0), 1.0/(206265*1000), 0.0, 3, FORMAT_DECIMAL_DEGREES));
  col.push_back(MonitorCellAngle::makeCell(colWidth, tracker->az_limits(1), 1.0/(206265*1000), 0.0, 3, FORMAT_DECIMAL_DEGREES));

  col.push_back(MonitorCellAngle::makeCell(colWidth, tracker->el_limits(0), 1.0/(206265*1000), 0.0, 3, FORMAT_DECIMAL_DEGREES));
  col.push_back(MonitorCellAngle::makeCell(colWidth, tracker->el_limits(1), 1.0/(206265*1000), 0.0, 3, FORMAT_DECIMAL_DEGREES));

  col.push_back(MonitorCellAngle::makeCell(colWidth, tracker->encoder_off(0), 1.0/(206265*1000), 0.0, 3, FORMAT_DECIMAL_DEGREES));
  col.push_back(MonitorCellAngle::makeCell(colWidth, tracker->encoder_off(1), 1.0/(206265*1000), 0.0, 3, FORMAT_DECIMAL_DEGREES));

  col.push_back(MonitorCellAngle::makeCell(colWidth, tracker->rates(0), 1.0/(206265*1000)));
  col.push_back(MonitorCellAngle::makeCell(colWidth, tracker->rates(1), 1.0/(206265*1000)));

  col.push_back(MonitorCellAngle::makeCell(colWidth, tracker->expected(0), 1.0/(206265*1000)));
  col.push_back(MonitorCellAngle::makeCell(colWidth, tracker->actual(0),   1.0/(206265*1000)));
  col.push_back(MonitorCellAngle::makeCell(colWidth, tracker->errors(0),   1.0/(206265*1000)));

  col.push_back(MonitorCellAngle::makeCell(colWidth, tracker->expected(1), 1.0/(206265*1000)));
  col.push_back(MonitorCellAngle::makeCell(colWidth, tracker->actual(1),   1.0/(206265*1000)));
  col.push_back(MonitorCellAngle::makeCell(colWidth, tracker->errors(1),   1.0/(206265*1000)));

  col.push_back(MonitorCellAngle::makeCell(colWidth, tracker->horiz_off(0), 1.0/(206265*1000)));
  col.push_back(MonitorCellAngle::makeCell(colWidth, tracker->horiz_off(1), 1.0/(206265*1000)));

  col.push_back(MonitorCellAngle::makeCell(colWidth, tracker->equat_off(0), 1.0/(206265*1000), 0.0, 3, FORMAT_HMS));
  col.push_back(MonitorCellAngle::makeCell(colWidth, tracker->equat_off(1), 1.0/(206265*1000)));

  col.push_back(MonitorCellAngle::makeCell(colWidth, tracker->sky_xy_off(0), 1.0/(206265*1000)));
  col.push_back(MonitorCellAngle::makeCell(colWidth, tracker->sky_xy_off(1), 1.0/(206265*1000)));

  return col;
}

SZA_COL_ADD_FN(getTrackerLocationColumn)
{
  std::vector<MonitorCellPtr> col;

  SzaSubsystem* sza = (SzaSubsystem*)container;
  carma::monitor::Tracker* tracker = &sza->tracker();

  col.push_back( MonitorCellAngle::makeCell(colWidth, tracker->siteFiducial(1), 1.0/(206265*1000), 0.0));
  col.push_back( MonitorCellAngle::makeCell(colWidth, tracker->siteFiducial(0), 1.0/(206265*1000), 0.0));
  col.push_back(MonitorCellScaled::makeCell(colWidth, tracker->siteFiducial(2), 0.001));

  col.push_back(MonitorCellScaled::makeCell(colWidth, tracker->location(1), 0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, tracker->location(2), 0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, tracker->location(0), 0.001));

  col.push_back( MonitorCellAngle::makeCell(colWidth, tracker->siteActual(1), 1.0/(206265*1000), 0.0));
  col.push_back( MonitorCellAngle::makeCell(colWidth, tracker->siteActual(0), 1.0/(206265*1000), 0.0));
  col.push_back(MonitorCellScaled::makeCell(colWidth, tracker->siteActual(2), 0.001));


  return col;
}

SZA_COL_ADD_FN(getTrackerPointingColumn)
{
  std::vector<MonitorCellPtr> col;
  MonitorCellMappedPtr cell;
  std::map<std::string, CellColor> colorMap;

  SzaSubsystem* sza = (SzaSubsystem*)container;
  carma::monitor::Tracker* tracker = &sza->tracker();

  // Which pointing model?

  cell = MonitorCellMapped::makeCell(colWidth, tracker->axis(), "OPTICAL", "RADIO");
  colorMap.clear();
  colorMap["RADIO"] = WHITE_CELL_COLOR;
  colorMap["OPTICAL"] = RED_CELL_COLOR;
  cell->setColorMap(colorMap);
  col.push_back(cell);

  // Pointing model terms

  col.push_back(MonitorCellAngle::makeCell(colWidth, tracker->tilts(0), 1.0/(206265*1000), 0.0));
  col.push_back(MonitorCellAngle::makeCell(colWidth, tracker->tilts(1), 1.0/(206265*1000), 0.0));
  col.push_back(MonitorCellAngle::makeCell(colWidth, tracker->tilts(2), 1.0/(206265*1000), 0.0));
  col.push_back(MonitorCellAngle::makeCell(colWidth, tracker->flexure(1), 1.0/(206265*1000)));

  col.push_back(MonitorCellAngle::makeCell(colWidth, tracker->encoder_off(0), 1.0/(206265*1000), 0.0, 3, FORMAT_DECIMAL_DEGREES));
  col.push_back(MonitorCellAngle::makeCell(colWidth, tracker->encoder_off(1), 1.0/(206265*1000), 0.0, 3, FORMAT_DECIMAL_DEGREES));

  col.push_back(MonitorCellAngle::makeCell(colWidth, tracker->sky_xy_off(0), 1.0/(206265*1000)));
  col.push_back(MonitorCellAngle::makeCell(colWidth, tracker->sky_xy_off(1), 1.0/(206265*1000)));

  MonitorCellPtr angleCell = MonitorCellAngle::makeCell(colWidth, tracker->collimation(0), 1.0/(206265*1000));
  angleCell->setColor(LIGHT_GRAY_TEXT_CELL_COLOR);
  col.push_back(angleCell);

  angleCell = MonitorCellAngle::makeCell(colWidth, tracker->collimation(1), 1.0/(206265*1000));
  angleCell->setColor(LIGHT_GRAY_TEXT_CELL_COLOR);
  col.push_back(angleCell);

  angleCell = MonitorCellAngle::makeCell(colWidth, tracker->flexure(0), 1.0/(206265*1000));
  angleCell->setColor(LIGHT_GRAY_TEXT_CELL_COLOR);
  col.push_back(angleCell);

  return col;
}

SZA_COL_ADD_FN(getTrackerLackingColumn)
{
  std::vector<MonitorCellPtr> col;

  SzaSubsystem* sza = (SzaSubsystem*)container;
  carma::monitor::Tracker* tracker = &sza->tracker();

  col.push_back(MonitorCellMapped::makeCell(colWidth, tracker->lacking(),  0, "false", "true"));
  col.push_back(MonitorCellMapped::makeCell(colWidth, tracker->lacking(), 10, "false", "true"));
  col.push_back(MonitorCellMapped::makeCell(colWidth, tracker->lacking(),  1, "false", "true"));
  col.push_back(MonitorCellMapped::makeCell(colWidth, tracker->lacking(),  2, "false", "true"));
  col.push_back(MonitorCellMapped::makeCell(colWidth, tracker->lacking(),  3, "false", "true"));
  col.push_back(MonitorCellMapped::makeCell(colWidth, tracker->lacking(),  4, "false", "true"));
  col.push_back(MonitorCellMapped::makeCell(colWidth, tracker->lacking(),  7, "false", "true"));
  col.push_back(MonitorCellMapped::makeCell(colWidth, tracker->lacking(),  9, "false", "true"));
  col.push_back(MonitorCellMapped::makeCell(colWidth, tracker->lacking(),  5, "false", "true"));
  col.push_back(MonitorCellMapped::makeCell(colWidth, tracker->lacking(),  6, "false", "true"));
  col.push_back(MonitorCellMapped::makeCell(colWidth, tracker->lacking(),  8, "false", "true"));

  // Set up color maps for all lacking cells

  std::map<std::string, CellColor> colorMap;
  colorMap["false"] = WHITE_CELL_COLOR;
  colorMap["true"]  = RED_CELL_COLOR;

  for(unsigned i=0; i < col.size(); i++) {
    MonitorCellPtr cell = col[i];
    MonitorCellMapped *cellmapped = dynamic_cast<MonitorCellMapped *>(cell.get());
    if (cellmapped == NULL)
        continue;

    cellmapped->setColorMap(colorMap);
  }

  return col;
}

std::string makeHelp() 
{
  ostringstream ost;
  ost << "       3.5M ANTENNA TRACKER HELP\n\n" 
      << "Status of the 3.5m antenna tracker subsystem. "
      << "Fill in more info here... ";
  return ost.str();

}

