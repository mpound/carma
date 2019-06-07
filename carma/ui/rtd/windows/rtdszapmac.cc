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
   
static SZA_COL_ADD_FN(getPmacPmacColumn);
static SZA_LABEL_FN(getPmacPmacLabels);

static SZA_COL_ADD_FN(getPmacMotorsColumn);
static SZA_LABEL_FN(getPmacMotorsLabels);

static SZA_COL_ADD_FN(getPmacInterlocksColumn);
static SZA_LABEL_FN(getPmacInterlocksLabels);

static SZA_CONTAINER_FN(getPmacContainer);
std::string makeHelp();

//------------------------------------------------------------
// Main -- serve tracker data
//------------------------------------------------------------

int Program::main() 
{
  // Create a dislay

  MonitorDisplay display("3.5m Antenna Pmac Status");
  display.setSpecificHelp("3.5m Pmac Help", makeHelp());

  SzaRtdUtils::addFolder(display, "pmac",       getPmacContainer, getPmacPmacColumn,       getPmacPmacLabels, 15);
  SzaRtdUtils::addFolder(display, "motors",     getPmacContainer, getPmacMotorsColumn,     getPmacMotorsLabels, 15);
  SzaRtdUtils::addFolder(display, "interlocks", getPmacContainer, getPmacInterlocksColumn, getPmacInterlocksLabels, 15);

  // Loop forever serving data to the client

  while (display.serveData()) {}

  return 0;
}

//------------------------------------------------------------
// Define labels for the tracker monitor points
//------------------------------------------------------------

SZA_LABEL_FN(getPmacPmacLabels)
{
  std::vector<std::string> labels;

  labels.push_back("AZ actual position");
  labels.push_back("EL actual position");

  labels.push_back("AZ error (arcmin)");
  labels.push_back("EL error (arcmin)");

  labels.push_back("-AZ soft limit");
  labels.push_back("+AZ soft limit");

  labels.push_back("-EL soft limit");
  labels.push_back("+EL soft limit");

  labels.push_back("AZ fatal fol err");
  labels.push_back("EL fatal fol err");

  labels.push_back("AZ load enc");
  labels.push_back("EL load enc");

  return labels;
}


SZA_LABEL_FN(getPmacInterlocksLabels)
{
  std::vector<std::string> labels;

  labels.push_back("+24V");
  labels.push_back("Az CCW");
  labels.push_back("Az CW");

  labels.push_back("El Horizon");
  labels.push_back("El Zenith");
  labels.push_back("HL Relay");

  labels.push_back("Ground E-stop");
  labels.push_back("Platform E-stop");

  labels.push_back("UMAC WD");
  labels.push_back("Lockout keyswitch");

  labels.push_back("AZ sector switch");
  labels.push_back("Local/remote");

  return labels;
}

SZA_LABEL_FN(getPmacMotorsLabels)
{
  std::vector<std::string> labels;

  labels.push_back("AZ motor position");
  labels.push_back("EL motor position");

  labels.push_back("AZ amp enabled");
  labels.push_back("EL amp enabled");

  labels.push_back("AZ servo loop");
  labels.push_back("EL servo loop");

  labels.push_back("AZ amp fault");
  labels.push_back("EL amp fault");

  labels.push_back("AZ Phase ref");
  labels.push_back("EL Phase ref");

  labels.push_back("AZ amp error code");
  labels.push_back("EL amp error code");

  labels.push_back("AZ amp temp (C)");
  labels.push_back("EL amp temp (C)");

  labels.push_back("AZ commanded quad I (Amps)");
  labels.push_back("EL commanded quad I (Amps)");

  labels.push_back("AZ actual quad I (Amps)");
  labels.push_back("EL actual quad I (Amps)");

  labels.push_back("AZ commanded A (Amps)");
  labels.push_back("EL commanded A (Amps)");

  labels.push_back("AZ actual A (Amps)");
  labels.push_back("EL actual A (Amps)");

  labels.push_back("AZ commanded B (Amps)");
  labels.push_back("EL commanded B (Amps)");

  labels.push_back("AZ actual B (Amps)");
  labels.push_back("EL actual B (Amps)");

  labels.push_back("AZ commanded C (Amps)");
  labels.push_back("EL commanded C (Amps)");

  labels.push_back("AZ res abs");
  labels.push_back("EL res abs");

  labels.push_back("AZ res inc");
  labels.push_back("EL res inc");

  labels.push_back("AZ res inc err");
  labels.push_back("EL res inc err");

  return labels;
}

//------------------------------------------------------------
// Return the tracker container
//------------------------------------------------------------

SZA_CONTAINER_FN(getPmacContainer)
{
  return &subsystem;
}

//------------------------------------------------------------
// Define Monitor points for the Pmacs
//------------------------------------------------------------

SZA_COL_ADD_FN(getPmacPmacColumn)
{
  std::vector<MonitorCellPtr> col;

  SzaSubsystem* sza = (SzaSubsystem*)container;
  carma::monitor::Pmac*    pmac    = &sza->pmac();

  col.push_back(MonitorCellScaled::makeCell(colWidth, pmac->az_pos()));
  col.push_back(MonitorCellScaled::makeCell(colWidth, pmac->el_pos()));

  col.push_back(MonitorCellScaled::makeCell(colWidth, pmac->az_err(), 0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, pmac->el_err(), 0.001));

  BINARY_BIT_MAPPED_MP(pmac->axis_stat(), 0, "RELEASED", "SET",     WHITE_CELL_COLOR, RED_CELL_COLOR);
  BINARY_BIT_MAPPED_MP(pmac->axis_stat(), 1, "RELEASED", "SET",     WHITE_CELL_COLOR, RED_CELL_COLOR);
  
  BINARY_BIT_MAPPED_MP(pmac->axis_stat(), 2, "RELEASED", "SET",     WHITE_CELL_COLOR, RED_CELL_COLOR);
  BINARY_BIT_MAPPED_MP(pmac->axis_stat(), 3, "RELEASED", "SET",     WHITE_CELL_COLOR, RED_CELL_COLOR);
  
  BINARY_BIT_MAPPED_MP(pmac->axis_stat(), 6, "OK",       "TRIPPED", WHITE_CELL_COLOR, RED_CELL_COLOR);
  BINARY_BIT_MAPPED_MP(pmac->axis_stat(), 7, "OK",       "TRIPPED", WHITE_CELL_COLOR, RED_CELL_COLOR);
  
  col.push_back(MonitorCellScaled::makeCell(colWidth, pmac->enc_conv_tab(0)));
  col.push_back(MonitorCellScaled::makeCell(colWidth, pmac->enc_conv_tab(1)));

  return col;
}

SZA_COL_ADD_FN(getPmacInterlocksColumn)
{
  std::vector<MonitorCellPtr> col;

  SzaSubsystem* sza = (SzaSubsystem*)container;
  carma::monitor::Pmac*    pmac    = &sza->pmac();

  BINARY_BIT_MAPPED_MP(pmac->axis_stat(), 16, "OK",       "BAD",   WHITE_CELL_COLOR, RED_CELL_COLOR);

  BINARY_BIT_MAPPED_MP(pmac->axis_stat(), 17, "RELEASED", "SET",   WHITE_CELL_COLOR, RED_CELL_COLOR);
  BINARY_BIT_MAPPED_MP(pmac->axis_stat(), 18, "RELEASED", "SET",   WHITE_CELL_COLOR, RED_CELL_COLOR);

  BINARY_BIT_MAPPED_MP(pmac->axis_stat(), 19, "RELEASED", "SET",   WHITE_CELL_COLOR, RED_CELL_COLOR);
  BINARY_BIT_MAPPED_MP(pmac->axis_stat(), 20, "RELEASED", "SET",   WHITE_CELL_COLOR, RED_CELL_COLOR);
  BINARY_BIT_MAPPED_MP(pmac->axis_stat(), 21, "CLOSED",   "OPEN",  WHITE_CELL_COLOR, RED_CELL_COLOR);

  BINARY_BIT_MAPPED_MP(pmac->axis_stat(), 22, "CLOSED",   "OPEN",  WHITE_CELL_COLOR, RED_CELL_COLOR);
  BINARY_BIT_MAPPED_MP(pmac->axis_stat(), 23, "CLOSED",   "OPEN",  WHITE_CELL_COLOR, RED_CELL_COLOR);

  BINARY_BIT_MAPPED_MP(pmac->axis_stat(), 24, "CLOSED",   "OPEN",  WHITE_CELL_COLOR, RED_CELL_COLOR);
  BINARY_BIT_MAPPED_MP(pmac->axis_stat(), 25, "CLOSED",   "OPEN",  WHITE_CELL_COLOR, RED_CELL_COLOR);

  col.push_back(MonitorCellMapped::makeCell(colWidth, pmac->axis_stat(), 8, "CCW", "CW"));
  BINARY_BIT_MAPPED_MP(pmac->axis_stat(),  9, "remote",   "local", WHITE_CELL_COLOR, RED_CELL_COLOR);

  return col;
}

SZA_COL_ADD_FN(getPmacMotorsColumn)
{
  std::vector<MonitorCellPtr> col;

  SzaSubsystem* sza = (SzaSubsystem*)container;
  carma::monitor::Pmac*    pmac    = &sza->pmac();

  col.push_back(MonitorCellScaled::makeCell(colWidth, pmac->mtr_pos(0)));
  col.push_back(MonitorCellScaled::makeCell(colWidth, pmac->mtr_pos(1)));

  BINARY_BIT_MAPPED_MP(pmac->mtr_stat(), 0, "DISABLED", "ENABLED", RED_CELL_COLOR,   WHITE_CELL_COLOR);
  BINARY_BIT_MAPPED_MP(pmac->mtr_stat(), 1, "DISABLED", "ENABLED", RED_CELL_COLOR,   WHITE_CELL_COLOR);

  BINARY_BIT_MAPPED_MP(pmac->mtr_stat(), 2, "CLOSED",   "OPEN",    WHITE_CELL_COLOR, RED_CELL_COLOR);
  BINARY_BIT_MAPPED_MP(pmac->mtr_stat(), 3, "CLOSED",   "OPEN",    WHITE_CELL_COLOR, RED_CELL_COLOR);

  BINARY_BIT_MAPPED_MP(pmac->mtr_stat(), 4, "FAULT",    "OK",      RED_CELL_COLOR,   WHITE_CELL_COLOR);
  BINARY_BIT_MAPPED_MP(pmac->mtr_stat(), 5, "FAULT",    "OK",      RED_CELL_COLOR,   WHITE_CELL_COLOR);

  BINARY_BIT_MAPPED_MP(pmac->mtr_stat(), 6, "OK",       "ERROR",   WHITE_CELL_COLOR, RED_CELL_COLOR);
  BINARY_BIT_MAPPED_MP(pmac->mtr_stat(), 7, "OK",       "ERROR",   WHITE_CELL_COLOR, RED_CELL_COLOR);

  col.push_back(MonitorCellScaled::makeCell(colWidth, pmac->amp_err_code(0), 1.0, 0.0, 0));
  col.push_back(MonitorCellScaled::makeCell(colWidth, pmac->amp_err_code(1), 1.0, 0.0, 0));

  col.push_back(MonitorCellScaled::makeCell(colWidth, pmac->amp_temp(0), 1.0/100));
  col.push_back(MonitorCellScaled::makeCell(colWidth, pmac->amp_temp(1), 1.0/100));

  col.push_back(MonitorCellScaled::makeCell(colWidth, pmac->mtr_com_i(0), 1.0/1000));
  col.push_back(MonitorCellScaled::makeCell(colWidth, pmac->mtr_com_i(1), 1.0/1000));

  col.push_back(MonitorCellScaled::makeCell(colWidth, pmac->mtr_mon_i(0), 1.0/1000));
  col.push_back(MonitorCellScaled::makeCell(colWidth, pmac->mtr_mon_i(1), 1.0/1000));

  col.push_back(MonitorCellScaled::makeCell(colWidth, pmac->mtr_com_i_phase_az(0), 1.0/1000));
  col.push_back(MonitorCellScaled::makeCell(colWidth, pmac->mtr_com_i_phase_el(0), 1.0/1000));

  col.push_back(MonitorCellScaled::makeCell(colWidth, pmac->mtr_i_mon_phase_az(0), 1.0/1000));
  col.push_back(MonitorCellScaled::makeCell(colWidth, pmac->mtr_i_mon_phase_el(0), 1.0/1000));

  col.push_back(MonitorCellScaled::makeCell(colWidth, pmac->mtr_com_i_phase_az(1), 1.0/1000));
  col.push_back(MonitorCellScaled::makeCell(colWidth, pmac->mtr_com_i_phase_el(1), 1.0/1000));

  col.push_back(MonitorCellScaled::makeCell(colWidth, pmac->mtr_i_mon_phase_az(1), 1.0/1000));
  col.push_back(MonitorCellScaled::makeCell(colWidth, pmac->mtr_i_mon_phase_el(1), 1.0/1000));

  col.push_back(MonitorCellScaled::makeCell(colWidth, pmac->mtr_com_i_phase_az(2), 1.0/1000));
  col.push_back(MonitorCellScaled::makeCell(colWidth, pmac->mtr_com_i_phase_el(2), 1.0/1000));

  col.push_back(MonitorCellScaled::makeCell(colWidth, pmac->res_abs(0)));
  col.push_back(MonitorCellScaled::makeCell(colWidth, pmac->res_abs(1)));

  col.push_back(MonitorCellScaled::makeCell(colWidth, pmac->res_inc_raw(0)));
  col.push_back(MonitorCellScaled::makeCell(colWidth, pmac->res_inc_raw(1)));

  BINARY_BIT_MAPPED_MP(pmac->res_inc_cnt_err(), 0, "OK", "ERROR", WHITE_CELL_COLOR, RED_CELL_COLOR);
  BINARY_BIT_MAPPED_MP(pmac->res_inc_cnt_err(), 1, "OK", "ERROR", WHITE_CELL_COLOR, RED_CELL_COLOR);

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

