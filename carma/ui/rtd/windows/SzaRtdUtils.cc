#include <iostream>

#include "carma/ui/rtd/windows/SzaRtdUtils.h"

#include "carma/ui/rtd/common/MonitorCellMapped.h"
#include "carma/ui/rtd/common/MonitorCellScaled.h"

#include "carma/monitor/SzaSubsystem.h"

#include "carma/util/programLogging.h"

using namespace std;
using namespace carma::util;
using namespace carma::ui::rtd;
using namespace carma::monitor;

const unsigned SzaRtdUtils::nSza_ = 8;
const unsigned SzaRtdUtils::szaOffsetInCarma_ = 16;

MonitorCellMappedPtr SzaRtdUtils::cell_;
std::map<std::string, CellColor> SzaRtdUtils::colorMap_;

//-----------------------------------------------------------------------
// Textual substitution to allow for more-or-less templatized XAC
// folder writing below
//-----------------------------------------------------------------------

#define MAP_XAC_MPS(col, ptr) \
  CAN_RECEIVED_MP(ptr);\
  col.push_back(MonitorCellScaled::makeCell(colWidth, ptr->serialNo(), 1.0, 0.0, 0)); \
  col.push_back(MonitorCellScaled::makeCell(colWidth, ptr->moduleType(), 1.0, 0.0, 0)); \
  col.push_back(MonitorCellScaled::makeCell(colWidth, ptr->rxErrors(), 1.0, 0.0, 0)); \
  col.push_back(MonitorCellScaled::makeCell(colWidth, ptr->txErrors(), 1.0, 0.0, 0)); \
  col.push_back(MonitorCellScaled::makeCell(colWidth, ptr->memoryErrors(), 1.0, 0.0, 0)); \
  col.push_back(MonitorCellScaled::makeCell(colWidth, ptr->schOverflowCnt(), 1.0, 0.0, 0)); \
  col.push_back(MonitorCellScaled::makeCell(colWidth, ptr->tSchOverflowCnt(), 1.0, 0.0, 0)); \
  col.push_back(MonitorCellScaled::makeCell(colWidth, ptr->swVersionStr()));\
  col.push_back(MonitorCellScaled::makeCell(colWidth, ptr->testMode(), 1.0, 0.0, 0)); \
  col.push_back(MonitorCellScaled::makeCell(colWidth, ptr->commErrCnt(), 1.0, 0.0, 0)); \
  col.push_back(MonitorCellScaled::makeCell(colWidth, ptr->timeErrCnt(), 1.0, 0.0, 0)); \
  col.push_back(MonitorCellScaled::makeCell(colWidth, ptr->swErrCnt(), 1.0, 0.0, 0)); \
  col.push_back(MonitorCellScaled::makeCell(colWidth, ptr->hwErrCnt(), 1.0, 0.0, 0)); \
  col.push_back(MonitorCellScaled::makeCell(colWidth, ptr->timeJitter(), 1.0, 0.0, 0)); \
  col.push_back(MonitorCellScaled::makeCell(colWidth, ptr->sinceLastTs(), 1.0, 0.0, 0)); \
  col.push_back(MonitorCellScaled::makeCell(colWidth, ptr->tsDelta(), 1.0, 0.0, 0)); \
  col.push_back(MonitorCellScaled::makeCell(colWidth, ptr->apiVer()));\
  col.push_back(MonitorCellScaled::makeCell(colWidth, ptr->uptime(), 1.0, 0.0, 0)); \
  BINARY_MAPPED_MP(ptr->bootLoader(), "ABSENT", "PRESENT", RED_CELL_COLOR, WHITE_CELL_COLOR);\
  col.push_back(MonitorCellScaled::makeCell(colWidth, ptr->buildDate())); \
  col.push_back(MonitorCellScaled::makeCell(colWidth, ptr->buildTime()));

/**.......................................................................
 * Constructor.
 */
SzaRtdUtils::SzaRtdUtils() {}

/**.......................................................................
 * Destructor.
 */
SzaRtdUtils::~SzaRtdUtils() {}

/**.......................................................................
 * Add a folder to the display, populated with a table containing data
 * for all SZA antennas
 */
void SzaRtdUtils::addFolder(MonitorDisplay& display, std::string folderName, 
			    SZA_CONTAINER_FN(*conFn), SZA_COL_ADD_FN(*colFn), SZA_LABEL_FN(*labFn), unsigned colWidth)
{
  RtFolderPtr folder(new RtFolder(folderName));
  display.add(folder);
  addTable(display, folder, "Table", conFn, colFn, labFn, colWidth);
}

void SzaRtdUtils::addFolder(MonitorDisplay& display, std::string folderName, 
			    GENERIC_COLUMN_FN(*columnFn), SZA_COL_ADD_FN(*columnAddFn), SZA_LABEL_FN(*rowLabelFn), unsigned colWidth)
{
  RtFolderPtr folder(new RtFolder(folderName));
  display.add(folder);
  addTable(display, folder, "Table", columnFn, columnAddFn, rowLabelFn, colWidth);
}

void SzaRtdUtils::addFolder(MonitorDisplay& display, std::string folderName, 
			    GENERIC_ROW_FN(*rowFn), SZA_ROW_ADD_FN(*rowAddFn), SZA_LABEL_FN(*columnLabelFn), 
			    std::vector<unsigned>& colWidths)
{
  RtFolderPtr folder(new RtFolder(folderName));
  display.add(folder);
  addTable(display, folder, "Table", rowFn, rowAddFn, columnLabelFn, colWidths);
}

void SzaRtdUtils::addFolder(MonitorDisplay& display, std::string folderName, 
			    GENERIC_ROW_FN(*rowFn), SZA_ROW_ADD_FN(*rowAddFn), SZA_LABEL_FN(*columnLabelFn), 
			    unsigned colWidth)
{
  RtFolderPtr folder(new RtFolder(folderName));
  display.add(folder);
  addTable(display, folder, "Table", rowFn, rowAddFn, columnLabelFn, colWidth);
}

/**.......................................................................
 * Add a table to the folder, populated with columns for each SZA
 * antenna
 */
void SzaRtdUtils::addTable(MonitorDisplay& display, RtFolderPtr folder, std::string tableName,
			   SZA_CONTAINER_FN(*conFn), SZA_COL_ADD_FN(*colFn), SZA_LABEL_FN(*labFn), unsigned colWidth)
{
  unsigned nSza = 8;
  unsigned szaOffsetInCarma = 16;

  std::ostringstream os;

  RtTablePtr table(new RtTable(tableName));
  folder->add(table);

  // Add column and row labels

  std::vector<string> labels = (*labFn)();

  for(unsigned iSza=0; iSza < nSza; iSza++) {
    os.str("");
    os << "C" << (iSza + szaOffsetInCarma);
    table->addCol(RtColumn::makeColumn(os.str()));
  }

  for(unsigned iRow=0; iRow < labels.size(); iRow++) {
    table->addRow(RtRow::makeRow(labels[iRow]));
  }

  // Now iterate over SZA antennas, creating a column for each one

  for(unsigned iSza=0; iSza < nSza; iSza++) {
    addColumn(display.cms().sza(iSza), table, conFn, colFn, colWidth);
  }
}

/**.......................................................................
 * Add a table with a list of column labels
 */
void SzaRtdUtils::addTable(MonitorDisplay& display, RtFolderPtr folder, std::string tableName,
			   GENERIC_COLUMN_FN(*columnFn), SZA_COL_ADD_FN(*columnAddFn), SZA_LABEL_FN(*rowLabelFn), unsigned colWidth)
{
  std::ostringstream os;

  RtTablePtr table(new RtTable(tableName));
  folder->add(table);

  //------------------------------------------------------------
  // Get the list of columns
  //------------------------------------------------------------

  std::vector<RtdColumn*> columns = (*columnFn)(display);

  for(unsigned iCol=0; iCol < columns.size(); iCol++) {
    os.str("");
    os << columns[iCol]->name_;
    table->addCol(RtColumn::makeColumn(os.str()));
  }

  //------------------------------------------------------------
  // Add row labels
  //------------------------------------------------------------

  std::vector<string> rowLabels = (*rowLabelFn)();

  for(unsigned iRow=0; iRow < rowLabels.size(); iRow++) {
    table->addRow(RtRow::makeRow(rowLabels[iRow]));
  }

  //------------------------------------------------------------
  // Now iterate over column entries, creating a column for each one
  //------------------------------------------------------------

  for(unsigned iCol=0; iCol < columns.size(); iCol++) {
    addColumn(columns[iCol]->container_, table, columnAddFn, colWidth);
  }
}

/**.......................................................................
 * Add a table with a list of row labels
 */
void SzaRtdUtils::addTable(MonitorDisplay& display, RtFolderPtr folder, std::string tableName,
			   GENERIC_ROW_FN(*rowFn), SZA_ROW_ADD_FN(*rowAddFn), SZA_LABEL_FN(*columnLabelFn), 
			   std::vector<unsigned>& colWidths)
{
  std::ostringstream os;

  RtTablePtr table(new RtTable(tableName));
  folder->add(table);

  //------------------------------------------------------------
  // Add column labels
  //------------------------------------------------------------

  std::vector<RtdRow*> rows = (*rowFn)(display);
  unsigned nRow = rows.size();

  for(unsigned iRow=0; iRow < nRow; iRow++) {
    os.str("");
    os << rows[iRow]->name_;
    table->addRow(RtRow::makeRow(os.str()));
  }

  //------------------------------------------------------------
  // Add column labels
  //------------------------------------------------------------

  std::vector<string> columnLabels = (*columnLabelFn)();
  unsigned nCol = columnLabels.size();

  for(unsigned iCol=0; iCol < nCol; iCol++) {
    table->addCol(RtColumn::makeColumn(columnLabels[iCol]));
  }

  unsigned nCell = nRow*nCol;
  
  std::vector<MonitorCellPtr> cells(nCell);

  for(unsigned iRow=0; iRow < nRow; iRow++) {
    std::vector<MonitorCellPtr> cols = (*rowAddFn)(rows[iRow]->container_, 1);

    // Populate the cell matrix in the order that the RtDisplay wants
    // it

    for(unsigned iCol=0; iCol < nCol; iCol++) {
      unsigned ind = iCol * nRow + iRow;
      cells[ind] = cols[iCol];
    }
  }

  //------------------------------------------------------------
  // Now add all cells to the table
  //------------------------------------------------------------

  for(unsigned iCell=0; iCell < nCell; iCell++)
    table->add(cells[iCell]);
}

/**.......................................................................
 * Add a table with a list of row labels
 */
void SzaRtdUtils::addTable(MonitorDisplay& display, RtFolderPtr folder, std::string tableName,
			   GENERIC_ROW_FN(*rowFn), SZA_ROW_ADD_FN(*rowAddFn), SZA_LABEL_FN(*columnLabelFn), 
			   unsigned colWidth)
{
  std::ostringstream os;

  RtTablePtr table(new RtTable(tableName));
  folder->add(table);

  //------------------------------------------------------------
  // Add column labels
  //------------------------------------------------------------

  std::vector<RtdRow*> rows = (*rowFn)(display);

  for(unsigned iRow=0; iRow < rows.size(); iRow++) {
    os.str("");
    os << rows[iRow]->name_;
    table->addRow(RtRow::makeRow(os.str()));
  }

  //------------------------------------------------------------
  // Add column labels
  //------------------------------------------------------------

  std::vector<string> columnLabels = (*columnLabelFn)();

  for(unsigned iCol=0; iCol < columnLabels.size(); iCol++) {
    table->addCol(RtColumn::makeColumn(columnLabels[iCol]));
  }

  //------------------------------------------------------------
  // Now iterate over row entries.  I'm iterating in this weird way
  // because I believe that the table wants cells added in (column,
  // row) order, i.e., walk down every row of the current column, then
  // move to the next column.
  //------------------------------------------------------------

  for(unsigned iCol=0; iCol < columnLabels.size(); iCol++) {

    // Now iterate down each row of the current column
    for(unsigned iRow=0; iRow < rows.size(); iRow++) {
      std::vector<MonitorCellPtr> cols = (*rowAddFn)(rows[iRow]->container_, colWidth);
      table->add(cols[iCol]);
    }
  }
}

/**.......................................................................
 * Add a column to the table, populated with monitor points for a
 * single SZA antenna
 */
void SzaRtdUtils::addColumn(SzaSubsystem& subsystem, RtTablePtr table,
			    SZA_CONTAINER_FN(*conFn), SZA_COL_ADD_FN(*fn), unsigned colWidth)
{
  // Get the container

  MonitorContainer* container = (*conFn)(subsystem);

  // Get the rows for this column

  std::vector<MonitorCellPtr> rows = (*fn)(container, colWidth);

  // And add them to the table

  for(unsigned iRow=0; iRow < rows.size(); iRow++) {
    table->add(rows[iRow]);
  }
}

/**.......................................................................
 * Add a column to the table, populated with monitor points for a
 * single SZA antenna
 */
void SzaRtdUtils::addColumn(MonitorContainer* container, RtTablePtr table,
			    SZA_COL_ADD_FN(*fn), unsigned colWidth)
{
  // Get the rows for this column

  std::vector<MonitorCellPtr> rows = (*fn)(container, colWidth);

  // And add them to the table

  for(unsigned iRow=0; iRow < rows.size(); iRow++) {
    table->add(rows[iRow]);
  }
}

/**.......................................................................
 * Generate standard labels for XAC monitor points common to all CAN
 * modules
 */
SZA_LABEL_FN(SzaRtdUtils::getXacLabels)
{
  std::vector<std::string> labels;

  labels.push_back("State");
  labels.push_back("Serial#");
  labels.push_back("ModuleType");
  labels.push_back("CanRxErrs");
  labels.push_back("CanTxErrs");
  labels.push_back("MemErrs");
  labels.push_back("SchedOverflows");
  labels.push_back("TimeOverflows");
  labels.push_back("FwVersion");
  labels.push_back("TestMode");
  labels.push_back("CommErrs");
  labels.push_back("TimeErrs");
  labels.push_back("SwErrs");
  labels.push_back("HwErrs");
  labels.push_back("TimeJitter");
  labels.push_back("TimeSinceLastTs (s)");
  labels.push_back("TsDelta (ms)");
  labels.push_back("ApiVer");
  labels.push_back("Uptime (s)");
  labels.push_back("Bootloader");
  labels.push_back("Build Date");
  labels.push_back("Build Time");

  return labels;
}

/**.......................................................................
 * More-or-less-templatized function for generating a column of monitor
 * points common to all SZA CAN modules
 */
SZA_COL_ADD_FN(SzaRtdUtils::getXacColumn)
{
  std::vector<MonitorCellPtr> col;

  // Check for Bias-tuned gunn

  carma::monitor::Bias* const bias = dynamic_cast<carma::monitor::Bias*>(container);
  
  if(bias != 0) {
    MAP_XAC_MPS(col, bias);
    return col;
  }

  // Check for Caltert

  carma::monitor::Caltert* const caltert = dynamic_cast<carma::monitor::Caltert*>(container);
    
  if(caltert != 0) {
    MAP_XAC_MPS(col, caltert);
    return col;
  }
  
  // Check for Ifmod

  carma::monitor::Ifmod* const ifmod = dynamic_cast<carma::monitor::Ifmod*>(container);
  
  if(ifmod != 0) {
    MAP_XAC_MPS(col, ifmod);
    return col;
  }

  // Check for Intmod

  carma::monitor::Intmod* const intmod = dynamic_cast<carma::monitor::Intmod*>(container);
  
  if(intmod != 0) {
    MAP_XAC_MPS(col, intmod);
    return col;
  }

  // Check for Rx

  carma::monitor::Rx* const rx = dynamic_cast<carma::monitor::Rx*>(container);
  
  if(rx != 0) {
    MAP_XAC_MPS(col, rx);
    return col;
  }

  // Check for Thermal

  carma::monitor::Thermal* const thermal = dynamic_cast<carma::monitor::Thermal*>(container);
  
  if(thermal != 0) {
    MAP_XAC_MPS(col, thermal);
    return col;
  }

  // Check for Tiltmeter

  carma::monitor::Tiltmeter* const tiltmeter = dynamic_cast<carma::monitor::Tiltmeter*>(container);
  
  if(tiltmeter != 0) {
    MAP_XAC_MPS(col, tiltmeter);
    return col;
  }

  // Check for varactor

  carma::monitor::Varactor* const varactor = dynamic_cast<carma::monitor::Varactor*>(container);
  
  if(varactor != 0) {
    MAP_XAC_MPS(col, varactor);
    return col;
  }

  // Check for Yig

  carma::monitor::Yig* const yig = dynamic_cast<carma::monitor::Yig*>(container);
  
  if(yig != 0) {
    MAP_XAC_MPS(col, yig);
    return col;
  }

  return col;
}

/**.......................................................................
 * A wrapper around addFolder to add the XAC folder that's common to
 * every CAN module
 */
void SzaRtdUtils::addXacFolder(MonitorDisplay& display, std::string moduleName,
			       SZA_CONTAINER_FN(*conFn))
{
  ostringstream os;
  os << moduleName << " (xac)";

  addFolder(display, os.str(), conFn, getXacColumn, getXacLabels, 11);
}

/**.......................................................................
 * Add a folder to the display, populated with a table containing data
 * for all SZA antennas
 */
void SzaRtdUtils::addCanModule(MonitorDisplay& display, std::string moduleName, 
			       SZA_CONTAINER_FN(*conFn), SZA_COL_ADD_FN(*colFn), SZA_LABEL_FN(*labFn), unsigned colWidth)
{
  // Add a folder for the module-specific points

  addFolder(display, moduleName, conFn, colFn, labFn, colWidth);

  // And add a folder for the common XAC monitor points

  addXacFolder(display, moduleName, conFn);
}

//------------------------------------------------------------
// Define labels for the bias monitor points
//------------------------------------------------------------

SZA_LABEL_FN(SzaRtdUtils::getBiasLabels)
{
  std::vector<std::string> labels;

  labels.push_back("State");
  labels.push_back("Phase lock status");		  
  labels.push_back("HW lock indicator");		  
  labels.push_back("Gunn frequency (GHz)");		  
  labels.push_back("Data Valid");			  
  labels.push_back("Ref status");			  
  labels.push_back("Multiplier");			  
  labels.push_back("Gunn voltage (V)");			  
  labels.push_back("Gunn current (mA)");		  

  labels.push_back("Loop Gain (%)");		  
  labels.push_back("Tuner Pos (micro-steps)");		  
  labels.push_back("Backshort Pos (micro-steps)");		  
  labels.push_back("Atten Pos (micro-steps)");		  
  labels.push_back("V Error (mV)");		  
  labels.push_back("IF Level (mV)");		  
  labels.push_back("Noise Meter V (mV)");		  
  labels.push_back("Crowbar State");		  
  labels.push_back("Crowbar Count");		  

  labels.push_back("Gunn");			  
  labels.push_back("Sweep");			  
  labels.push_back("IF Mon");			  
  labels.push_back("Auto Relock");			  
  labels.push_back("Relocks");			  
  labels.push_back("Num Zabers");			  
  labels.push_back("All Zabers");			  

  labels.push_back("calTableState");			  
  labels.push_back("Gunn ID");				  
  labels.push_back("Cal Date");				  

  labels.push_back("Board temperature (C)");		  

  labels.push_back("+24V PSU (V)");			  
  labels.push_back("+5V Digital PSU (V)");		  
  labels.push_back("+15V Analog PSU (V)");		  
  labels.push_back("+12V Analog PSU (V)");		  
  labels.push_back("+5V Analog PSU (V)");		  
  labels.push_back("-12V Analog PSU (V)");		  
  labels.push_back("+6V Analog PSU (V)");                 

  return labels;
}

//------------------------------------------------------------
// Return the bias container
//------------------------------------------------------------

SZA_CONTAINER_FN(SzaRtdUtils::getBiasContainer)
{
  return &subsystem.bias();
}

//------------------------------------------------------------
// Define Monitor points for the Biass
//------------------------------------------------------------

SZA_COL_ADD_FN(SzaRtdUtils::getBiasColumn)
{
  std::vector<MonitorCellPtr> col;
  MonitorCellMappedPtr cell;
  std::map<std::string, CellColor> colorMap;

  carma::monitor::Bias* bias = (carma::monitor::Bias*)container;

  // State of this module

  CAN_RECEIVED_MP(bias);

  // Phase state

  std::map<std::string, std::string> stateMap;
  stateMap["0"] = "UNLOCKED";
  stateMap["1"] = "WAITING FOR TUNER";
  stateMap["2"] = "WAITING FOR BACKSHORT";
  stateMap["3"] = "WAITING FOR ATTEN";
  stateMap["4"] = "SEARCHING";
  stateMap["5"] = "REDUCING ERROR VOLTAGE";
  stateMap["6"] = "ADJUSTING LOOP GAIN";
  stateMap["7"] = "LOCKED";
  stateMap["8"] = "HOMING TUNER ACTUATOR";
  stateMap["9"] = "RELOCKING";

  colorMap.clear();
  colorMap["UNLOCKED"]               = RED_CELL_COLOR;
  colorMap["WAITING FOR TUNER"]      = RED_CELL_COLOR;
  colorMap["WAITING FOR BACKSHORT"]  = RED_CELL_COLOR;
  colorMap["WAITING FOR ATTEN"]      = RED_CELL_COLOR;
  colorMap["SEARCHING"]              = RED_CELL_COLOR;
  colorMap["REDUCING ERROR VOLTAGE"] = RED_CELL_COLOR;
  colorMap["ADJUSTING LOOP GAIN"]    = RED_CELL_COLOR;
  colorMap["LOCKED"]                 = WHITE_CELL_COLOR;
  colorMap["HOMING TUNER ACTUATOR"]  = RED_CELL_COLOR;
  colorMap["RELOCKING"]              = RED_CELL_COLOR;

  cell = MonitorCellMapped::makeCell(colWidth, bias->phaseLockState(), stateMap);
  cell->setColorMap(colorMap);
  col.push_back(cell);

  // HW lock status

  colorMap.clear();
  colorMap["UNLOCKED"] = RED_CELL_COLOR;  
  colorMap["LOCKED"] = WHITE_CELL_COLOR;

  cell = MonitorCellMapped::makeCell(colWidth, bias->hwLockStatus(), "UNLOCKED", "LOCKED");
  cell->setColorMap(colorMap);
  col.push_back(cell);

  //------------------------------------------------------------
  // Gunn Frequency
  //------------------------------------------------------------

  col.push_back(MonitorCellScaled::makeCell(colWidth, bias->gunnFrequency(), 0.01));

  //------------------------------------------------------------
  // Data Valid
  //------------------------------------------------------------

  colorMap.clear();
  colorMap["false"] = RED_CELL_COLOR;  
  colorMap["true"]  = WHITE_CELL_COLOR;

  cell = MonitorCellMapped::makeCell(colWidth, bias->dataValid(), "false", "true");
  cell->setColorMap(colorMap);
  col.push_back(cell);

  //------------------------------------------------------------
  // Reference status 
  //------------------------------------------------------------

  cell = MonitorCellMapped::makeCell(colWidth, bias->refLockStatus(), "UNLOCKED", "LOCKED");
  cell->setColorMap(colorMap);
  col.push_back(cell);

  //------------------------------------------------------------
  // Multiplier
  //------------------------------------------------------------

  col.push_back(MonitorCellScaled::makeCell(colWidth, bias->multiplier(), 1.0, 0.0, 0));

  //------------------------------------------------------------
  // Gunn voltage
  //------------------------------------------------------------

  col.push_back(MonitorCellScaled::makeCell(colWidth, bias->gunnVoltage(),   0.01));

  //------------------------------------------------------------
  // Gunn current
  //------------------------------------------------------------

  col.push_back(MonitorCellScaled::makeCell(colWidth, bias->gunnCurrent(), 1.0, 0.0, 0));

  //------------------------------------------------------------
  // Loop gain
  //------------------------------------------------------------

  col.push_back(MonitorCellScaled::makeCell(colWidth, bias->loopGain(), 0.01));

  //------------------------------------------------------------
  // Tuner position
  //------------------------------------------------------------

  col.push_back(MonitorCellScaled::makeCell(colWidth, bias->tunerPosition(), 1.0, 0.0, 0));

  //------------------------------------------------------------
  // Backshort position
  //------------------------------------------------------------

  col.push_back(MonitorCellScaled::makeCell(colWidth, bias->backShortPosition(), 1.0, 0.0, 0));

  //------------------------------------------------------------
  // Attenuator position
  //------------------------------------------------------------

  col.push_back(MonitorCellScaled::makeCell(colWidth, bias->attenuatorPosition(), 1.0, 0.0, 0));

  //------------------------------------------------------------
  // Error voltage
  //------------------------------------------------------------

  col.push_back(MonitorCellScaled::makeCell(colWidth, bias->errorVoltage(), 1.0, 0.0, 0));

  //------------------------------------------------------------
  // IF Level
  //------------------------------------------------------------

  col.push_back(MonitorCellScaled::makeCell(colWidth, bias->ifLevel(), 1.0, 0.0, 0));

  //------------------------------------------------------------
  // Noise meter voltage
  //------------------------------------------------------------

  col.push_back(MonitorCellScaled::makeCell(colWidth, bias->noiseMeterVoltage(), 1.0, 0.0, 0));

  //------------------------------------------------------------
  // Crowbar state
  //------------------------------------------------------------

  stateMap.clear();
  stateMap["0"] = "Normal";
  stateMap["1"] = "Crowbarred";
  stateMap["2"] = "Resetting";
  stateMap["3"] = "Disabled";

  colorMap.clear();
  colorMap["Normal"]     = WHITE_CELL_COLOR;  
  colorMap["Crowbarred"] = RED_CELL_COLOR;
  colorMap["Resetting"]  = RED_CELL_COLOR;
  colorMap["Disabled"]   = RED_CELL_COLOR;

  cell = MonitorCellMapped::makeCell(colWidth, bias->crowbarState(), stateMap);
  cell->setColorMap(colorMap);
  col.push_back(cell);
  
  //------------------------------------------------------------
  // Crowbar count
  //------------------------------------------------------------

  col.push_back(MonitorCellScaled::makeCell(colWidth, bias->crowbarCount(), 1.0, 0.0, 0));

  //------------------------------------------------------------
  // Gunn status
  //------------------------------------------------------------

  colorMap.clear();
  colorMap["OFF"] = RED_CELL_COLOR;  
  colorMap["ON"]  = WHITE_CELL_COLOR;

  cell = MonitorCellMapped::makeCell(colWidth, bias->gunnStatus(), "OFF", "ON");
  cell->setColorMap(colorMap);
  col.push_back(cell);

  //------------------------------------------------------------
  // Sweep status
  //------------------------------------------------------------

  cell = MonitorCellMapped::makeCell(colWidth, bias->sweepStatus(), "OFF", "ON");
  cell->setColorMap(colorMap);
  col.push_back(cell);

  //------------------------------------------------------------
  // IF Monitor status
  //------------------------------------------------------------

  cell = MonitorCellMapped::makeCell(colWidth, bias->ifMonState(), "OFF", "ON");
  cell->setColorMap(colorMap);
  col.push_back(cell);

  //------------------------------------------------------------
  // Auto relock status
  //------------------------------------------------------------

  cell = MonitorCellMapped::makeCell(colWidth, bias->autoRelock(), "OFF", "ON");
  cell->setColorMap(colorMap);
  col.push_back(cell);

  //------------------------------------------------------------
  // Relock count
  //------------------------------------------------------------

  col.push_back(MonitorCellScaled::makeCell(colWidth, bias->relockCount(), 1.0, 0.0, 0));

  //------------------------------------------------------------
  // Number of zabers
  //------------------------------------------------------------

  col.push_back(MonitorCellScaled::makeCell(colWidth, bias->numZabers(), 1.0, 0.0, 0));

  //------------------------------------------------------------
  // All zabers
  //------------------------------------------------------------

  BINARY_MAPPED_MP(bias->allZabers(), "No", "Yes", RED_CELL_COLOR, WHITE_CELL_COLOR);

  //------------------------------------------------------------
  // Cal Table monitor point
  //------------------------------------------------------------

  colorMap.clear();
  colorMap["VALID"]   = WHITE_CELL_COLOR;  
  colorMap["INVALID"] = RED_CELL_COLOR;

  cell = MonitorCellMapped::makeCell(colWidth, bias->calTableState(), "VALID", "INVALID");
  cell->setColorMap(colorMap);
  col.push_back(cell);

  //------------------------------------------------------------
  // Gunn id
  //------------------------------------------------------------

  col.push_back(MonitorCellScaled::makeCell(colWidth, bias->gunnId(), 1.0, 0.0, 0));

  //------------------------------------------------------------
  // Cal date
  //------------------------------------------------------------

  col.push_back(MonitorCellScaled::makeCell(colWidth, bias->calDate()));

  //------------------------------------------------------------
  // Board temperature
  //------------------------------------------------------------

  col.push_back(MonitorCellScaled::makeCell(colWidth, bias->boardTemperature(), 0.01));

  // Voltages

  col.push_back(MonitorCellScaled::makeCell(colWidth, bias->pos24VAnalogVoltage(), 0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, bias->pos5VDigitalVoltage(), 0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, bias->pos15VAnalogVoltage(), 0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, bias->pos12VAnalogVoltage(), 0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, bias->pos5VAnalogVoltage(),  0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, bias->neg12VAnalogVoltage(), 0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, bias->pos6VAnalogVoltage(),  0.001));
  
  return col;
}

//------------------------------------------------------------
// Define labels for the intmod monitor points
//------------------------------------------------------------

SZA_LABEL_FN(SzaRtdUtils::getIntmodLabels)
{
  std::vector<std::string> labels;

  labels.push_back("State");		  

  labels.push_back("10MHz Photo Level (dBm)");
  labels.push_back("10MHz Lock");
  labels.push_back("10MHz Photo");

  labels.push_back("50MHz Photo Level (dBm)");
  labels.push_back("50MHz Photo");

  labels.push_back("LO Term Photo Level (dBm)");

  labels.push_back("LO Term Power State");

  labels.push_back("LO Term RF In (dBm)");
  labels.push_back("LO Term RF Out (dBm)");
  labels.push_back("LO Term Temp (C)");
  labels.push_back("LO Term Atten (dB)");

  labels.push_back("10 MHz Serial #");
  labels.push_back("50 MHz Serial #");
  labels.push_back("LO Term Serial #");

  labels.push_back("Module Temp (C)");

  labels.push_back("+24V Board (V)");
  labels.push_back("+5V Digital (V)");
  labels.push_back("+15V External (V)");
  labels.push_back("-9V External (V)");
  labels.push_back("-5V External (V)");
  labels.push_back("-15V External (V)");
  labels.push_back("+5V External (V)");
  labels.push_back("+9V External");

  return labels;
}

//------------------------------------------------------------
// Return the yig container
//------------------------------------------------------------

SZA_CONTAINER_FN(SzaRtdUtils::getIntmodContainer)
{
  return &subsystem.intmod();
}

//------------------------------------------------------------
// Define Monitor points for the Yigs
//------------------------------------------------------------

SZA_COL_ADD_FN(SzaRtdUtils::getIntmodColumn)
{
  std::vector<MonitorCellPtr> col;

  carma::monitor::Intmod* intmod = (carma::monitor::Intmod*)container;

  CAN_RECEIVED_MP(intmod);

  col.push_back(MonitorCellScaled::makeCell(colWidth, intmod->photoLev10MHz(), 0.01));
  BINARY_MAPPED_MP(intmod->lockStatus10MHz(),   "BAD", "GOOD", RED_CELL_COLOR, WHITE_CELL_COLOR);
  BINARY_MAPPED_MP(intmod->photoStatus10MHz(),  "BAD", "GOOD", RED_CELL_COLOR, WHITE_CELL_COLOR);

  col.push_back(MonitorCellScaled::makeCell(colWidth, intmod->photoLev50MHz(), 0.01));
  BINARY_MAPPED_MP(intmod->photoStatus50MHz(),   "BAD", "GOOD", RED_CELL_COLOR, WHITE_CELL_COLOR);

  col.push_back(MonitorCellScaled::makeCell(colWidth, intmod->photoLevLOTerm(), 0.01));

  // LO Term power state

  std::map<std::string, CellColor> colorMap;
  std::map<std::string, std::string> stateMap;

  stateMap["0"] = "Idle";
  stateMap["1"] = "Setting";
  stateMap["2"] = "OK";
  stateMap["3"] = "Low";
  stateMap["4"] = "High";

  colorMap["Idle"]    = WHITE_CELL_COLOR;
  colorMap["Setting"] = YELLOW_CELL_COLOR;
  colorMap["OK"]      = WHITE_CELL_COLOR;
  colorMap["Low"]     = RED_CELL_COLOR;
  colorMap["High"]    = RED_CELL_COLOR;

  MonitorCellMappedPtr cell;
  cell = MonitorCellMapped::makeCell(colWidth, intmod->loTermPowerState(), stateMap);
  cell->setColorMap(colorMap);
  col.push_back(cell);

  // End LO Term power state

  col.push_back(MonitorCellScaled::makeCell(colWidth, intmod->loRFInLev(),0.01));
  col.push_back(MonitorCellScaled::makeCell(colWidth, intmod->loRFOutLev(),0.01));
  col.push_back(MonitorCellScaled::makeCell(colWidth, intmod->loTempLev(),0.01));
  col.push_back(MonitorCellScaled::makeCell(colWidth, intmod->pamAtten()));

  col.push_back(MonitorCellScaled::makeCell(colWidth, intmod->sn10MHzModule(),  1.0, 0.0, 0));
  col.push_back(MonitorCellScaled::makeCell(colWidth, intmod->sn50MHzModule(),  1.0, 0.0, 0));
  col.push_back(MonitorCellScaled::makeCell(colWidth, intmod->snLoTermModule(), 1.0, 0.0, 0));

  col.push_back(MonitorCellScaled::makeCell(colWidth, intmod->modTemp(), 0.01));

  col.push_back(MonitorCellScaled::makeCell(colWidth, intmod->powSupNeg28V(),   0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, intmod->powSupPosDig5V(), 0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, intmod->powSupPos15V(),   0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, intmod->powSupNeg9V(),    0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, intmod->powSupNeg5V(),    0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, intmod->powSupNeg15V(),   0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, intmod->powSupPos5V(),    0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, intmod->powSupPos9V(),    0.001));
  
  return col;
}

//------------------------------------------------------------
// Define labels for the yig monitor points
//------------------------------------------------------------

SZA_LABEL_FN(SzaRtdUtils::getYigLabels)
{
  std::vector<std::string> labels;

  labels.push_back("State");		  
  labels.push_back("Lock State");		  
  labels.push_back("Lock Bit Status");			  
  labels.push_back("YigFreq (GHz)");		  
  labels.push_back("DataValid");		  

  labels.push_back("RefStatus");
  labels.push_back("LoopGain (%)");			  
  labels.push_back("DampingRes (Ohms)");			  
  labels.push_back("IfLevel (V)");			  

  labels.push_back("Error Voltage (V)");			  
  labels.push_back("YigCurrent (mA)");			  
  labels.push_back("NoiseMeterVoltage (V)");			  
  labels.push_back("SweepStatus");
  labels.push_back("AutoRelockStatus");
  labels.push_back("AutoRelockCount");

  labels.push_back("YigId");
  //  labels.push_back("Cal Date");
  labels.push_back("ModTemp(C)");
  labels.push_back("+24V (V)");
  labels.push_back("+5V (V)");
  labels.push_back("+5V Analog (V)");
  labels.push_back("+9V (V)");
  labels.push_back("+15V (V)");
  labels.push_back("-5V Analog (V)");
  labels.push_back("-15V (V)");

  return labels;
}

//------------------------------------------------------------
// Return the yig container
//------------------------------------------------------------

SZA_CONTAINER_FN(SzaRtdUtils::getYigContainer)
{
  return &subsystem.yig();
}

//------------------------------------------------------------
// Define Monitor points for the Yigs
//------------------------------------------------------------

SZA_COL_ADD_FN(SzaRtdUtils::getYigColumn)
{
  std::vector<MonitorCellPtr> col;
  MonitorCellMappedPtr cell;
  std::map<std::string, CellColor> colorMap;

  carma::monitor::Yig* yig = (carma::monitor::Yig*)container;

  //------------------------------------------------------------
  // Received state
  //------------------------------------------------------------

  CAN_RECEIVED_MP(yig);

  //------------------------------------------------------------
  // Yig lock status
  //------------------------------------------------------------

  cell = MonitorCellMapped::makeCell(colWidth, yig->lockState(),
							   "UNLOCKED", "SEARCHING", "REFINING", "LOCKED", "RELOCK");
  colorMap.clear();
  colorMap["UNLOCKED"]  = RED_CELL_COLOR;
  colorMap["SEARCHING"] = RED_CELL_COLOR;
  colorMap["REFINING"]  = RED_CELL_COLOR;
  colorMap["LOCKED"]    = WHITE_CELL_COLOR;
  colorMap["RELOCK"]    = RED_CELL_COLOR;
  cell->setColorMap(colorMap);
  col.push_back(cell);

  // Yig lock bit

  BINARY_MAPPED_MP(yig->lockBit(), "UNLOCKED", "LOCKED", RED_CELL_COLOR, WHITE_CELL_COLOR);

  col.push_back(MonitorCellScaled::makeCell(colWidth, yig->frequency(),          0.001));

  // Data valid mp

  BINARY_MAPPED_MP(yig->dataValid(), "false", "true", RED_CELL_COLOR, WHITE_CELL_COLOR);

  // RefStatus

  BINARY_MAPPED_MP(yig->refStatus(),   "BAD", "GOOD", RED_CELL_COLOR, WHITE_CELL_COLOR);

  // Unmapped monitor points

  col.push_back(MonitorCellScaled::makeCell(colWidth, yig->loopGainResistance(), 0.01));
  col.push_back(MonitorCellScaled::makeCell(colWidth, yig->dampingResistance()));
  col.push_back(MonitorCellScaled::makeCell(colWidth, yig->ifLevel(),            0.001));

  col.push_back(MonitorCellScaled::makeCell(colWidth, yig->errorVoltage(),       0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, yig->current()));
  col.push_back(MonitorCellScaled::makeCell(colWidth, yig->noiseMeterVoltage(),  0.001));

  // Status monitor points

  BINARY_MAPPED_MP(yig->sweepStatus(), "OFF", "ON",   RED_CELL_COLOR, WHITE_CELL_COLOR);  
  BINARY_MAPPED_MP(yig->autoRelock(),  "OFF", "ON",   RED_CELL_COLOR, WHITE_CELL_COLOR);
  col.push_back(MonitorCellScaled::makeCell(colWidth, yig->relockCount(), 1.0, 0.0, 0));

  col.push_back(MonitorCellScaled::makeCell(colWidth, yig->id(), 1.0, 0.0, 0));
  //  col.push_back(MonitorCellScaled::makeCell(colWidth, yig->calDate()));
  col.push_back(MonitorCellScaled::makeCell(colWidth, yig->boardTemperature(),   0.01));
  col.push_back(MonitorCellScaled::makeCell(colWidth, yig->maxChnl(0),           0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, yig->maxChnl(1),           0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, yig->maxChnl(2),           0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, yig->maxChnl(3),           0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, yig->maxChnl(4),           0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, yig->maxChnl(5),           0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, yig->maxChnl(6),           0.001));


  return col;
}

//------------------------------------------------------------
// Define labels for the varactor monitor points
//------------------------------------------------------------

SZA_LABEL_FN(SzaRtdUtils::getVaractorLabels)
{
  std::vector<std::string> labels;

  labels.push_back("State");		  
  labels.push_back("Phase Lock");
  labels.push_back("Reference");
  //  labels.push_back("Commanded Freq (GHz)");

  labels.push_back("Gunn Current (mA)");
  labels.push_back("Loop Gain (Ohms)");
  labels.push_back("Error Voltage (V)");
  labels.push_back("IF AGC Level (V)");
  labels.push_back("Phase Noise (V)");

  labels.push_back("Gunn Enabled");
  labels.push_back("Sweep Enabled");
  labels.push_back("IF Enabled");
  labels.push_back("Data Valid");

  labels.push_back("Temperature (C)");
  labels.push_back("+24V (V)");
  labels.push_back("+5V Digital (V)");
  labels.push_back("+15V PSU (V)");
  labels.push_back("+12V Analog (V)");
  labels.push_back("+6V Analog (V)");
  labels.push_back("-15V Analog (V)");
  labels.push_back("+9V Analog (V)");
  labels.push_back("+5V Analog (V)");

  return labels;
}

//------------------------------------------------------------
// Return the varactor container
//------------------------------------------------------------

SZA_CONTAINER_FN(SzaRtdUtils::getVaractorContainer)
{
  return &subsystem.varactor();
}

//------------------------------------------------------------
// Define Monitor points for the Varactors
//------------------------------------------------------------

SZA_COL_ADD_FN(SzaRtdUtils::getVaractorColumn)
{
  std::vector<MonitorCellPtr> col;

  carma::monitor::Varactor* varactor = (carma::monitor::Varactor*)container;

  CAN_RECEIVED_MP(varactor);
  BINARY_MAPPED_MP(varactor->lockStatus(), "UNLOCKED", "LOCKED", RED_CELL_COLOR, WHITE_CELL_COLOR);
  BINARY_MAPPED_MP(varactor->refStatus(),  "false",    "true",   RED_CELL_COLOR, WHITE_CELL_COLOR);
  //  labels.push_back("Commanded Freq (GHz)");

  col.push_back(MonitorCellScaled::makeCell(colWidth, varactor->biasCurrent()));
  col.push_back(MonitorCellScaled::makeCell(colWidth, varactor->loopGainResistance()));
  col.push_back(MonitorCellScaled::makeCell(colWidth, varactor->errorVoltage(),      0.01));
  col.push_back(MonitorCellScaled::makeCell(colWidth, varactor->ifLevel(),           0.01));
  col.push_back(MonitorCellScaled::makeCell(colWidth, varactor->noiseMeterVoltage(), 0.01));

  BINARY_MAPPED_MP(varactor->gunnStatus(), "false",    "true",   RED_CELL_COLOR, WHITE_CELL_COLOR);
  BINARY_MAPPED_MP(varactor->sweepStatus(),"false",    "true",   RED_CELL_COLOR, WHITE_CELL_COLOR);
  BINARY_MAPPED_MP(varactor->ifMonStatus(),"false",    "true",   RED_CELL_COLOR, WHITE_CELL_COLOR);;
  BINARY_MAPPED_MP(varactor->dataValid(),  "false",    "true",   RED_CELL_COLOR, WHITE_CELL_COLOR);;

  col.push_back(MonitorCellScaled::makeCell(colWidth, varactor->boardTemperature(),  0.01));
  col.push_back(MonitorCellScaled::makeCell(colWidth, varactor->powSupPos24V(),     0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, varactor->powSupPosDig5V(),   0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, varactor->powSupPosDig15V(),  0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, varactor->powSupPos12V(),     0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, varactor->powSupPos6V(),      0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, varactor->powSupNeg15V(),     0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, varactor->powSupPos9V(),      0.001));
  col.push_back(MonitorCellScaled::makeCell(colWidth, varactor->powSupPos5V(),      0.001));
  
  return col;
}
