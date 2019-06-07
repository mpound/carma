/*
 * @file
 * 
 * Gets data from the BIMA rxs and displays
 *
 * @author Erik Leitch
 */
#include <sstream>
#include <vector>

#include "carma/monitor/BimaSubsystem.h"

#include "carma/monitor/MonitorSystem.h"

#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorSingleTableFolder.h"
#include "carma/ui/rtd/common/MonitorCell.h"
#include "carma/services/Global.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"

using namespace std;
using namespace carma::monitor;
using namespace carma::services;
using namespace carma::util;
using namespace carma::ui::rtd;

const int colWidth = 12;

void makeCols(RtTablePtr table){
    for(int i = 7; i < 16; i++){
    ostringstream oss;
    oss << "C" << i;
    table->addCol( RtColumn::makeColumn(oss.str()) );
  }
}

//------------------------------------------------------------
// Define labels for the rx monitor points
//------------------------------------------------------------

void createBiasTable(const MonitorDisplay &display, RtFolderPtr folder){

  RtTablePtr table(new RtTable("cmrx"));
  
  makeCols(table);

  table->addRow( RtRow::makeRow("Stage 1 Drain Current (mA)"));
  table->addRow( RtRow::makeRow("Stage 2 Drain Current (mA)"));
  table->addRow( RtRow::makeRow("Stage 3 Drain Current (mA)"));
  table->addRow( RtRow::makeRow("Stage 4 Drain Current (mA)"));

  table->addRow( RtRow::makeRow("Stage 1 Gate Voltage (V)"));
  table->addRow( RtRow::makeRow("Stage 2 Gate Voltage (V)"));
  table->addRow( RtRow::makeRow("Stage 3 Gate Voltage (V)"));
  table->addRow( RtRow::makeRow("Stage 4 Gate Voltage (V)"));

  table->addRow( RtRow::makeRow("Stage 1 Gate Current (mA)"));
  table->addRow( RtRow::makeRow("Stage 2 Gate Current (mA)"));
  table->addRow( RtRow::makeRow("Stage 3 Gate Current (mA)"));
  table->addRow( RtRow::makeRow("Stage 4 Gate Current (mA)"));

  table->addRow( RtRow::makeRow("30 GHz IF Amp Voltage (V)"));
  table->addRow( RtRow::makeRow("30 GHz IF Amp Current (mA)"));

  table->addRow( RtRow::makeRow("30 GHz Mixer Current (mA)"));
  table->addRow( RtRow::makeRow("30 GHz LED Current   (mA)"));

  table->addRow( RtRow::makeRow("HEMT Stage Temperature (K)"));
  table->addRow( RtRow::makeRow("2nd Stage Temperature (K)"));
  table->addRow( RtRow::makeRow("Shield Temperature (K)"));
  table->addRow( RtRow::makeRow("Horn Temperature (K)"));
  table->addRow( RtRow::makeRow("Board Temperature (C)"));

  table->addRow( RtRow::makeRow("-15V Analog Supply (V)"));
  table->addRow( RtRow::makeRow("+5V Digital Supply (V)"));
  table->addRow( RtRow::makeRow("-5V Digital Supply (V)"));
  table->addRow( RtRow::makeRow("+15V Analog Supply (V)"));
  table->addRow( RtRow::makeRow("+24V Analog Supply (V)"));
  table->addRow( RtRow::makeRow("Reqested Drain Voltage1 (V)"));
  table->addRow( RtRow::makeRow("Reqested Drain Voltage2 (V)"));
  table->addRow( RtRow::makeRow("Reqested Drain Voltage3 (V)"));
  table->addRow( RtRow::makeRow("Reqested Drain Voltage4 (V)"));
  

  for(int i = 0; i < 9; i++){
    RxBias &rx = display.cms().bima(i).rx();
    BimaSubsystem::RxBiasTemps &rxTemps = display.cms().bima(i).rxBiasTemps();
    table->add(MonitorCell::makeCell(colWidth, rx.drainCurrent30GHz(0)));
    table->add(MonitorCell::makeCell(colWidth, rx.drainCurrent30GHz(1)));
    table->add(MonitorCell::makeCell(colWidth, rx.drainCurrent30GHz(2)));
    table->add(MonitorCell::makeCell(colWidth, rx.drainCurrent30GHz(3)));

    table->add(MonitorCell::makeCell(colWidth, rx.gateVoltage30GHz(0)));
    table->add(MonitorCell::makeCell(colWidth, rx.gateVoltage30GHz(1)));
    table->add(MonitorCell::makeCell(colWidth, rx.gateVoltage30GHz(2)));
    table->add(MonitorCell::makeCell(colWidth, rx.gateVoltage30GHz(3)));

    table->add(MonitorCell::makeCell(colWidth, rx.gateCurrent30GHz(0)));
    table->add(MonitorCell::makeCell(colWidth, rx.gateCurrent30GHz(1)));
    table->add(MonitorCell::makeCell(colWidth, rx.gateCurrent30GHz(2)));
    table->add(MonitorCell::makeCell(colWidth, rx.gateCurrent30GHz(3)));

    table->add(MonitorCell::makeCell(colWidth, rx.ifAmpVoltage30GHz()));
    table->add(MonitorCell::makeCell(colWidth, rx.ifAmpCurrent30GHz()));

    table->add(MonitorCell::makeCell(colWidth, rx.mixerCurrent30GHz()));
    table->add(MonitorCell::makeCell(colWidth, rx.ledCurrent30GHz()));

    table->add(MonitorCell::makeCell(colWidth, rxTemps.tempHemtStage()));
    table->add(MonitorCell::makeCell(colWidth, rxTemps.tempSecondStage()));
    table->add(MonitorCell::makeCell(colWidth, rxTemps.shieldTemp()));
    table->add(MonitorCell::makeCell(colWidth, rxTemps.hornTemp()));
    table->add(MonitorCell::makeCell(colWidth, rx.boardTemperature()));

    table->add(MonitorCell::makeCell(colWidth, rx.neg15VAnalogVoltage()));
    table->add(MonitorCell::makeCell(colWidth, rx.pos5VDigitalVoltage()));
    table->add(MonitorCell::makeCell(colWidth, rx.neg5VDigitalVoltage()));
    table->add(MonitorCell::makeCell(colWidth, rx.pos15VAnalogVoltage()));
    table->add(MonitorCell::makeCell(colWidth, rx.pos24VAnalogVoltage()));
    for(int j = 0; j <=3; j++){
      table->add(MonitorCell::makeCell(colWidth, rx.drainSetVoltage30GHz(j)));
    }
  }
  const int numRows = table->getNumRows();
  folder->add(table);
  table->setMinRows( numRows );
  table->setPrefRows( numRows );
  return;
}


void createXacTable(const MonitorDisplay &display, RtFolderPtr folder){

  RtTablePtr table(new RtTable("xac"));
  
  makeCols(table);

  table->addRow( RtRow::makeRow("State"));
  table->addRow( RtRow::makeRow("Serial#"));
  table->addRow( RtRow::makeRow("ModuleType"));
  table->addRow( RtRow::makeRow("CanRxErrs"));
  table->addRow( RtRow::makeRow("CanTxErrs"));
  table->addRow( RtRow::makeRow("MemErrs"));
  table->addRow( RtRow::makeRow("SchedOverflows"));
  table->addRow( RtRow::makeRow("TimeOverflows"));
  table->addRow( RtRow::makeRow("FwVersion"));
  table->addRow( RtRow::makeRow("TestMode"));
  table->addRow( RtRow::makeRow("CommErrs"));
  table->addRow( RtRow::makeRow("TimeErrs"));
  table->addRow( RtRow::makeRow("SwErrs"));
  table->addRow( RtRow::makeRow("HwErrs"));
  table->addRow( RtRow::makeRow("TimeJitter"));
  table->addRow( RtRow::makeRow("TimeSinceLastTs (s)"));
  table->addRow( RtRow::makeRow("TsDelta (ms)"));
  table->addRow( RtRow::makeRow("ApiVer"));
  table->addRow( RtRow::makeRow("Uptime (s)"));
  table->addRow( RtRow::makeRow("Bootloader"));
  table->addRow( RtRow::makeRow("Build Date"));
  table->addRow( RtRow::makeRow("Build Time"));


  for(int i = 0; i < 9; i++){
    RxBias &rx = display.cms().bima(i).rx();
    table->add(MonitorCell::makeCell(colWidth, rx.state()));
    table->add(MonitorCell::makeCell(colWidth, rx.xac().serialNo()));
    table->add(MonitorCell::makeCell(colWidth, rx.xac().modType()));
    table->add(MonitorCell::makeCell(colWidth, rx.xac().nCanRxErrs()));
    table->add(MonitorCell::makeCell(colWidth, rx.xac().nCanTxErrs()));
    table->add(MonitorCell::makeCell(colWidth, rx.xac().nMemErrs()));
    table->add(MonitorCell::makeCell(colWidth, rx.xac().nSchedOverflows()));
    table->add(MonitorCell::makeCell(colWidth, rx.xac().nTimeOverflows()));
    table->add(MonitorCell::makeCell(colWidth, rx.xac().fwVersion()));
    table->add(MonitorCell::makeCell(colWidth, rx.xac().testMode()));
    table->add(MonitorCell::makeCell(colWidth, rx.xac().nCommErrs()));
    table->add(MonitorCell::makeCell(colWidth, rx.xac().nTimeErrs()));
    table->add(MonitorCell::makeCell(colWidth, rx.xac().nSwErrs()));
    table->add(MonitorCell::makeCell(colWidth, rx.xac().nHwErrs()));
    table->add(MonitorCell::makeCell(colWidth, rx.xac().timeJitter()));
    table->add(MonitorCell::makeCell(colWidth, rx.xac().timeSinceLastTs()));
    table->add(MonitorCell::makeCell(colWidth, rx.xac().tsDelta()));
    table->add(MonitorCell::makeCell(colWidth, rx.xac().apiVer()));
    table->add(MonitorCell::makeCell(colWidth, rx.xac().uptime()));
    table->add(MonitorCell::makeCell(colWidth, rx.xac().bootloader()));
    table->add(MonitorCell::makeCell(colWidth, rx.xac().buildDate()));
    table->add(MonitorCell::makeCell(colWidth, rx.xac().buildTime()));
  }
  const int numRows = table->getNumRows();
  folder->add(table);
  table->setMinRows( numRows );
  table->setPrefRows( numRows );
  return;
}


std::string makeHelp() 
{
  ostringstream ost;
  ost << "       BIMA ANTENNA CM RX HELP\n\n" 
      << "Fill in more info here... ";
  return ost.str();

}

//------------------------------------------------------------
// Main -- serve rx data
//------------------------------------------------------------


int Program::main() 
{
  // Create a dislay

  MonitorDisplay display("Bima Antenna CM Rx Status");
  display.setSpecificHelp("Bima Rx Help", makeHelp());

  RtFolderPtr biasFolder(new RtFolder("CM Rx"));
  RtFolderPtr xacFolder(new RtFolder("CM Rx (xac)"));

  createBiasTable(display,biasFolder);
  createXacTable(display,xacFolder);
  // Loop forever serving data to the client

  display.add(biasFolder);
  display.add(xacFolder);

  while (display.serveData()) {}

  return 0;
}
