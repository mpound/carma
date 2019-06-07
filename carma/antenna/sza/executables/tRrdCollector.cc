#include <iostream>
#include <iomanip>

#include <cmath>
#include <vector>

#include "carma/szautil/Program.h"
#include "carma/szautil/RrdCollectorPing.h"

#include "carma/util/ErrorException.h"

#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/SystemStatus.h"

using namespace std;
using namespace sza::util;
using namespace carma::monitor;

PROGRAM_KEYWORDS = {
  {"host",    "acc.carma.pvt",             "s", USAGE "Host to query"},
  {"rrdname", "acc_carma_pvt_ping_79.rrd", "s", USAGE "Rrd database name"},
  { "readoutInterval",  "30", "i", USAGE "Sensor readout interval, in seconds"},
  { END_OF_KEYWORDS}
};

PROGRAM_INITIALIZE_USAGE {};

/**.......................................................................
 * Main -- readout thermometry pertinent to the CARMA system
 */
int Program::main()
{
  int errCode=0;

  //------------------------------------------------------------
  // Get the interval, in seconds, on which we will read out the
  // sensors
  //------------------------------------------------------------

  unsigned readoutInterval = Program::getIntParameter("readoutInterval");

  //------------------------------------------------------------
  // See if we have a monitor system.  If not, we will readout anyway,
  // to allow for debugging
  //------------------------------------------------------------

  SystemStatusSubsystemMutex* ss = 0;

  try {
    ss = new SystemStatusSubsystemMutex();
  } catch(carma::util::ErrorException& err) {
    COUT("Caiught an error instantiating the monitor system: " << err.what());
    ss = 0;
  } catch(...) {
    COUT("Caiught an error instantiating the monitor system");
    ss = 0;
  }

  std::vector<RrdCollectorPing*> machines;

  if(ss) {

    SystemStatusSubsystem::Computers& cs = ss->ss_->devices().computers();

    machines.push_back(new RrdCollectorPing("acc.carma.pvt", "acc_carma_pvt_ping_79.rrd",    ss, 
					    cs.acc().pingTime(),        cs.acc().sampleTime(),        
					    cs.acc().status(), cs.acc().ok(), readoutInterval));

    machines.push_back(new RrdCollectorPing("acc.carma.pvt", "boot1_carma_pvt_ping_208.rrd", ss, 
					    cs.boot1().pingTime(),      cs.boot1().sampleTime(),      
					    cs.boot1().status(), cs.boot1().ok(), readoutInterval));

    machines.push_back(new RrdCollectorPing("acc.carma.pvt", "db2_ping_462.rrd",             ss, 
					    cs.db2().pingTime(),        cs.db2().sampleTime(),        
					    cs.db2().status(), cs.db2().ok(), readoutInterval));

    machines.push_back(new RrdCollectorPing("acc.carma.pvt", "cedarflat1_ping_169.rrd",      ss, 
					    cs.cedarflat1().pingTime(), cs.cedarflat1().sampleTime(), 
					    cs.cedarflat1().status(), cs.cedarflat1().ok(), readoutInterval));

    machines.push_back(new RrdCollectorPing("acc.carma.pvt", "cedarflat3_ping_191.rrd",      ss, 
					    cs.cedarflat3().pingTime(), cs.cedarflat3().sampleTime(), 
					    cs.cedarflat3().status(), cs.cedarflat3().ok(), readoutInterval));

    machines.push_back(new RrdCollectorPing("acc.carma.pvt", "cedarflat4_ping_68.rrd",       ss, 
					    cs.cedarflat4().pingTime(), cs.cedarflat4().sampleTime(), 
					    cs.cedarflat4().status(), cs.cedarflat4().ok(), readoutInterval));

    machines.push_back(new RrdCollectorPing("acc.carma.pvt", "cedarflat5_ping_229.rrd",      ss, 
					    cs.cedarflat5().pingTime(), cs.cedarflat5().sampleTime(), 
					    cs.cedarflat5().status(), cs.cedarflat5().ok(), readoutInterval));

  } else {
    machines.push_back(new RrdCollectorPing("acc.carma.pvt", "acc_carma_pvt_ping_79.rrd",    readoutInterval));
    machines.push_back(new RrdCollectorPing("acc.carma.pvt", "boot1_carma_pvt_ping_208.rrd", readoutInterval));
    machines.push_back(new RrdCollectorPing("acc.carma.pvt", "db2_ping_462.rrd",             readoutInterval));
    machines.push_back(new RrdCollectorPing("acc.carma.pvt", "cedarflat1_ping_169.rrd",      readoutInterval));
    machines.push_back(new RrdCollectorPing("acc.carma.pvt", "cedarflat3_ping_191.rrd",      readoutInterval));
    machines.push_back(new RrdCollectorPing("acc.carma.pvt", "cedarflat4_ping_68.rrd",       readoutInterval));
    machines.push_back(new RrdCollectorPing("acc.carma.pvt", "cedarflat5_ping_229.rrd",      readoutInterval));
  }

  //------------------------------------------------------------
  // Now spawn the sensors in their own threads
  //------------------------------------------------------------

  for(unsigned i=0; i < machines.size(); i++) {
    machines[i]->spawn();
  }

#if 0
  //------------------------------------------------------------
  // And loop forever, reading the sensors every readoutInterval
  // seconds
  //------------------------------------------------------------

  try {

    while(true) {

      for(unsigned i=0; i < machines.size(); i++) {
	machines[i]->readSensor();
      }

      sleep(readoutInterval);
    }

  } catch(Exception& err) {
    errCode = 1;
  }
#else
  machines[0]->blockForever();
#endif

  for(unsigned i=0; i < machines.size(); i++) {
    delete machines[i];
  }

  return errCode;
}
