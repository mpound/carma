#include <iostream>
#include <iomanip>

#include <cmath>
#include <vector>

#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/SystemStatus.h"

#include "carma/szautil/AbsoluteTimer.h"
#include "carma/szautil/Program.h"
#include "carma/szautil/RemoteSensorIServer.h"
#include "carma/szautil/RemoteSensorRrd.h"
#include "carma/szautil/RemoteSensorTrippLite.h"
#include "carma/szautil/RrdCollectorPing.h"

#include "carma/util/ErrorException.h"

using namespace std;
using namespace sza::util;
using namespace carma::monitor;

PROGRAM_KEYWORDS = {
  {"rrdReadoutInterval",    "30", "i", USAGE "RRD collector interval, in seconds"},
  {"sensorReadoutInterval", "30", "i", USAGE "Thermometry readout interval, in seconds"},
  { END_OF_KEYWORDS}
};

PROGRAM_INITIALIZE_USAGE {};

void initializeRrdCollectors(std::vector<RemoteSensor*>& sensors, 
			     SystemStatusSubsystemMutex* ss, unsigned defaultReadoutIntervalInSeconds);

void initializeRemoteSensors(std::vector<RemoteSensor*>& sensors, 
			     SystemStatusSubsystemMutex* ss, unsigned defaultReadoutIntervalInSeconds);

static ABSOLUTE_TIMER_HANDLER(writeMonitorSystem);

void writeSystemStatus(SystemStatusSubsystem* ss);

void writePlacesStatus(SystemStatusSubsystem* ss);

void writeDevicesStatus(SystemStatusSubsystem* ss);
void writeDevicesComputersStatus(SystemStatusSubsystem* ss);

/**.......................................................................
 * Main -- readout thermometry and metwork statistics pertinent to the
 * SystemStatus subsystem
 */
int Program::main()
{
  int errCode=0;

  //------------------------------------------------------------
  // Get the interval, in seconds, on which we will read out the
  // sensors
  //------------------------------------------------------------

  unsigned defaultRrdReadoutIntervalInSeconds    = Program::getIntParameter("rrdReadoutInterval");
  unsigned defaultSensorReadoutIntervalInSeconds = Program::getIntParameter("sensorReadoutInterval");

  unsigned minIntervalInSeconds = 
    defaultRrdReadoutIntervalInSeconds < defaultSensorReadoutIntervalInSeconds ? 
    defaultRrdReadoutIntervalInSeconds : defaultSensorReadoutIntervalInSeconds; 

  // Declare the array of 'sensors' we will read out

  std::vector<RemoteSensor*> sensors;

  //------------------------------------------------------------
  // See if we have a monitor system.  If not, we will readout anyway,
  // to allow for debugging
  //------------------------------------------------------------

  sza::util::SystemStatusSubsystemMutex* ssm = 0;

  try {

    // Just catch problems instantiating the monitor system here.
    // Maybe we want to run without it, for example, to test the code.

    try {
      ssm = new sza::util::SystemStatusSubsystemMutex();
    } catch(carma::util::ErrorException& err) {
      COUT("Error instantiating the monitor system: " << err.what());
      ssm = 0;
    } catch(...) {
      COUT("Unknown error instantiating the monitor system");
      ssm = 0;
    }

    // Initialize objects that will collect data for the SystemStatus
    // subsystem

    initializeRrdCollectors(sensors, ssm, defaultRrdReadoutIntervalInSeconds);
    initializeRemoteSensors(sensors, ssm, defaultSensorReadoutIntervalInSeconds);
    
    // Now spawn all objects in their own threads
    
    for(unsigned i=0; i < sensors.size(); i++) {
      sensors[i]->spawn();
    }
    
    // Create a periodic timer, that will write to the monitor system
    // at the shorter of the two readout intervals
    
    AbsoluteTimer monitorWriterTimer;
    monitorWriterTimer.addHandler(writeMonitorSystem, (void*)ssm);
    monitorWriterTimer.enableTimer(true, 
				   10, 0,      // Initial delay 
				   100000000,  // Set to fire 100 ms after the absolute second boundary
				   10, 0);     // Interval is every 10s
    
    monitorWriterTimer.spawn();
    
    // And block the calling thread
    
    Runnable::blockForever();
    
  } catch(sza::util::Exception& err) {
    COUT("Caught an error: " << err.what());
    errCode = 1;
  } catch(carma::util::ErrorException& err) {
    COUT("Caught an error: " << err.what());
    errCode = 1;
  } catch(...) {
    COUT("Caught an unknown error");
    errCode = 1;
  }

  // Free any memory allocated in the parent thread

  if(ssm) {
    delete ssm;
  }

  // Free any sensors allocated 

  for(unsigned i=0; i < sensors.size(); i++) {
    delete sensors[i];
  }

  return errCode;
}

/**.......................................................................
 * Handler called whenever the monitor system should be written to
 */
ABSOLUTE_TIMER_HANDLER(writeMonitorSystem)
{
  SystemStatusSubsystemMutex* ssm = (SystemStatusSubsystemMutex*)args;

  CTOUT(std::endl << pthread_self() << " About to write monitor system");

  ssm->lock();

  writePlacesStatus(ssm->ss_);
  writeDevicesStatus(ssm->ss_);
  writeSystemStatus(ssm->ss_);

  try {
    ssm->ss_->write();
  } catch(...) {
  }

  CTOUT(pthread_self() << " About to write monitor system... DONE" << std::endl);

  ssm->unlock();
}

/**.......................................................................
 * Write a global status for the Places container, based on the
 * individual statuses recorded since the last monitor system write
 */
void writePlacesStatus(SystemStatusSubsystem* ss)
{
  // First accumulate any subsplace statuses into single statuses for
  // their parent places

  bool ok = 
    ss->places().computerRoomCF().upsTemp().ok().getValue() &&
    ss->places().computerRoomCF().roomTemp().ok().getValue();

  ss->places().computerRoomCF().ok().setValue(ok);

  // Then accumulate all place statuses into a single status for the
  // Places subsystem

  ok = 
    ss->places().computerRoomCF().ok().getValue() &&
    ss->places().computerRoomBldg12().ok().getValue() &&
    ss->places().correlatorRoom().ok().getValue() &&
    ss->places().generatorBldg1().ok().getValue() &&
    ss->places().generatorBldg2().ok().getValue();

  ss->places().ok().setValue(ok);
}

/**.......................................................................
 * Write a global status for the Devices container, based on the
 * individual statuses recorded since the last monitor system write
 */
void writeSystemStatus(SystemStatusSubsystem* ss)
{
  bool ok = 
    ss->places().ok().getValue() &&
    ss->devices().ok().getValue();

  ss->ok().setValue(ok);
}

/**.......................................................................
 * Write a global status for the Devices container, based on the
 * individual statuses recorded since the last monitor system write
 */
void writeDevicesStatus(SystemStatusSubsystem* ss)
{
  writeDevicesComputersStatus(ss);

  bool ok = 
    ss->devices().computers().ok().getValue();

  ss->devices().ok().setValue(ok);
}

/**.......................................................................
 * Write a global status for the Devices container, based on the
 * individual statuses recorded since the last monitor system write
 */
void writeDevicesComputersStatus(SystemStatusSubsystem* ss)
{
  bool ok = 
    ss->devices().computers().ups1().ok().getValue() &&
    ss->devices().computers().nfs2().ok().getValue() &&
    ss->devices().computers().corrTemp().ok().getValue() &&
    ss->devices().computers().gen1cool().ok().getValue() &&
    ss->devices().computers().gen2cool().ok().getValue() &&
    ss->devices().computers().acc().ok().getValue() &&
    ss->devices().computers().boot1().ok().getValue() &&
    ss->devices().computers().db2().ok().getValue() &&
    ss->devices().computers().cedarflat1().ok().getValue() &&
    ss->devices().computers().cedarflat3().ok().getValue() &&
    ss->devices().computers().cedarflat4().ok().getValue() &&
    ss->devices().computers().cedarflat5().ok().getValue();

  ss->devices().computers().ok().setValue(ok);
}

/**.......................................................................
 * Initialize objects that will collect network statistics from the
 * RRDs on acc
 */
void initializeRrdCollectors(std::vector<RemoteSensor*>& sensors, 
			     SystemStatusSubsystemMutex* ssm, unsigned defaultReadoutIntervalInSeconds)
{
  if(ssm) {

    SystemStatusSubsystem::Computers& cs = ssm->ss_->devices().computers();

    sensors.push_back(new RrdCollectorPing("acc.carma.pvt", 
					   "acc_carma_pvt_ping_79.rrd",    
					   ssm,  
					   cs.acc().pingTime(),        
					   cs.acc().sampleTime(),        
					   cs.acc().status(),
					   cs.acc().ok(),
					   defaultReadoutIntervalInSeconds));

    sensors.push_back(new RrdCollectorPing("acc.carma.pvt", 
					   "boot1_carma_pvt_ping_208.rrd", 
					   ssm, 
					   cs.boot1().pingTime(),      
					   cs.boot1().sampleTime(),      
					   cs.boot1().status(),
					   cs.boot1().ok(),
					   defaultReadoutIntervalInSeconds));

    sensors.push_back(new RrdCollectorPing("acc.carma.pvt", 
					   "sdp_mmarray_org_ping_798.rrd",
					   ssm, 
					   cs.db2().pingTime(),        
					   cs.db2().sampleTime(),        
					   cs.db2().status(),
					   cs.db2().ok(),
					   defaultReadoutIntervalInSeconds));

    sensors.push_back(new RrdCollectorPing("acc.carma.pvt", 
					   "cedarflat1_ping_169.rrd",      
					   ssm, 
					   cs.cedarflat1().pingTime(), 
					   cs.cedarflat1().sampleTime(), 
					   cs.cedarflat1().status(),
					   cs.cedarflat1().ok(),
					   defaultReadoutIntervalInSeconds));

    sensors.push_back(new RrdCollectorPing("acc.carma.pvt", 
					   "cedarflat3_ping_191.rrd",      
					   ssm, 
					   cs.cedarflat3().pingTime(), 
					   cs.cedarflat3().sampleTime(), 
					   cs.cedarflat3().status(),
					   cs.cedarflat3().ok(),
					   defaultReadoutIntervalInSeconds));

    sensors.push_back(new RrdCollectorPing("acc.carma.pvt", 
					   "cedarflat4_ping_68.rrd",       
					   ssm, 
					   cs.cedarflat4().pingTime(), 
					   cs.cedarflat4().sampleTime(), 
					   cs.cedarflat4().status(),
					   cs.cedarflat4().ok(),
					   defaultReadoutIntervalInSeconds));

    sensors.push_back(new RrdCollectorPing("acc.carma.pvt", 
					   "cedarflat5_ping_229.rrd",      
					   ssm, 
					   cs.cedarflat5().pingTime(), 
					   cs.cedarflat5().sampleTime(), 
					   cs.cedarflat5().status(),
					   cs.cedarflat5().ok(),
					   defaultReadoutIntervalInSeconds));

  } else {

    sensors.push_back(new RrdCollectorPing("acc.carma.pvt", 
					    "acc_carma_pvt_ping_79.rrd", 
					    defaultReadoutIntervalInSeconds));

    sensors.push_back(new RrdCollectorPing("acc.carma.pvt", 
					    "boot1_carma_pvt_ping_208.rrd", 
					    defaultReadoutIntervalInSeconds));

    sensors.push_back(new RrdCollectorPing("acc.carma.pvt", 
					    "db2_ping_462.rrd", 
					    defaultReadoutIntervalInSeconds));

    sensors.push_back(new RrdCollectorPing("acc.carma.pvt", 
					    "cedarflat1_ping_169.rrd", 
					    defaultReadoutIntervalInSeconds));

    sensors.push_back(new RrdCollectorPing("acc.carma.pvt", 
					    "cedarflat3_ping_191.rrd", 
					    defaultReadoutIntervalInSeconds));

    sensors.push_back(new RrdCollectorPing("acc.carma.pvt", 
					    "cedarflat4_ping_68.rrd", 
					    defaultReadoutIntervalInSeconds));

    sensors.push_back(new RrdCollectorPing("acc.carma.pvt", 
					    "cedarflat5_ping_229.rrd", 
					    defaultReadoutIntervalInSeconds));
  }
}

/**.......................................................................
 * Spawn threads that will collect data from remote sensors throughout
 * the system
 */
void initializeRemoteSensors(std::vector<RemoteSensor*>& sensors, 
			     SystemStatusSubsystemMutex* ssm, unsigned defaultReadoutIntervalInSeconds)
{
  if(ssm) {

    SystemStatusSubsystem::Computers& cs     = ssm->ss_->devices().computers();
    SystemStatusSubsystem::Places&    places = ssm->ss_->places();

    Temperature minTemp, maxTemp;

    minTemp.setC(15.0);
    maxTemp.setC(26.0);

    sensors.push_back(new RemoteSensorTrippLite("ups1.carma.pvt",
						ssm,
						cs.ups1().reachable(),
						cs.ups1().ok(),
						places.computerRoomCF().upsTemp().ok(),
						places.computerRoomCF().upsTemp().status(),
						places.computerRoomCF().upsTemp().sampleTime(),
						places.computerRoomCF().upsTemp().currentTemperature(),
						places.computerRoomCF().upsTemp().lowTemperature(),
						places.computerRoomCF().upsTemp().highTemperature(),
						minTemp, maxTemp,
						defaultReadoutIntervalInSeconds));

    sensors.push_back(new RemoteSensorRrd("nfs2.fileserver.pvt",
					  ssm,
					  cs.nfs2().reachable(),
					  cs.nfs2().ok(),
					  places.computerRoomBldg12().ok(),
					  places.computerRoomBldg12().status(),
					  places.computerRoomBldg12().sampleTime(),
					  places.computerRoomBldg12().currentTemperature(),
					  places.computerRoomBldg12().setTemperature(),
					  minTemp, maxTemp,
					  defaultReadoutIntervalInSeconds));

    sensors.push_back(new RemoteSensorIServer("compTemp.carma.pvt",
					      ssm,
					      cs.compTemp().reachable(),
					      cs.compTemp().ok(),
					      places.computerRoomCF().roomTemp().ok(),
					      places.computerRoomCF().roomTemp().status(),
					      places.computerRoomCF().roomTemp().sampleTime(),
					      places.computerRoomCF().roomTemp().currentTemperature(),
					      minTemp, maxTemp,
					      defaultReadoutIntervalInSeconds));

    minTemp.setC(15.0);
    maxTemp.setC(29.6);

    sensors.push_back(new RemoteSensorIServer("corrTemp.carma.pvt",
					      ssm,
					      cs.corrTemp().reachable(),
					      cs.corrTemp().ok(),
					      places.correlatorRoom().ok(),
					      places.correlatorRoom().status(),
					      places.correlatorRoom().sampleTime(),
					      places.correlatorRoom().currentTemperature(),
					      minTemp, maxTemp,
					      defaultReadoutIntervalInSeconds));

    minTemp.setF(40);
    maxTemp.setF(120);

    sensors.push_back(new RemoteSensorIServer("gen1cool.carma.pvt",
					      ssm,
					      cs.gen1cool().reachable(),
					      cs.gen1cool().ok(),
					      places.generatorBldg1().ok(),
					      places.generatorBldg1().status(),
					      places.generatorBldg1().sampleTime(),
					      places.generatorBldg1().currentTemperature(),
					      minTemp, maxTemp,
					      defaultReadoutIntervalInSeconds));

    sensors.push_back(new RemoteSensorIServer("gen2cool.carma.pvt",
					      ssm,
					      cs.gen2cool().reachable(),
					      cs.gen2cool().ok(),
					      places.generatorBldg2().ok(),
					      places.generatorBldg2().status(),
					      places.generatorBldg2().sampleTime(),
					      places.generatorBldg2().currentTemperature(),
					      minTemp, maxTemp,
					      defaultReadoutIntervalInSeconds));
  } else {

    sensors.push_back(new RemoteSensorTrippLite("ups1.carma.pvt",   
						defaultReadoutIntervalInSeconds));

    sensors.push_back(new RemoteSensorRrd("nfs2.fileserver.pvt",    
					  defaultReadoutIntervalInSeconds));

    sensors.push_back(new RemoteSensorIServer("corrTemp.carma.pvt", 
					      defaultReadoutIntervalInSeconds));

    sensors.push_back(new RemoteSensorIServer("gen1cool.carma.pvt", 
					      defaultReadoutIntervalInSeconds));

    sensors.push_back(new RemoteSensorIServer("gen2cool.carma.pvt", 
					      defaultReadoutIntervalInSeconds));

  }
}
