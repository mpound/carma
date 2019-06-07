#include <iostream>
#include <iomanip>

#include <cmath>

#include "carma/szautil/Program.h"
#include "carma/szautil/RemoteSensorIServer.h"
#include "carma/szautil/RemoteSensorTrippLite.h"
#include "carma/szautil/RemoteSensorRrd.h"

#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/SystemStatus.h"

using namespace std;
using namespace sza::util;
using namespace carma::monitor;

PROGRAM_KEYWORDS = {
  { "readoutInterval",  "30", "i", USAGE "Sensor readout interval, in seconds"},
  { END_OF_KEYWORDS}
};

PROGRAM_INITIALIZE_USAGE {};

/**.......................................................................
 * Main -- readout thermometry pertinent to the CARMA system
 */
int Program::main()
{
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
    ss = new sza::util::SystemStatusSubsystemMutex();
  } catch(...) {
    ss = 0;
  }

  //------------------------------------------------------------
  // Now allocate the array of sensors we will read out
  //------------------------------------------------------------

  std::vector<RemoteSensor*> sensors;

#if 0
  if(ss) {

    COUT("Running with CARMA Monitor system");

    SystemStatusSubsystem::Computers& cmps   = ss->ss_->devices().computers();
    SystemStatusSubsystem::Places&    places = ss->ss_->places();

    sensors.push_back(new RemoteSensorTrippLite("ups1.carma.pvt",
						ss,
						cmps.ups1().reachable(),
						places.computerRoomCF().currentTemperature(),
						places.computerRoomCF().lowTemperature(),
						places.computerRoomCF().highTemperature()));

    sensors.push_back(new RemoteSensorRrd("nfs2.fileserver.pvt",
					  ss,
					  cmps.nfs2().reachable(),
					  places.computerRoomBldg12().currentTemperature(),
					  places.computerRoomBldg12().setTemperature()));

    sensors.push_back(new RemoteSensorIServer("corrTemp.carma.pvt",
					      ss,
					      cmps.corrTemp().reachable(),
					      places.correlatorRoom().currentTemperature()));

    sensors.push_back(new RemoteSensorIServer("gen1cool.carma.pvt",
					      ss,
					      cmps.gen1cool().reachable(),
					      places.generatorBldg1().currentTemperature()));

    sensors.push_back(new RemoteSensorIServer("gen2cool.carma.pvt",
					      ss,
					      cmps.gen2cool().reachable(),
					      places.generatorBldg1().currentTemperature()));

  } else {

    COUT("Running without CARMA Monitor system");

#if 0
    sensors.push_back(new RemoteSensorTrippLite("ups1.carma.pvt"));
    sensors.push_back(new RemoteSensorRrd("nfs2.fileserver.pvt"));
    sensors.push_back(new RemoteSensorIServer("corrTemp.carma.pvt"));
    sensors.push_back(new RemoteSensorIServer("gen1cool.carma.pvt"));
    sensors.push_back(new RemoteSensorIServer("gen2cool.carma.pvt"));
#else
    sensors.push_back(new RemoteSensorTrippLite("ups1.carma.pvt",   readoutInterval));
    sensors.push_back(new RemoteSensorRrd("nfs2.fileserver.pvt",    readoutInterval));
    sensors.push_back(new RemoteSensorIServer("corrTemp.carma.pvt", readoutInterval));
    sensors.push_back(new RemoteSensorIServer("gen1cool.carma.pvt", readoutInterval));
    sensors.push_back(new RemoteSensorIServer("gen2cool.carma.pvt", readoutInterval));
#endif

  }
#endif
  //------------------------------------------------------------
  // Now spawn the sensors in their own threads
  //------------------------------------------------------------

  for(unsigned i=0; i < sensors.size(); i++) {
    sensors[i]->spawn();
  }

  //------------------------------------------------------------
  // And loop forever, reading the sensors every readoutInterval
  // seconds
  //------------------------------------------------------------

#if 0
  try {

    while(true) {

      for(unsigned i=0; i < sensors.size(); i++) {
	sensors[i]->readSensor();
      }

      sleep(readoutInterval);

      for(unsigned i=0; i < sensors.size(); i++) {
	sensors[i]->printTemps();
      }

    }

  } catch(Exception& err) {
    return 1;
  }
#else
  sensors[0]->blockForever();
#endif

  return 0;
}
