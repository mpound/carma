#include <pthread.h>
#include <iostream>

#include "carma/antenna/sza/antenna/control/AntennaMaster.h"

#include "carma/szautil/AntNum.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/Directives.h"
#include "carma/szautil/Logger.h"

#include "carma/util/ErrorException.h"
#include "carma/util/programLogging.h"

using namespace std;
using namespace sza::antenna::control;
using namespace sza::util;

#include "carma/szautil/Program.h"

// Define recognized Program keywords

PROGRAM_KEYWORDS = {
  { "antenna",         "0",                       "i", USAGE "Antenna number"},
  { "host",            "localhost",               "s", USAGE "ACC control host"},
  { "nameserver",      "corba.carma.pvt:20000",   "s", USAGE "Name Server host:port"},
  { "eventserver",     "corba.carma.pvt:10001",   "s", USAGE "Event Server host:port"},
  { "notifyserver",    "corba.carma.pvt:4006",    "s", USAGE "Notification Server host:port"},
  { "debuglevel",      "7",                       "i", USAGE "Debugging level (0==off)"},
  { "simcanbus",       "f",                       "b", USAGE "Simulate having a CANbus?"},
  { "simpmac",         "f",                       "b", USAGE "Simulate having a PMAC?"},
  { "connect",         "f",                       "b", USAGE "Connect to the control program?"},
  { "newcaltert",      "t",                       "b", USAGE "Use new caltert?"},
  { "ifnodeid",        "1",                       "i", USAGE "IF module node id"},
  { "logf",            "(none)",                  "s", USAGE "Logfile prefix"},
  { "logd",            ".",                       "s", USAGE "Logfile directory"},
  { "ignorewraplogic", "t",                       "b", USAGE "True to ignore wrap logic"},
  { END_OF_KEYWORDS},
};

PROGRAM_INITIALIZE_USAGE {};
   
/**.......................................................................
 * Create an AntennaMaster object with subsystems running in separate
 * threads.
 *
 * Defining Program::main() automatically gives us the
 * command-line parsing in Program.  
 */
int Program::main(void)
{
  Debug::setLevel(Program::getiParameter("debuglevel"));

  bool waserr=false;
  //AntennaMaster* ant=0;

  try {

    AntNum antNum;

    // If no value was specified for the antenna number on the
    // command, initialize it from the host name.

    if(isDefault("antenna")) {
      antNum.setIdFromHost();
      cout << "Setting id from host: " << antNum << endl;
    } else {
      antNum.setId(Program::getiParameter("antenna"));
      cout << "Setting id from command line" << endl;
    }

    if(!isDefault("logf")) {
      sza::util::Logger::setLogFilePrefix(getParameter("logf"));
      sza::util::Logger::setLogFileDirectory(getParameter("logd"));
      sza::util::Logger::openLogFile();
    }
    
    // Setup carma logging
    ostringstream logname;
    logname << "carma.antenna.sza" << antNum.getCarmaAntennaIndex();
    //programLogInfo(string("Setting instance logname to: " + logname.str()));
    setInstanceLogname(logname.str()); 
    

    // And create the object which will instantiate the control system

    AntennaMaster master(Program::getParameter("host"),
			 Program::getParameter("nameserver"),
			 antNum.getObjectName(),
			 Program::getParameter("eventserver"),
			 antNum.getEventChannelName(),
			 Program::getParameter("notifyserver"),
			 Program::getbParameter("simcanbus"),
			 antNum,
			 Program::getbParameter("simpmac"),
			 Program::getiParameter("ifnodeid"),
			 Program::getbParameter("connect"),
			 Program::getbParameter("newcaltert"),
			 Program::getExtraArgc(),
			 Program::getExtraArgv(),
			 Program::getbParameter("ignorewraplogic"));

  } catch(Exception& err) {
    COUT("Caught an exception: " << err.what());
    waserr = true;
  } catch(carma::util::ErrorException& err) {
    COUT("Caught an error: " << err.what());
    waserr = true;
  } catch(...) {
    COUT("Caught an unknown exception");
    waserr = true;
  }

  return waserr ? 1 : 0;
}

