#include <iostream>

#include "carma/szautil/Exception.h"
#include "carma/szautil/Program.h"

#include "carma/antenna/sza/antenna/control/PmacComms.h"

using namespace std;
using namespace sza::util;

PROGRAM_KEYWORDS = {
  { "setip",         "f",          "b", USAGE "True to set IP address.  False to query it"},
  { "setipaddr",     "192.6.94.5", "s", USAGE "IP address to set, if setip=t"},
  { "connectipaddr", "192.6.94.5", "s", USAGE "IP address to connect to"},
  { END_OF_KEYWORDS}
};

PROGRAM_INITIALIZE_USAGE {};

using namespace sza::util;
using namespace sza::antenna::control;

int Program::main(void)
{
  PmacComms comms;

  try {
    if(comms.connect(Program::getParameter("connectipaddr"))) {
      
      if(Program::getBoolParameter("setip")) {
	comms.setIpAddress(Program::getParameter("setipaddr"));
      } else {
	COUT("Calling getIpAddress()");
	COUT("IP address is now: " << comms.getIpAddress());
      }
      
    } else {
      COUT("Unable to connect to the pmac");
    }
  } catch(Exception& err) {
    COUT("Caught an error: " << err.what());
  }

  sleep(5);

  return 0;
}
