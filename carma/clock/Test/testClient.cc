/** @file
 * $Id: testClient.cc,v 1.15 2012/01/17 22:10:04 mpound Exp $
 * $Author: mpound $
 *
 // @version  Version: $Revision: 1.15 $ $Date: 2012/01/17 22:10:04 $
 *
 // @usage  testClient [imr=imrhost]
 *
 // @description
 //   This is a test program for the MasterClock class.  This will be
 //   used to do a quick test of the CORBA functionality of the Master
 //   Clock by calling the initialize command
 *
 // @key none  0  i   no arguments
 *
 // @logger TEST_FACILITY carma.test.clock.testClient
 */

// system includes
#include <string>
#include <unistd.h>

// carma includes
#include "carma/corba/corba.h"
#include "carma/util/Program.h"
#include "carma/corba/Client.h"
#include "carma/clock/clockSystem.h"

int runClient();

int carma::util::Program::main() {
  int status = runClient();
  return status;
}
            
int runClient() {
  int status = EXIT_SUCCESS;
  
  // resolve Master Clock
  try {
      carma::corba::Client & client = carma::util::Program::getProgram().getCorbaClient();
      carma::clock::System_var system = 
         client.resolveName<carma::clock::System>("carma.clock.ClockControl");
    if (CORBA::is_nil(system)) {
      std::cerr << "System reference not found for Clock." << std::endl;
      return EXIT_FAILURE;
    }
    
    // create new MasterClock object -- NOTE!!!  need to define node
    // correctly 
    carma::clock::ClockControl *clock = system->Clock();
    
    clock->initialize(carma::clock::GPSPPS,
		      carma::clock::FREE,
		      carma::clock::GPSB,
		      carma::clock::INTERNAL,
		      carma::clock::ENABLE);
    std::cout << "Initialize Clock Device." << std::endl;

  } catch (const CORBA::Exception& ex) {
    std::cerr << "Corba system exception caught - exiting." << std::endl;
    std::cerr << ex << std::endl;
  } catch (...) {
    std::cerr << "Unknown exception" << std::endl;
  }
  return status;
}
