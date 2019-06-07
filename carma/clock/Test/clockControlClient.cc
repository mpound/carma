/** @file
 * $Id: clockControlClient.cc,v 1.14 2012/01/17 22:10:04 mpound Exp $
 * Author: Chul Gwon
 *
 // @version  Version: $Revision: 1.14 $
 *
 // @usage  clockControlClient [imr=imrhost]
 *
 // @description
 //   This is a test program for the MasterClock class.  This will be
 //   used to test the CORBA functionality of the Master Clock
 *
 // @noKeys
 *
 // @logger TEST_FACILITY carma.test.clock.clockControlClient
 *
 */

// system includes
#include <iostream>
#include <string>
#include <unistd.h>

// carma includes
#include "carma/corba/corba.h"
#include "carma/util/Program.h"
#include "carma/corba/Client.h"
#include "carma/clock/clockSystem.h"

using namespace std;

int runClient();

int carma::util::Program::main() {

  int status = runClient();
  return status;

}
            
int runClient() {
  int status = EXIT_SUCCESS;
  
  // resolve Master Clock
  try {
    carma::corba::Client  & client = carma::util::Program::getProgram().getCorbaClient();
    carma::clock::System_var system = 
      client.resolveName<carma::clock::System>("carma.clock.ClockControl");
    if (CORBA::is_nil(system)) {
      std::cerr << "System reference not found for Clock." << endl;
      return EXIT_FAILURE;
    }
    
    // create new MasterClock object -- NOTE!!!  need to define node
    // correctly 
    carma::clock::ClockControl *clock = system->Clock();

    int commandSelection;

    bool done = false;
    while (!done) {
      cout << "\nSelect command : " << endl;
      cout << " [0] initialize " << endl;
      cout << " [1] set PPS mode" << endl;
      cout << " [2] set Rb mode " << endl;
      cout << " [3] resync 10MHz with GPS " << endl;
      cout << " [4] set GPS source " << endl;
      cout << " [5] set 10MHz source " << endl;
      cout << " [6] set heartbeat delay " << endl;
      cout << " [7] reset Rb " << endl;
      cout << " [8] exit " << endl;
      cout << " ==> ";
      cin >> commandSelection;

      CORBA::UShort ppsSource, rbMode, 
	gpsSource, tenMHzSource, walshSync,
	delay, reg;
      switch (commandSelection) {
      case 0:
	cout << "\n Initial PPS source [0 = GPS, 1 = 10MHz]: ";
	cin >> ppsSource;
	cout << "rbMode [0 = free, 1 = GPS locked]: ";
	cin >> rbMode;
	cout << "GPS source [0 = GPSA, 1 = GPSB]: ";
	cin >> gpsSource;
	cout << "10MHz source [0 = internal, 1 = external]: ";
	cin >> tenMHzSource;
	cout << "align state [0 = enable, 1 = disable]: ";
	cin >> walshSync;
	clock->initialize((carma::clock::ppsModeType)ppsSource,
			  (carma::clock::rbModeType) rbMode,
			  (carma::clock::gpsSourceType)gpsSource,
			  (carma::clock::tenMHzSourceType)tenMHzSource,
			  (carma::clock::walshSyncType)walshSync);
	cout << "Initialize Clock Device." << endl;
	break;
      case 1:
	cout << "\n PPS source [0 = GPS, 1 = 10MHz]: ";
	cin >> ppsSource;
	clock->setPpsMode((carma::clock::ppsModeType)ppsSource);
	break;
      case 2:
	cout << "rbMode [0 = free, 1 = GPS locked]: ";
	cin >> rbMode;
	clock->setRbMode((carma::clock::rbModeType)rbMode);
	break;
      case 3:
	cout << "align state [0 = enable, 1 = disable]: ";
	cin >> walshSync;
	clock->resync10MHzGps((carma::clock::walshSyncType)walshSync);
	break;
      case 4:
	cout << "GPS source [0 = GPSA, 1 = GPSB]: ";
	cin >> gpsSource;
	clock->setGpsSource((carma::clock::gpsSourceType)gpsSource);
	break;
      case 5:
	cout << "10MHz source [0 = internal, 1 = external]: ";
	cin >> tenMHzSource;
	clock->set10MHzSource((carma::clock::tenMHzSourceType)tenMHzSource);
	break;
      case 6:
	cout << "delay : ";
	cin >> delay;
	cout << "register [1, 2]: ";
	cin >> reg;
	clock->setHbDelay(delay, (carma::clock::delayRegister)reg);
	break;
      case 7:
	clock->resetRb();
	break;
      case 8:
	done = true;
	break;
      default:
	cout << "Please choose a value from 0-8" << endl;
	break;
      }
    }
  } catch (const CORBA::Exception& ex) {
    std::cerr << "Corba system exception caught - exiting." << endl;
    std::cerr << ex << endl;
  } catch (...) {
    std::cerr << "Unknown exception" << endl;
  }
  return status;
}
