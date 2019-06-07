/**
 * @file
 * $Id: loberotatorControlClient.cc,v 1.16 2012/01/27 21:17:58 mpound Exp $
 *
 * @author Chul Gwon
 * @description
 *   Short test code to work out the control calls on the LR
 *
 *
 */

#include <iostream>
#include <string>

#include "carma/corba/corba.h"
#include "carma/corba/Client.h"
#include "carma/util/Program.h"
#include "carma/util/ErrorException.h"
#include "carma/loberotator/Loberotator.h"
#include "carma/loberotator/LoberotatorControl.h"

using namespace std;
using namespace carma::loberotator;
using namespace carma::util;

int runClient();

//
// @noKeys
//
// @logger INTERFEROMETRY_FACILITY carma.interferometry

int carma::util::Program::main() {
  int status;

  status = runClient();

  return status;
}

int runClient() {
    LoberotatorControl_var lr;
    
    try {
        carma::corba::Client & client = Program::getProgram().getCorbaClient();
        // resolve LR
        lr = 
          client.resolveName<LoberotatorControl>("carma.loberotator.loberotatorControl");
        if (CORBA::is_nil(lr)) {
            cout << "Reference not found for LoberotatorControl" << endl;
            return EXIT_FAILURE;
        } 
    } 
    catch(...) {
        cerr << "Caught exception on resolveName" << endl;
    }
   
    int command;

    bool done = false;

    while (!done) {
      cout << "\n Select command: " << endl;
      cout << " [0] setInputDelay" << endl;
      cout << " [1] setInputLOFreq" << endl;
      cout << " [2] artificial fringe methods" << endl;
      cout << " [3] loadPhaseSwitchColumn" << endl;
      cout << " [4] setDDSPhase" << endl;
      cout << " [5] setDDSFreq" << endl;
      cout << " [7] enableDDSPhaseSwitching" << endl;
      cout << " [9] disableDDS" << endl;
      cout << " [10] enableDDS" << endl;
      cout << " [14] exit" << endl;
      cout << " ===> ";
      cin >> command;

  try {

      // throw in some bogus numbers for each of these calls
      switch(command) {
          case 0:
            //lr->setInputDelay(12, 0.01, 53335.2, false);
            break;
          case 1:
           //lr->setInputLOFreq(3, 115.2, 1, 1, 1);
           break;
          case 2:
            lr->setOffsetControl(2, true);
            lr->setOffsetPhase(2, 46.1);
            lr->setOffsetRate(2, 0.12);
            break;
          case 3:
            lr->loadPhaseSwitchColumn(4, 2);
            break;
          case 4:
            //lr->setDDSPhase(104, 7);
            break;
          case 5:
            //lr->setDDSFreq(1, 12000000);
            break;
          case 6:
            //lr->setInputFrequency(15, 115);
            break;
          case 7:
            lr->enablePhaseSwitching(106, true);
            break;
          case 8:
            //lr->setOutputRegs(17, 'a', 'b');
            break;
          case 9:
            // lr->enableDDS(0, false);
            break;
          case 10:
            // lr->enableDDS(0, true);
            break;
          case 11:
            lr->resetBoard(0);
            break;
          case 12:
            //lr->setDDSClock(22, 2.1222);
            break;
          case 13:
            //lr->setDDSRegister(19, 2);
            break;
          case 14:
            done = true;
            break;
          default:
            cerr << "Unrecognized input" << endl;
            break;
          }
      } catch (const CORBA::Exception &ex) {
        cerr << "Corba exception caught!" << endl;
        cerr << ex << endl;
      }
    } // end while(!done)
  return EXIT_SUCCESS;
}
