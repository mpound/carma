
/**
 * $Id: tSignalPath.cc,v 1.3 2006/04/14 18:40:47 tcosta Exp $
 *
 * @author Marc Pound
 *
 * @version $Revision: 1.3 $
 * @description
 * test program for the SignalPath to find the bug in it.
 *
 * @noKeys
 * @logger TEST_FACILITY carma.test.monitor.tSignalPath
 *
 */

#include "carma/monitor/SignalPath.h"
#include "carma/util/Program.h"
#include <iostream>
using namespace std;

int carma::util::Program::main() {

    try {
	carma::monitor::SignalPath sp;
	cout << "trying getCarmaAntennaNo()..." << endl;
	cout << "Ovro1 => " << sp.getCarmaAntennaNo("Ovro1") << endl;
	cout << "Ovro2 => " << sp.getCarmaAntennaNo("Ovro2") << endl;
	cout << "Ovro3 => " << sp.getCarmaAntennaNo("Ovro3") << endl;
	cout << "Ovro4 => " << sp.getCarmaAntennaNo("Ovro4") << endl;
	cout << "Bima1 => " << sp.getCarmaAntennaNo("Bima1") << endl;
	cout << "Bima2 => " << sp.getCarmaAntennaNo("Bima2") << endl;
	cout << "Bima3 => " << sp.getCarmaAntennaNo("Bima3") << endl;
	cout << "Sza4=> " << sp.getCarmaAntennaNo("Sza4") << endl;
	cout << "trying getCarmaAntennaName()..." << endl;
	cout << "1 => " << sp.getCarmaAntennaName(1) << endl;
	cout << "2 => " << sp.getCarmaAntennaName(2) << endl;
	cout << "3 => " << sp.getCarmaAntennaName(3) << endl;
	cout << "4 => " << sp.getCarmaAntennaName(4) << endl;
	cout << "9 => " << sp.getCarmaAntennaName(9) << endl;
	cout << "10 => " << sp.getCarmaAntennaName(10) << endl;
	cout << "11 => " << sp.getCarmaAntennaName(11) << endl;
	cout << "22 => " << sp.getCarmaAntennaName(22) << endl;
	cout << "23 => " << sp.getCarmaAntennaName(23) << endl;
	cout << "trying getInputNo()..." << endl;
	cout << "Ovro1 => " << sp.getInputNo("Ovro1") << endl;
	cout << "Bima1=> " << sp.getInputNo("Bima1") << endl;
	cout << "Bima9 => " << sp.getInputNo("Bima9") << endl;
	cout << "Sza1=> " << sp.getInputNo("Sza1") << endl;
    } catch ( const BaseException& be ) {
	cout << "Program::main caught error. " 
	     << be.getMessage()
	     << endl;
	return EXIT_FAILURE;
    } catch (...) {
	cout << "Program::main caught unknown error. Bye" << endl;
	return EXIT_FAILURE;
    }
    
    return EXIT_SUCCESS;
}
