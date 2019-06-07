
/**
 *
 * Simple test for Walsh function generation 
 *
 * @author: Steve Scott
 * $Id: tWalsh.cc,v 1.2 2006/03/21 22:34:46 tcosta Exp $
 *
 * $CarmaCopyright$
 *
 */

 
#include "carma/util/Program.h"
#include "carma/util/WalshFunction.h"
#include "carma/util/ErrorException.h"

#include <iostream>

//
// @version	$Revision: 1.2 $ $Date: 2006/03/21 22:34:46 $ 
//
// @usage	test of methods to get root and conf directories in Program
//
// @logger TEST_FACILITY  carma.test.util.tWalsh
// @key antennaNo 1 int  which one am I?
// @description
//	Test program for Walsh functions
//

using namespace std;
using namespace carma::util;

int carma::util::Program::main()
{

    bool printIt = false; // get printouts
    int nStates = 2;
    for (int i=0; i<6; i++) {
        const WalshFunction& w = WalshFunction(nStates);
        if (printIt) {
            w.graph();
            cout << endl;
        }
        nStates *= 2;
    }
    
    WalshFunction w(16);
    if (printIt) w.dump(true);

    bool caughtIt = false;
    try {
        w.getValue(-1, 3);
    }
    catch (ErrorException& e) {        
        if (printIt) cerr << "Caught expected exception:\n" << e << endl;
        caughtIt = true;
    }
    if (!caughtIt) {
        cerr << "Did not catch expected exception" << endl;
        return EXIT_FAILURE;
    }

    caughtIt = false;
    try {
        w.getValue(4, 17);
    }
    catch (ErrorException& e) {        
        caughtIt = true;
    }
    if (!caughtIt) {
        cerr << "Did not catch expected exception" << endl;
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}





