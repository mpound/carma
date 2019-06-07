/** @file
 * CppUnit test application for Carma Wideband, Spectral-line and Common
 * Downconverter classes.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.5 $
 * $Date: 2012/01/25 22:26:07 $
 * $Id: wbdcTest.cc,v 1.5 2012/01/25 22:26:07 abeard Exp $
 */

// System includes
#include <unistd.h>
#include <signal.h>

// CPPUNIT includes
#include "cppunit/TextTestRunner.h"

// CARMA includes
#include "carma/downconverter/Test/WbdcMasterTest.h"
#include "carma/util/Program.h"
#include "carma/util/ExceptionUtils.h"

using namespace std;

void setSigAction(); 

/**
 * @version $Revision: 1.5 $
 *
 * @usage test
 *
 * @description
 * \ntest is a complete CppUnit test application.  It effectively tests all
 * user defined classes in the downconverter/common library, wideband and
 * spectral-line applications.  Please do not let the small number of tests
 * reported by CppUnit fool you (less than 5).  The number of tests was kept
 * to a minimum to cut down on the excessive overhead of creating and destroying
 * the Master and Server classes which is done for each tests.  In addition
 * the classes implement the carma::canbus DO framework and as such it is 
 * very difficult to test the classes stand-alone, thus they are tested within
 * the context of their running framework.  This test application should 
 * produce atleast 80% coverage which is reported to the user when running
 * 'make tests' from the $(CARMA)/carma/downconverter parent directory.
 *
 * @noKeys
 *
 * @logger TEST_FACILITY carma.test.downconverter.wbdcTest
 *
 */
int carma::util::Program::main()
{
    setSigAction();
    try {
        CppUnit::TextTestRunner runner;
        runner.addTest( WbdcMasterTest::suite() );
        runner.run();
        return EXIT_SUCCESS;
    } catch (...) {
        cerr << getStringForCaught() << endl;
        return EXIT_FAILURE;
    }
}

// -----------------------------------------------------------------------------
void setSigAction() 
{
    int SIGNUM = 2; // SIGINT
    struct sigaction action;
    // action.sa_flags = SA_SIGINFO;
    sigfillset (&(action.sa_mask));
    action.sa_handler = SIG_DFL; // Specify Default action - abort().
    action.sa_restorer = NULL;
    sigaction (SIGNUM, &action, NULL);
}
