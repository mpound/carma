/**@file
 * Helper Test class for WbdcMasterTest CppUnit class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.6 $
 * $Date: 2010/07/12 23:01:10 $
 * $Id: ClientTest.h,v 1.6 2010/07/12 23:01:10 abeard Exp $
 */

#ifndef CLIENT_TEST_H
#define CLIENT_TEST_H

// Carma includes
#include "carma/corba/corba.h"
#include "carma/downconverter/common/downconverterSystem.h"

/**
 * Helper class for WbdcMasterTest.
 * 
 * In order to properly test DO calls on the WbdcMasterTest class, 
 * it was necessary to create this client class to invoke the DO methods.
 * The test itself basically consists of spawning a seperate thread which
 * declares this class and invokes the methods defined below.
 */
class ClientTest {
public:

    // Create the test
    ClientTest(carma::downconverter::System_ptr sys);

    // Run the tests
    int runSystemControlsTest();
    int runDownconverterControlsTest();
    int runQuadModControlsTest();
    int runNoiseSourceControlsTest();
    int runLoMonitorControlsTest();

private:
    carma::downconverter::System_var system_;
};
#endif
