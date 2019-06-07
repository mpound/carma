/**
 * @file
 * Implementation of the unit tests for the MonitorDataQueryManager class
 *
 * @author: Dave Mehringer
 *
 * @version $Revision: 1.8 $
 *
 * $CarmaCopyright$
 *
 */

// @usage  tMonitorDataQueryManager [conffile=[conf/dbms/dbms.conf]]
// @description 
// Test for the MonitorDataQueryManager class
// @key conffile dbms/dbms.conf string file from which to get directory info on where to put files and where to create symlinks, with the file location interpreted by Program::getConfFile()
// @logger TEST_FACILITY carma.test.dbms.tMonitorDataQueryManager

#include <cppunit/TextTestRunner.h>
#include "carma/util/Program.h"
#include "carma/util/Trace.h"
#include "MonitorDataQueryManagerTest.h"

#include "carma/dbms/DBConfigurator.h"
#include "carma/dbms/DBConnection.h"

#include <unistd.h>
#include <iostream>

using namespace std;
using namespace carma::dbms;
using namespace carma::util;

int Program::main() {
    MonitorDataQueryManagerTest::setConffile
        (getConfFile(getStringParameter("conffile")));
    CppUnit::TextTestRunner runner;
    // Add in test suites here
    runner.addTest( MonitorDataQueryManagerTest::suite() );
    bool ok = runner.run();
    return ok ? EXIT_SUCCESS : EXIT_FAILURE;
}

