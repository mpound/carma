/**
 * @file
 * Implementation of the unit tests for the dbms library
 *
 * @author: Dave Mehringer
 * @version $Id: dbmsTest.cc,v 1.15 2006/12/01 22:31:14 hravlin Exp $
 *
 * $CarmaCopyright$
 *
 * @usage  dbmsTest [--keywords] [--help] [--usage] [conffile=[conf/dbms/dbms.test.conf]]
 *
 * @key conffile conf/dbms/dbms.test.conf string  db config file to use
 * @key mpmlfile none                     string  mpml file to use in test
 * @key testconnection	true		bool	Run connection test?
 * @key testdbffio	true		bool	Run dbFFIO test?
 *
 * @logger TEST_FACILITY carma.test.dbms.dbmsTest
 */



#if !defined(CPPUNIT_STD_NEED_ALLOCATOR)
#define CPPUNIT_STD_NEED_ALLOCATOR 0
#endif

#include <cppunit/TextTestRunner.h>
#include "carma/util/Program.h"
#include "DBConnectionTest.h"
#include "dbFFIOTest.h"
#include <unistd.h>

using namespace std;
using namespace carma::dbms;
using namespace carma::util;

int Program::main() {
    
    DBConnectionTest::conffile = getStringParameter("conffile");
    DBConnectionTest::mpmlFileName = getStringParameter("mpmlfile");
    bool testconnection = getBoolParameter("testconnection");
    bool testdbffio = getBoolParameter("testdbffio");
    CppUnit::TextTestRunner runner;
    // Add in test suites here
    if(testconnection)
      runner.addTest( DBConnectionTest::suite() );
    else
      cout << "Skipping DBConnectionTest\n";
    if(testdbffio)
      runner.addTest( dbFFIOTest::suite() );
    else
      cout << "Skipping dbFFIO test\n";

    bool ok = runner.run();
    return ok ? EXIT_SUCCESS : EXIT_FAILURE;
}

