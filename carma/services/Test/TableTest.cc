/** 
 * @file 
 *
 * $Id: TableTest.cc,v 1.4 2009/05/27 00:19:01 mpound Exp $
 *
 * CppUnit test fixture for carma::services::Table
 *
 * Author: Peter Teuben
 * Version: $Revision: 1.4 $
 * $Date: 2009/05/27 00:19:01 $
 *
 */
#include <cmath>
#include <limits>
#include <iostream>
#include "TableTest.h"
#include "carma/util/Program.h"
#include "carma/util/ErrorException.h"
#include "carma/util/NumberFormatException.h"


using namespace CppUnit;
using namespace std;
using namespace carma::services;
using namespace carma::util;

void TableTest::setUp() 
{   

}

void TableTest::tearDown() 
{   

}

void TableTest::testTable1()
{

    Table t;
    string catalog = Program::getConfFile("catalogs/Observatory.cat");
    t.open(catalog);
    std::vector<std::string> name = t.getColumn("obs");

}

void TableTest::testBug454()
{

    bool OK = true;
    ostringstream os;
    try {
        cout << "Trying to open a truncated table...";
        Table t;
        string catalog("carma_src/carma/services/Test/bad.table");
        t.open(catalog);
        cout << "OK." << endl;
    } catch ( const ErrorException & ex ) {
        OK = false;
        cout << "Caught exception: " << ex.getMessage() << endl;
    }

    CPPUNIT_ASSERT( OK );

}

void TableTest::testDataVerification()
{
    bool OK = false;
    const string catalog = Program::getConfFile("data/badformat.cat");
    try {
        cout << "Reading a catalog columnn with badly formatted data ["
             << catalog <<"]...";
        Table t;
        t.open(catalog);
        // this should throw
        std::vector<double> x = t.getDoubleColumnAndVerify("Velocity");
    } catch ( const NumberFormatException & nfe ) {
        OK = true;
        cout << "NumberFormat Exception correctly thrown." << endl;
    } catch ( const ErrorException & ee ) {
        OK = false;
        cout << "WRONG EXCEPTION: " << ee.getMessage() << endl;
    } catch ( ... ) {
        OK = false;
        cout << "REALLY WRONG EXCEPTION" << endl;
    }

    CPPUNIT_ASSERT( OK );

    bool RAOK = false;
    try {
        Table t;
        cout << "Reading catalog column with missing RA colon...";
        t.open(catalog);
        // this should throw
        std::vector<double> x = t.getHMSColumn("RA");
    } catch ( const ErrorException & ee ) {
        RAOK = true;
        cout << "Error Exception correctly thrown." << endl;
    } catch ( ... ) {
        RAOK = false;
        cout << "CAUGHT WRONG EXCEPTION" << endl;
    }

    CPPUNIT_ASSERT( RAOK );

}
