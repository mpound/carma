/** 
 * @file carma/environment/Test/DBConnectionTest.cc
 *
 * CppUnit test fixture for carma::dbms::DBConnection
 *
 * Author: Dave Mehringer
 */
#include <cmath>
#include <iostream>
#include "TableTest.h"
#include "carma/dbms/Table.h"
#include "carma/util/IllegalArgumentException.h"

using namespace carma::dbms;
using namespace CppUnit;
using namespace std;

void TableTest::setUp() {
}

void TableTest::tearDown() {   
}

void TableTest::populateTests() {
    cout << "***** carma::dbms::Test::tTable *****" << endl;
    string name = "a table";
    Table t(name);
    cout << "Testing name " << endl;
    CPPUNIT_ASSERT( t.getName() == name); 
    cout << "Adding columns..." << endl;
    string cdname = "Double column";
    Column<double> cd(cdname);
    CPPUNIT_ASSERT( cd.getName() == cdname); 
    cd.push_back(22.5);
    cd.push_back(85.2);
    cd.push_back(-27.6);
    cout << cd.toString() << endl;
    t.addColumn<double>(cd);
    cout << "row count " << t.rowCount() << endl;
    cout << "column size " << cd.size() << endl;
    CPPUNIT_ASSERT( t.rowCount() == 3); 
    CPPUNIT_ASSERT( t.columnCount() == 1); 
    string csname = "short column";
    Column<short> cs(csname);
    CPPUNIT_ASSERT( cs.getName() == csname); 
    cs.push_back(5);
    cs.push_back(-1024);
    cs.push_back(-92);
    t.addColumn<short>(cs);
    CPPUNIT_ASSERT( t.rowCount() == 3); 
    CPPUNIT_ASSERT( t.columnCount() == 2); 
    CPPUNIT_ASSERT( t.getColumn<short>(1)[0] == 5);
    Column<string> bad("bad column");
    bad.push_back("s");
    bad.push_back("bbb");
    bad.push_back("gfaskj");
    bad.push_back("aferke");
    // we shouldn't be able to add this column since it has 4 rows where the
    // others have 3
    try {
        t.addColumn<std::string>(bad);
        cout << "We were able to add a column with 4 rows to a 3 row table "
             << "which shouldn't be allowed" << endl;
        CPPUNIT_ASSERT(true);
    } catch (const carma::util::IllegalArgumentException & ) {
        cout << "IllegalArgumentException caught as expected" << endl;
    }
    Column<int> ci("int col");
    ci.push_back(2);
    ci.push_back_null();
    ci.push_back(10);
    CPPUNIT_ASSERT(ci.indexOf(2) == 0);
    cout << "index of 10 is " << ci.indexOf(10) << endl;
    CPPUNIT_ASSERT(ci.indexOf(10) == 2);
    cout << "index of null value ( " << ci.getNullValue() << " ) is " << ci.indexOf(ci.getNullValue()) << endl;
    CPPUNIT_ASSERT(ci.indexOf(ci.getNullValue()) == -1);
    CPPUNIT_ASSERT(ci.indexOf(20) == -1);
    CPPUNIT_ASSERT(ci.hasNulls());
    CPPUNIT_ASSERT(ci.getNullIndices().size() == 1);
    CPPUNIT_ASSERT(ci.getNullIndices()[0] == 1);
    CPPUNIT_ASSERT(!ci.isElementNull(0));
    CPPUNIT_ASSERT(ci.isElementNull(1));
    CPPUNIT_ASSERT(ci[1] == ci.getNullValue());
    ci.setNullValue(295);
    CPPUNIT_ASSERT(ci[1] == ci.getNullValue());
    CPPUNIT_ASSERT(ci.getNullValue() == 295);
    cout << ci.toString() << endl;
    cout << cd.toString() << endl;
    t.addColumn<int>(ci);

    Column<int> k("int");
    k.push_back(5);
    k.push_back(3);
    k.push_back(5);
    k.push_back_null();
    k.push_back_null();
    cout << "k " << k.toString() << endl;
    Column<int> j = k.getDistinctValues();
    cout << "j (distinct k with null) " << j.toString() << endl;
    CPPUNIT_ASSERT(k.size() == 5);
    CPPUNIT_ASSERT(j.size() == 3);
    j = k.getDistinctValues("j",false);
    CPPUNIT_ASSERT(j.size() == 2);
    cout << "j (distinct k without null) " << j.toString() << endl;
}

