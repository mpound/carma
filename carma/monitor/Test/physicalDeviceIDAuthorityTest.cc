/**
 * @file
 * Implementation of the unit tests for PhysicalDeviceIDAuthority
 *
 * @author: Dave Mehringer
 * $CarmaCopyright$
 */

//
// @usage physicalDeviceIDAuthority conffile=<conffile>
//
// @description 
// Test for PhysicalDeviceIDAuthority
//
// @key conffile "default" string  dbms conffile to use
//
// @logger TEST_FACILITY carma.test.monitor.physDevIdAuthTest
//

//#include <cppunit/TextTestRunner.h>
#include "carma/dbms/PhysicalDeviceIDAuthority.h"

//#include <unistd.h>
#include "carma/util/Program.h"
#include "carma/dbms/DBConnection.h"
#include "carma/dbms/DBConfigurator.h"
#include "carma/dbms/MonitorConfigurationDatabase.h"
#include "carma/util/CommonExceptions.h"

using namespace std;
using namespace carma;
using namespace carma::dbms;
using namespace carma::util;

int Program::main() {
    string conffile = getConfFile(getStringParameter("conffile"));
    DBConfigurator *dbconf = new DBConfigurator(conffile);
    DBConnection *dbc = 0;
    try {
        dbc = DBConnectionFactory::createConnection(dbconf);
        // populate tables used in this test
        //dbc->populateLocationTable();
        MonitorConfigurationDatabase mcdb(dbc);
        mcdb.populateLocationsTable();
        //dbc->populateDeviceTable();
        mcdb.populateDevicesTable();
        //Table locations = dbc->getLocationTable();
        Table locations = mcdb.getLocationsTable();
        cout << locations.toString() << endl;
        //Table devices = dbc->getDeviceTable();
        Table devices = mcdb.getDevicesTable();
        cout << devices.toString() << endl;
        delete dbc;
        dbc = 0;
    } catch ( const DBConnectionException & exc ) {
        cout << "DBConnectionException caught" << endl;
        cout << exc.what() << endl;
        delete dbc;
        return EXIT_FAILURE;
    }
    dbms::PhysicalDeviceIDAuthority pdida 
        = dbms::PhysicalDeviceIDAuthority::getAuthority(true,dbconf);
    cout << "Number of locations is " << pdida.getLocationCount() << endl;
    cout << "Number of devices is " << pdida.getDeviceCount() << endl;
    cout << "Location ID 5 is assigned to " << pdida.getLocation(5) << endl;
    cout << "Location BIMA5 has ID " << pdida.getLocationID("BIMA5") << endl;
    // do some sanity checks
    try {
        pdida.getLocation(999);
        cout << "Location ID 999 exists but it shouldn't!" << endl;
        return EXIT_FAILURE;
    } catch ( const NotFoundException & exc ) {
        cout << "Exception caught as it should be, message follows " << endl;
        cout << exc.what() << endl;
    }
    try {
        pdida.getDevice(2099);
        cout << "Device ID 2099 exists but it shouldn't!" << endl;
        return EXIT_FAILURE;
    } catch ( const NotFoundException & exc ) {
        cout << "Exception caught as it should be, message follows " << endl;
        cout << exc.what() << endl;
    }
    try {
        pdida.getLocationID("a bad place");
        cout << "Location 'a bad place' exists but it shouldn't!" << endl;
        return EXIT_FAILURE;
    } catch ( const NotFoundException & exc ) {
        cout << "Exception caught as it should be, message follows " << endl;
        cout << exc.what() << endl;
    }
    try {
        pdida.getDeviceID("a bad device");
        cout << "Device 'a bad device' exists but it shouldn't!" << endl;
        return EXIT_FAILURE;
    } catch ( const NotFoundException & exc ) {
        cout << "Exception caught as it should be, message follows " << endl;
        cout << exc.what() << endl;
    }
    pdida.closeAuthority();
    return EXIT_SUCCESS;

}

