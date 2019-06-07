/**
 * @author Dave Mehringer
 *
 * @version 1.0
 * @usage postgresTest
 * @description
 * tests postgres RDBMS
 *
 * @key conffile "conf/dbms/dbms.conf" string  config file to use to use
 *
 * @logger DEFAULT_FACILITY carma.dbms.odbcInfo
 */

#include "carma/util/Program.h"
#include "carma/dbms/DBConfigurator.h"
#include "carma/dbms/DBConnection.h"

#include <iostream>

using namespace std;
using namespace carma::dbms;
using namespace carma::util;



int Program::main() {
    string conffile = Program::getStringParameter("conffile");
    DBConfigurator *dbconf = new DBConfigurator(conffile);
    DBConnection *dbc = 0;
    try {
        dbc = DBConnectionFactory::createConnection(dbconf);
        dbc->odbcInfo();
    }
    catch (const DBConnectionException & exc) {
        exc.report();
    }
    delete dbc;
    delete dbconf;

    return 0;
}

