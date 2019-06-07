/**
 * @author Dave Mehringer
 *
 * @version 1.0
 * @usage monitorQueryTest tagID=<tagID> conffile=<conffile> locallevel=<loglevel> 
 * @description
 * This program tests the forming and execution of monitor data queries
 *
 * @key tagID    -1 int, tagid of monitor point for forming query
 * @key conffile conf/dbms/dbms.conf string, file to get configuration data
 * @key   localLevel   8       i    level of output for this file
 */

#include <dirent.h>
#include <iostream>
#include <sstream>
#include <string>
#include "carma/dbms/DBConfigurator.h"
#include "carma/dbms/DBConnection.h"
#include "carma/dbms/MonitorDataQueryComponent.h"
#include "carma/dbms/NumericRangeEndPoint.h"
#include "carma/util/NotFoundException.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"

using namespace std;
using namespace carma::dbms;

int carma::util::Program::main() {
    string conffile = Program::getStringParameter("conffile");
    int tagID = Program::getIntParameter("tagID");
    carma::util::Trace::TraceLevel localLevel =
        (carma::util::Trace::TraceLevel)getIntParameter("localLevel");

    carma::dbms::DBConfigurator *dbconf = 0;
    try {
        dbconf = new DBConfigurator(conffile);
    } catch (const NotFoundException & exc) {
        // FIXME change to FATAL in production mode
        getLogger() << log4cpp::Priority::WARN
                    << "Unable to read configation file " << conffile;
        exc.report();
        return EXIT_FAILURE;
    }
    DBConnection *dbc = 0;
    try {
        dbc = DBConnectionFactory::createConnection(dbconf);
        if(dbc == 0) {
            string emsg = "Cannot connect to unknown rdbms " 
                + dbconf->getRDBMS();
            CPTRACE(carma::util::Trace::TRACE0, emsg);
            return EXIT_FAILURE;
        }
        CPTRACE(carma::util::Trace::TRACE6, "Connection successful");
    } catch (const DBConnectionException & exc) {
        delete dbc;
        delete dbconf;
        exc.report();
        return EXIT_FAILURE;
    } catch (const SQLException & exc) {
        delete dbc;
        delete dbconf;
        exc.report();
        return EXIT_FAILURE;
    } catch (const NotFoundException & exc) {
        delete dbc;
        delete dbconf;
        exc.report();
        return EXIT_FAILURE;
    }
    try {
        MonitorDataQueryComponent mdqc(tagID,dbc);
    } catch (const NotFoundException & exc) {
        exc.report();
        delete dbc;
        delete dbconf;
        return EXIT_FAILURE;
    }        
    delete dbc;
    delete dbconf;
    return 0;
}
