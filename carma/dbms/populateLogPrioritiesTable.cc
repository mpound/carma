/**
 * @author Original Dave Mehringer
 *
 * $id$
 *
 * @usage populateLogPrioritiesTable [conffile=[dbms/dbms.conf]]
 * @description
 * populateLogPrioritiesTable populates the LogPrioritiesTable in the database
 * and should be run as part of CARMA db initialization
 * @key conffile dbms/dbms.conf string  file from which to get database config
 *                                      info; the conffile location interpreted
 *                                      by Program::getConfFile()
 *
 * @logger DEFAULT_FACILITY carma.dbms.monitorConfigurationLoader
 */

#include "carma/dbms/DBConfigurator.h"
#include "carma/dbms/DBConnection.h"
#include "carma/dbms/LogDatabase.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"

using namespace std;
using namespace carma::dbms;

int carma::util::Program::main() {
    const string conffile = getConfFile(getStringParameter("conffile"));
    auto_ptr<DBConfigurator> dbconf(new DBConfigurator(conffile));
    if(!DBConnection::isUp(dbconf.get())) {
      util::Program::getLogger() << ::log4cpp::Priority::ERROR 
				 << "Database cannot be contacted";
      return EXIT_FAILURE;
    }
    CPTRACE(carma::util::Trace::TRACEALL, "database is up, "
	    << "trying to create a connection...");
    auto_ptr<DBConnection> dbc;
    try {
      auto_ptr<DBConnection> temp
	(DBConnectionFactory::createConnection(dbconf.get()));
      dbc = temp;
      dbc->beginTransaction();
      CPTRACE(carma::util::Trace::TRACEALL, 
	      "database connection successfully opened");
      LogDatabase logdb(dbc.get());
      logdb.populateLogPrioritiesTable();
      dbc->commitTransaction();
    } catch (const DBConnectionException &exc) {
      cerr << "DBConnectionException caught" << endl;
      exc.report();
      dbc->rollBackTransaction();
      return EXIT_FAILURE;
    } catch (const InsertDeniedException &exc) {
      cerr << "InsertDeniedException caught" << endl;
      exc.report();
      dbc->rollBackTransaction();
      return EXIT_FAILURE;
    } catch (const SQLException &exc) {
      cerr << "SQLException caught" << endl;
      exc.report();
      dbc->rollBackTransaction();
      return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}
