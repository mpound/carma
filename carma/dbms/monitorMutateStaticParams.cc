/**
 * @author Original Harold Ravlin (from MCL)
 *
 * @usage monitorMutateStaticParams conffile=<configuration file>
 * @description
 * monitorMutateStaticParams supports editing of the mutable fields
 * in the MonitorConfigStaticParms table.
 *
 * @key changefile     none           string  name of file generated
 * by monitorConfigurationLoader to be used to make the change(s).
 * @key conffile dbms/dbms.conf string  file from which to get database config info; the conffile location interpreted by Program::getConfFile()
 *
 * @logger DEFAULT_FACILITY carma.dbms.monitorMutateStaticParams
 */

#include "mysql/mysql_version.h"
//#include <stdlib.h> // for debuging getenv() call
#include "carma/dbms/DBConfigurator.h"
#include "carma/dbms/DBConnection.h"
#include "carma/dbms/MonitorConfigurationDatabase.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"
#include <iostream>
#include <fstream>

using namespace std;
using namespace carma::dbms;

int carma::util::Program::main() {
  std::ifstream infile;

    const string conffile = getConfFile(getStringParameter("conffile"));
    CPTRACE(carma::util::Trace::TRACE6, "getStringParameter(\"conffile\") " 
            << getStringParameter("conffile"));
    CPTRACE(carma::util::Trace::TRACE6, "conffile " << conffile);

    const string changefile = getStringParameter("changefile");
    CPTRACE(carma::util::Trace::TRACE6, "getStringParameter(\"changefile\") " 
            << changefile);

    infile.open(changefile.c_str());
    if(!infile)
    { CPTRACE(carma::util::Trace::TRACE3, "Could not open " << changefile);
      return EXIT_FAILURE;
    }else
      CPTRACE(carma::util::Trace::TRACE6, "Opened input file " << changefile);

    { auto_ptr<DBConfigurator> dbconf(new DBConfigurator(conffile));

            if(DBConnection::isUp(dbconf.get())) {
                CPTRACE(carma::util::Trace::TRACE6, "database is up, "
                        << "trying to create a connection...");
                auto_ptr<DBConnection> dbc;

		try {   auto_ptr<DBConnection> temp
                        (DBConnectionFactory::createConnection(dbconf.get()));
                    dbc = temp;
                    dbc->beginTransaction();
                    CPTRACE(carma::util::Trace::TRACE7,
                            "database connection successfully opened");
                    MonitorConfigurationDatabase mcdb(dbc.get());

		    // Pass each line in the file to mutateStaticParms().
		    while(infile)
		    { string line;
		      getline(infile, line);
		      if(line != "")
			{ CPTRACE(carma::util::Trace::TRACE4, line.c_str());
			  mcdb.mutateStaticParms(line);
			}
		      if(infile.eof())
			break;
		    }

                    dbc->commitTransaction();

                } catch (const DBConnectionException &exc) {
                    exc.report();
                    dbc->rollBackTransaction();
                    return EXIT_FAILURE;
                } catch (const InsertDeniedException &exc) {
                    exc.report();
                    dbc->rollBackTransaction();
                    return EXIT_FAILURE;
                } catch (const SQLException &exc) {
                    exc.report();
                    dbc->rollBackTransaction();
                    return EXIT_FAILURE;
                }
            } else {
                CPTRACE(carma::util::Trace::TRACE4, "Database cannot be "
                        << "contacted so descriptions cannot be loaded into "
                        << "it");
            }
    infile.close();
    return EXIT_SUCCESS;
    }
}
