/**
 * @author Dave Mehringer
 *
 * @version 1.0
 * @usage integrateData rdbms=<rdbms> dbname=<database name> user=<user>
 * @description
 * Integrates data in the half-second table and inserts the average into
 * the integrated table
 *
 * @key rdbms postgres string, which rdbms to use (postgres|mysql)
 * @key dbname none string, scratch database to connect to
 * @key user none string, user to access db as
 * @key tabletype myisam string, table type to create (mysql only, myisam|innodb)
 */

#include <iostream>
#include <sstream>
#include <string>
#include <time.h>
#include "carma/util/Program.h"
#include "carma/util/Time.h"
#include "carma/dbms/PostgresDBConnection.h"
#include "carma/dbms/MySQLDBConnection.h"
#include "mysql/mysql.h"
#include "postgresql/libpq-fe.h"

using namespace std;
using namespace carma::dbms;

int carma::util::Program::main() {
    time_t ta,tb;
    double t_diff;
    string inpRDBMS = Program::getStringParameter("rdbms");
    string inpDBname = Program::getStringParameter("dbname");
    string inpUser = Program::getStringParameter("user");
    string inpTableType = Program::getStringParameter("tabletype");
    stringstream stmt;
    string inTable = "numericMonitorData";
    string outTable = "numericIntegratedMonitorData";
    string dropCommand = "DROP TABLE " + outTable;
    DBConnection *dbc = 0;

    stringstream createCommand;
    string notnull = " NOT NULL";

    createCommand << "CREATE TABLE " << outTable 
                  << " (tagID int" << notnull << ", time int" << notnull 
                  << ", blankingFlag smallint" << notnull << ", "
                  << "validFlag smallint" << notnull 
                  << ", value double precision" << notnull << ", "
                  << "coverage double precision" << notnull 
                  << ", PRIMARY KEY (time, tagID))";
    string rdbms;
    if (inpRDBMS == "postgres") {
        rdbms = PostgresDBConnection::RDBMS;
        PGconn     *conn;
        PGresult   *res;
        /* Make a connection to the database */
        string connectStr = "dbname=" + inpDBname;
        conn = PQconnectdb(connectStr.c_str());
        /* Check to see that the backend connection was successfully made */
        if (PQstatus(conn) != CONNECTION_OK) {
            cerr << "Connection to database " << PQdb(conn) << " failed!" 
                 << endl;
            PQfinish(conn);
            return 1;
        }
        PQexec(conn,dropCommand.c_str());
        cout << dropCommand << endl;
        PQexec(conn, createCommand.str().c_str());
        cout << createCommand << endl;
        PQfinish(conn);
    } else if (inpRDBMS == "mysql") {
        rdbms = MySQLDBConnection::RDBMS;
        MYSQL *sock,mysql;
        mysql_init(&mysql);
        if (!(sock = mysql_real_connect(&mysql,NULL,"root",NULL,
                                        inpDBname.c_str(),0,NULL,0))) {
            cerr << "Couldn't connect to mysql! " << mysql_error(&mysql) 
                 << endl;
            return 1;
        }
        mysql_query(sock,dropCommand.c_str());
        createCommand << " type=" << inpTableType;
        cout << createCommand.str() << endl;
        if(mysql_query(sock,createCommand.str().c_str())) {
            cerr << "Create table failed: " <<  mysql_error(sock) << endl;
            mysql_close(sock);
            return(1);
        }
    }
    uint currentWorkingIntegration;
    uint lastWorkingIntegration = 0;
    uint maxMonitorTime = 0;
    uint *maxMTPtr = 0;
    bool behind = false;
    try {
        cout << "Making odbc connection" << endl;
        dbc = DBConnectionFactory::createConnection(rdbms);
        if (dbc == NULL) {
            throw DBConnectionException("Unknown RDBMS " + inpRDBMS,__FILE__,
                                        __LINE__);
        }
        cout << "odbc connection successful" << endl;
        cout << "Integrating data..." << endl;
        while(true) {
            if(!behind) {
                ta = time(NULL);
                maxMTPtr = dbc->getAggregateUInt("time",inTable,"max");
                tb = time(NULL);
                t_diff = difftime(tb,ta);
                cout << inpRDBMS << " time to find max(time) " << t_diff 
                     << " seconds" << endl;
            }
            while(!behind && maxMTPtr == NULL) {
                cout << "Waiting for first data in " << inTable << endl;
                sleep(15);
                maxMTPtr = dbc->getAggregateUInt("time",inTable,"max");
            }
            if(maxMTPtr != NULL) {
                maxMonitorTime = *maxMTPtr;
                delete maxMTPtr;
                maxMTPtr = 0;
            }
            cout << "Max time in " << inTable << " is " << maxMonitorTime 
                 << endl;
            // the " - 1 " prevents integrating of data that are still in the
            // process of being written
            currentWorkingIntegration = ((maxMonitorTime)/120) - 1;
            if(lastWorkingIntegration == 0) {
                // first time thru
                lastWorkingIntegration = currentWorkingIntegration - 1;
            }
            if(currentWorkingIntegration == lastWorkingIntegration) {
                sleep(60);
            } else {
                if(currentWorkingIntegration > (lastWorkingIntegration + 1)) {
                    // prevent skips
                    behind = true;
                    currentWorkingIntegration = lastWorkingIntegration + 1;
                } else {
                    behind = false;
                }
                ta = time(NULL);
                dbc->integrate(120*lastWorkingIntegration, 
                               120*currentWorkingIntegration, inTable, 
                               outTable);
                tb = time(NULL);
                t_diff = difftime(tb,ta);
                cout << inpRDBMS << " time to integrate float rows " << t_diff 
                     << " seconds" << endl;
                lastWorkingIntegration = currentWorkingIntegration;
            }
        }
    } catch (const DBConnectionException & exc) {
        exc.report();
    }
    delete dbc;
    return 0;
}
