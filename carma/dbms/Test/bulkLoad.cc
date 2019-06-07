/**
 * @author Dave Mehringer
 *
 * @version 1.0
 * @usage bulkLoad rdbms=<rdbms> useProdDB=<0|1> @pk=<use primary keys> 
 *        trans=<use transactions> datafile=<datafile> | directory=<dirname>
 *        tabletype=<mysql table type> append=<append>
 * @description
 * times bulk loading of data into the rdbms
 *
 * @key rdbms postgres string, which rdbms to use (postgres|mysql)
 * @key useProdDB 0 int, use the production database?
 * @key user none string, user to access db as
 * @key pk 1 int, use primary keys
 * @key trans 1 int, use transactions?
 * @key datafile none string, file containing data to load
 * @key directory none string, directory containing files containing data to load, the files are required to have the extension .done and will be deleted by this program after data are loaded. datafile and dirname are mutually exclusive, if both are provided, datafile is used and dirname is ignored
 * @key tabletype MyISAM string, mysql table type to create
 * @key append 0 int, delete an existing table (0), or append to an existing table (1) 
 * @key wait 0 int, number of frames to wait before exiting.  Only is used whendirectory is set.  This is useful when writeDataTest is being run simultaneously with nfpf > 0.  This program will wait the specified number of frames when it can't find any files to load.  This gives writeDataTest the chance to write more files.  Generally, this should be set to slightly greater than the nfpf value which writeDataTest was run with. 
 * @key   localLevel   8       i    level of output for this file
 */

#include <dirent.h>
#include <iostream>
#include <sstream>
#include <string>
#include <sys/types.h>
#include <time.h>
#include "carma/dbms/PostgresDBConnection.h"
#include "carma/dbms/MySQLDBConnection.h"
#include "carma/dbms/TableManager.h"
#include "carma/util/Program.h"
#include "carma/util/Time.h"
#include "mysql/mysql.h"
#include "postgresql/libpq-fe.h"

using namespace std;
using namespace carma::dbms;

void loadData(const DBConnection* dbc, const string& inpDatafile,
              const string& inpDirname, const string& tableName,
              const string& inpUser, const int& inpTrans, 
              const bool& inpUseProdDB, const int& inpWait) {
    time_t ta,tb;
    double t_diff = 0;
    struct dirent **namelist;
    int n;
    string filename;
    int length;
    bool hasWaited = false;
    if(inpDatafile != "none") {
        ta = time(NULL);
        dbc->loadDataFromFile(inpDatafile,tableName,inpUser,inpTrans,
                              inpUseProdDB);


        tb = time(NULL);
        t_diff = difftime(tb,ta);
    } else {
        t_diff = 0;
        int waitTime = (inpWait/2) + (inpWait % 2);
        bool foundFile = true;
        while(foundFile) {
            foundFile = false;
            n = scandir(inpDirname.c_str(), &namelist, 0, alphasort);
            if (n < 0) {
                cerr << "scandir returned " << n << endl;
                return;
            } else {
                for(int i = 0; i < n; i++) {
                    filename = inpDirname + "/" + string(namelist[i]->d_name);
                    length = filename.length();
                    if(length >= 5 
                       && filename.substr(length-5) == ".done") {
                        cout << "Loading data from " << filename << endl;
                        ta = time(NULL);
                        dbc->loadDataFromFile(filename,tableName,inpUser, 
                                              inpTrans, inpUseProdDB);
                        tb = time(NULL);
                        t_diff += difftime(tb,ta);

                        remove(filename.c_str());
                        foundFile = true;
                        hasWaited = false;
                        free(namelist[i]);
                    }
                }
                free(namelist);
            }
            if(!foundFile && !hasWaited && inpWait > 0) {
                cout << "Waiting " << waitTime << " seconds for more data"
                     << endl;
                foundFile = true;
                hasWaited = true;
                sleep(waitTime);
            }
        }
    }
    cout << dbc->rdbmsName() << " time to insert float rows " << t_diff 
         << " seconds" << endl;
}


int carma::util::Program::main() {
    string inpRDBMS = Program::getStringParameter("rdbms");
    string inpDatafile = Program::getStringParameter("datafile");
    string inpDirname = Program::getStringParameter("directory");
    bool inpUseProdDB = (Program::getIntParameter("useProdDB") == 1);
    string inpTableType = Program::getStringParameter("tabletype");
    int inpPK = Program::getIntParameter("pk");
    int inpTrans = Program::getIntParameter("trans");
    int inpWait = Program::getIntParameter("wait");
    int inpAppend = Program::getIntParameter("append");
    string inpUser = Program::getStringParameter("user");
    carma::util::Trace::TraceLevel localLevel =
        (carma::util::Trace::TraceLevel)getIntParameter("localLevel");


    stringstream stmt;
    string pkpart = "";
    if(inpPK) {
        pkpart = ", PRIMARY KEY (time, tagID, iSample)";
    }
    string indexPart = ", index(tagID)";
    stringstream createStatement;
    DBConnection *dbc = 0;
    string rdbms;
    string notnull = "";
    try {
        cout << "Making odbc connection" << endl;
        if(inpRDBMS == "postgres") {
            rdbms = PostgresDBConnection::RDBMS;
        } else if (inpRDBMS == "mysql") {
            rdbms = MySQLDBConnection::RDBMS;
        }
        else {
            cout << "Unknown rdbms " << inpRDBMS << endl;
            return 1;
        }
        dbc = DBConnectionFactory::createConnection(false,rdbms);
        TableManager tm(false, inpUseProdDB, rdbms);
        stringstream ss;
        ss << carma::util::Time::computeCurrentFrame();
        string tableName;
        int partitionID = tm.createMonitorDataTable(FRAME, NUMERIC, 20,
                                                    tableName);
        if(partitionID < 0) {
            CPTRACE(carma::util::Trace::TRACE0, "no "
                        << "partitions with enough space to hold the "
                        << "requested table");
            return EXIT_FAILURE;
        }
        CPTRACE(carma::util::Trace::TRACE6, "partition ID "
                    << "on which the new table named " << tableName << " will "
                    << "be placed is " << partitionID);
        // FIXME debugging...
        return EXIT_SUCCESS;
        loadData(dbc, inpDatafile, inpDirname, tableName, inpUser, 
                 inpTrans, inpUseProdDB, inpWait);
    } catch (const DBConnectionException & exc) {
        exc.report();
    } catch (const SQLException & exc) {
        exc.report();
    }
    delete dbc;
    return 0;
}
