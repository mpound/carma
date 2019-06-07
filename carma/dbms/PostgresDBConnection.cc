/**
 * Implementation for the PostgresDBConnection 
 * @author: Dave Mehringer
 *
 * $CarmaCopyright$
 *
 */

#include "carma/dbms/PostgresDBConnection.h"

using namespace std;
using namespace carma::dbms;

const string PostgresDBConnection::RDBMS = "PostgreSQL";
const string PostgresDBConnection::ODBC_DATASRC = "DSN=postgres;UID=dmehring";
const string PostgresDBConnection::TEST_ODBC_DATASRC 
    = "DSN=postgres_test;UID=dmehring";

PostgresDBConnection::PostgresDBConnection(const bool& useProductionDB,
                                           const string* const odbcini) 
    : DBConnection(useProductionDB,odbcini) {
    rdbmsName_ = RDBMS;
    dataSource_ = useProductionDB ? ODBC_DATASRC : TEST_ODBC_DATASRC;
    if(!isDBUp()) {
        throw DBConnectionException("Connection to PostgreSQL database " 
                                    + dbname_ + " was not successful"
                                    ,__FILE__,__LINE__);
    } else {
        openODBCConnection();
    }
    ostringstream os;
    os << "PostgresDBConnection(): Connection to PostgreSQL database "
       << dbname_ << " was successful" << endl;
    logger_ << log4cpp::Priority::INFO << os.str();
}


/*
 * Overriding base class method because the ODBC implementation doesn't
 * work properly if the postgres database cannot be contacted
 */
bool PostgresDBConnection::isDBUp() const { 
    PGconn *pg_conn;
    pg_conn = PQconnectdb(("dbname=" + dbname_).c_str());
    bool res = (PQstatus(pg_conn) == CONNECTION_OK);
    PQfinish(pg_conn);
    return res;
}


void PostgresDBConnection::loadDataFromFile(const string& filename, 
                                            const string& table, 
                                            const string& user,
                                            const bool& useTransactions,
                                            const bool& useProductionDB) 
    const {
    map<string,string> file2Table;
    file2Table[filename] = table;
    loadDataFromFiles(file2Table,user,useTransactions,useProductionDB);
}

void PostgresDBConnection::loadDataFromFiles(const file2TableMap& file2Table, 
                                             const string& user, 
                                             const bool& useTransactions,
                                             const bool& useProductionDB) 
    const {

    PGconn     *conn;
    PGresult   *res;
    string dbname = (useProductionDB) ? MONITOR_DB : TEST_MONITOR_DB;
    /* Make a connection to the database */
    string connectStr = "dbname=" + dbname;
    conn = PQconnectdb(connectStr.c_str());
    /* Check to see that the backend connection was successfully made */
    stringstream emsg;
    if (PQstatus(conn) != CONNECTION_OK) {
        PQfinish(conn);
        emsg << "Native connection to database " << PQdb(conn) << " failed!";
        throw DBConnectionException(emsg.str(),__FILE__,__LINE__);
    } 
    stringstream stmt;

    for(map<string,string>::const_iterator i=file2Table.begin(); 
        i != file2Table.end(); ++i) {
        stmt.str("");
        // ta = time(NULL);
        if(useTransactions) {
            res = PQexec(conn, "BEGIN WORK");
            if (PQresultStatus(res) != PGRES_COMMAND_OK) {
                PQfinish(conn);
                emsg << "Begin transaction failed" 
                     << PQresultErrorMessage(res);
                PQclear(res);
                throw DBConnectionException(emsg.str(),__FILE__,__LINE__);
            }
            PQclear(res);
        }
        stmt << "COPY " << i->second << " FROM '" << i->first << "'";
        ostringstream os;
        os << stmt.str() << endl;
        logger_ << log4cpp::Priority::INFO << os.str();
        res = PQexec(conn,stmt.str().c_str());
        if (PQresultStatus(res) != PGRES_COMMAND_OK) {
            if(useTransactions) {
                PQexec(conn,"ROLLBACK");
            }
            PQfinish(conn);
            emsg << stmt.str() << " failed " << PQresultErrorMessage(res);
            PQclear(res);
            throw DBConnectionException(emsg.str(),__FILE__,__LINE__);
        }
        PQclear(res);
        if(useTransactions) {
            res = PQexec(conn, "COMMIT");
            if (PQresultStatus(res) != PGRES_COMMAND_OK) {
                PQfinish(conn);
                emsg << "Commit transaction failed" 
                     << PQresultErrorMessage(res);
                PQclear(res);
                throw DBConnectionException(emsg.str(),__FILE__,__LINE__);
            }
            PQclear(res);
        }
        //tb = time(NULL);
        //double t_diff = difftime(tb,ta);
        //cout << "Postgresql time to insert float rows " << t_diff << endl;
    }
    PQfinish(conn);
}


void PostgresDBConnection::beginTransaction() const {
    SQLRETURN retcode = SQLSetConnectOption(hdbc_, SQL_AUTOCOMMIT, 
                                            SQL_AUTOCOMMIT_OFF);
    if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO) {
        string emsg = "Error setting up transaction";
        throw CARMA_EXCEPTION(DBConnectionException,emsg);
    }
}

void PostgresDBConnection::rollBackTransaction() const {
    SQLRETURN retcode = SQLEndTran(SQL_HANDLE_DBC, hdbc_, SQL_ROLLBACK);
    if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO) {
        string emsg = "Error rolling back transaction";
        throw CARMA_EXCEPTION(DBConnectionException,emsg);
    }
    retcode = SQLSetConnectOption(hdbc_, SQL_AUTOCOMMIT, SQL_AUTOCOMMIT_ON);
    if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO) {
        string emsg = "Error setting auto commit mode to on";
        throw CARMA_EXCEPTION(DBConnectionException,emsg);
    }
    if(retcode == SQL_SUCCESS_WITH_INFO) {
        cout << "Info" << endl;
    }
    cout << "rolled back" << endl;
}

void PostgresDBConnection::commitTransaction() const {
    SQLRETURN retcode = SQLEndTran(SQL_HANDLE_DBC, hdbc_, SQL_COMMIT);
    if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO) {
        string emsg = "Error committing transaction";
        throw CARMA_EXCEPTION(DBConnectionException,emsg);
    }
    retcode = SQLSetConnectOption(hdbc_, SQL_AUTOCOMMIT, SQL_AUTOCOMMIT_ON);
    if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO) {
        string emsg = "Error setting auto commit mode to on";
        throw CARMA_EXCEPTION(DBConnectionException,emsg);
    }
}

bool PostgresDBConnection::inTransaction() const {
    UWORD *value = new UWORD;
    SQLINTEGER *obuf;
    SQLRETURN retcode = SQLGetConnectOption(hdbc_, SQL_ATTR_AUTOCOMMIT, value);
    if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO) {
        string emsg = "Error getting value of autocommit attribute";
        throw CARMA_EXCEPTION(DBConnectionException,emsg);
    }
    bool res = (*value == SQL_AUTOCOMMIT_OFF);
    delete value;
    return res;
}

string PostgresDBConnection::createMonitorDataTable(
                                    const monitorTableAverageType& averageType,
                                    const monitorTableDataType dataType,
                                    const string& tag, 
                                    const string *const location=NULL) const {
    //FIXME put in real code
    return string("");
}

