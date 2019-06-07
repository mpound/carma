/**
 * Implementation for the MySQLDBConnection 
 * @author: Dave Mehringer
 *
 * $CarmaCopyright$
 *
 */
#include <sstream>
#include "carma/dbms/MonitorDataDatabase.h"
#include "carma/dbms/MySQLDBConnection.h"
#include "carma/dbms/TableNames.h"
#include "carma/util/FileUtils.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/StopWatch.h"
#include "carma/util/Trace.h"

using namespace std;
using namespace carma::dbms;
using namespace carma::util;

//const string MySQLDBConnection::DBUSER = "root";
const string MySQLDBConnection::RDBMS = "MySQL";
//const string MySQLDBConnection::ODBC_DATASRC = "DSN=myodbc3;UID=" + DBUSER;
//const string MySQLDBConnection::TEST_ODBC_DATASRC 
//    = "DSN=myodbc3_test;UID=root";


MySQLDBConnection::MySQLDBConnection(const DBConfigurator *dbconf)
    : DBConnection(dbconf) {
    rdbmsName_ = RDBMS;
    //cout << "mysql henv before " << henv_ << endl;
    //cout << "mysql hdbc before " << hdbc_ << endl;
    openODBCConnection_();
    ostringstream ss;
    //cout << "mysql henv after " << henv_ << endl;
    //cout << "mysql hdbc after " << hdbc_ << endl;
    ss << "ODBC connection to MySQL database " << dbname_ 
       << " using connect string " << odbcConnectString_ << " successful" 
       << endl;
    CARMA_CPTRACE (carma::util::Trace::TRACE5, ss.str());
}

void MySQLDBConnection::loadDataFromFile
    (const string& filename, const string& table, const bool& useTransactions,
     const int& linesToIgnore, const string& columnDelimiter) const {
    file2TableMap file2Table;
    file2Table[filename] = table;
    loadDataFromFiles(file2Table,useTransactions,linesToIgnore,
                      columnDelimiter);
}

void MySQLDBConnection::loadDataFromFiles
    (const file2TableMap& file2Table, const bool& useTransactions, 
     const int& linesToIgnore, const string& columnDelimiter) const {
    MYSQL *sock,mysql;
    CARMA_CPTRACE (carma::util::Trace::TRACEALL, "Running mysql_init()");
    if(mysql_init(&mysql) == NULL) {
        ostringstream emsg;
        emsg << "Insufficient memory to allocate a new MYSQL object";
        throw CARMA_EXCEPTION(DBConnectionException,emsg.str());
    }        
    const char *passwd = (passwordFile_ == "") ? NULL 
        : readPassword_().c_str();
    CARMA_CPTRACE (carma::util::Trace::TRACEALL, "Attempting to make db connection "
             << "using the native MySQL interface");
    if (!(sock = mysql_real_connect(&mysql, NULL, dbuser_.c_str(), passwd,
                                    dbname_.c_str(), port_, socket_.c_str(), 
                                    0))) {
        ostringstream emsg;
        emsg << "Couldn't connect to database " << dbname_ << " as user "
             << dbuser_ << " using native MySQL interface " 
             << mysql_error(&mysql) << endl;
        mysql_close(sock);
        throw CARMA_EXCEPTION(DBConnectionException,emsg.str());
    }
    CARMA_CPTRACE (carma::util::Trace::TRACEALL, "Native connection successful.");
    CARMA_CPTRACE (carma::util::Trace::TRACEALL, "Loading data from " 
             << file2Table.size());
    for(map<string,string>::const_iterator i=file2Table.begin(); 
        i != file2Table.end(); ++i) {
        ostringstream stmt;
        stmt << "LOAD DATA INFILE '" << i->first << "' INTO TABLE " 
             << i->second;
        // field seperator, \t (tab) is the default
        if(columnDelimiter != "\t") {
            stmt << " FIELDS TERMINATED BY '" << columnDelimiter << "'";
        }

        // skip the first n lines if so directed
        if(linesToIgnore > 0) {
            stmt << " IGNORE " << linesToIgnore << " LINES";
        }
        ostringstream ss;
        ss << stmt.str() << endl;
        CARMA_CPTRACE (carma::util::Trace::TRACE5, ss.str());
        if(mysql_query(sock,stmt.str().c_str())) {
            ostringstream emsg;
            emsg << stmt.str() << " failed " << mysql_error(&mysql);
            mysql_close(sock);
            throw CARMA_EXCEPTION(DBConnectionException,emsg.str());
        }
    }
    CARMA_CPTRACE (carma::util::Trace::TRACEALL, "Closing native MySQL connection");
    mysql_close(sock);
}

void MySQLDBConnection::beginTransaction() const {
	//programLogInfo("MySQLDBConnection::beginTransaction()");
    SQLRETURN retcode;
    retcode = SQLSetConnectOption(hdbc_, SQL_AUTOCOMMIT, SQL_AUTOCOMMIT_OFF);
    if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO) {
        ostringstream ss;
        ss << "Error setting up transaction. Return code was: " << retcode ;
	    programLogError(ss.str());
        throw CARMA_EXCEPTION(DBConnectionException, ss.str());
    }
}

void MySQLDBConnection::rollBackTransaction() const {
	//programLogInfo("MySQLDBConnection::rollBackTransaction()");
    SQLRETURN retcode = SQLEndTran(SQL_HANDLE_DBC, hdbc_, SQL_ROLLBACK);
    if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO) {
        string emsg = "Error rolling back transaction. Note that ";
        emsg += "transactions are only supported by InnoDB tables (e.g., ";
        emsg += "MyISAM tables do not support transactions)";
	    programLogError(emsg);
        throw CARMA_EXCEPTION(DBConnectionException, emsg);
    }
    CARMA_CPTRACE (carma::util::Trace::TRACE5, "Transaction rolled back" << endl);
    retcode = SQLSetConnectOption(hdbc_, SQL_AUTOCOMMIT, SQL_AUTOCOMMIT_ON);
    if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO) {
        string emsg = "Error setting autocommit mode to on";
	    programLogError(emsg);
        throw CARMA_EXCEPTION(DBConnectionException, emsg);
    }
}

void MySQLDBConnection::commitTransaction() const {
	//programLogInfo("MySQLDBConnection::commitTransaction()");
    SQLRETURN retcode = SQLEndTran(SQL_HANDLE_DBC, hdbc_, SQL_COMMIT);
    if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO) {
        string emsg = "Error committing transaction";
        throw CARMA_EXCEPTION(DBConnectionException,emsg);
    }
    retcode = SQLSetConnectOption(hdbc_, SQL_AUTOCOMMIT, SQL_AUTOCOMMIT_ON);
    if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO) {
        string emsg = "Error setting auto commit mode to on";
	    programLogError(emsg);
        throw CARMA_EXCEPTION(DBConnectionException,emsg);
    }
}

bool MySQLDBConnection::inTransaction() const {
    UWORD *value = new UWORD;
    SQLRETURN retcode = SQLGetConnectOption(hdbc_, SQL_ATTR_AUTOCOMMIT, value);
    if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO) {
        string emsg = "Error getting value of autocommit attribute";
        throw CARMA_EXCEPTION(DBConnectionException,emsg);
    }
    bool res = (*value == SQL_AUTOCOMMIT_OFF);
    ostringstream o;
    o << "MySQLDBConnection::inTransaction() returns " << res;
    //programLogInfo(o.str());
    delete value;
    return res;
}


string MySQLDBConnection::createMonitorDataTable(
                                    const MonitorAverageType& averageType,
                                    const MonitorAggregateType& aggType,
                                    const string& tag, 
                                    const string *const location) const {
    string tableName, columnClause, pkClause;
    MonitorDataDatabase::getCreateMonitorTableStatementParts
        (averageType, aggType, tag, tableName, columnClause, pkClause);
    // we purposefully do not use the pkClause; an index on tagID is added
    // after the table has been completely populated
    //string stmt = "CREATE TABLE " + tableName + "(" + columnClause + pkClause
    //+ ") ENGINE=MYISAM";
    string stmt = "CREATE TABLE " + tableName + "(" + columnClause
        + ") ENGINE=MYISAM";
    if(location != NULL) {
        stmt += " DATA DIRECTORY='" + *location + "' INDEX DIRECTORY='" 
            + *location + "'";
    }
    directSQLExec_(stmt);
    return tableName;
}

void MySQLDBConnection::monitorDataTableHasBeenPopulated(
                                 const string& tableName,
                                 const MonitorAverageType& averageType) 
    const {
    if(getRowCount(tableName) > 0) {
      // Removed table indexing 9/28/06
      //#define ADD_INDEX
#if defined(ADD_INDEX)
        string stmt = "ALTER TABLE " + tableName + " ADD INDEX(tagID)";
        CARMA_CPTRACE (carma::util::Trace::TRACE5, "Adding index to fully populated "
                 << "table " << tableName << ": " << stmt);
        CARMA_CPTRACE (carma::util::Trace::TRACE5, " (via call to directSQLExec_)");
        directSQLExec_(stmt);
        CARMA_CPTRACE (carma::util::Trace::TRACE5, "Back from call to directSQLExec_");
#else
	{static bool doneOnce = false;	// Just once to minimize msgs.
	  if(!doneOnce)
	  { ostringstream msg;
	    msg << "Skipping table indexing.";
	    CARMA_CPTRACE (carma::util::Trace::TRACE6, msg.str());
	    logger_ << log4cpp::Priority::INFO << msg.str();
	    doneOnce = true;
	  }
	}
#endif

        // compress the table
        /*
        stmt = "SELECT partition FROM " + getTableName(MONITOR_INDEX_TABLE) 
            + " AS mit," + getTableName(PARTITIONS_TABLE) 
            + " AS p WHERE mit.tableName='" + tableName 
            + "' AND mit.partitionID=p.partitionID";
        Table t = execSQLSelect(stmt);
        log4cpp::Category& logger = carma::util::Program::getLogger();
        ostringstream emsg;
        if(t.rowCount() != 1) {
            emsg << tableName << " not found in " << "table " 
                 << getTableName(MONITOR_INDEX_TABLE);
            CARMA_CPTRACE (carma::util::Trace::TRACE1, emsg.str());
            logger << log4cpp::Priority::ERROR << emsg.str();
            return;
        }
        string tablePath = t.getStringColumn("partition")[0] + "/" + tableName;
        */
        MonitorDataDatabase mddb(this);
        string tablePath = mddb.getCanonicalSystemName(tableName);
	//#define DO_TABLEPACKING
#if defined(DO_TABLEPACKING)
        string cmd = "myisampack " + tablePath;
        if(system(cmd.c_str()) != 0) {
            ostringstream emsg;
            emsg << "Error running system command " << cmd; 
            CARMA_CPTRACE (carma::util::Trace::TRACE1, emsg.str());
            logger_ << log4cpp::Priority::ERROR << emsg.str();
        }
        cmd = "myisamchk -rq --sort-index --analyze " + tablePath;
        if(system(cmd.c_str()) != 0) {
            ostringstream emsg;
            emsg << "Error running system command " << cmd; 
            CARMA_CPTRACE (carma::util::Trace::TRACE1, emsg.str());
            logger_ << log4cpp::Priority::ERROR << emsg.str();
        }
        CARMA_CPTRACE (carma::util::Trace::TRACE6, "Compressed " << tablePath 
                 << " with myisampack");
#else
	// Packing has been turned off to see if it's causing corruption of
	// FrameStringMonitorData_xxxx.MYI files.
	{static bool doneOnce = false;	// Just once to minimize msgs.
	  if(!doneOnce)
	  { ostringstream msg;
	    //	  msg << "Skipping compression of " << tablePath << " with myisampack";
	    msg << "Skipping table compression with myisampack.";
	    CARMA_CPTRACE (carma::util::Trace::TRACE6, msg.str());
	    logger_ << log4cpp::Priority::INFO << msg.str();
	    doneOnce = true;
	  }
	}
#endif
    }
    DBConnection::monitorDataTableHasBeenPopulated(tableName,averageType);
}
    
unsigned MySQLDBConnection::maxTablesPerJoin() const {
    // FIXME store this in a static const variable
    return 31;
}

unsigned MySQLDBConnection::maxColumnsPerTable() const {
    // FIXME store this in a static const variable
    return 2100;
}

void MySQLDBConnection::repairMonitorDataTable(const string& tableName) const {
    MonitorDataDatabase mddb(this);
    string tablePath = mddb.getCanonicalSystemName(tableName);
    string cmd = "myisamchk -rq --sort-index --analyze " + tablePath;
    if(system(cmd.c_str()) == 0) {
        ostringstream msg;
        msg << "Successfully repaired table " << tableName;
        CARMA_CPTRACE (carma::util::Trace::TRACE4, msg.str());
    } else {
        ostringstream emsg;
        emsg << "Error running system command " << cmd; 
        CARMA_CPTRACE (carma::util::Trace::TRACE2, emsg.str());
        logger_ << log4cpp::Priority::ERROR << emsg.str();
    }
}
