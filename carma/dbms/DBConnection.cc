/**
 * Implementation for the DBConnection and DBConnectionFactory
 *
 * @author: Dave Mehringer
 *
 * @version $Id: DBConnection.cc,v 1.118 2011/12/21 22:56:43 mpound Exp $
 * $CarmaCopyright$
 *
 */
#include <iostream>
#include <fstream>

#include "carma/dbms/DBConnection.h"
#include "carma/dbms/DBConfigurator.h"
#include "carma/dbms/MonitorDataDatabase.h"
#include "carma/dbms/MonitorSystemAndDBMSRelationships.h"
//#include "mysql/mysql_version.h"
#include "carma/dbms/MySQLDBConnection.h"
#include "carma/dbms/TableNames.h"
#include "carma/util/FileUtils.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/NotFoundException.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/StringUtils.h"
#include "carma/util/Trace.h"

using namespace ::std;
using namespace carma::dbms;
using namespace carma::util;


DBConnection::DBConnection( const DBConfigurator * dbconf ) :
logger_( Program::getLogger( ) ),
inTxn_( false ),
hdbc_( 0 ),
henv_( 0 ) {

    if(dbconf == 0) {
        dataSource_ = getResource(DEFAULT_DATASRC);
        dbuser_ = getResource(DEFAULT_DBUSER);
        // DEFAULT_DBUSER must have no password
        
        odbcini_ = getResource(DEFAULT_ODBCINI);
        // FIXME odbcini_ must also passed to the environment
        //  but I'm not doing that right now so no one connects by mistake 
        // via TagIDAuhtority
        //setenv("ODBCINI",odbcini_.c_str(),1);
        socket_ = getResource(DEFAULT_SOCKET);
        port_ = atoi(getResource(DEFAULT_PORT).c_str());
        

    } else {
        dataSource_ = dbconf->getDataSource();
        dbuser_ = dbconf->getDBUser();
        dbname_ = dbconf->getDBName();
        port_ = dbconf->getPort();
        socket_ = dbconf->getSocket();
        passwordFile_ = dbconf->getPasswordFile();
        odbcini_ = dbconf->getODBCini();
        if ( FileUtils::exists(odbcini_) == false ) {
            string emsg = "ODBC config file " + odbcini_ + " does not exist "
                + " or cannot be read";
            throw CARMA_EXCEPTION(NotFoundException,emsg);
        }
        // necessary to set this as an env variable because that's how
        // the ODBC third party code gets it
        if(odbcini_ != "") {
            setenv("ODBCINI",odbcini_.c_str(),1);
        }
    }
}

DBConnection::~DBConnection() {
    closeODBCConnection_();
    henv_ = 0;
    hdbc_ = 0;
}


//------------------- Database status/info methods --------------------

 bool DBConnection::isUp(const DBConfigurator *dbconf) {
    auto_ptr<DBConnection> dbc;
    try {
        auto_ptr<DBConnection> tmp(DBConnectionFactory
            ::createConnection(dbconf));
        dbc = tmp;
        return true;
    } catch(const DBConnectionException &exc) {
        return false;
    }
}

bool DBConnection::isDBUp() {
    SQLHDBC hdbc = 0;
    SQLHENV henv = 0;
    bool isUp;
    try {
        openODBCConnection_(&henv, &hdbc);
        isUp = true;
    } catch (const DBConnectionException &exc) {
        isUp = false;
    }
    closeODBCConnection_(henv, hdbc);
    return isUp;
}

void DBConnection::odbcInfo() const {
    SQLSMALLINT len = 50;
    SQLCHAR *dataSource = new SQLCHAR[len];
    SQLRETURN res = SQLGetInfo(hdbc_, SQL_DATA_SOURCE_NAME, dataSource, len, 0);
    cout << "ODBC data source: " << dataSource << endl;
    delete [] dataSource;

    len = 50;
    SQLCHAR *rdbms = new SQLCHAR[len];
    res = SQLGetInfo(hdbc_, SQL_DBMS_NAME, rdbms, len, 0);
    len = 150;
    SQLCHAR *rdbmsVersion = new SQLCHAR[len];
    res = SQLGetInfo(hdbc_, SQL_DBMS_VER, rdbmsVersion, len, 0);
    cout << endl;
    cout << "RDBMS product: " << rdbms << " version " << rdbmsVersion << endl;
    delete [] rdbmsVersion;
    delete [] rdbms;
 
    len = 50;
    SQLCHAR *database = new SQLCHAR[len];
    res = SQLGetInfo(hdbc_, SQL_DATABASE_NAME, database, len, 0);
    cout << endl;
    cout << "Database name: " << database << endl;
    delete [] database;
 
    len = 50;
    SQLCHAR *driverName = new SQLCHAR[len];
    res = SQLGetInfo(hdbc_, SQL_DRIVER_NAME, driverName, len, 0);
    cout << endl;
    cout << "Driver name: " << driverName << endl;
    delete [] driverName;
 
    len = 50;
    SQLCHAR *driverODBCVer = new SQLCHAR[len];
    res = SQLGetInfo(hdbc_, SQL_DRIVER_ODBC_VER, driverODBCVer, len, 0);
    cout << endl;
    cout << "ODBC version supported by the driver: " << driverODBCVer
         << endl;
    delete [] driverODBCVer;
 
    SQLUSMALLINT txnRes = 0;
    res = SQLGetInfo(hdbc_, SQL_TXN_CAPABLE, (SQLPOINTER)&txnRes, sizeof(txnRes), 0);
    string dml = "Data Manipulation Language (DML) statements (SELECT, ";
    dml += "INSERT, UPDATE, DELETE)";
    string ddl = "Data Definition Language (DDL) statements (CREATE TABLE, ";
    ddl += "DROP INDEX, and so on)";
 
    cout << endl;
    cout << "To what extent are transactions supported by the ODBC driver?"
         << endl;
     
    switch(txnRes) {
    case SQL_TC_NONE:
        cout << "Transactions not supported." << endl;
        break;
    case SQL_TC_DML:
        cout << "Transactions can contain only " << dml << ". " << ddl
             << " encountered in a transaction cause an error." << endl;
        break;
    case SQL_TC_DDL_COMMIT:
        cout << "Transactions can contain only " << dml << ". " << ddl
             << " encountered in a transaction cause the transaction to be "
             << "committed." << endl;
        break;
    case SQL_TC_DDL_IGNORE:
        cout << "Transactions can contain only " << dml << ". " << ddl
             << " encountered in a transaction are ignored." << endl;
        break;
    case SQL_TC_ALL:
        cout << "Transactions can contain " << dml << " and " << ddl
             << " in any order." << endl;
        break;
  
    default:
        cout << "I don't recognize return code " << txnRes << endl;
    }
 
    len = 2;
    SQLCHAR *mtxnRes = new SQLCHAR[len];
    res = SQLGetInfo(hdbc_, SQL_MULTIPLE_ACTIVE_TXN, mtxnRes, len, 0);
    cout << endl;
    cout << "Are multiple active transactions supported by the ODBC driver? "
         << mtxnRes << endl;
    delete [] mtxnRes;
 
    len = 20;
    SQLCHAR *dmVer = new SQLCHAR[len];
    res = SQLGetInfo(hdbc_, SQL_DM_VER, dmVer, len, 0);
    cout << endl;
    cout << "ODBC Driver manager version: " << dmVer
         << endl;
    delete [] dmVer;
 
    len = 11;
    SQLCHAR *odbcVer = new SQLCHAR[len];
    res = SQLGetInfo(hdbc_, SQL_ODBC_VER, odbcVer, len, 0);
    cout << endl;
    cout << "ODBC version supported by the ODBC driver manager: " << odbcVer
         << endl;
    delete [] odbcVer;
}

long DBConnection::getRowCount(const string& tableName) const {
    Table::ColumnType ctype;
    void *tmp = getAggregate("*", tableName, "count", ctype);
    // ctype should be returned from the function as an int column, probably
    // should check for that
    assert (tmp != 0);
    long rowCount;
    if(ctype == Table::INT_COLUMN_TYPE )
    {  rowCount = *static_cast<int *>(tmp);
       delete (int *)tmp;
    }
    else if (ctype == Table::BIGINT_COLUMN_TYPE )
    {  rowCount = *static_cast<long *>(tmp);
       delete (long *)tmp;
    }
    else
      assert( 0 && "Illegal Column type.");
    return rowCount;
}

void* DBConnection::getAggregate(const string& column, 
                                 const string& table,
                                 const string& function,
                                 Table::ColumnType& type) const {
    ostringstream ss;
    // With Mysql 5, results of functions like 'sum', 'max', etc. are returned
    // as decimals. Using cast seems to get around that. If the decimal support
    // should stop working (eg. If decimals are no longer returned as chars),
    // enabling the "CAST(...)" might get things going again.
#if 1
    ss << "SELECT " << function << "(" << column << ") FROM " << table;
#else
    ss << "SELECT CAST(" << function << "(" << column << ") AS SIGNED) FROM " << table;
#endif

    Table t;
    ostringstream emsg;
    try {
        t = execSQLSelect(ss.str());
    } catch (const SQLException &exc) {
        emsg << "DBConnection QUERY BUG!: " << exc.what();
        throw CARMA_ERROR(emsg.str());
    }
    int nRows = t.rowCount();
    /*
    CPTRACE(Trace::TRACEALL, "Resulting column type of aggregate "
            << "function " << function << " is "
            << t.columnType2String(t.getColumnTypes()[0]));
    */
    if(nRows == 0) {
        return 0;
    } else if(nRows > 1) {
        emsg << "Result table has " << nRows << ". It should have a maximum "
             << "of 1";
        throw CARMA_EXCEPTION(DBConnectionException,emsg.str());
    }
    type = t.getColumnTypes()[0];
    switch(type) {
    case Table::DOUBLE_COLUMN_TYPE: 
        if(t.getDoubleColumn(0).isElementNull(0)) {
            return 0;
        } else {
            return new double(t.getDoubleColumn(0)[0]);
        }
        break;
    case Table::FLOAT_COLUMN_TYPE: 
        if(t.getFloatColumn(0).isElementNull(0)) {
            return 0;
        } else {
            return new float(t.getFloatColumn(0)[0]);
        }
        break;
    case Table::INT_COLUMN_TYPE: 
        if(t.getIntColumn(0).isElementNull(0)) {
            return 0;
        } else {
            return new int(t.getIntColumn(0)[0]);
        }
        break;
    case Table::SHORT_COLUMN_TYPE: 
        if(t.getShortColumn(0).isElementNull(0)) {
            return 0;
        } else {
            return new short(t.getShortColumn(0)[0]);
        }
        break;
    case Table::STRING_COLUMN_TYPE: 
        if(t.getStringColumn(0).isElementNull(0)) {
            return 0;
        } else {
            return new string(t.getStringColumn(0)[0]);
        }
        break;
    case Table::DECIMAL_COLUMN_TYPE: 
        if(t.getDecimalColumn(0).isElementNull(0)) {
            return 0;
        } else {
            return new string(t.getDecimalColumn(0)[0]);
        }
        break;
    case Table::BIGINT_COLUMN_TYPE: 
        if(t.getBigIntColumn(0).isElementNull(0)) {
            return 0;
        } else {
            return new long(t.getBigIntColumn(0)[0]);
        }
        break;

    default:
        emsg << "Unhandled column type " << type << " " 
             << t.columnType2String(type);
        throw CARMA_ERROR(emsg.str());
    }
}




/*
// a work in progress....
map<T, U> DBConnection::getColumn2ColumnMap(const string& columnA, 
                                            const string& columnB, 
                                            const string& table) const {
      stringstream ss;
    ss << "SELECT " << columnA << "," << columnB << " FROM " << table;
    Table t;
    try {
    t = execSQLSelect(ss.str());
    Column<int> tagIDs = t.getIntColumn("tagID");
    Column<string> names = t.getStringColumn("name");
    map<int, string> m;
    for(int i = 0; i < t.rowCount(); i++) {
        m[tagIDs[i]] = names[i];
    }
    return m;
}
*/
    

void DBConnection::dropScratchTable(const string& table) const {
    if(!tableExists(table, "scratch")) {
        programLogInfo("Scratch table '" + table + 
            "' does not exist, so it can't be dropped");
        // table doesn't exist so cannot be dropped
        return;
    }
    string statement = "DROP TABLE scratch." + table;
    try {
        directSQLExec_(statement);
    } catch(const SQLException &exc) {
        string m = "Exception caught when droppling table " + table;
        programLogError(m + "\n " + exc.getMessage());
        throw CARMA_EXCEPTION(SQLException, exc.getMessage());
    }
}

bool DBConnection::tableExists(const string& table, const string& database) 
    const {
    Table t = databaseInfo(database);
    return (t.getStringColumn("TABLE_NAME").indexOf(table) >= 0);
}

Table
DBConnection::databaseInfo( const string & database ) const {
    SQLHSTMT hstmt;
    bool needToFreeStatement = true;
    
    try {
        const SQLRETURN retcode1 = SQLAllocHandle( SQL_HANDLE_STMT,
                                                   hdbc_,
                                                   &hstmt );
            
        if ( retcode1 != SQL_SUCCESS ) {
            throw CARMA_EXCEPTION( DBConnectionException, 
                                   "Unable to allocate statement");
        }
    
        vector< SQLCHAR > catalogName( database.begin( ), database.end( ) );

        const SQLRETURN retcode2 =
            SQLTables( hstmt,
                       (catalogName.empty( ) ? 0 : &(catalogName[ 0 ])),
                       catalogName.size( ),
                       0, 0,
                       0, 0,
                       0, 0 );

        if ( retcode2 != SQL_SUCCESS && retcode2 != SQL_SUCCESS_WITH_INFO ) {
            throw CARMA_EXCEPTION( DBConnectionException,
                                   "Error getting sql tables" );
        }

        Table t = odbcResultSetToTable_( hstmt );

        needToFreeStatement = false;
        freeSQLStatement_( hstmt );
        
        t.setName( "Database Information" );
        
        return t;
    } catch ( ... ) {
        if ( needToFreeStatement )
            freeSQLStatement_( hstmt );

        throw;
    }
}


Table
DBConnection::execSQLSelect( const string & statement,
                             const string & resultTableName ) const {
    const string tmp = StringUtils::lowASCIIAlphaNumericToLower( statement );

    if ( tmp.find( "select" ) == string::npos ) {
        ostringstream emsg;
    
        emsg << "Submitted statement " + statement + " is not an SQL SELECT "
             << "statement" << endl;
        
        throw CARMA_EXCEPTION(SQLException, emsg.str());
    }
    
    SQLHSTMT hstmt;
    bool needToFreeStatement = true;
    
    try {
        if ( SQLAllocHandle( SQL_HANDLE_STMT, hdbc_, &hstmt ) != SQL_SUCCESS ) {
            throw CARMA_EXCEPTION(DBConnectionException, 
                                  "Unable to allocate statement");
        }

        directSQLExec_( hstmt, statement );
    
        Table t = odbcResultSetToTable_( hstmt );

        needToFreeStatement = false;
        freeSQLStatement_( hstmt );
    
        t.setName( resultTableName );

        return t;
    } catch ( ... ) {
        if ( needToFreeStatement )
            freeSQLStatement_( hstmt );

        throw;
    }
}

static const char *sql2name(int val)
{
  if(val == SQL_UNKNOWN_TYPE)
  {	return "SQL_UNKNOWN_TYPE";
  }else if(val == SQL_CHAR)
  {	return "SQL_CHAR";
  }else if(val == SQL_NUMERIC)
  {	return "SQL_NUMERIC";
  }else if(val == SQL_DECIMAL)
  {	return "SQL_DECIMAL";
  }else if(val == SQL_INTEGER)
  {	return "SQL_INTEGER";
  }else if(val == SQL_SMALLINT)
  {	return "SQL_SMALLINT";
  }else if(val == SQL_FLOAT)
  {	return "SQL_FLOAT";
  }else if(val == SQL_REAL)
  {	return "SQL_REAL";
  }else if(val == SQL_DOUBLE)
  {	return "SQL_DOUBLE";
  }else if(val == SQL_DATETIME)
  {	return "SQL_DATETIME";
  }else if(val == SQL_VARCHAR)
  {	return "SQL_VARCHAR";
  }
  return "UNKNOWN SQL type";
}

Table
DBConnection::odbcResultSetToTable_( const SQLHSTMT & hstmt ) const {
    SQLSMALLINT nCols = 0;
    const SQLRETURN retcode = SQLNumResultCols(hstmt,&nCols);

    if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO) {
        string emsg = "Error getting number of result columns";
        throw CARMA_EXCEPTION(DBConnectionException,emsg);
    }

    void *colVals[nCols], *columns[nCols];
    SQLLEN colValInds[nCols];
    SQLSMALLINT nameLen = 0, dataType = 0, decimalDigits = 0, nullable = 0;
    SQLULEN columnSize = 0;
    int cType = 0, buflen = 0, maxLen = 500, i = 0;
    SQLCHAR *colName = new SQLCHAR[maxLen];
    vector<Table::ColumnType> colTypes;
    for(i = 0; i < nCols; i++) {
        SQLDescribeCol(hstmt, i+1, colName, maxLen, &nameLen, &dataType,
                       &columnSize, &decimalDigits, &nullable);
        if(dataType == SQL_DOUBLE) {
            colVals[i] = new SQLDOUBLE;
            buflen = 0;
            cType = SQL_C_DOUBLE;
            columns[i] = new Column<double>(string((char *)colName));
            colTypes.push_back(Table::DOUBLE_COLUMN_TYPE);
        } else if(dataType == SQL_FLOAT) {
            colVals[i] = new SQLFLOAT;
            buflen = 0;
            cType = SQL_C_FLOAT;
            columns[i] = new Column<float>(string((char *)colName));
            colTypes.push_back(Table::FLOAT_COLUMN_TYPE);
        } else if(dataType == SQL_INTEGER) {
            colVals[i] = new SQLINTEGER;
            buflen = 0;
            cType = SQL_C_LONG;
            columns[i] = new Column<int>(string((char *)colName));
            colTypes.push_back(Table::INT_COLUMN_TYPE);
        } else if(dataType == SQL_CHAR || dataType == SQL_VARCHAR 
                  || dataType == SQL_LONGVARCHAR || dataType == SQL_BINARY 
                  || dataType == SQL_VARBINARY
                  || dataType == SQL_WVARCHAR
                  || dataType == SQL_LONGVARBINARY) {
            colVals[i] = new SQLCHAR[maxLen];
            buflen = maxLen;
            cType = SQL_C_CHAR;
            columns[i] = new Column<string>(string((char *)colName));
            colTypes.push_back(Table::STRING_COLUMN_TYPE);
#if 0
        } else if(dataType == SQL_DECIMAL){
	  // This works if the calling function treats strings as
	  // containing decimals.
            colVals[i] = new SQLCHAR[maxLen];
            buflen = maxLen;
            cType = SQL_C_CHAR;
            columns[i] = new Column<string>(string((char *)colName));
            colTypes.push_back(Table::STRING_COLUMN_TYPE);
#else
        } else if(dataType == SQL_DECIMAL){
            colVals[i] = new SQLCHAR[maxLen];
            buflen = maxLen;
	    cType = SQL_C_CHAR; // Decimal is sent as CHAR.
            columns[i] = new Column<string>(string((char *)colName));
            colTypes.push_back(Table::DECIMAL_COLUMN_TYPE);
#endif
        } else if(dataType == SQL_SMALLINT) {
            colVals[i] = new SQLSMALLINT;
            buflen = 0;
            cType = SQL_C_SHORT;
            columns[i] = new Column<short>(string((char *)colName));
            colTypes.push_back(Table::SHORT_COLUMN_TYPE);
        } else if(dataType == SQL_BIGINT) {
            colVals[i] = new SQLBIGINT;
            buflen = 0;
            cType = SQL_C_UBIGINT;
            columns[i] = new Column<long long>(string((char *)colName));
            colTypes.push_back(Table::BIGINT_COLUMN_TYPE);
        } else {
            // this branch should never be reached, if it is, it indicates
            // a column type should be taken care of above.
            /*
            for(i = 0; i < nCols; i++) {
                delete colVals[i];
                delete columns[i];
            }
            */
            ostringstream emsg;
            emsg << "Unable to find datatype " << dataType 
		 << "  (" << sql2name(dataType) << ")"
                 << " for column number " << i << " column name " << colName 
                 << endl;
#if 0
	    // If this is enabled, the process will sleep if the file
	    // "/tmp/testDB" exists to allow time to attach a debugger.
	    { struct stat buf;
	      int print=1;
	      static const char *DBFILE="/tmp/testDB";
	        while(stat(DBFILE, &buf) == 0)
		{ if(print)
		  {   cerr
		    << "odbcResultSetToTable_. Remove \""
		    << DBFILE << "\" to continue: " << endl;
		    print = 0;
		  }
		  sleep(2);
		}
	    }
#endif

            delete [] colName;
            throw CARMA_ERROR(emsg.str());
        }
        SQLBindCol(hstmt, i+1, cType, colVals[i], buflen, &colValInds[i]);
    }     
    delete [] colName;
    SQLRETURN rc;
    Column<double> *dbltmp;
    Column<float> *flttmp;
    Column<int> *inttmp;
    Column<long long> *biginttmp;
    Column<short> *shorttmp;
    Column<string> *stringtmp;
    Column<string> *decimaltmp;	// Decimals are implemented using string.
    
    // populate the columns in the loop below
    while(true) {
        rc = SQLFetch(hstmt);
        if (rc == SQL_SUCCESS || rc == SQL_SUCCESS_WITH_INFO) {
            for(i = 0; i < nCols; i++) {
                switch(colTypes[i]) {
                case Table::BIGINT_COLUMN_TYPE: {
                    biginttmp = static_cast<Column<long long> *>(columns[i]);
                    if(colValInds[i] == SQL_NULL_DATA) {
                        biginttmp->push_back_null();
                    } else {
                        biginttmp->push_back(*(long long *)colVals[i]);
                    }
                    break;
                }
                case Table::DOUBLE_COLUMN_TYPE: {
                    dbltmp = static_cast<Column<double> *>(columns[i]);
                    if(colValInds[i] == SQL_NULL_DATA) {
                        dbltmp->push_back_null();
                    } else {
                        dbltmp->push_back(*(double *)colVals[i]);
                    }
                    break;
                }
                case Table::FLOAT_COLUMN_TYPE: {
                    flttmp = static_cast<Column<float> *>(columns[i]);
                    if(colValInds[i] == SQL_NULL_DATA) {
                        flttmp->push_back_null();
                    } else {
                        flttmp->push_back(*(float *)colVals[i]);
                    }
                    break;
                }
                case Table::INT_COLUMN_TYPE: {
                    inttmp = static_cast<Column<int> *>(columns[i]);
                    if(colValInds[i] == SQL_NULL_DATA) {
                        inttmp->push_back_null();
                    } else {
                        inttmp->push_back(*(int *)colVals[i]);
                    }
                    break;
                }
                case Table::SHORT_COLUMN_TYPE: {
                    shorttmp = static_cast<Column<short> *>(columns[i]);
                    if(colValInds[i] == SQL_NULL_DATA) {
                        shorttmp->push_back_null();
                    } else {
                        shorttmp->push_back(*(short *)colVals[i]);
                    }
                    break;
                }
                case Table::STRING_COLUMN_TYPE: {
                    stringtmp = static_cast<Column<string> *>(columns[i]);
                    if(colValInds[i] == SQL_NULL_DATA) {
                        stringtmp->push_back_null();
                    } else {
                        stringtmp->push_back(string((char *)colVals[i]));
                    }
                    break;
                }
                case Table::DECIMAL_COLUMN_TYPE: {
                    decimaltmp = static_cast<Column<string> *>(columns[i]);
                    if(colValInds[i] == SQL_NULL_DATA) {
                        decimaltmp->push_back_null();
                    } else {
                        decimaltmp->push_back(string((char *)colVals[i]));
                    }
                    break;
                }
                }
            }
        } else {
            break;
        }
    }
    
    //freeSQLStatement_(hstmt);
    
    // create and populate the Table containing the results
    Table t;
    
    for(i = 0; i < nCols; i++) {
        switch(colTypes[i]) {
        case Table::BIGINT_COLUMN_TYPE: {
            t.addBigIntColumn(*static_cast<Column<long long> *>(columns[i]));
            break;
        }
        case Table::DOUBLE_COLUMN_TYPE: {
            t.addDoubleColumn(*static_cast<Column<double> *>(columns[i]));
            break;
        }
        case Table::FLOAT_COLUMN_TYPE: {
            t.addFloatColumn(*static_cast<Column<float> *>(columns[i]));
            break;
        }
        case Table::INT_COLUMN_TYPE: {
            t.addIntColumn(*static_cast<Column<int> *>(columns[i]));
            break;
        }
        case Table::SHORT_COLUMN_TYPE: {
            t.addShortColumn(*static_cast<Column<short> *>(columns[i]));
            break;
        }
        case Table::STRING_COLUMN_TYPE: {
            t.addStringColumn(*static_cast<Column<string> *>(columns[i]));
            break;
        }
        case Table::DECIMAL_COLUMN_TYPE: {
            t.addDecimalColumn(*static_cast<Column<string> *>(columns[i]));
            break;
        }
        }
    }
    
    // delete pointers to avoid memory leaks
    for(i = 0; i < nCols; i++) {
        switch(colTypes[i]) {
        case Table::BIGINT_COLUMN_TYPE:
            delete (SQLBIGINT *)colVals[i];
            delete static_cast<Column<int> *>(columns[i]);
            break;
        case Table::DOUBLE_COLUMN_TYPE:
            delete (SQLDOUBLE *)colVals[i];
            delete static_cast<Column<double> *>(columns[i]);
            break;
        case Table::FLOAT_COLUMN_TYPE:
            delete (SQLFLOAT *)colVals[i];
            delete static_cast<Column<float> *>(columns[i]);
            break;
        case Table::INT_COLUMN_TYPE:
            delete (SQLINTEGER *)colVals[i];
            delete static_cast<Column<int> *>(columns[i]);
            break;
        case Table::SHORT_COLUMN_TYPE:
            delete (SQLSMALLINT *)colVals[i];
            delete static_cast<Column<short> *>(columns[i]);
            break;
        case Table::STRING_COLUMN_TYPE:
            delete [] (SQLCHAR *)colVals[i];
            delete static_cast<Column<string> *>(columns[i]);
            break;
        case Table::DECIMAL_COLUMN_TYPE:
            delete [] (SQLCHAR *)colVals[i];
            delete static_cast<Column<string> *>(columns[i]);
            break;
        }
    }
    
    return t;
}

Table DBConnection::execSQLSelect
    (const vector<string>& columnNames, const string& tableName,
     const string& whereClause, const string& orderByClause,
     const string& groupByClause, const string& havingClause,
     const string& resultTableName) const {
    if(columnNames.size() == 0) {
        string emsg = "columnNames parameter is not permitted to have 0 ";
        emsg += "elements";
        throw CARMA_EXCEPTION(IllegalArgumentException, emsg);
    }
    ostringstream statement;
    statement << "SELECT " << util::vectorToString(columnNames) 
              << " FROM " << tableName;
    if(whereClause != "") {
        statement << " WHERE " << whereClause;
    }
    if(orderByClause != "") {
        statement << " ORDER BY " << orderByClause;
    }
    if(groupByClause != "") {
        statement << " GROUP BY " << groupByClause;
    }
    if(havingClause != "") {
        statement << " HAVING " << groupByClause;
    }
    return execSQLSelect(statement.str());
}
        



void DBConnection::execSQLDropTable(const string& tableName) const {
    if(tableName.find("MonitorData") == string::npos 
       && tableName.find("Log") == string::npos) {
        string emsg = "Dropping table " + tableName + " is not permitted. "
            + "Only volatile (monitor data and log) tables can be dropped "
            + "using DBConnection::execSQLDropTable()";
        throw CARMA_EXCEPTION(DBConnectionException,emsg);
    }
    string stmt = "DROP TABLE " + tableName;
    directSQLExec_(stmt);
}

void DBConnection::deleteFromMonitorIndexTable(const string& whereClause) 
    const {
    string stmt = "DELETE FROM " + getTableName(MONITOR_INDEX_TABLE) + " " 
        + whereClause;
    directSQLExec_(stmt);
}    

void DBConnection::monitorDataTableHasBeenPopulated(const string& tableName,
                               const MonitorAverageType& averageType) 
    const {
    if(getRowCount(tableName) == 0) {
        // no rows in table, remove it
        directSQLExec_("DROP TABLE " + tableName);
        directSQLExec_("DELETE FROM " + getTableName(MONITOR_INDEX_TABLE) 
                      + " WHERE tableName='" + tableName + "'");
    } else {
        MonitorDataDatabase mddb(this);
        mddb.updateMonitorIndexTable(tableName, averageType);
    }
}

void DBConnection::beginTransaction() const {
    UWORD v = SQL_AUTOCOMMIT_OFF;
    SQLRETURN retcode = 0;
    retcode = SQLSetConnectAttr(hdbc_, SQL_AUTOCOMMIT, &v, sizeof(v));    
    if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO) {
        string emsg = "Error setting up transaction";
        throw CARMA_EXCEPTION(DBConnectionException,emsg);
    }
    //programLogInfo("****Beginning transaction****");
}

void DBConnection::rollBackTransaction() const {
    const SQLRETURN retcode1 = SQLEndTran(SQL_HANDLE_DBC, hdbc_, SQL_ROLLBACK);
    if (retcode1 != SQL_SUCCESS && retcode1 != SQL_SUCCESS_WITH_INFO) {
        string emsg = "Error rolling back transaction";
        logger_ << log4cpp::Priority::ERROR << emsg;
        throw CARMA_EXCEPTION(DBConnectionException, emsg);
    }
    UWORD v = SQL_AUTOCOMMIT_ON;
    const SQLRETURN retcode2 = SQLSetConnectAttr(hdbc_, SQL_AUTOCOMMIT, &v, sizeof(v));
    if (retcode2 != SQL_SUCCESS && retcode2 != SQL_SUCCESS_WITH_INFO) {
        string emsg = "Error setting auto commit mode to on";
        logger_ << log4cpp::Priority::ERROR << emsg;
        throw CARMA_EXCEPTION(DBConnectionException,emsg);
    }
    //programLogInfo("****Rollback transaction****");
}

void DBConnection::commitTransaction() const {
    const SQLRETURN retcode1 = SQLEndTran(SQL_HANDLE_DBC, hdbc_, SQL_COMMIT);
    if (retcode1 != SQL_SUCCESS && retcode1 != SQL_SUCCESS_WITH_INFO) {
        string emsg = "Error committing transaction";
        logger_ << log4cpp::Priority::ERROR << emsg;
        throw CARMA_EXCEPTION(DBConnectionException,emsg);
    }
    UWORD v = SQL_AUTOCOMMIT_ON;
    const SQLRETURN retcode2 = SQLSetConnectAttr(hdbc_, SQL_AUTOCOMMIT, &v, sizeof(v));
    if (retcode2 != SQL_SUCCESS && retcode2 != SQL_SUCCESS_WITH_INFO) {
        string emsg = "Error setting auto commit mode to on";
        logger_ << log4cpp::Priority::ERROR << emsg;
        throw CARMA_EXCEPTION(DBConnectionException,emsg);
    }
}

bool DBConnection::inTransaction() const {
    UWORD *value = new UWORD;
    SQLINTEGER *obuf = 0;
    const SQLRETURN retcode =
        SQLGetConnectAttr( hdbc_,
                           SQL_ATTR_AUTOCOMMIT,
                           value,
                           static_cast< SQLINTEGER >(sizeof(UWORD)),
                           obuf );
    if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO) {
        string emsg = "Error getting value of autocommit attribute";
        logger_ << log4cpp::Priority::ERROR << emsg;
        throw CARMA_EXCEPTION(DBConnectionException,emsg);
    }
    bool res = (*value == SQL_AUTOCOMMIT_OFF);
    ostringstream o;
    o << "inTransaction() returns " << res;
    //programLogInfo(o.str());
    delete value;
    return res;
}



//----------------------- Log table related methods ---------------------


void DBConnection::insertLogMessage(const string& message, 
                                    const frameType& frameCount) 
    const {
    ostringstream ss;
    ss << "INSERT INTO Log (frameCount, message) VALUES (" << frameCount 
       << ", '" << message << "')";
    directSQLInsert_(ss.str());
}

Table DBConnection::getLogMessages(const frameType& start,
                                   const frameType& end,
                                   const string *matchString) const {
    ostringstream ss;
    ss << "SELECT frameCount, message FROM Log WHERE frameCount>=" << start 
       << " AND frameCount<=" << end;
    if(matchString != 0) {
        ss << " AND message LIKE '%" << *matchString << "%'";
    }
    try {
        return execSQLSelect(ss.str());
    } catch (const SQLException &exc) {
        ostringstream emsg;
        emsg << "DBConnection QUERY BUG!: " << exc.what();
        throw CARMA_ERROR(emsg.str());
    }
}


 


// -------------- DBConnection protected methods -------------------------

//------------------------ Database access methods -----------------------

void DBConnection::openODBCConnection_() {
    openODBCConnection_(&henv_,&hdbc_);
}

void DBConnection::openODBCConnection_(::SQLHENV *henv, ::SQLHDBC *hdbc) {
    string emsg = "";
    if (SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, henv) != SQL_SUCCESS) 
        {
        emsg = "Could not allocate ODBC environmnet";
        CPTRACE(Trace::TRACE1, emsg);
        throw CARMA_EXCEPTION(DBConnectionException,emsg);
    }
    SQLSetEnvAttr (*henv, SQL_ATTR_ODBC_VERSION, (SQLPOINTER) SQL_OV_ODBC3, 0);
    if (SQLAllocHandle (SQL_HANDLE_DBC, *henv, hdbc) != SQL_SUCCESS) {
        if (henv) {
            //SQLFreeHandle(SQL_HANDLE_ENV,henv);
            SQLFreeEnv(*henv);
        }
        emsg = "Could not allocate ODBC connection handle";
        CPTRACE(Trace::TRACE1, emsg);
        throw CARMA_EXCEPTION(DBConnectionException, emsg);
    }


    string connectString = "DSN=" + dataSource_ + ";UID=" + dbuser_;

    //#if MYSQL_VERSION_ID >= 50000
    // (The flag should be harmless for mysql 4.0).
    { // The private file, MyODBC/driver/myodbc3.h defines the flag:
      //const int FLAG_NO_BIGINT = 16384; /* Change BIGINT to INT */
      // It is used in utility.c. An example of its usage was found
      // at: 
      // http://forum.fachinformatiker.de/showthread.php?mode=hybrid&t=73584
      // (It's in German).
      // This tells odbc to return columns as SQL_INTS that would otherwise
      // be returned as SQL_BIGINT. We don't use BIGINTs, but somewhere
      // between mysql 4.0 and 5.0, "SELECT COUNT(*) from <table>" commands
      // started to be returned at BIGINTs which broke routines trying
      // to get the # of rows in a table.
      connectString += ";Option=16384";
    }
    //#endif

    odbcConnectString_ = connectString;
    if(passwordFile_ != "") {
        connectString += ";PWD=" + readPassword_();
        odbcConnectString_ += ";PWD=********";
    }

    /*
    SQLSMALLINT outBuflen;
    SQLSMALLINT inBufLen = 4096;
    SQLCHAR buf[inBufLen];
    const char *cConnectString = connectString.c_str();
    int status = SQLDriverConnect(*hdbc, 0, (SQLCHAR *)cConnectString,
                                  strlen(cConnectString), buf, inBufLen,
                                  &outBuflen,  SQL_DRIVER_NOPROMPT);
    */

    const char *szDSN = connectString.c_str();
    SQLCHAR szOut[4096];
    SQLSMALLINT nLen;
    int status = SQLDriverConnect( *hdbc, 0, (SQLCHAR*)szDSN, strlen(szDSN), 
                                   szOut, 4096, &nLen, SQL_DRIVER_NOPROMPT );
    if (status != SQL_SUCCESS && status != SQL_SUCCESS_WITH_INFO) {
        emsg = "Could not connect to the database using connect string "
            + odbcConnectString_ + ": ";
        if(status == SQL_ERROR) {
            unsigned i = 1;
            SQLCHAR sqlState[6], msg[255];
            SQLINTEGER nativeError;
            SQLSMALLINT msgLen;
            SQLRETURN rc = SQLGetDiagRec(SQL_HANDLE_DBC, *hdbc, i, sqlState, 
                                         &nativeError, msg, sizeof(msg), 
                                         &msgLen);
            if(rc != SQL_INVALID_HANDLE) {
                if (rc == SQL_NO_DATA) {
                    emsg += "return status SQL_ERROR, ";
                    emsg += "but SQLGetDiagRec returned SQL_NO_DATA";
                }
                else {
                    emsg += string((char*)msg);
                }    
                while ((rc != SQL_NO_DATA) && (rc == SQL_SUCCESS)) {
                    emsg += "\n  " + string((char*)msg);
                    i++;
                    rc = SQLGetDiagRec(SQL_HANDLE_DBC, hdbc, i, sqlState, 
                                       &nativeError, msg, sizeof(msg), &msgLen);
                    if (rc == SQL_SUCCESS) {
                        emsg += "\n  " + string((char*)msg);
                    }
                }
            }
            else {
                emsg += "return status SQL_ERROR, ";
                emsg += "but SQLGetDiagRec returned SQL_INVALID_HANDLE";
            }
        }
        else if (status == SQL_NO_DATA) {
            emsg += "Return status SQL_NO_DATA"; 
        }
        else if (status == SQL_STILL_EXECUTING) {
            emsg += "Return status SQL_STILL_EXECUTING"; 
        }
        else if (status == SQL_INVALID_HANDLE) {
            emsg += "Return status SQL_INVALID_HANDLE"; 
        }
        else {
            emsg += "Return status is unknown value";
        }
        closeODBCConnection_(henv,hdbc);
        programLogError(emsg);
        CPTRACE(Trace::TRACE1, emsg);
        throw CARMA_EXCEPTION(DBConnectionException, emsg);
    }
}

void DBConnection::closeConnection() {
    closeODBCConnection_();
}

string DBConnection::createJoinClause
    (const string& leftTable, const string& rightTable,
     const string& leftColumn, const string& rightColumn, 
     const string& rightTableAlias, const string& indexCol) {
    string rColumn = (rightColumn == "") ? leftColumn : rightColumn;
    string clause;
    clause = "JOIN " + rightTable;
    clause += (rightTableAlias == "") ? "" : " AS " + rightTableAlias;
    clause += (indexCol == "") ? "" : " USE INDEX(" + indexCol + ")";
    clause += " ON " +  leftTable + "." + leftColumn
        + "=";
    clause += (rightTableAlias == "") ? rightTable : rightTableAlias;
    clause += "." + rColumn;
    return clause;
}

string DBConnection::createJoinClause 
    (const string& joinTable, const string& onClause) {
    string clause;
    clause = "JOIN " + joinTable + " ON " +  onClause;
    return clause;
}

string DBConnection::createInsertStatement(const string& tableName, 
                             const vector<string> columns, 
                             const vector<string> values) {
    string statement = "INSERT INTO ";
    statement += tableName + " (" + util::vectorToString(columns) 
        + ") VALUES (" + util::vectorToString(values) + ")";
    return statement;
}

string DBConnection::createInsertStatement(const string& tableName, 
                                           const vector<string> columns, 
                                           const string values) {
    string statement = "INSERT INTO ";
    statement += tableName + " (" + util::vectorToString(columns) 
        + ") VALUES (" + values + ")";
    return statement;
}


void DBConnection::closeODBCConnection_() {
    CPTRACE( util::Trace::TRACEALL, "begin closeODBCConnection(");
    closeODBCConnection_(henv_,hdbc_);
    CPTRACE( util::Trace::TRACEALL, "end closeODBCConnection(");
}

void DBConnection::closeODBCConnection_(SQLHENV henv, SQLHDBC hdbc) const {

    if (hdbc) {
        SQLDisconnect (hdbc);
        SQLFreeHandle (SQL_HANDLE_DBC, hdbc);
    }
    if (henv) {
        // I have seen a very infrequent problem here that something gets
        // stuck in the SQLFreeHandle call, so I'm adding a couple of trace
        // statements
        SQLFreeHandle(SQL_HANDLE_ENV,henv);
    }
    /*
    // copying how imyodbc does it...
    SQLDisconnect( hdbc );
    SQLFreeConnect( hdbc );
    SQLFreeEnv( henv );
    */
}

void DBConnection::freeSQLStatement_(const SQLHSTMT& hstmt) const {
    if (hstmt) {
        SQLFreeHandle(SQL_HANDLE_STMT,hstmt);
    }
};

//------------------------ Database query methods ----------------------

void DBConnection::directSQLInsert_(const string& statement) const {
    const string tmp = StringUtils::lowASCIIAlphaNumericToLower(statement);

    if ( tmp.find("insert") == string::npos ) {
        ostringstream emsg;
        
        emsg << "Submitted statement " + statement + " is not an SQL INSERT "
             << "statement" << endl;
        
        throw CARMA_EXCEPTION(SQLException, emsg.str());
    }
    
    directSQLExec_(statement);
}

void DBConnection::directSQLExec_(const string& statement) const {
    SQLHSTMT hstmt;
    if (SQLAllocHandle (SQL_HANDLE_STMT, hdbc_, &hstmt) != SQL_SUCCESS) {
        programLogError("Unable to allocate statement("+statement+")");
        freeSQLStatement_(hstmt);
        throw CARMA_EXCEPTION(DBConnectionException, 
                              "Unable to allocate statement");
    }
    try {
        directSQLExec_(hstmt, statement);
    } catch (const SQLException &exc) {
        programLogError("SQLException executing \""+statement+"\"\n" + 
            exc.getMessage());
        freeSQLStatement_(hstmt);
        throw CARMA_EXCEPTION(SQLException,exc.getMessage());
    } catch (const BaseException& exc) {
        programLogError("BaseException executing \""+statement+"\"\n" + 
            exc.getMessage());
        freeSQLStatement_(hstmt);
        throw CARMA_EXCEPTION(SQLException, exc.getMessage());
    } catch (const std::exception& exc) {
        programLogError("std::exception executing \""+statement+"\"\n" + 
            exc.what());
        freeSQLStatement_(hstmt);
        throw CARMA_EXCEPTION(SQLException, exc.what());
    } catch (...) {
        programLogError("Unknown exception executing \""+statement+"\"");
        freeSQLStatement_(hstmt);
        throw CARMA_EXCEPTION(SQLException, 
            "Unknown exception in directSQLExec_");
    }
    freeSQLStatement_(hstmt);
}

void DBConnection::directSQLExec_(const SQLHSTMT hstmt, const string& statement)
    const {
    //programLogInfo("directSQLExec_("+statement+")");
    ostringstream os;
    os << statement << endl;
    const SQLRETURN retcode = 
            SQLExecDirect(hstmt, (SQLCHAR*)statement.c_str(), SQL_NTS);
    if (retcode != SQL_SUCCESS && retcode != SQL_SUCCESS_WITH_INFO) {
        string emsg = "SQL Error: " + statement + "\n" ;
        if(retcode == SQL_ERROR) {
            unsigned i = 1;
            SQLCHAR sqlState[6], msg[255];
            SQLINTEGER nativeError;
            SQLSMALLINT msgLen;
            SQLRETURN rc = SQLGetDiagRec(SQL_HANDLE_STMT, hstmt, i, sqlState, 
                                         &nativeError, msg, sizeof(msg), 
                                         &msgLen);
            while (rc != SQL_NO_DATA && (rc == SQL_SUCCESS)) {
                emsg += string((char *)msg);
                i++;
                rc = SQLGetDiagRec(SQL_HANDLE_STMT, hstmt, i, sqlState, 
                                   &nativeError, msg, sizeof(msg), &msgLen);
            }
        }
        logger_ << log4cpp::Priority::ERROR << emsg;
        throw CARMA_EXCEPTION(SQLException,"SQLException: " + emsg);
    }
}    

string DBConnection::readPassword_() const {
    ifstream fin(passwordFile_.c_str());
    if(!fin) {
        string emsg = "Unable to open password file " + passwordFile_ + " for "
            "reading";
        throw CARMA_EXCEPTION(NotFoundException,emsg);
    }
    string line;
    while(!fin.eof()) {
        getline(fin,line);
        break;
    }
    fin.close();
    return line;
}




//--------------------------- DBConnectionFactory --------------------

DBConnection *DBConnectionFactory::createConnection(const DBConfigurator *dbconf) {
    DBConnection *dbc = 0;
    string rdbms;
    rdbms = (dbconf == 0 || dbconf->getRDBMS() == "") 
        ? StringUtils::lowASCIIAlphaNumericToLower( getResource(DEFAULT_RDBMS) ) 
        : StringUtils::lowASCIIAlphaNumericToLower( dbconf->getRDBMS() );
    if(rdbms == StringUtils::lowASCIIAlphaNumericToLower(MySQLDBConnection::RDBMS)) {
        CPTRACE(Trace::TRACE6, "Attempting to create connection "
                << "to MySQL database");
        dbc = new MySQLDBConnection(dbconf);
    } 
#ifdef CARMA_DBMS_POSTGRESQL
    if(rdbms == lowASCIIAlphaNumericToLower(PostgresDBConnection::RDBMS)) {
        dbc = new PostgresDBConnection(dbconf);
    }
#endif
    if (dbc == 0) {
        string emsg = "Unknown RDBMS " + rdbms;
        throw CARMA_EXCEPTION(DBConnectionException, emsg);
    } else {
        return dbc;
    }
}

