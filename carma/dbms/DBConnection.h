#ifndef CARMA_DBMS_DBCONNECTION_H
#define CARMA_DBMS_DBCONNECTION_H

/**
 * @file
 * DBConnection and DBConnectionFactory classes.
 *
 * @author: Dave Mehringer, Ray Plante
 *
 * $Id: DBConnection.h,v 1.80 2011/12/21 22:56:43 mpound Exp $
 * $CarmaCopyright$
 *
 */


#include "carma/dbms/MonitorSystemAndDBMSRelationships.h"
#include "carma/dbms/Table.h"
#include "carma/util/ErrorException.h"
#include "carma/util/Logger.h"
#include "carma/util/Time.h"
#include <map>
#include <string>
#include <vector>
#include <unixODBC/sqlext.h>

namespace carma {
namespace dbms {

    class DBConfigurator;

/**
 * an abstract class from which database queries may be launched.  
 * To obtain an object, Objects of this class are created via 
 * <TT>DBConnectionFactory::createConnection()</TT>.
 * DBConnection  objects use configuration information contained in a
 * DBConfiguration object which is passed to 
 * <TT>DBConnectionFactory::createConnection()</TT>. Configuration parameters
 * used by DBConnection objects are
 * <CODE>
 * dbname, the database name to which to connect (e.g,. carma)
 * user, the user to connect as (e.g., mld)
 * passwordFile, the file from which to read the dbuser's password (this file
 *               only be readable by the unix user!)
 * odbcini, the file from which to obtain ODBC configuration information
 */
class DBConnection {

public:
    friend class DBConnectionTest;
    friend class LogDatabase;
    friend class MonitorConfigurationDatabase;
    friend class MonitorDataDatabase;
    friend class MonitorDataQueryManager;

    /**
     * type to map data file names to tables they should be loaded into
     */
    typedef std::map<std::string, std::string> file2TableMap;


    /**
     * destructor, derived classes may want to override
     */
    virtual ~DBConnection();

    // -------------------- Database status/info methods --------------------
    /**
     * is the database specified by the dbconf object up and accepting 
     * connections?
     * @param dbconf the DBConfigurator object containing info on how to
     *        connect to the database, if NULL use sensible defaults
     * @return true if the specified database is up and available for 
     *         connections
     */
    static bool isUp(const DBConfigurator *dbconf = NULL); 
                     

    /**
     * is the RDBMS in question up and accepting connections
     * @return true if a connection can be made to the database
    */
    virtual bool isDBUp();

    /**
     * print various details about ODBC support
     */
    void odbcInfo() const;

    /**
     * get the name of the RDBMS associated with this object
     * @return the name of the RDBMS associated with this object
     */
    inline std::string rdbmsName() const {return rdbmsName_;}

    /**
     * is this object using the production database?
     * @return if this object using the production database
     */
    //inline bool usingProductionDB() const {return useProductionDB_;}

    /**
     * get the data source string used for an ODBC connection
     * @return the datasource string
     */
    inline std::string dataSource() const {return dataSource_;}


    //------------------ Monitor data tables access --------------------

    /**
     * create a monitor data table
     * @param averageType the average type of the table
     * @param dataType the data type of the table
     * @param tag the tag to be appended to the table name
     * @param location location (directory) at which to create the table, if
     *        NULL, use the default location determined by the RDBMS
     * @return the name of the created table
     * @throws DBConnecitonException
    */
    virtual std::string createMonitorDataTable(
                                    const MonitorAverageType& averageType,
                                    const MonitorAggregateType& aggType,
                                    const std::string& tag, 
                                    const std::string *const location=NULL) 
        const = 0;


    /**
     * load data into a table from a disk file.  Because this is not an
     * ODBC supported operation, derived classes must implement this method
     * using their own native RDBMS interfaces
     * @param filename the name of the file from which to load the data
     * @param table the database table into which to load the data
     * @param useTransactions use transcations to load the data, how this
     *        parameter is used is determined by the implenting class
     * @param linesToIgnore number of lines to ignore (e.g. header files)
     *        at the beginning of the file 
     */
    virtual void loadDataFromFile
        (const std::string& filename, const std::string& table,
         const bool& useTransactions, const int& linesToIgnore=0,
         const std::string& columnDelimiter="\t") const = 0;

    /**
     * load data into tables from disk files.  Because this is not an
     * ODBC supported operation, derived classes must implement this method
     * using their own native RDBMS interfaces
     * @param file2Table the file to table map
     * @param useTransactions should transactions be used (implementing classes
     *        decide how to use this parameter
     * @param linesToIgnore number of lines to ignore (e.g. header files)
     *        at the beginning of the file 
     */
    virtual void loadDataFromFiles
        (const file2TableMap& file2Table, const bool& useTransactions, 
         const int& linesToIgnore=0, const std::string& columnDelimiter="\t") 
        const = 0;

    /**
     * get the specified aggregate value of the specified column from the
     * specified table. the returned pointer is created internally by calling 
     * "new"; callers are responsible for running "delete" on it to prevent 
     * memory leaks, the type of the returned pointer can be determined by
     * the output "type" parameter
     * @param column [in] the column from which to get the aggregate value
     * @param table [in] table from which to get the aggregate value
     * @param function [in] the aggregate function to use
     * @param type [out] the type of the returned pointer
     * @return the pointer to the value or null
     */
    virtual void* getAggregate(const std::string& column, 
                               const std::string& table,
                               const std::string& function,
                               carma::dbms::Table::ColumnType& type) const;

    /**
     * get the number of rows in the specified table
     * @param tableName the name of the table
     * @return the number of rows in the specified table
     */
    virtual long getRowCount(const std::string& tableName) const;
    

    //---------------------- General table access ------------------------

    /**
     * execute a SELECT query
     * @param statement the SELECT query to execute
     * @param resultTableName the name of the results table
     * @return a Table containing the results of the query
     * @throws DBConnectionException
     */
    virtual carma::dbms::Table execSQLSelect(const std::string& statement, 
                             const std::string& resultTableName="RESULT TABLE")
        const;


    /**
     * execute a SELECT query
     * @param columnNames list of column names to be returned in the results
     *        table
     * @param tableName table to execute the query on
     * @param whereClase the SQL WHERE clause ("" => no WHERE clause)
     * @param orderByClause the SQL ORDER BY clause ("" => no ORDER BY clause)
     * @param groupByClause the SQL GROUP BY clause ("" => no GROUP BY clause)
     * @param havingClause the SQL HAVING clause ("" => no HAVING clause)
     * @param resultTableName the name of the results table
     * @return a Table containing the results of the query
     * @throws DBConnectionException
     */
    virtual carma::dbms::Table execSQLSelect
        (const std::vector<std::string>& columnNames, 
         const std::string& tableName,
         const std::string& whereClause="", 
         const std::string& orderByClause="",
         const std::string& groupByClause="",
         const std::string& havingClause="",
         const std::string& resultTableName="RESULT TABLE")
        const;

    /**
     * drop a table.  Only volatile tables (monitor data, log) are permitted
     * to be dropped via this interface
     * @param tableName the table to be dropped
     * @throws DBConnectionException
     */
    virtual void execSQLDropTable(const std::string& tableName) const;

    /**
     * delete rows from the MONITOR_INDEX_TABLE
     * @param whereClause the where clause specifying which rows should be
     *        deleted
     * @throws DBConnecitonException
     */
    virtual void deleteFromMonitorIndexTable(const std::string& whereClause)
        const;

    /**
     * performs tasks after a monitor data table has been completely populated.
     * currently this means a call to updateMonitorIndexTable() is made,
     * although derived classes may have different implementations
     */
    virtual void monitorDataTableHasBeenPopulated(const std::string& tableName,
                                  const MonitorAverageType& averageType) 
        const;

    /**
     * drop a table from the scratch database
     * @param table the table to drop
     */
    virtual void dropScratchTable(const std::string& table) const;

    //------------------- Transaction support -------------------------
    /**
     * begin a transaction
     * @throws DBConnectionException
     */
    virtual void beginTransaction() const;

    /**
     * rollback a transaction
     * @throws DBConnectionException
     */
    virtual void rollBackTransaction() const;

    /**
     * commit a transaction
     * @throws DBConnectionException
     */
    virtual void commitTransaction() const;

    /**
     * is a transaction being executed?
     * @return true if a transaction is being executed
     * @throws DBConnectionException
     */
    virtual bool inTransaction() const;


    //------------------- Log table related methods ---------------------
    
    /**
     * add a log message to the Log table
     * @param message the message to add
     * @param frameCount the corresponding frameCount which applies to the 
     *        message
     * @throws DBConnectionException
     */
     void insertLogMessage(const std::string& message, 
                          const carma::util::frameType& frameCount
                          =carma::util::Time::computeCurrentFrame()) const;


    /**
     * get the log messages in the specified frameCount range.  The returned 
     * Table contains two columns the first contains the times and the second 
     * contains the messages, e.g, a client can access the info via
     * <code>
     * Table t = dbc->getLogMessage(<starttime>,<stoptime>);
     * vector<int> times = t.getIntegerColumn(0);
     * vector<string> msgs = t.getStringColumn(1);
     * </code>
     * @param start the start frameCount
     * @param end the end frameCount
     * @param matchString do additional filtering based on matches of this
     *        string in the messages, NULL => no filtering based on substring
     *        match
     * @return a two column table containing times (first column) and 
     *         messages (second column)
     * @throws DBConnectionException
     */
    Table getLogMessages(const carma::util::frameType& start,
                         const carma::util::frameType& end
                         =carma::util::Time::computeCurrentFrame(),
                         const std::string *matchString=NULL) const;

    /**
     * close the ODBC database connection
     */
    void closeConnection();

    //--------------------------- Database info ----------------------
    /**
     * does the specified table exist in the specified database?
     * @param table the table to check the existence of
     * @param database the database in which this table is located. 
     *        "" =&gt; current database
     * @return true if the table exists
     */
    bool tableExists(const std::string& table, const std::string& database) 
        const;

    /**
     * get info on the database
     * @param database database for which to get info ""=>current database
     * @return Table containing database info
     */
    carma::dbms::Table databaseInfo(const std::string& database="") const;

    // --------------------------SQL Helpers -------------------------
    /**
     * create a (inner) join clause
     * @param leftTable the left table in the join, if this table is to have
     *        an alias, the alias should be used in place of the table name
     * @param rightTable the (right) table to join
     * @param leftColumn the left table column to join on
     * @param rightColumn the right table join column, if blank 
     *       (the default), the columns to join in the two tables have the same
     *       name
     * @param the alias for the right table, if any
     * @param indexCol <b> MySQL specific</b> Add a USE INDEX(@p indexCol) 
     *        clause to the table reference if not "".
     * @return the JOIN clause
     */
    static std::string createJoinClause
        (const std::string& leftTable, const std::string& rightTable,
         const std::string& leftColumn, const std::string& rightColumn="",
         const std::string& rightTableAlias="", 
         const std::string& indexCol="");

    /**
     * create a (inner) join clause
     * @param joinTable the (right) table to join
     * @param onClause the ON clause describing how the tables should be joined
     * @return the JOIN clause
     */
    static std::string createJoinClause
        (const std::string& joinTable, const std::string& onClause);

    /**
     * create an SQL insert statement
     * @param table the table into which to insert a new record
     * @param columns the columns to which the values correspond
     * @param values the values to insert (converted to strings)
     */
    static std::string createInsertStatement
        (const std::string& tableName, const std::vector<std::string> columns,
         const std::vector<std::string> values);
                                             
    /**
     * create an SQL insert statement
     * <code>"INSERT INTO " + tableName + "(" 
     * + carma::util::vectorToString(columns) + ") VALUES (" + values + ")"
     * </code>
     * @param table the table into which to insert a new record
     * @param columns the columns to which the values correspond
     * @param values the stringified version of values to insert 
     */
    static std::string createInsertStatement
        (const std::string& tableName, const std::vector<std::string> columns,
         const std::string values);
                                             

    //-------------------------- RDBMS limits --------------------------
    /**
     * the maximum number of tables the RDBMS allows in a join
     * @return the maximum number of tables which can be joined in a SELECT
     *         statement
     */
    virtual unsigned maxTablesPerJoin() const = 0;

    /**
     * the maximum number of columns the RDBMS allows in a table
     * @return the maximum number of columns permitted in a table
     */
    virtual unsigned maxColumnsPerTable() const = 0;

    /**
     * get the modifier necessary for a case-sensitive text search
     * @return the modifier necessary for a case-sensitive text search
     */
    std::string virtual caseSensitiveSearchModifier() const { return " "; }

    //----------------------- table repair ---------------------

    /**
     * attempt to repair a corrupt monitor data table
     * @param tableName the table name to repair
     */
    virtual void repairMonitorDataTable(const std::string& tableName) 
        const = 0;



protected:
    std::string rdbmsName_;
    std::string dataSource_;
    std::string dbuser_;
    std::string odbcConnectString_;
    std::string socket_;
    unsigned port_;
                            
    // dbname is needed for methods which use native rdbms api calls
    std::string dbname_;
    std::string passwordFile_;
    log4cpp::Category& logger_;
    bool inTxn_;
    std::vector<SQLHSTMT> activeStatements;
    //bool useProductionDB_;
    /**
     * odbc config file
     */
    std::string odbcini_;


    /**
     * connection and environment handles for ODBC interface
     */
    SQLHDBC hdbc_;
    SQLHENV henv_;

    /**
     * create a DBConnection object. This constructor does not actually
     * establish a database connection; derived class constructors are
     * responsible for doing that by calling e.g. createODBCConnection()
     * within their constructors
     * @param dbconf the DBConfigurator object containing configuration
     *        info used by this object, if NULL use sensible defaults
     * @see DBConnectionFactory::createConnection
     */
    DBConnection(const carma::dbms::DBConfigurator *dbconf); 

    //----------------------- Database access methods -----------------------
    /**
     * open a database connection using ODBC, <code>henv_</code> and <code>
     * hdbc_ are initialized here
     * @throws DBConnectionException if the connection cannot be established
     */
    void openODBCConnection_(SQLHENV *henv, SQLHDBC *hdbc);
    void openODBCConnection_();

    /**
     * close a database connection using ODBC, <code>henv_</code> and <code>
     * hdbc_ are freed here
     */
    void closeODBCConnection_();
    void closeODBCConnection_(SQLHENV henv, SQLHDBC hdbc) const;

    /**
     * free an ODBC statment handle
     * @param hstmt the ODBC statement handle
     */
    void freeSQLStatement_(const SQLHSTMT& hstmt) const;


    //----------------------- Database query methods ------------------

    /**
     * perform a single sql insert statement directly (ie, no prepared 
     * statements
     * @param statement SQL statment to execute
     * @throws DBConnection exception
     */
    void directSQLInsert_(const std::string& statement) const;

    /**
     * execute a single SQL statement
     * @param hstmt the previously allocated ODBC statement
     * @param statement the SQL statement to execute
     * @return the result of the ODBC directExec function call
     */
    void directSQLExec_(const SQLHSTMT hstmt, const std::string& statement) 
        const;

    /**
     * convert an ODBC result set to a carma::dbms::Table object
     * @param resultSet the ODBC result set
     * @return the carma::dbms::Table object holding the @p resultSet
     */
    carma::dbms::Table odbcResultSetToTable_(const SQLHSTMT& hstmt) const;

    /**
     * execute a single SQL statement directly
     * @param statement the SQL statement to execute
     * @return the result of the ODBC directExec function call
     */
    void directSQLExec_(const std::string& statement) const;

    //---------------- query construction methods ---------------------
    /**
     * read the database user's password so that it can be sent to the ODBC
     * driver as part of the connect string
     * <B> IMPORTANT! Passwords are considered sensitive information and so
     *     the files in which they reside should be readable only by
     *     the unix account which owns them
     * </B>
     * @return the password
     */
     std::string readPassword_() const;

};

/**
 * @class DBConnectionFactory factory for creating a database connection via 
 *        the static createConnection() method
 */


 class DBConnectionFactory {
     public:
    /**
     * connect to the carma database.
     * clients must delete the returned DBConnection when they are 
     * done with it.  
     * The function uses the rdbms key supplied by the DBConfigurator oject
     * to determine which rdbms backend to connect to. If the dbconf==NULL
     * or the rdbms key in the non-null dbconf object isn't defined, this
     * method uses the value of carma::dbms::DEFAULT_RDBMS as the rdbms to
     * connect to. This method  passes the same 
     * DBConfigurator point on to the DBConnection constructor which uses
     * several other keys to configure the return DBConnection object
     * @param dbconf the DBConfigurator object from which to get various
     *        configuration parameters, if NULL use sensible defaults
     * @return pointer to a DBConnection object, <B> this pointer must be
     *         deleted by the client when the client is finished with the
     *         connection to avoid memory leaks!</B>
     * @throws DBConnectionException if a connection cannot be established
     * @see DBConnection
     */

    static DBConnection *createConnection(const DBConfigurator *dbconf=NULL);

 };



// DBConnectionException definition

/**
 * an exception indicating there is a problem communicating with
 * the database
 */
class DBConnectionException : public carma::util::ErrorException {
public:
    DBConnectionException (const char* msg, const char* fileName = __FILE__, 
                           const int lineNum = __LINE__) 
        : carma::util::ErrorException (msg, fileName, lineNum) {};

    DBConnectionException (const std::string msg, 
                           const char* fileName = __FILE__, 
                           const int lineNum = __LINE__) 
        : carma::util::ErrorException (msg.c_str(), fileName, lineNum) {};

};

/**
 * an exception indicating there is a problem executing an SQL statement
 * (more often than not indicating there is a syntax error in the SQL 
 * statement 
 */
class SQLException : public carma::util::ErrorException {
public:
    SQLException (const char* msg, const char* fileName = __FILE__, 
                  const int lineNum = __LINE__) 
        : carma::util::ErrorException (msg, fileName, lineNum) {};

    SQLException (const std::string msg, 
                  const char* fileName = __FILE__, 
                  const int lineNum = __LINE__) 
        : carma::util::ErrorException (msg.c_str(), fileName, lineNum) {};

};

class InsertDeniedException : public carma::util::ErrorException {
public:
    InsertDeniedException (const char* msg, const char* fileName = __FILE__, 
                  const int lineNum = __LINE__) 
        : carma::util::ErrorException (msg, fileName, lineNum) {};

    InsertDeniedException (const std::string msg, 
                  const char* fileName = __FILE__, 
                  const int lineNum = __LINE__) 
        : carma::util::ErrorException (msg.c_str(), fileName, lineNum) {};

};

}}


#endif // CARMA_DBMS_DBCONNECTION_H

