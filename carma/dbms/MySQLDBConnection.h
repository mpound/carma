#ifndef CARMA_DBMS_MYSQLDBCONNECTION_H
#define CARMA_DBMS_MYSQLDBCONNECTION_H
/**
 * @file
 *
 * MySQLDBConnection class
 *
 * @author: Dave Mehringer
 *
 * $CarmaCopyright$
 *
 */

#include <iostream>
// putting #include "mysql/mysql.h" before #include "carma/dbms/DBConnection.h"
// causes build failures when using mysql 4.1, but not when using 4.0
#include "mysql/mysql_version.h"
#include "carma/dbms/DBConnection.h"
#include "mysql/mysql.h"

namespace carma {
namespace dbms {

/**
 * an DBConnection implementation for MySQL
 */
class MySQLDBConnection : public DBConnection {

public:

    static const std::string DBUSER;
    static const std::string RDBMS;
    static const std::string ODBC_DATASRC;
    static const std::string TEST_ODBC_DATASRC;

    /**
     * constructor
     */
    MySQLDBConnection(const DBConfigurator *dbconf);

    /**
     * destructor
     */
    //~MySQLDBConnection();


    void loadDataFromFile
        (const std::string& filename, const std::string& table,
         const bool& useTransactions, const int& linesToIgnore=0,
         const std::string& columnDelimiter="\t") const;


    void loadDataFromFiles
        (const file2TableMap& file2Table, const bool& useTransactions,
        const int& linesToIgnore=0, const std::string& columnDelimiter="\t") 
        const;

    void beginTransaction() const;

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
                                    const std::string *const location) const;

    /**
     * performs tasks after a monitor data table has been completely populated.
     * the input table is altered by adding an index on tagID and then
     * a call to the base method of the same name is made, 
     * @param tableName name of the table that has been populated
     * @param averageType the average type of the table
     * @throws DBConnectionException
     */
    virtual void monitorDataTableHasBeenPopulated(const std::string& tableName,
                                  const MonitorAverageType& averageType) 
        const;

    /**
     * the maximum number of tables the RDBMS allows in a join
     * @return the maximum number of tables which can be joined in a SELECT
     *         statement
     */
    virtual unsigned maxTablesPerJoin() const;

    /**
     * the maximum number of columns the RDBMS allows in a table
     * @return the maximum number of columns permitted in a table
     */
    virtual unsigned maxColumnsPerTable() const;

    /**
     * get the modifier necessary for a case-sensitive text search
     * @return the modifier necessary for a case-sensitive text search
     */
    std::string virtual caseSensitiveSearchModifier() const { 
        return "BINARY "; 
    }

    /**
     * attempt to repair a corrupt monitor data table
     * @param tableName the table name to repair
     */
    virtual void repairMonitorDataTable(const std::string& tableName) const;



private:
    // MYSQL *sock_, mysql_;
};


}}
#endif // CARMA_DBMS_MYSQLDBCONNECTION_H
