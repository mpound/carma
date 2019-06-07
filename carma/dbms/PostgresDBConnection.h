#ifndef CARMA_DBMS_POSTGRESDBCONNECTION_H
#define CARMA_DBMS_POSTGRESDBCONNECTION_H


/**
 * @file
 *
 * PostgresDBConnection class
 *
 * @author: Dave Mehringer
 *
 * $CarmaCopyright$
 *
 */

#include <iostream>
#include "postgresql/libpq-fe.h"
#include "carma/dbms/DBConnection.h"


namespace carma {
namespace dbms {


/**
 * an DBConnection implementation for PostgreSQL
 */
class PostgresDBConnection : public DBConnection {

public:
    static const std::string RDBMS;
    static const std::string ODBC_DATASRC;
    static const std::string TEST_ODBC_DATASRC;

    /**
     * constructor
     */
    PostgresDBConnection(const bool& useProductionDB, 
                         const string* const odbcini);

    /**
     * destructor
     */
    //~PostgresDBConnection();

    /**
     * return true if the DB is up and available for connections
     * this overrides the method in the base class and uses a native
     * connection implementation because for postgres, the ODBCDriverConnect()
     * method hangs if the database cannot be contacted
     */
    bool isDBUp() const;


    void loadDataFromFile(const std::string& filename, 
                          const std::string& table, const std::string& user,
                          const bool& useTransactions,
                          const bool& useProductionDB) const;

    void loadDataFromFiles(const file2TableMap& file2Table, 
                           const std::string& user, 
                           const bool& useTransactions,
                           const bool& useProductionDB) const;


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
                                    const monitorTableAverageType& averageType,
                                    const monitorTableDataType dataType,
                                    const std::string& tag, 
                                    const std::string *const location) const;


};
}}
#endif // CARMA_DBMS_POSTGRESDBCONNECTION_H

