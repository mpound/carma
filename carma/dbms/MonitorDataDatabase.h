#ifndef CARMA_DBMS_MONITORDATADATABASE_H
#define CARMA_DBMS_MONITORDATADATABASE_H

/**
 * @file
 * MonitorDataDatabase
 *
 * @author: Dave Mehringer
 * @version $Id: MonitorDataDatabase.h,v 1.6 2008/04/23 21:42:04 abeard Exp $
 *
 * $CarmaCopyright$
 *
 */

#include <map>
#include "carma/dbms/MonitorSystemAndDBMSRelationships.h"
#include "carma/util/Logger.h"
#include "carma/util/Time.h"

namespace carma {
namespace dbms {

    class DBConnection;
    class Table;
    class TimeRangeFilter;

/**
 * This class contains methods for accessing monitor configuration information
 */
class MonitorDataDatabase {

public:
    /**
     * constructor
     */
    MonitorDataDatabase(const carma::dbms::DBConnection* const dbc);

    virtual ~MonitorDataDatabase( );

    //carma::dbms::Table getMonitorDataTableIndexTable();
    
    /**
     * get entries in the MonitorDataIndexTable corresponding to the supplied
     * filters
     * @param trFilter the time range filter to use
     * @param aggType the aggregate data type
     * @param includeNullTimes include entries which have NULLs for 
     *        min/maxIntegration? Such entries usually indicate that the 
     *        tables are in the process of being written.
     * @return a Table with the specified entries
     */
    carma::dbms::Table getMonitorDataIndexTableEntries
        (const carma::dbms::TimeRangeFilter& trFilter, 
         const carma::dbms::MonitorAggregateType& aggType, 
         const bool includeNullTimes=true) const;

    /**
     * insert a row into the monitor data index table
     * The SQL statement that is executed is 
     * "INSERT INTO " + MONITOR_INDEX_TABLE + " (tableName, creationFrame, "
     * + "partitionID) VALUES('" + tableName + "', " + creationFrame + ", " 
     * + partitionID + ")"
     *
     * @param tableName the name of the table to be added
     * @param paritionID the id of the partition on which the table is located
     * @param creationFrame the frame at which the table was created
     * @throws DBConnectionException
     */
    virtual void insertIntoMonitorIndexTable
        (const std::string& tableName, const int& partitionID,
         const MonitorAverageType& averageType,
         const MonitorAggregateType& aggType,
         const carma::util::frameType& creationFrame) const;

    /*
     * get the base name of the specified table type
     * @param averageType the average type for which the base name is desired
     * @param aggType the aggreate type for which the base name is desired
     * @return the base name of the table
     */
    static std::string getMonitorDataTableBaseName
        (const MonitorAverageType& averageType,
         const MonitorAggregateType aggType);

    /**
     * construct portions of the statement used to create a monitor table
     * the basic creation statement is "CREATE TABLE " + tableName + "("
     * + columnClause + pkClause + ")"
     * @param [in] averageType the monitor table average type
     * @param [in] dataType the monitor table data type
     * @param [in] tag tag to add to the base monitor table name
     * @param [out] tableName the table name
     * @param [out] columnClause the columnClause 
     * @param [out] pkClause the primary key cluase
     */
    static void getCreateMonitorTableStatementParts
        (const MonitorAverageType& averageType, 
         const MonitorAggregateType& aggType, const std::string& tag, 
         std::string& tableName, std::string& columnClause,
         std::string& pkClause);

    /**
     * insert the max and min frame counts into the specified row in the
     * monitor index table.  Generally called after data has been copletely
     * loaded into a table
     * @param tableName update the data for this table
     * @throws DBConnectionException
     */
    void updateMonitorIndexTable
        (const std::string& tableName, const MonitorAverageType& averageType) 
        const;

    /**
     * insert the min/max frameCounts/Integrations into the monitor index
     * table for all tables with NULLs currently in these columns with
     * creationFrames > minAge frames ago
     * @param the minimum age (in half-second frames) a table has to have 
     * for it to be updated
     */
    void updateMonitorIndexTable (const int& minAge) const;

    /**
     * get the canonical file system path to the specified monitor data table
     * @param tableName the table name for which to get the system path
     * @return the canonical (complete path) table name
     */
    std::string getCanonicalSystemName(const std::string& tableName) const;


private:
    const DBConnection *dbc_;
    log4cpp::Category& logger_;

    /**
     * disable default constructor
     */
    MonitorDataDatabase();

};
}}

#endif // CARMA_DBMS_MONITORDATADATABASE_H
