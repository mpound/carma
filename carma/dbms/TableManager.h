#ifndef CARMA_DBMS_TABLEMANAGER_H
#define CARMA_DBMS_TABLEMANAGER_H

/**
 * @file
 * TableManager class
 *
 * @author: Dave Mehringer
 * @version $Id: TableManager.h,v 1.16 2011/12/21 22:56:43 mpound Exp $
 *
 * $CarmaCopyright$
 *
 */
#include "carma/dbms/DBConnection.h"
//#include "carma/util/Trace.h"
#include <vector>

namespace carma {
namespace dbms {


/**
 * This class is responsible for managing the large, short lived tables
 * produced by CARMA.  Examples of such tables are the monitor data tables
 * and the log tables.  All requests to create new tables therefore must
 * be submitted via this class.  An object of this class maintains a map
 * of partitions where tables can be located.  Once partitions fill up,
 * this class will drop old tables as it receives create table requests.
 * @todo it would be nice to have paritition/minimum space info in a config
 * file, rather than hardcoded in the header file, which is read as necessary 
 * (ie, without having to restart the app should this file be updated).
 */

class TableManager {


public:
  struct Partition {
      std::string name;
      unsigned minFreeSpace;
  };

  /**
   * constructor
   * @param persistentConnection maintain a persistent connection to the db
   *        for the life of this object?
   * @param dbconf DBConfigurator object used to configure the DBConnection
   *        objects this class creates.  To guard against the caller deleting
   *        this pointer after a TableManager has been created, this 
   *        constructor creates a copy of the dbconf object which is pointed
   *        to
   * @throws DBConnectionException
   */ 
  TableManager(const bool& persistentConnection=true, DBConfigurator 
               *dbconf = NULL);

  virtual ~TableManager();
                     
  /**
   * (re)load config info regarding partitions from the db
   * this is called by the constructor and can be called periodically after
   * construction to check for configuration changes
   * @throws DBConnectionException
   */
  void loadConfigurationInformation();

  /**
   * create a monitor data table table
   * @param averageType   [in] averageType the average type of the table
   * @param aggType       [in] dataType the type of data that will be loaded into the table
   * @param requiredSpace [in] requiredSpace the space required for the table, in MB
   * @param tableName    [out] tableName the name of the created table which will have
   *              the tag parameter at the end
   * @param tag           [in] tag string to append to the end of the table base name to
   *             assure that the table name is unique
   * @return the partition ID of the partition on which the table was created
   *         or < 0 if the table couldn't be created
   *         
   * @throws DBConnectionException
   */
  virtual int createMonitorDataTable(
                       const carma::dbms::MonitorAverageType& averageType,
                       const carma::dbms::MonitorAggregateType& aggType,
                       const unsigned& requiredSpace, 
                       std::string& tableName, const std::string& tag);



  /**
   * insert the max and min frame counts into the specified row in the
   * volatile tables table.  Generally called after data has been copletely
   * loaded into a table
   * @param tableName update the data for this table
   * @throws DBConnectionException
   */
  void updateMonitorIndexTable(const std::string& tableName,
                               const MonitorAverageType& averageType);


protected:
  bool persistentConnection_;
  bool useProductionDB_;
  std::string rdbms_;
  carma::dbms::DBConnection *dbc_;
  std::map<int,Partition> partitions_;
  DBConfigurator *dbconf_;

  static int minimumFrameRetentionFrames_;
  static int minimumCorrelRetentionFrames_;
  static int minimumMinuteRetentionFrames_;

  //carma::util::Trace *trace_;

  /**
   * open a connection to the database if one is not currently established
   * @throws DBConnectionException
   */
  void openDBConnectionIfNecessary();

  /**
   * close the db connection and delete the db connection object if
   * !persistentConnection_
   */
  void closeDBConnectionIfNecessary();

  /**
   * close the DBConnection
   */
  void closeDBConnection();

  /**
   * find a partition on which a new table can be created.  This method will
   * delete tables to free up the specified amount of disk space if necessary.
   * The target partition will be the partition with the oldest table which 
   * contains at least the required amount of space when empty
   * @param requiredSpace the amount of space (in MB) above the minimum
   *        allowed space that must be made available
   * @param connectionIsOpen the caller (a method from this class) has already
   *        opened the connection to the database
   * @return partition ID for the partition on which the table should be
   *         created 
   * @throw TableManagerException if no available partition can be found
   */
  virtual int getAvailablePartition(const int& requiredSpace, 
                                    const bool& connectionIsOpen);

  /**
   * get the partitions which do not have any volatile tables on them
   * This method queries the database to determine this; it does not 
   * interrogate the filesystem
   * the returned map has partition IDs as its keys and Partition objects
   * as its values
   * @param connectionIsOpen the caller (a method from this class) has already
   *        opened the connection to the database
   * @return map of partitionIDs -> partition objects for partitions with
   *         no volatile tables on them
   */
  std::map<int, Partition> getEmptyPartitions(const bool& connectionIsOpen);

  /**
   * get the available space (for normal users) in MB of the specified 
   * partition
   * @param partition the partition for which to get the available space
   * @return available space, in MB
   */
  unsigned getFreeMB(const std::string& partition) const;

  /**
   * get the total (used+free) size, in megabytes, of the specified partition
   * @param partition the partition for which to get the total size
   * @return the total partition size in megabytes
   */
  unsigned getTotalMB(const std::string& partition) const;


  /**
   * removes a table from the db and removes its entry in MONITOR_INDEX_TABLE
   * Callers from this class must already have opened a DBConnection
   * @param tableName the table to be removed
   * @throws DBConnectionException
   */

  void dropTable(const std::string& tableName) const;


}; 

class TableManagerException : public carma::util::ErrorException {
public:
    TableManagerException (const char* msg, const char* fileName = __FILE__, 
                           const int lineNum = __LINE__) 
        : carma::util::ErrorException (msg, fileName, lineNum) {};

    TableManagerException (const std::string msg, 
                           const char* fileName = __FILE__, 
                           const int lineNum = __LINE__) 
        : carma::util::ErrorException (msg.c_str(), fileName, lineNum) {};

};




}}
#endif
