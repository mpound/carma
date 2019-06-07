/**
 * Implementation for the TableManager class
 *
 * @author: Dave Mehringer
 * @version $Id: TableManager.cc,v 1.30 2011/12/21 22:56:43 mpound Exp $
 *
 * $CarmaCopyright$
 *
 */
#include "carma/dbms/MonitorDataDatabase.h"
#include "carma/dbms/TableManager.h"
#include "carma/dbms/TableNames.h"
#include "carma/util/Trace.h"
#include "carma/util/Time.h"
#include "carma/util/Program.h"
//#include <glibtop/fsusage.h>
#include <sys/vfs.h>

using namespace std;
using namespace carma::dbms;

int TableManager::minimumFrameRetentionFrames_ = 20 * 24 * 60 * 60 * 2;
int TableManager::minimumCorrelRetentionFrames_ = 15 * 24 * 60 * 60 * 2;
int TableManager::minimumMinuteRetentionFrames_ = 180 * 24 * 60 * 60 * 2;

TableManager::TableManager(const bool& persistentConnection, 
                           DBConfigurator *dbconf) : dbc_(0) {
    persistentConnection_ = persistentConnection;
    // we have to gaurd against the caller deleting the dbconf pointer
    dbconf_ = &(*dbconf);
    loadConfigurationInformation();
}

TableManager::~TableManager() {
    closeDBConnection();
    //    carma::util::Trace::destroy();
}


void TableManager::loadConfigurationInformation() {
    // garuantee that partitions are in ascending ID order
    string stmt = "SELECT partitionID, partition, minFreeSpace FROM ";
    stmt += "Partitions ORDER BY partitionID";
    openDBConnectionIfNecessary();
    Table t = dbc_->execSQLSelect(stmt,"PARTITIONS");
    closeDBConnectionIfNecessary();
    Column<int> partitionIDs = t.getIntColumn("partitionID");
    Column<string> partitionNames = t.getStringColumn("partition");
    Column<int> minFreeSpace = t.getIntColumn("minFreeSpace");
    partitions_.clear();
    for(unsigned int i=0; i<t.rowCount(); i++) {
        Partition p;
        p.name = partitionNames[i];
        p.minFreeSpace=minFreeSpace[i];
        partitions_[partitionIDs[i]] = p;
    }
}

void TableManager::openDBConnectionIfNecessary() {
    if(dbc_ == NULL) {
        dbc_ = DBConnectionFactory::createConnection(dbconf_);
    }
}

void TableManager::closeDBConnectionIfNecessary() {
    if(!persistentConnection_) {
        closeDBConnection();
    }
}

void TableManager::closeDBConnection() {
    if(dbc_ != 0) {
        dbc_->closeConnection();
        delete dbc_;
        dbc_ = 0;
    }
}

int TableManager::getAvailablePartition(const int& requiredSpace, 
                                        const bool& connectionIsOpen) {
    // interrogate the filesystems to determine current usage
    map<int,int> partitionID2FreeMB;
    map<int,Partition>::iterator iter;
    string msg;
    ostringstream smsg;
    for(iter = partitions_.begin(); iter != partitions_.end(); iter++) {
        partitionID2FreeMB[iter->first] = getFreeMB((iter->second).name);
    }
    if(!connectionIsOpen) {
        openDBConnectionIfNecessary();
    }
    map<int,Partition> emptyPartitions = getEmptyPartitions(true);
    int i;
    for(iter = emptyPartitions.begin(); iter != emptyPartitions.end(); 
        iter++) {
        i = iter->first;
	int neededSpace  = partitions_[i].minFreeSpace + requiredSpace;
        if(partitionID2FreeMB[i] >= neededSpace ) {
            // partition with enough space found
            if(!connectionIsOpen) {
                closeDBConnectionIfNecessary();
            }
            return i;
        }
        smsg.str("");
        smsg << "Although partition " << i << " does not yet have any "
             << "tables, the amount of free space (" << partitionID2FreeMB[i] 
             << " MB) is not enough. There needs to be at least " 
             << (partitions_[i].minFreeSpace + requiredSpace) << "MB" ;
        CPTRACE(carma::util::Trace::TRACE6, smsg.str());
        partitionID2FreeMB.erase(i);
    }
    if(emptyPartitions.size() == 0) {
        CPTRACE(carma::util::Trace::TRACE6, "All partitions have tables");
    } else {
        smsg.str("");
        smsg << emptyPartitions.size() << " partition(s) don't have any "
             << "tables yet, but do not have enough free space to hold the "
             << "requested table";
        CPTRACE(carma::util::Trace::TRACE6, smsg.str());
    }
    if(emptyPartitions.size() == partitions_.size()) {
        smsg.str("");
        smsg << "All partitions specified in the " 
             << getTableName(PARTITIONS_TABLE) 
             << " table do not contain any monitor data tables. However, "
             << " none of them contain enough free space to create the "
             << "requested table. Disk space needs to be freed";
        CPTRACE(carma::util::Trace::TRACE1, smsg.str());
        throw CARMA_EXCEPTION(TableManagerException, smsg.str());
    }

    // no empty partitions with enough free space to hold the requested table, 
    // so proceed to partitions which contain tables
    // see if there are any partitions with enough free space
    vector<int> candidateIDs;
    map<int,int>::iterator mIter;
    for(mIter = partitionID2FreeMB.begin(); mIter != partitionID2FreeMB.end(); 
        mIter++) {
        i = mIter->first;
        smsg.str("");
        smsg << "Partition " << i << " free space " << partitionID2FreeMB[i] 
             << " needed " << " space " << (partitions_[i].minFreeSpace 
                                            + requiredSpace);
        CPTRACE(carma::util::Trace::TRACE6, smsg.str());
	int neededSpace  = partitions_[i].minFreeSpace + requiredSpace;
        if( partitionID2FreeMB[i] >= neededSpace ) {
            // parition with enough space found
            smsg.str("");
            smsg << "enough free space found on partition " << i 
                 << ". No tables were dropped";
            CPTRACE(carma::util::Trace::TRACE6, smsg.str());
            if(!connectionIsOpen) {
                closeDBConnectionIfNecessary();
            }
            return i;
        }
        candidateIDs.push_back(i);    
    }
    msg = "TableManager: No partitions with enough free space were found. ";
    msg += "One or more tables need to be dropped";
    CPTRACE(carma::util::Trace::TRACE6, msg);


    // no partitions with enough free space, so now we have to determine
    // which partitions have enough total (used+free) space to hold this table
    // partitions which do not are deleted from the list of candidates
    vector<int>::iterator vIter = candidateIDs.begin();
    while(vIter != candidateIDs.end()) {
      unsigned tmb = getTotalMB(partitions_[*vIter].name);
      unsigned needed = partitions_[*vIter].minFreeSpace + requiredSpace; 
        if( tmb < needed) {
            candidateIDs.erase(vIter);
        } else {
            vIter++;
        }
    }
    if(candidateIDs.size() == 0) {
        // no partitions with enough total space to hold the requested table
        msg = "no partitions with enough total space to hold the requested ";
        msg += "table";
        CPTRACE(carma::util::Trace::TRACE1, msg);
        if(!connectionIsOpen) {
            closeDBConnectionIfNecessary();
        }
        throw CARMA_EXCEPTION(TableManagerException, msg);
    }

    // the site database and the archive (NCSA) database require different
    // algorithms for table removal.  At the site, the table with containing
    // the earliest integrations must be removed first, even if multiple
    // tables which sit on different partitions must be removed.
    ostringstream stmt;
    //FIXME this will need to be fixed for subarray integrations so that
    //that the "minIntegration" or equivalent is referenced in terms of 
    // frameCounts
    stmt << "SELECT tableName,minIntegration,partitionID FROM " 
         << getTableName(MONITOR_INDEX_TABLE) 
         << " WHERE minIntegration is not NULL "
         << "ORDER BY minIntegration ASC";         
    CPTRACE(carma::util::Trace::TRACE5, stmt.str());
    Table t = dbc_->execSQLSelect(stmt.str(),
                                  "Monitor Tables - Oldest Data to Newest");
    //vector<string> tableList = t.getStringColumn("tableName").getData();
    Column<string> tableList = t.getStringColumn("tableName");
    //vector<int> partitionList = t.getIntColumn("partitionID").getData();
    Column<int> partitionList = t.getIntColumn("partitionID");
    Column<int> frameList = t.getIntColumn("minIntegration");

#if 0
    // Delete oldest tables first.
    for(unsigned int i=0; i<tableList.size(); i++) {
        dropTable(tableList[i]);
	// If we have to delete, delete alot to avoid doing it again soon.
        if(getFreeMB(partitions_[partitionList[i]].name) 
           >= (partitions_[partitionList[i]].minFreeSpace + requiredSpace*100)) {
            CPTRACE(carma::util::Trace::TRACE5, "New table should be written "
                    << "to partition with ID " << partitionList[i]);
            return partitionList[i];
        }
    }
#else
    int currentFrame = static_cast<int>(carma::util::Time::computeClosestFrame());
    for(unsigned int i=0; i < tableList.size(); i++) {
      bool isFrame = (tableList[i].compare(0, 5, "Frame") == 0);
      bool isCorrel = (tableList[i].compare(2, 6, "Correl") == 0);
      bool isMinute = (tableList[i].compare(0, 6, "Minute") == 0);
      if((isFrame  && ((currentFrame - frameList[i]) > minimumFrameRetentionFrames_))  ||
	 (isCorrel && ((currentFrame - frameList[i]) > minimumCorrelRetentionFrames_)) ||
	 (isMinute && ((currentFrame - frameList[i]) > minimumMinuteRetentionFrames_))){
	dropTable(tableList[i]);
      }
      if(getFreeMB(partitions_[partitionList[i]].name) 
	 >= (partitions_[partitionList[i]].minFreeSpace + requiredSpace*100)){
	CPTRACE(carma::util::Trace::TRACE5, "New table should be written "
		<< "to partition with ID " << partitionList[i]);
	return partitionList[i];
      }

    }
    // Preferentially delete Frame (1/2 sec) tables over others.
    // Delete others only if all Frame tables are gone, 
    // (First pass - Frame. Second pass - others).
/*
    for(int npass=0; npass<2; npass++) {
      for(unsigned int i=0; i<tableList.size(); i++) {
	bool isFrame = (tableList[i].compare(0, 5, "Frame") == 0);
	if((!isFrame && (npass==0))||(isFrame&&(npass!=0)))
	  continue;
        dropTable(tableList[i]);
	// If we have to delete, delete alot to avoid doing it again soon.
        if(getFreeMB(partitions_[partitionList[i]].name) 
           >= (partitions_[partitionList[i]].minFreeSpace + requiredSpace*100)) {
            CPTRACE(carma::util::Trace::TRACE5, "New table should be written "
                    << "to partition with ID " << partitionList[i]);
            return partitionList[i];
        }
      }
    }
*/
#endif

    msg = "All known monitor tables have been removed, ";
    msg += "but there is still not enough space for the requested table. ";
    msg += "This indicates that there are other files on the monitor data ";
    msg += "partitions which are not associated with monitor data tables";
    CPTRACE(carma::util::Trace::TRACE1, msg);
    if(!connectionIsOpen) {
        closeDBConnectionIfNecessary();
    }
    throw CARMA_EXCEPTION(TableManagerException, msg);

        
    // FIXME
    // #ifdef..#endif will not be used in the final version, I'm just using
    // it here to make sure this code isn't built.  The NCSA archive algorithm
    // still needs to be developed
#ifdef CARMA_ARCHIVE

    // now find the candidate with the oldest table and remove tables from
    // that partition

    stmt << "SELECT creationFrame, partitionID FROM " 
         << getTableName(MONITOR_INDEX_TABLE)
         << " WHERE partitionID IN (";         
    int nCandidates = candidateIDs.size();
    for(int i=0; i<nCandidates; i++) {
        stmt << candidateIDs[i];
        if(i < (nCandidates-1)) {
            stmt << ", ";
        }
    }
    stmt << ") GROUP BY partitionID ORDER BY creationFrame ASC";
    CPTRACE(carma::util::Trace::TRACE5, stmt.str());

    Table t = dbc_->execSQLSelect(stmt.str(),"Partition Candidates");
    if(t.rowCount() == 0) {
        // no entries
        if(!connectionIsOpen) {
            closeDBConnectionIfNecessary();
        }
        return -1;
    }
    vector<int> partitionsWithTablesIDs = t.getIntColumn("partitionID")
        .getData();
    vector<string> tableList;
    i=0;
    bool found = false;
    int targetID;
    CPTRACE(carma::util::Trace::TRACE6, "number of partitions with "
            << "tables " << partitionsWithTablesIDs.size());
    while(!found && i < partitionsWithTablesIDs.size()) {
        // need to do this in a loop since it is possible than even after 
        // deleting all tables on a single partition there will still not be 
        // enough space for the table
        targetID = partitionsWithTablesIDs[i];
        stmt.str("");
        stmt << "SELECT tableName FROM " << getTableName(MONITOR_INDEX_TABLE) 
             << " WHERE "
             << "partitionID=" << targetID << " ORDER BY creationFrame";
        t = dbc_->execSQLSelect(stmt.str());
        tableList = t.getStringColumn("tableName").getData();
        int j = 0;
        while(!found && j < tableList.size()) {
            dropTable(tableList[j]);
            if(getFreeMB(partitions_[targetID].name) 
               >= (partitions_[targetID].minFreeSpace + requiredSpace)) {
                found = true;
            }
            j++;
        }
        i++;
    }
    if(!connectionIsOpen) {
        closeDBConnectionIfNecessary();
    }
    if(found) {
        return targetID;
    } else {
        // no space anywhere!
        //FIXME throw exception
        return -1;
    }
#endif // CARMA_ARCHIVE
}

int TableManager::createMonitorDataTable(
                                  const MonitorAverageType& averageType,
                                  const MonitorAggregateType& aggType,
                                  const unsigned& requiredSpace, 
                                  string& tableName, const string& tag)
{
    openDBConnectionIfNecessary();
    int partitionID;
    int retVal;
    try {
        partitionID = getAvailablePartition(requiredSpace, true);
        if(partitionID < 0) {
            return partitionID;
        }
        dbc_->beginTransaction();
        tableName = dbc_->createMonitorDataTable(averageType, aggType, 
                                               tag,
                                               &partitions_[partitionID].name);
        CPTRACE(carma::util::Trace::TRACE6, "Table " << tableName
                    << " created ");
        MonitorDataDatabase mddb(dbc_);
        mddb.insertIntoMonitorIndexTable
            (tableName, partitionID, averageType, aggType, 
             carma::util::Time::computeCurrentFrame());
        dbc_->commitTransaction();
        retVal = partitionID;
    } catch(const DBConnectionException & exc) {
        exc.logException(log4cpp::Priority::WARN);
        retVal = -1;
    }
    closeDBConnectionIfNecessary();
    return retVal;
}

    
map<int, TableManager::Partition> TableManager::getEmptyPartitions(
                                               const bool& connectionIsOpen) {
    string stmt = "SELECT partitionID FROM " 
        + getTableName(MONITOR_INDEX_TABLE) 
        + " GROUP BY partitionID ORDER BY partitionID";
    if(!connectionIsOpen) {
        openDBConnectionIfNecessary();
    }
    Table t = dbc_->execSQLSelect(stmt);
    if(!connectionIsOpen) {
        closeDBConnectionIfNecessary();
    }

    map<int,Partition> emptyPartitions;
    map<int,Partition>::iterator iter;
    if(t.rowCount() == 0) {
        // no partitions have tables yet
        return partitions_;
    }
    //vector<int> occupiedPartitionIDs = t.getIntColumn("partitionID").getData();
    Column<int> occupiedPartitionIDs = t.getIntColumn("partitionID");
    for(iter = partitions_.begin(); iter != partitions_.end(); iter++) {
        bool found = false;
        for(unsigned int j=0 ; j < occupiedPartitionIDs.size(); j++) {
            found = (iter->first == occupiedPartitionIDs[j]);
            if(found) {
                break;
            }
        }
        if(!found) {
            emptyPartitions[iter->first] = iter->second;
        }
    }
    return emptyPartitions;
}


unsigned TableManager::getFreeMB(const string& partition) const {
    /*
    const int blocksPerMB = 2097;
    glibtop_fsusage *fsusage = new glibtop_fsusage;
    glibtop_get_fsusage(fsusage, partition.c_str());
    int bavail = fsusage->bavail;
    return bavail/blocksPerMB;
    */
    // now using statfs(2)
    struct statfs *statbuf = new struct statfs;
    if(statfs(partition.c_str(),statbuf) != 0) {
        string s = "statfs of directory " + partition + " failed";
        CPTRACE(carma::util::Trace::TRACE1, s);
        delete statbuf;
        throw CARMA_EXCEPTION(TableManagerException, s);
    }
    // available space in MB
    double bavail = statbuf->f_bavail;
    double bsize = statbuf->f_bsize;
    // multiplying the first two factors can exceed the max of a 4 byte int
    double availMB = (bavail)*(bsize)/1024/1000;
    CPTRACE(carma::util::Trace::TRACE6, "Available MB on " << partition
            << "=" << availMB);
    delete statbuf;
    return static_cast< unsigned >( availMB );
}

unsigned TableManager::getTotalMB(const string& partition) const {
    // get total (free + used) partition size in MB
    //const int blocksPerMB = 2097;
    /*
    glibtop_fsusage *fsusage = new glibtop_fsusage;
    glibtop_get_fsusage(fsusage, partition.c_str());
    int blocks = fsusage->blocks;
    return blocks/blocksPerMB;
    */
    // now using statfs(2)
    struct statfs *statbuf = new struct statfs;
    if(statfs(partition.c_str(),statbuf)) {
        string s = "Unable to statfs " + partition;
        CPTRACE(carma::util::Trace::TRACE1, s);
        delete statbuf;
        throw CARMA_EXCEPTION(TableManagerException, s);
    }
    // total partition space in MB
    double blocks = statbuf->f_blocks;
    double bsize = statbuf->f_bsize;
    // multiplying the first two factors can exceed the max of a 4 byte int
    double totalMB = (blocks*bsize)/1024/1000;
    CPTRACE(carma::util::Trace::TRACE6, "Total MB on " << partition
            << "=" << totalMB);
    delete statbuf;
    return static_cast< unsigned>( totalMB );
}

void TableManager::updateMonitorIndexTable(const std::string& tableName, 
                                           const MonitorAverageType& 
                                           averageType) {
    openDBConnectionIfNecessary();
    MonitorDataDatabase mddb(dbc_);
    mddb.updateMonitorIndexTable(tableName, averageType);
    closeDBConnectionIfNecessary();
}    

void TableManager::dropTable(const string& tableName) const {
    CPTRACE(carma::util::Trace::TRACE6, "TableManager: Dropping table "
            << tableName);
    try {
        dbc_->beginTransaction();
        CPTRACE(carma::util::Trace::TRACE6, "Transaction begun");
        try {
            dbc_->execSQLDropTable(tableName);
            CPTRACE(carma::util::Trace::TRACE6, "Table " << tableName 
                    << " successfully dropped");
        } catch (const SQLException & exc) {
            // an SQLException caught here indicates that the table
            // doesn't exist on disk, so just delete it from
            // MONITOR_INDEX_TABLE and move on
            CPTRACE(carma::util::Trace::TRACE6, "SQLException "
                    << "caught when trying to drop table " << tableName);
            exc.report();
        }
        dbc_->deleteFromMonitorIndexTable("WHERE tableName='" 
                                          + tableName + "'");
        CPTRACE(carma::util::Trace::TRACE6, tableName 
                << " deleted from " << getTableName(MONITOR_INDEX_TABLE));
	{   log4cpp::Category & logger = carma::util::Program::getLogger();
	string msg = "Dropped table " + tableName;
	logger << log4cpp::Priority::NOTICE << msg;
	}
        dbc_->commitTransaction();
        CPTRACE(carma::util::Trace::TRACE6, "Transaction committed");
    } catch (const DBConnectionException & exc) {
        exc.logException(log4cpp::Priority::WARN);
        dbc_->rollBackTransaction();
        CPTRACE(carma::util::Trace::TRACE6, "Transaction rolled back");
    }
}
