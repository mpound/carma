/**
 * MonitorDataDatabase implementation
 *
 * @author: Dave Mehringer
 * @version $Id: MonitorDataDatabase.cc,v 1.16 2011/12/21 22:56:43 mpound Exp $
 *
 * $CarmaCopyright$
 *
 */
#include "carma/dbms/ColumnNames.h"
#include "carma/dbms/DBConnection.h"
#include "carma/dbms/MonitorDataDatabase.h"
#include "carma/dbms/MonitorSystemAndDBMSRelationships.h"
#include "carma/dbms/TableNames.h"
#include "carma/dbms/filter/TimeFilter.h"
#include "carma/dbms/filter/TimeRangeFilter.h"
#include "carma/util/CommonExceptions.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"

using namespace std;
using namespace carma::dbms;


MonitorDataDatabase::MonitorDataDatabase
    (const DBConnection * const dbc) 
        : logger_(carma::util::Program::getLogger()) {
    if(dbc == NULL) {
        string emsg = "NULL DBConnection pointer not allowed in ";
        emsg += "MonitorDataDatabase constructor";
        throw CARMA_EXCEPTION
            (carma::util::IllegalArgumentException, emsg);
    }
    dbc_ = dbc;
}

// private constructor
MonitorDataDatabase::MonitorDataDatabase() 
    : logger_(carma::util::Program::getLogger()) {}

MonitorDataDatabase::~MonitorDataDatabase() { }

/*
carma::dbms::Table MonitorDataDatabase::getMonitorDataTableIndexTable() {
        try {
        return dbc_->execSQLSelect("SELECT * from " 
                                   + getTableName(MONITOR_INDEX_TABLE));
    } catch (const SQLException& exc) {
        ostringstream emsg;
        emsg << "MonitorConfigurationDatabase QUERY BUG!: " << exc.what();
        throw CARMA_ERROR(emsg.str());
    }
}
*/

Table MonitorDataDatabase::getMonitorDataIndexTableEntries
    (const TimeRangeFilter& trFilter, const MonitorAggregateType& aggType,
     const bool includeNullTimes) 
    const {
    vector<const TimeFilter * > trChildren = trFilter.getChildren();
    assert (trChildren.size() == 2);
    ostringstream statement;
    string nullTimeFilter;
    if(includeNullTimes) {
        nullTimeFilter = " OR minIntegration IS NULL OR maxIntegration ";
        nullTimeFilter += "IS NULL";
    }
    statement << "SELECT * "
              << "FROM " << getTableName(MONITOR_INDEX_TABLE) 
              << " WHERE averageType=" 
              <<         averageTypeToDB(trFilter.getAverageType())
              <<         " AND ((minIntegration<=" 
              <<               trChildren[1]->getValue() 
              <<         " AND maxIntegration>=" << trChildren[0]->getValue()
              <<         ") " << nullTimeFilter << ") AND aggregateType=" 
              <<         aggregateTypeToDB(aggType);
    try {
        return dbc_->execSQLSelect(statement.str());
    } catch (const SQLException& exc) {
        ostringstream emsg;
        emsg << "MonitorDataDatabase QUERY BUG!: " << exc.what();
        throw CARMA_ERROR(emsg.str());
    }
}

void MonitorDataDatabase::insertIntoMonitorIndexTable
    (const string& tableName, const int& partitionID, 
     const MonitorAverageType& averageType,const MonitorAggregateType& aggType,
     const carma::util::frameType& creationFrame) const {
    ostringstream stmt;
    stmt << "INSERT INTO " << getTableName(MONITOR_INDEX_TABLE) 
         << " (tableName, averageType, "
         << "aggregateType, creationFrame, partitionID) VALUES('" << tableName 
         << "', " << averageTypeToDB(averageType) << "," 
         << aggregateTypeToDB(aggType) << "," << creationFrame 
         << ", " << partitionID << ")";
    dbc_->directSQLExec_(stmt.str());
}

string MonitorDataDatabase::getMonitorDataTableBaseName
    (const MonitorAverageType& averageType, const MonitorAggregateType aggType)
{
    return toString(averageType) + toString(aggType) + "MonitorData";
}

void MonitorDataDatabase::getCreateMonitorTableStatementParts
    (const MonitorAverageType& averageType,
     const MonitorAggregateType& aggType, const string& tag, string& tableName,
     string& columnClause, string& pkClause) {
    vector<string> columnDescriptions;
    vector<string> pKeys;
    if(averageType == FRAME_AVG || averageType == MINUTE_AVG) {
        columnDescriptions.push_back("frameCount INT");
    } else {
        columnDescriptions.push_back("integrationID INT");
    }
    columnDescriptions.push_back("tagID INT");
    columnDescriptions.push_back(getColumnName(COLUMN_BLANKINGFLAGID) 
                                 + " SMALLINT");
    columnDescriptions.push_back(getColumnName(COLUMN_VALIDITYID) 
                                 + " SMALLINT");
    pKeys.push_back("frameCount");
    pKeys.push_back("tagID");
    switch(aggType) {
    case STRING_TYPE: {
        if(averageType == FRAME_AVG) { 
            columnDescriptions.push_back("value VARCHAR(255)");
        } else {
            columnDescriptions.push_back("integratedValue VARCHAR(255)");
        }
        break;
    }
    case NUMERIC_TYPE: {
        if(averageType == FRAME_AVG) { 
            columnDescriptions.push_back("value DOUBLE PRECISION");
        } else {
            columnDescriptions.push_back("integratedValue DOUBLE PRECISION");
            columnDescriptions.push_back("max DOUBLE PRECISION");
            columnDescriptions.push_back("min DOUBLE PRECISION");
        }
        break;
    }
    case SHORT_TYPE: {
        if(averageType == FRAME_AVG) { 
            columnDescriptions.push_back("value SMALLINT");
        } else {
            columnDescriptions.push_back("integratedValue SMALLINT");
            columnDescriptions.push_back("max SMALLINT");
            columnDescriptions.push_back("min SMALLINT");
        }
        break;
    }
    case COMPLEX_TYPE: {
        if(averageType == FRAME_AVG) { 
            columnDescriptions.push_back("realpart DOUBLE PRECISION");
            columnDescriptions.push_back("imagpart DOUBLE PRECISION");
        } else {
            columnDescriptions.push_back(
                                       "integratedRealPart DOUBLE PRECISION");
            columnDescriptions.push_back(
                                       "integratedImagPart DOUBLE PRECISION");
            columnDescriptions.push_back("maxRealPart DOUBLE PRECISION");
            columnDescriptions.push_back("maxImagPart DOUBLE PRECISION");
            columnDescriptions.push_back("minRealPart DOUBLE PRECISION");
            columnDescriptions.push_back("minImagPart DOUBLE PRECISION");
        }
        break;
    }
    case MAX_AGGREGATE_DATA_TYPE :
		   // Do nothing here as it is just an end of enum 
		   // marker.  This case is to quiet a compiler
		   // warning.
	break;
    }
    if(aggType != STRING_TYPE) {
        pKeys.push_back("iSample");
        if(aggType != STRING_TYPE) {
            columnDescriptions.push_back("iSample SMALLINT");
        }
    }
    if(averageType != FRAME_AVG) {
        columnDescriptions.push_back("nValidSamples SMALLINT");
        columnDescriptions.push_back("nTotalSamples SMALLINT");
    }
    columnClause = "";
    unsigned short nColumns = columnDescriptions.size();
    for(unsigned short i = 0; i < nColumns ; i++) {
        columnClause += columnDescriptions[i];
        if(i < (nColumns - 1)) {
            columnClause += ", ";
        }
    }
    unsigned short nKeys = pKeys.size();
    pkClause = "";
    if(nKeys > 0) {
        pkClause += ", PRIMARY KEY (";
        for (unsigned i=0; i<nKeys; i++) {
            pkClause += pKeys[i];
	    unsigned end = nKeys - 1;
            if( i < end ) {
                pkClause += ", ";
            }
        }
        pkClause += ")";
    }
    tableName = getMonitorDataTableBaseName(averageType,aggType) 
        + "_" + tag;
}

void MonitorDataDatabase::updateMonitorIndexTable (const int& minAge) const {
    int maxCreationFrame = carma::util::Time::computeCurrentFrame() - minAge;
    ostringstream stmt;
    stmt << "SELECT tableName, averageType FROM " 
         << getTableName(MONITOR_INDEX_TABLE)
         << " WHERE (maxIntegration IS NULL OR minIntegration IS NULL) AND "
         << "creationFrame<=" << maxCreationFrame;
    Table t = dbc_->execSQLSelect(stmt.str());
    CARMA_CPTRACE(carma::util::Trace::TRACE5, stmt.str());

    for(unsigned int i=0; i<t.rowCount(); i++) {
        updateMonitorIndexTable
            (t.getStringColumn("tableName")[i],
             dbToAverageType(t.getShortColumn("averageType")[i]));
    }
}


void MonitorDataDatabase::updateMonitorIndexTable
    (const std::string& tableName, const MonitorAverageType& averageType) 
    const {
    // FIXME this should be done using subqueries, but I'm making it MySQL 4.0
    // friendly
    carma::dbms::Table::ColumnType ctype;
    void *tmp;
    try {
        tmp = dbc_->getAggregate("*", tableName, "COUNT", ctype);
    } catch (const SQLException& exc) {
        // in the case of MySQL MyISAM tables, an SQLException can be thrown 
        // if the monitor data table queried exists but is corrupt, in this
        // case the remedy is to repair the table and retry the query, if the
        // query fails again, then either the table cannot be repaired or there
        // is another problem
        dbc_->repairMonitorDataTable(tableName);
        tmp = dbc_->getAggregate("*", tableName, "COUNT", ctype);
    }        
    ostringstream stmt;
    if(tmp == NULL || *static_cast<int *>(tmp) == 0) {
        // table has no rows
        delete static_cast< int * >(tmp);
        stmt << "DELETE FROM " + getTableName(MONITOR_INDEX_TABLE) 
             << " WHERE tableName='" << tableName << "'";
        dbc_->directSQLExec_(stmt.str());
        stmt.str("");
        stmt << "DROP TABLE " << tableName;
        dbc_->directSQLExec_(stmt.str());
        return;
    }

    int minIntegration;
    int maxIntegration;
    string columnName;
    if (averageType == FRAME_AVG || averageType == MINUTE_AVG) {
        columnName = "frameCount";
    } else {
        columnName = "integrationID";
    }
    try {
        tmp = dbc_->getAggregate(columnName, tableName, "min", ctype);
    } catch (const SQLException& exc) {
        // in the case of MySQL MyISAM tables, an SQLException can be thrown 
        // if the monitor data table queried exists but is corrupt, in this
        // case the remedy is to repair the table and retry the query, if the
        // query fails again, then either the table cannot be repaired or there
        // is another problem
        dbc_->repairMonitorDataTable(tableName);
        tmp = dbc_->getAggregate(columnName, tableName, "min", ctype);
    }        
    if(tmp == NULL) {
        return;
    }
    minIntegration = *static_cast<int *>(tmp);
    delete static_cast< int * >(tmp);
    tmp = dbc_->getAggregate(columnName, tableName, "max",ctype);
    if(tmp == NULL) {
        return;
    }
    maxIntegration = *static_cast<int *>(tmp);
    delete static_cast< int * >(tmp);

    // SQL UPDATE necessary here
    stmt << "UPDATE " << getTableName(MONITOR_INDEX_TABLE) 
         << " set minIntegration=" 
         << minIntegration << ", maxIntegration=" << maxIntegration
         << " WHERE tableName='" << tableName << "'";
    CARMA_CPTRACE(carma::util::Trace::TRACE5, stmt.str());
    dbc_->directSQLExec_(stmt.str());
}    

string MonitorDataDatabase::getCanonicalSystemName(const string& tableName) 
    const {
    string stmt = "SELECT partition FROM " + getTableName(MONITOR_INDEX_TABLE) 
        + " AS mit," + getTableName(PARTITIONS_TABLE) + " AS p WHERE "
        + "mit.tableName='" + tableName + "' AND "
        + "mit.partitionID=p.partitionID";
    Table t = dbc_->execSQLSelect(stmt);
    ostringstream emsg;
    if(t.rowCount() != 1) {
        ostringstream emsg;
        emsg << tableName << " not found in " << "table " 
             << getTableName(MONITOR_INDEX_TABLE);
        CARMA_CPTRACE (carma::util::Trace::TRACE1, emsg.str());
        throw CARMA_EXCEPTION(::carma::util::NotFoundException, emsg.str());
    }
    return t.getStringColumn("partition")[0] + "/" + tableName;
}
