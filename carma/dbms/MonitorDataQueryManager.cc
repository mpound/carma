/**
 * Implementation for the MonitorDataQueryManager class
 *
 * @author: Dave Mehringer
 * @version $Id: MonitorDataQueryManager.cc,v 1.25 2011/12/21 22:56:43 mpound Exp $
 *
 * $CarmaCopyright$
 */
#include <deque>
#include <sstream>
#include "carma/dbms/ColumnNames.h"
#include "carma/dbms/DBConnection.h"
#include "carma/dbms/MonitorConfigurationDatabase.h"
#include "carma/dbms/MonitorDataDatabase.h"
#include "carma/dbms/MonitorDataQueryManager.h"
#include "carma/dbms/ResultsCache.h"
#include "carma/dbms/TableNames.h"
#include "carma/dbms/filter/MonitorPointFilter.h"
#include "carma/dbms/filter/TagIDFilter.h"
#include "carma/dbms/filter/TagIDSetFilter.h"
#include "carma/dbms/filter/TimeRangeFilter.h"
#include "carma/dbms/MonitorPointSelector.h"
#include "carma/dbms/Table.h"
#include "carma/util/CommonExceptions.h"
#include "carma/util/programLogging.h"
#include "carma/util/Program.h"
#include "carma/util/StopWatch.h"
#include "carma/util/StringUtils.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"

using namespace std;
using namespace carma::dbms;
using namespace carma::util;

const string MonitorDataQueryManager::TAGID_TO_DATATYPE = "TagID&DataType";

int MonitorDataQueryManager::tCount_ = 0;

/*
MonitorDataQueryManager::MonitorDataQueryManager
    (const set<const MonitorPointSelector *>& selectors, 
     const TimeRangeFilter * const trFilter, const Filter * const otherFilter,
     const DBConnection* const dbc) 
        : selectors_(selectors), dbc_(dbc), trFilter_(trFilter), 
          otherFilter_(otherFilter) {
    if(otherFilter == NULL) {
        string emsg = "parameter otherFilter cannot be NULL";
        throw CARMA_EXCEPTION(carma::util::IllegalArgumentException,emsg);
    }        
    doSanityChecks_();
    avgType_ = trFilter_->getAverageType();
    // create the ResultsCache singleton if it doesn't already exist
    if(!ResultsCache::cacheExists()) {
        ResultsCache::getCache(dbc_);
        if(!ResultsCache::cacheExists()) {
            string msg = "Unable to instantiate the ResultsCache singleton";
            carma::util::Program::getLogger() << log4cpp::Priority::WARN 
                                              << msg;
        }
    }
    cacheTagIDToDataType_();
    determineAggregateTableTypes_();
    determineTablesToQuery_();
    createQueries_();
}
*/

MonitorDataQueryManager::MonitorDataQueryManager(
    const MPSelectorSet& selectors,
    const TimeRangeFilter * const               trFilter,
    const DBConnection * const                  dbc ) :
dbc_(dbc),
trFilter_(trFilter),
mpFilters_(),
selectors_(selectors),
haveRunSetUpQueries_(false),
haveRunWideSetUpQueries_(false) {
    init_();
}


MonitorDataQueryManager::MonitorDataQueryManager(
    const MPSelectorSet& selectors,
    const TimeRangeFilter * const              trFilter,
    const set<const MonitorPointFilter * > &   mpfilters,
    const DBConnection * const                 dbc ) :
dbc_(dbc),
trFilter_(trFilter), 
mpFilters_(mpfilters),
selectors_(selectors),
haveRunSetUpQueries_(false), 
haveRunWideSetUpQueries_(false) {
    init_();
}


MonitorDataQueryManager::~MonitorDataQueryManager() {
    CPTRACE( util::Trace::TRACEALL, "in destructor");
    vector<string>::const_iterator iter = tablesToDropUponDestruction_.begin();
    for( ; iter != tablesToDropUponDestruction_.end(); iter++) {
        // Logging added after some suspicious hangs on the drop -
        // 2011-08-27, S. Scott
        ostringstream o; 
        o << "Dropping table: " << *iter;
        programLogInfo(o.str());
        dbc_->dropScratchTable(*iter);
        o.str(""); 
        o << "Table drop completed: "<< *iter;
        programLogInfo(o.str());
    }
}

void MonitorDataQueryManager::doSanityChecks_() {
    if(selectors_.size() == 0) {
        string emsg = "parameter selectors cannot contain 0 members";
        throw CARMA_EXCEPTION(carma::util::IllegalArgumentException,emsg);
    }
    int minTime = trFilter_->getChildren()[0]->getValue();
    if(trFilter_->getAverageType() == FRAME_AVG) {
	int prodTime = getBeginningOfProduction();
        if( minTime < prodTime ){
            ostringstream emsg;
            emsg << "Minimum frameCount of the specified time range (" 
                 << minTime << ") is less than the frameCount at the "
                 << "beginning of CARMA database production (" 
                 << getBeginningOfProduction() << "). Adjust appropriately "
                 << "and resubmit";
            throw CARMA_EXCEPTION(carma::util::IllegalArgumentException, 
                                  emsg.str());
        }
    }
    set<const MonitorPointFilter * >::const_iterator fiter 
        = mpFilters_.begin();
    int tagID;
    for( ; fiter != mpFilters_.end(); fiter++ ) {
        tagID = (*fiter)->getTagID();
        if(tagID2MPFilter_.count(tagID) > 0) {
            ostringstream emsg;
            emsg << "The set of monitor point filters has at least two "
                 << "filters with tagID " << tagID;
            throw CARMA_EXCEPTION(carma::util::IllegalArgumentException, 
                                  emsg.str());
        } else {
            if(!tagID2MPFilter_.insert(make_pair(tagID,*fiter)).second) {
                ostringstream emsg;
                emsg << "Error inserting pair into tagID2MPFilter_ with tagID "
                     << tagID;
                throw CARMA_ERROR(emsg.str());
            }
        }
    }
}

set<MonitorAggregateType> MonitorDataQueryManager::getAggregateDataTypes() 
    const {
    return aggTypes_;
}

carma::util::frameType MonitorDataQueryManager::getBeginningOfProduction() {
    // FIXME  just a placeholder for now
    // 02Oct04 02:40:00
    return 300000000;
}

void MonitorDataQueryManager::determineAggregateTableTypes_() {
    // first, get the aggregate types of the Selectors
    MPSelectorSet::const_iterator siter 
        = selectors_.begin();
    MonitorAggregateType mat;
    for( ; siter != selectors_.end(); siter++ ) {
        mat = dataType2AggregateType
            ((*siter)->getMonitorConfiguration().getDataType());
        aggTypes_.insert(mat);
        aggType2MPSelectors_[mat].push_back(*siter);
    }
    // next get the aggregate types from the monitor point filters
    if(mpFilters_.size() == 0) {
        return;
    }
    set<const MonitorPointFilter * >::const_iterator fiter 
        = mpFilters_.begin();
    for( ; fiter != mpFilters_.end(); fiter++ ) {
        mat = dataType2AggregateType((*fiter)->getDataType());
        aggTypes_.insert(mat);
        aggType2MPFilters_[mat].push_back(*fiter);
    }
}

void  MonitorDataQueryManager::determineTablesToQuery_() {
    MonitorDataDatabase mddb(dbc_);
    set<MonitorAggregateType>::const_iterator iter = aggTypes_.begin();
    Table t;
    for( ; iter!=aggTypes_.end(); iter++) {
        t = mddb.getMonitorDataIndexTableEntries(*trFilter_,*iter,true);
        if(t.rowCount() == 0) {
            ostringstream emsg;
            emsg << "There are no monitor data tables of aggregate type " 
                 << toString(*iter) << " which match your query parameters. "
                 << "Please adjust your query and resubmit.";
            if(avgType_ != FRAME_AVG) {
                emsg << endl << "If you submitted this query to the short "
                     << "term database at the site, you may have to submit it "
                     << "to the long term archive at NCSA";
            }
            throw CARMA_EXCEPTION(carma::util::NotFoundException, emsg);
        }
        tablesToQuery_[*iter] = t;
    }
}

map<MonitorAggregateType, vector<string> > 
    MonitorDataQueryManager::getTablesToQuery() const {
    map<MonitorAggregateType,Table>::const_iterator iter 
        = tablesToQuery_.begin();
    map<MonitorAggregateType, vector<string> > m;    
    for( ; iter!=tablesToQuery_.end(); iter++) {
        m[iter->first] 
            = (vector<string>)iter->second.getStringColumn("tableName");
    }
    return m;
}


void MonitorDataQueryManager::cacheTagIDToDataType_() {
    if(ResultsCache::cacheExists()) {
        const Table &t = ResultsCache::getCache()
            .getCurrentMonitorConfigurationTable();
        vector<string> cnames;
        cnames.push_back("tagID");
        cnames.push_back("dataTypeID");
        tagIDToDataTypeID_ = t.createSubTable(cnames);
    } else {
        MonitorConfigurationDatabase mcdb(dbc_);
        tagIDToDataTypeID_ = mcdb.getTagIDToDataTypeIDTable();
    }
}

void MonitorDataQueryManager::createQueries_() {
    //WARNING this uses MySQL specific syntax and features!
    createSetUpQueries_();
    createNarrowRSQueries_();
    createWideRSQuery_();
}

void MonitorDataQueryManager::execSetUpQueries_() const {
    vector<string>::const_iterator iter = setUpQueries_.begin();
    carma::util::StopWatch sw;
    sw.start();
    for( ; iter != setUpQueries_.end(); iter++) {
        dbc_->directSQLExec_(*iter);
    }
    sw.stop();
    cout << "Time to execute set up queries " << sw.getElapsedTime() << endl;
}

map<int,Table> MonitorDataQueryManager::execNarrowQueries() {
    if(!haveRunSetUpQueries_) {
        execSetUpQueries_();
        haveRunSetUpQueries_ = true;
    }
    map<int,string>::const_iterator iter = narrowRSQueries_.begin();
    map<int, Table> resultSets;
    pair<int,Table> p;
    carma::util::StopWatch sw;
    sw.start();
    for( ; iter!=narrowRSQueries_.end(); iter++) {
        p = make_pair(iter->first, dbc_->execSQLSelect(iter->second));
        if(!(resultSets.insert(p)).second) {
            ostringstream emsg;
            emsg << "Error inserting result set pair for tagID " 
                 << iter->first; 
            throw CARMA_ERROR(emsg.str());
        }
    }
    sw.stop();
    cout << "Time to execute narrow queries " << sw.getElapsedTime() << endl;
    return resultSets;
}

void MonitorDataQueryManager::execWideSetUpQueries_() const {
    vector<string>::const_iterator iter = wideSetUpQueries_.begin();
    carma::util::StopWatch sw;
    sw.start();
    for( ; iter != wideSetUpQueries_.end(); iter++) {
        dbc_->directSQLExec_(*iter);
    }
    sw.stop();
    cout << "Time to execute wide set up queries " << sw.getElapsedTime() 
         << endl;
}

Table MonitorDataQueryManager::execWideQuery() {
    if(wideRSQuery_ == "") {
        throw CARMA_EXCEPTION(carma::util::IllegalArgumentException,
                              wideErrMsg_);
    }
    if(!haveRunSetUpQueries_) {
        execSetUpQueries_();
        haveRunSetUpQueries_ = true;
    }
    if(!haveRunWideSetUpQueries_) {
        execWideSetUpQueries_();
        haveRunWideSetUpQueries_ = true;
    }
    carma::util::StopWatch sw;
    sw.start();
    Table t = dbc_->execSQLSelect(wideRSQuery_);
    sw.stop();
    cout << "Time to execute wide  query " << sw.getElapsedTime() 
         << endl;
    return t;
}


void MonitorDataQueryManager::createSetUpQueries_() {
    //    map<MonitorAggregateType, vector<const MonitorPointSelector * > >
    //::const_iterator iter = aggType2MPSelectors_.begin();
    MonitorAggregateType aggType;
    vector<string>::const_iterator kiter;
    set<unsigned> tagIDs;
    set<MonitorAggregateType>::const_iterator aggIter = aggTypes_.begin();
    string tableName;
    vector<string> currentTables2Query;
    map<MonitorAggregateType,vector<string> > t2q = getTablesToQuery();
    ostringstream os,statement;
    for ( ; aggIter != aggTypes_.end() ; aggIter++) {
        aggType = *aggIter;
        os.str("");
        os << "temp_" << carma::util::Time::computeCurrentFrame() << "_" 
           << tCount_;
        tablesToDropUponDestruction_.push_back(os.str());
        tableName = "scratch." + os.str();
        if(!tmpTableName_.insert(make_pair(aggType, tableName)).second) {
            ostringstream emsg;
            emsg << "Error inserting " << aggType << ", " << tableName
                 << " pair into tmpTableName_ map";
            throw CARMA_ERROR(emsg.str());
        }
        statement.str("");
        statement << "CREATE TABLE " << tableName << " ";
        /*
        if(aggType != STRING_TYPE) {
            // WARNING ENGINE=HEAP is mysql-specific
            // heap tables do not support TEXT columns
        }
        */
        statement << "ENGINE=HEAP ";
        currentTables2Query = t2q.find(aggType)->second;
        for(kiter=currentTables2Query.begin();
            kiter!=currentTables2Query.end(); kiter++) {
            statement << "SELECT * FROM " << *kiter << " WHERE " 
                      << aggType2FilterClause_.find(aggType)->second;
            if(kiter != currentTables2Query.end() - 1) {
                statement << " UNION ";
            }
        }
        statement << " -- WARNING! MySQL specific features and syntax";
        setUpQueries_.push_back(statement.str());
        statement.str("");
        statement << "ALTER TABLE " << tableName << " ADD INDEX("
                  << trFilter_->getColumnName() 
                  << "," 
                  << getColumnName(COLUMN_TAGID) 
                  << ")";
        setUpQueries_.push_back(statement.str());
        // add indices to the table
        // the order in which the indices are added is important, reversing
        // the order negatively impacts performance
        // In addition, note that executing these two statements results in
        // better performance when producing the narrow tables than 
        // ALTER TABLE ... ADD INDEX(frameCount,tagID)
        /*
        statement.str("");
        // good for wide query
        statement << "ALTER TABLE " << tableName << " ADD INDEX("
                  << trFilter_->getColumnName() << ")";
        setUpQueries_.push_back(statement.str());
        */
        // good for narrow queries
        statement.str("");
        statement << "ALTER TABLE " << tableName << " ADD INDEX("
                  << getColumnName(COLUMN_TAGID) << ")";
        setUpQueries_.push_back(statement.str());
        tCount_++;
    }
}

void MonitorDataQueryManager::createNarrowRSQueries_() {
    ostringstream statement, os, whereClause;
    map<MonitorAggregateType, vector<const MonitorPointSelector * > >
        ::const_iterator iter = aggType2MPSelectors_.begin();
    vector<const MonitorPointSelector * >::const_iterator jiter;
    MonitorAggregateType aggType;
    const string tagIDCol = getColumnName(COLUMN_TAGID);
    const string timeCol = trFilter_->getColumnName();
    const string blankingFlagsTable = getTableName(BLANKING_FLAGS_TABLE);
    const string validitiesTable = getTableName(VALIDITIES_TABLE);
    const string blankingFlagIDCol = getColumnName(COLUMN_BLANKINGFLAGID);
    const string validityIDCol = getColumnName(COLUMN_VALIDITYID);

    for ( ; iter != aggType2MPSelectors_.end() ; iter++) {
        aggType = iter->first;
        for(jiter=iter->second.begin(); jiter!=iter->second.end(); jiter++) {
            const MonitorPointSelector * const mps = *jiter;
            string name = mps->getMonitorConfiguration().getName();
            int tagID = mps->getTagID();
            os.str("");
            bool doValidity = mps->getSelectionMask().validity;
            bool doBlankingFlag = mps->getSelectionMask().blankingFlag;
            //string tname = (join) ? tmpTableName_.find(aggType)->second 
            string tname = tmpTableName_.find(aggType)->second; 
            // WARNING STRAIGHT_JOIN is mysql-specific
            os << "SELECT STRAIGHT_JOIN " << mps->toString(avgType_,tname,name)
               << " FROM " << tname << " ";
            //<< " USE INDEX(" << tagIDCol << ")";
            if(doValidity) {
                os << DBConnection::createJoinClause
                    (tname,validitiesTable, validityIDCol) << " ";
            }
            if(doBlankingFlag) {
                os << DBConnection::createJoinClause
                    (tname,blankingFlagsTable,blankingFlagIDCol) << " ";
            }
            if(mps->doSelectEnumString()) {
                string onClause = tname;
                onClause += "." + tagIDCol + "="
                    + getTableName(MONITOR_ENUM_TABLE) + "." 
                    + tagIDCol + " AND " 
		    + tname + "." + mdqmGetColumnName(COLUMN_VALUE) + "="
                    + getTableName(MONITOR_ENUM_TABLE) + "." 
                    + getColumnName(COLUMN_ENUMINDEX);
                os << DBConnection::createJoinClause
                    (getTableName(MONITOR_ENUM_TABLE), onClause) 
                   << " " << DBConnection::createJoinClause
                    (getTableName(MONITOR_ENUM_TABLE),
                     getTableName(MONITOR_ENUM_INDEX_TABLE),
                     getColumnName(COLUMN_ENUMID)) << " ";
            }
            os << "WHERE ";
            bool join = (doValidity || doBlankingFlag 
                         || mps->doSelectEnumString());
            if(join) {
                os << tname << "." ;
            }
            os << tagIDCol << "=" << tagID;
            os << " ORDER BY " << timeCol;
            if(!narrowRSQueries_.insert
               (make_pair(tagID, os.str())).second) {
                ostringstream emsg;
                emsg << "Error inserting " << tagID << ", " << os.str()
                     << " pair into narrowRSQueries_";
                throw CARMA_ERROR(emsg.str());
            }
        }
    }
}

void MonitorDataQueryManager::init_() {
    doSanityChecks_();
    avgType_ = trFilter_->getAverageType();
    // create the ResultsCache singleton if it doesn't already exist
    if(!ResultsCache::cacheExists()) {
        ResultsCache::getCache(dbc_);
        if(!ResultsCache::cacheExists()) {
            string msg = "Unable to instantiate the ResultsCache singleton";
            carma::util::Program::getLogger() << log4cpp::Priority::WARN 
                                              << msg;
        }
    }
    cacheTagIDToDataType_();
    determineAggregateTableTypes_();
    determineTablesToQuery_();
    constructFilterClauses_();
    createQueries_();
}

void MonitorDataQueryManager::constructFilterClauses_() {
    vector<const MonitorPointSelector * > mpss;
    vector<const MonitorPointFilter * > mpfs;
    vector<const MonitorPointSelector * >::const_iterator mpsiter;
    vector<const MonitorPointFilter * >::const_iterator mpfiter;
    set<MonitorAggregateType>::const_iterator iter = aggTypes_.begin();
     set<unsigned> selectorTagIDs;
    set<unsigned> filterTagIDs;
    MonitorAggregateType aggType;
    for ( ; iter != aggTypes_.end() ; iter++) {
        aggType = *iter;
        set<MultiComponentFilter * > tmpmcfs;
        set<MultiComponentFilter * >::const_iterator mcfiter;
        selectorTagIDs.clear();
        filterTagIDs.clear();
        if(aggType2MPFilters_.count(aggType) > 0) {
            mpfs = aggType2MPFilters_.find(aggType)->second;
            for(mpfiter = mpfs.begin(); mpfiter != mpfs.end(); mpfiter++) {
                filterTagIDs.insert((*mpfiter)->getTagID());
            }
        }
        assert(mpfs.size() == filterTagIDs.size());
        if(aggType2MPSelectors_.count(aggType) > 0) {
            mpss = aggType2MPSelectors_.find(aggType)->second;
            for(mpsiter=mpss.begin(); mpsiter!=mpss.end(); mpsiter++) {
                unsigned tagID = (*mpsiter)->getTagID();
                if(filterTagIDs.count(tagID) == 0) {
                    selectorTagIDs.insert(tagID);
                }
            }
        }
        assert(selectorTagIDs.size() > 0 || filterTagIDs.size() > 0);
        auto_ptr<const OneComponentFilter> tagIDFilter;
        if(selectorTagIDs.size() == 1) {
            auto_ptr<const TagIDFilter> 
                tmp(new TagIDFilter(*selectorTagIDs.begin()));
            tagIDFilter = tmp;
        } else if (selectorTagIDs.size() > 1) {
            auto_ptr<const TagIDSetFilter> 
                tmp(new TagIDSetFilter(selectorTagIDs));
            tagIDFilter = tmp;
        } else {
            assert(selectorTagIDs.size() == 0);
        }
        auto_ptr<const MultiComponentFilter> mcfFilter;
        if(tagIDFilter.get() != NULL) {
            assert(selectorTagIDs.size() > 0);
            auto_ptr<const MultiComponentFilter> tmp
                (new MultiComponentFilter(trFilter_,tagIDFilter.get(),
                                          MultiComponentFilter::AND));
            mcfFilter = tmp;
        }
        auto_ptr<const MultiComponentFilter> finalFilter;
        if(filterTagIDs.size() == 0) {
            assert (selectorTagIDs.size() > 0 && mcfFilter.get() != NULL);
            finalFilter = mcfFilter;
        } else if(filterTagIDs.size() == 1) {
            const MultiComponentFilter *firstFilter 
                = (mcfFilter.get() == NULL) ? trFilter_ : mcfFilter.get();
            auto_ptr<const MultiComponentFilter> tmp
                (new MultiComponentFilter(firstFilter,*mpfs.begin(),
                                          MultiComponentFilter::AND));
            finalFilter = tmp;
        } else if(filterTagIDs.size() > 1) {
            bool hasBegun = false;
            const MultiComponentFilter *firstFilter = 0;
            MultiComponentFilter *allMPFs = 0;
            for(mpfiter = mpfs.begin(); mpfiter != mpfs.end(); mpfiter++) {
                if(mpfiter != mpfs.begin()) {
                    firstFilter = (hasBegun) ? allMPFs : *mpfs.begin();
                    MultiComponentFilter *tmp = new MultiComponentFilter
                        (firstFilter,*mpfiter, MultiComponentFilter::OR);
                    tmpmcfs.insert(tmp);
                    allMPFs = tmp;
                    hasBegun = true;
                }
            }
            assert(allMPFs != NULL);
            firstFilter = (mcfFilter.get() == NULL) 
                ? trFilter_ : mcfFilter.get();
            auto_ptr<const MultiComponentFilter> tmp
                (new MultiComponentFilter(firstFilter,allMPFs,
                                          MultiComponentFilter::AND));
            finalFilter = tmp;
        }
        assert(finalFilter.get() != NULL);
        string filterClause = finalFilter->toString();
        for(mcfiter = tmpmcfs.begin(); mcfiter != tmpmcfs.end(); 
            mcfiter++) {
            delete *mcfiter;
        }
        if(!aggType2FilterClause_.insert(make_pair(aggType,filterClause))
           .second) {
            ostringstream emsg;
            emsg << "Error inserting " << aggType << "," 
                 << finalFilter->toString() << " pair into "
                "aggType2FilterClause_";
            throw CARMA_ERROR(emsg.str());
        }
    }
}

void MonitorDataQueryManager::createWideRSQuery_() {
    MPSelectorSet::const_iterator iter = selectors_.begin();
    unsigned int totalJoinCount = 0;
    unsigned int nSelectedColumns = 0;
    unsigned int maxTablesPerJoin = dbc_->maxTablesPerJoin();
    vector< vector <const MonitorPointSelector * > > mpssVecs;
    vector<const MonitorPointSelector * > mpssVecTmp;
    unsigned int joinCount = 0;
    for ( ; iter != selectors_.end() ; iter++) {
        const MonitorPointSelector * const mps = *iter;
        unsigned int jt = 1 + mps->getSupplementalTableJoinCount();
        totalJoinCount += jt;
        nSelectedColumns += mps->selectedColumnCount(avgType_);
        if ((joinCount + jt) > maxTablesPerJoin) {
            assert(mpssVecTmp.size() > 0);
            mpssVecs.push_back(mpssVecTmp);
            mpssVecTmp.clear();
            joinCount = jt;
        } else {
            joinCount += jt;
        }
        mpssVecTmp.push_back(mps);
    }
    if(mpssVecTmp.size() > 0) {
        mpssVecs.push_back(mpssVecTmp);
    }
    unsigned int count = 0;
    for(unsigned int i = 0; i < mpssVecs.size(); i++) {
        count += mpssVecs[i].size();
    }
    assert(count == selectors_.size());

    if(nSelectedColumns > dbc_->maxColumnsPerTable()) {
        ostringstream msg;
        msg << "The total number of columns selected is " << nSelectedColumns
            << " which exceeds the maximum number of columns per table of "
            << dbc_->maxColumnsPerTable() << ". The wide (multi join) query "
            << "cannot be executed.";
        carma::util::Program::getLogger() << log4cpp::Priority::WARN 
                                          << msg.str();
        return;
    }
    if(mpssVecs.size() > maxTablesPerJoin) {
        ostringstream msg;
        msg << "This selection which involves " << nSelectedColumns 
            << " columns and " << selectors_.size() << " monitor point "
            << "selectors requires joining of " << totalJoinCount 
            << " tables.  This is not permitted";
        carma::util::Program::getLogger() << log4cpp::Priority::WARN 
                                          << msg.str();
        wideErrMsg_ = msg.str();
        return;
    }
    createJoinSetUpQueries_(mpssVecs);
}

void MonitorDataQueryManager::createJoinSetUpQueries_
    (const vector< vector<const MonitorPointSelector * > >& mpssVec) {
    ostringstream statement, os, whereClause;
    const string blankingFlagsTable = getTableName(BLANKING_FLAGS_TABLE);
    const string validitiesTable = getTableName(VALIDITIES_TABLE);
    const string blankingFlagIDCol = getColumnName(COLUMN_BLANKINGFLAGID);
    const string validityIDCol = getColumnName(COLUMN_VALIDITYID);
    const string tagIDCol = getColumnName(COLUMN_TAGID);
    const string enumeratorsTable = getTableName(MONITOR_ENUM_TABLE);
    const string enumIndexTable = getTableName(MONITOR_ENUM_INDEX_TABLE);
    //    const string valueCol = getColumnName(COLUMN_VALUE);
    const string valueCol = mdqmGetColumnName(COLUMN_VALUE);
    const string enumIndexCol = getColumnName(COLUMN_ENUMINDEX);
    const string enumIDCol = getColumnName(COLUMN_ENUMID);
    const string timeCol = trFilter_->getColumnName();
    int tableCount = 0;
    vector<string> tableAliases;
    map<string,string> tableAlias2Table;
    vector<string> queries;
    vector<string> finalSelectedColumns;
    deque<bool> containsStrings;
    for (unsigned i = 0; i < mpssVec.size(); i++) {
        vector<const MonitorPointSelector * > mpss = mpssVec[i];
        vector<const MonitorPointSelector * >::const_iterator iter 
            = mpss.begin();
        vector<string> selectedColumns;
        bool first = true;
        ostringstream fromClause;
        ostringstream whereClause;
        whereClause << "WHERE";
        string firstTable;
        bool containsStringCol = false;
        for( ; iter != mpss.end(); iter++) {
            const MonitorPointSelector * const mps = *iter;
            MonitorAggregateType aggType = dataType2AggregateType
                (mps->getDataType());
            containsStringCol = containsStringCol 
                || (aggType == STRING_TYPE && mps->getSelectionMask().value);
            string tname = tmpTableName_.find
                (aggType)->second; 
            int tagID = mps->getTagID();
            ostringstream tmpstream;
            tmpstream << tagID;
            // making the column prepender the tagID rather than the canonical
            // name because MySQL column names are limited to a maximum of
            // 64 characters, some monitor point canonical names exceed this
            // limit
            string colPrepender = tmpstream.str();
            os.str("");
            ostringstream tableAlias;
            tableAlias << "T" << tableCount;
            if(first) {
                first = false;
                firstTable = tableAlias.str();
                fromClause << "FROM " << tname << " AS " << firstTable;
                    //<< " USE INDEX(" << timeCol << ")";
                string joinCol = firstTable + "." + timeCol;
                selectedColumns.push_back(joinCol);
            } else {
                fromClause << " " 
                           << DBConnection
                    ::createJoinClause(firstTable,tname,timeCol,timeCol,
                                       tableAlias.str(), timeCol); 
                whereClause << " AND";
            }
            whereClause << " " << tableAlias.str() << "." << tagIDCol << "=" 
                        << tagID;
            bool joinValidity = mps->getSelectionMask().validity;
            bool joinBlankingFlag = mps->getSelectionMask().blankingFlag;
            bool joinEnumString = mps->doSelectEnumString();
            string validityAlias = "";
            if(joinValidity) {
                ostringstream vAlias;
                vAlias << "V" << tableCount;
                validityAlias = vAlias.str();
                fromClause << " " 
                           << DBConnection
                    ::createJoinClause(tableAlias.str(),validitiesTable,
                                       validityIDCol,validityIDCol,
                                       validityAlias);
            }
            string blankingFlagAlias = "";
            if(joinBlankingFlag) {
                ostringstream bAlias;
                bAlias << "B" << tableCount;
                blankingFlagAlias = bAlias.str();
                fromClause << " " 
                           << DBConnection
                    ::createJoinClause(tableAlias.str(),blankingFlagsTable,
                                       blankingFlagIDCol,blankingFlagIDCol,
                                       bAlias.str());
            }
            string enumeratorIndexAlias = "";
            if(joinEnumString) {
                ostringstream eiAlias;
                eiAlias << "EI" << tableCount;
                enumeratorIndexAlias = eiAlias.str();
                ostringstream meAlias;
                meAlias << "ME" << tableCount;

                string onClause = tableAlias.str();
                onClause += "." + tagIDCol + "=" + meAlias.str() + "." 
                    + tagIDCol + " AND " + tableAlias.str() + "." + valueCol 
                    + "=" + meAlias.str() + "." + enumIndexCol;
                fromClause << " " << DBConnection::createJoinClause
                    (enumeratorsTable + " AS " + meAlias.str(), onClause) 
                           << " " << DBConnection::createJoinClause
                    (meAlias.str(), enumIndexTable, enumIDCol, enumIDCol,
                     eiAlias.str());
                containsStringCol = true;
            }
            tableCount++;
            selectedColumns.push_back
                (mps->toString
                 (avgType_,tableAlias.str(),colPrepender,blankingFlagAlias,
                  validityAlias,enumeratorIndexAlias));
            finalSelectedColumns
                .push_back(mps->toString
                           (avgType_,tableAlias.str(),colPrepender,
                            blankingFlagAlias,validityAlias,
                            enumeratorIndexAlias,true));
        }
        containsStrings.push_back(containsStringCol);
        ostringstream query;
            // WARNING STRAIGHT_JOIN is mysql-specific
        query << "SELECT STRAIGHT_JOIN " 
              << carma::util::vectorToString(selectedColumns) 
              << " " << fromClause.str() << " " << whereClause.str();
        queries.push_back(query.str());
    }
    assert(!queries.empty());
    assert(queries.size() == containsStrings.size());
    if(queries.size() == 1) {
        // no need to create scratch tables for joins, just execute a single
        // query
        wideRSQuery_ = queries.front();
        wideRSQuery_ += " ORDER BY " + timeCol;
    } else {
        // have to create scratch tables via joins and then join all those
        wideRSQuery_ = "SELECT STRAIGHT_JOIN " + carma::util::vectorToString
            (finalSelectedColumns);
        string firstTable;
        for(unsigned int i=0 ; i<queries.size(); i++) {
            ostringstream q;
            ostringstream tname;
            tname << "tempWide_" << carma::util::Time::computeCurrentFrame() 
                  << "_" << tCount_;
            /*
             * unfortunately, mysql prohibits a temporary table from being
             * reference more than once in a select clause, so these tables
             * cannot be created using the TEMPORARY keyword.  They must
             * be kept track of and are dropped in the destructor of this
             * object. Because we don't want to grant DROP privileges to
             * the generic db user on the carma database, the scratch tables
             * are located in the scratch database
             */
            tablesToDropUponDestruction_.push_back(tname.str());
            tCount_++;
            string tableName = "scratch." + tname.str();
            if(i == 0) {
                firstTable = tableName;
                wideRSQuery_ += " FROM " + firstTable;
            } else {
                wideRSQuery_ += " " + DBConnection::createJoinClause
                    (firstTable, tableName, timeCol);
            }
            q << "CREATE TABLE " << tableName << " ";
            /*
            if(!containsStrings[i]) {
                // WARNING: ENGINE=HEAP is mysql-specific
                // HEAP tables do not support TEXT columns
                q << "ENGINE=HEAP ";
            }
            */
            q << "ENGINE=HEAP ";
            q << queries[i];
            wideSetUpQueries_.push_back(q.str());
            q.str("");
            q << "ALTER TABLE " << tableName << " ADD INDEX(" << timeCol 
              << ")";
            wideSetUpQueries_.push_back(q.str());
        }
        assert(wideSetUpQueries_.size() == 2*queries.size());
        wideRSQuery_ += " ORDER BY " + firstTable + "." + timeCol;
    }
}

/*
void MonitorDataQueryManager::handleOneComponentFilter_
    (const OneComponentFilter * const ocf) {
    const TagIDFilter *tif = dynamic_cast<const TagIDFilter *>(otherFilter_);
    if(tif != NULL) {
        handleTagIDFilter_(tif);
        return;
    }
    const TagIDSetFilter *tisf 
        = dynamic_cast<const TagIDSetFilter *>(otherFilter_);
    if(tisf != NULL) {
        handleTagIDSetFilter_(tisf);
        return;
    }
    ostringstream emsg;
    emsg << "The only OneComponentFilters which are currently handled are "
         << "TagIDFilter and TagIDSetFilter";
    throw CARMA_EXCEPTION(carma::util::IllegalArgumentException, emsg.str());
}    
*/

/*
void MonitorDataQueryManager::handleTagIDFilter_(const TagIDFilter * const tif)
{
    int index = tagIDToDataTypeID_.getIntColumn("tagID")
        .indexOf(tif->getValue());
    assert(index >= 0);
    aggTypes_.insert(dataType2AggregateType
                     (db2mpDataType(tagIDToDataTypeID_
                                    .getShortColumn("dataTypeID")[index])));
}
*/

    /*
void MonitorDataQueryManager::handleTagIDSetFilter_
    (const TagIDSetFilter * const tisf) {
    set<int> tagIDs = tisf->getValues();
    set<int>::iterator iter = tagIDs.begin();
    int index;
    for( ; iter != tagIDs.end(); iter++) {
        index = tagIDToDataTypeID_.getIntColumn("tagID").indexOf(*iter);
        assert(index >= 0);
        aggTypes_.insert(dataType2AggregateType
                         (db2mpDataType(tagIDToDataTypeID_.getShortColumn
                                        ("dataTypeID")[index])));
        if(aggTypes_.size() == MAX_AGGREGATE_DATA_TYPE) {
            return;
        }
    }
}
    */

    /*
void MonitorDataQueryManager::handleMultiComponentFilter_
    (const MultiComponentFilter * const tcf) {
    vector<const OneComponentFilter * > v;
    tcf->getOneComponentFilterDescendants(&v);
    vector<const OneComponentFilter * >::const_iterator iter = v.begin();
    for( ; iter!=v.end(); iter++ ) {
        handleOneComponentFilter_(*iter);
    }
}    
    */


string MonitorDataQueryManager::mdqmGetColumnName(DBColumn column)const
{ if((column == COLUMN_VALUE) && (avgType_ != FRAME_AVG))
	 column = COLUMN_INTEGRATEDVALUE;
         return getColumnName(column);
}
