/**
 *
 * Implementation for ResultsCache
 *
 * @author: Dave Mehringer
 * @version $Id: ResultsCache.cc,v 1.11 2007/06/29 21:52:45 abeard Exp $ *
 * $CarmaCopyright$
 *
 */
#include <sstream>

#include "carma/dbms/DBConnection.h"
#include "carma/dbms/MonitorConfigurationDatabase.h"
#include "carma/dbms/MonitorDataDatabase.h"
#include "carma/dbms/ResultsCache.h"
#include "carma/dbms/Table.h"
#include "carma/util/CommonExceptions.h"
#include "carma/util/Program.h"
#include "carma/util/ScopedPthreadMutexLock.h"
#include "carma/util/Trace.h"

using namespace ::std;
using namespace carma::dbms;

ResultsCache *ResultsCache::resultsCache_ = 0;
const Table *ResultsCache::fullMCTable_ = 0;
const Table *ResultsCache::currentMCTable_ = 0;
//const vector<int> *ResultsCache::allTagIDs_ = 0;
map<string,Table> * ResultsCache::tableCache_ = 0;
carma::util::PthreadMutex ResultsCache::resultsCacheMutex_;
map<int,string> * ResultsCache::tagIDToNameMap_ = 0;


ResultsCache::ResultsCache( const DBConnection * const dbc ) :
dbc_( dbc ),
logger_( carma::util::Program::getLogger() ) {
    CPTRACE(carma::util::Trace::TRACEALL, "ResultsCache constructor called");
    MonitorConfigurationDatabase mcdb(dbc_);
    fullMCTable_ = new Table(mcdb.getFullMonitorConfigurationTable());
    currentMCTable_ = new Table(mcdb.getFullMonitorConfigurationTable
                                (NULL,true));
    //allTagIDs_ = new Column<int>(mcdb.getTagIDs());
    tableCache_ = new map<string,Table>;
    tagIDToNameMap_ = new map<int,string>(mcdb.getTagIDs("%"));
}


void ResultsCache::closeCache() {
    carma::util::ScopedPthreadMutexLock scopelock( resultsCacheMutex_ );
    delete resultsCache_;
    resultsCache_ = 0;
    delete fullMCTable_;
    fullMCTable_ = 0;
    delete currentMCTable_;
    currentMCTable_ = 0;
    //delete allTagIDs_;
    //allTagIDs_ = 0;
    delete tableCache_;
    tableCache_ = 0;
    delete tagIDToNameMap_;
    tagIDToNameMap_ = 0;
}

bool ResultsCache::cacheExists() {
    return resultsCache_ != NULL;
}

ResultsCache& ResultsCache::getCache(const DBConnection * const dbc) {
    carma::util::ScopedPthreadMutexLock scopelock( resultsCacheMutex_ );
    if (resultsCache_ == 0) {
        if(dbc==NULL) {
            string emsg = "ResultsCache object has not yet been created, so ";
            emsg += "DBConnection parameter cannot be NULL";
            CPTRACE(carma::util::Trace::TRACE1, emsg);
            throw CARMA_EXCEPTION(carma::util::IllegalArgumentException, emsg);
        } 
        CPTRACE(carma::util::Trace::TRACEALL, "Creating ResultsCache "
                << "singleton");
        resultsCache_ = new ResultsCache(dbc);
    } else {
        CPTRACE(carma::util::Trace::TRACEALL, "ResultsCache singleton already "
                << "exists, just returning it");
        if(tableCache_ != NULL) {
            CPTRACE(carma::util::Trace::TRACEALL, "Number of elements in the "
                    << "table cache is " << tableCache_->size());
        }
    }        
    return *resultsCache_;
}

/* this is currently very expensive so is not practical
const ID2MDMap&  ResultsCache::getMonitorConfigurations() {
    if(tagID2MD_ == NULL) {
        MonitorConfigurationDatabase mcdb(dbc_);
        tagID2MD_ = new ID2MDMap(mcdb.getMonitorConfigurations());
    } else {
        CPTRACE(carma::util::Trace::TRACEALL, "tagID2MD_ has already been "
                << "created. Just returning that instance");
    }
    return *tagID2MD_;
}
*/

const Table&  ResultsCache::getFullMonitorConfigurationTable() {
    return *fullMCTable_;
}

const Table&  ResultsCache::getCurrentMonitorConfigurationTable() {
    return *currentMCTable_;
}

/*
const vector<int>& ResultsCache::getAllTagIDs() {
    return *allTagIDs_;
}
*/

void ResultsCache::cache(const string& identifier, const Table& table) {
    carma::util::ScopedPthreadMutexLock scopelock( resultsCacheMutex_ );
    if(tableCache_->find(identifier) != tableCache_->end()) {
        string emsg = "Identifier " + identifier + " already exists in the ";
        emsg += "table cache";
        throw CARMA_EXCEPTION(carma::util::IllegalArgumentException, emsg);
    }
    pair<string,Table> p = make_pair(identifier,table);
    tableCache_->insert(p);
}

const Table& ResultsCache::getTable(const string& identifier) const {
    if(tableCache_->empty()) {
        string emsg = "The table cache is empty, thus " + identifier;
        emsg += " does not exist in the table cache";
        throw CARMA_EXCEPTION(carma::util::NotFoundException, emsg);
    }
    map<string,Table>::const_iterator iter = tableCache_->find(identifier);
    if(iter == tableCache_->end()) {
        string emsg = "Identifier " + identifier + " does not exist in the ";
        emsg += "table cache";
        throw CARMA_EXCEPTION(carma::util::NotFoundException, emsg);
    }
    return iter->second;
}

bool ResultsCache::tableIDExists(const string identifier) const {
    return (tableCache_->find(identifier) != tableCache_->end());
}

const std::map<int,std::string>& ResultsCache::getTagIDToNameMap() const {
    return *tagIDToNameMap_;
}
