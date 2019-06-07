/** 
 * @file carma/dbms/Test/MonitorDataQueryManagerTest.cc
 *
 * CppUnit test fixture for carma::dbms::MonitorDataQueryManagerTest
 *
 * @author Dave Mehringer
 * @version $Id: MonitorDataQueryManagerTest.cc,v 1.18 2007/02/15 00:29:01 scott Exp $
 */

#include <iomanip>
#include "carma/dbms/DBConfigurator.h"
#include "carma/dbms/DBConnection.h"
#include "carma/dbms/MonitorDataQueryManager.h"
#include "carma/dbms/ResultsCache.h"
#include "carma/dbms/filter/TimeRangeFilter.h"
#include "carma/dbms/filter/MonitorPointFilter.h"
#include "carma/dbms/filter/TagIDFilter.h"
#include "carma/dbms/filter/TagIDSetFilter.h"
#include "carma/dbms/filter/ValiditySetFilter.h"
#include "carma/dbms/MonitorPointSelector.h"
#include "carma/dbms/Table.h"
#include "carma/dbms/TableNames.h"
#include "carma/dbms/Test/MonitorDataQueryManagerTest.h"
#include "carma/util/CommonExceptions.h"
#include "carma/util/StopWatch.h"
#include "carma/util/StringUtils.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"

using namespace carma::dbms;
using namespace carma::util;
using namespace CppUnit;
using namespace std;

std::string MonitorDataQueryManagerTest::conffile_ = "conf/dbms/dbms.conf";

/*
if you use this here it will cause a segfault since argv in program has not
been defined
std::string MonitorDataQueryManagerTest::conffile_ 
    = Program::getConfFile("dbms/dbms.conf");
*/

void MonitorDataQueryManagerTest::setUp() {
    DBConfigurator *dbconf = 0;
    try {
        dbconf = new DBConfigurator(conffile_);
    } catch (const carma::util::NotFoundException& exc) {
        cerr << "conffile " << conffile_ << " not found " << endl;
        throw exc;
    }
    try {
        dbc_ = DBConnectionFactory::createConnection(dbconf);
        ResultsCache rc = ResultsCache::getCache(dbc_);
    } catch(const DBConnectionException& exc) {
        cerr << exc.what() << endl;
        delete dbconf;
        throw exc;
    }
    delete dbconf;
}

void MonitorDataQueryManagerTest::constructionTests() {
    /*
    TimeRangeFilter trFilter(46666, 50000, FRAME_AVG);
    CPTRACE( util::Trace::TRACEALL, "using tagID=" << tagID);
    MonitorPointSelector s(tagID,dbc_);
    CPPUNIT_ASSERT ( s.getTagID() == tagID);
    set<const Selector *> vs;
    vs.insert(&s);
    int minTime = MonitorDataQueryManager::getBeginningOfProduction();
    TimeRangeFilter trFilter2(minTime+2, minTime+200, FRAME_AVG);
    */
    map<int,string> t2n = ResultsCache::getCache().getTagIDToNameMap();
    int tagID;
    // determine what minimum time to use to actually get data
    ostringstream statement;
    statement << "SELECT min(minIntegration) FROM " 
              << getTableName(MONITOR_INDEX_TABLE) << " WHERE averageType="
              << averageTypeToDB(FRAME_AVG);
    Table t = dbc_->execSQLSelect(statement.str());
    int minTime = t.getIntColumn(0)[0];
    cout << "minTime " << minTime << endl;
    TimeRangeFilter trFilter3(minTime+10, minTime+3000, FRAME_AVG);
    MPSelectorSet mps;
    map<int,string>::const_iterator iter = t2n.begin();
    map<int, const MonitorPointSelector * > tagID2mps;
    int count=0;
    set<carma::monitor::MonitorPoint::VALIDITY> validitySet;
    validitySet.insert(carma::monitor::MonitorPoint::VALID);
    validitySet.insert(carma::monitor::MonitorPoint::INVALID_NO_DATA);
    ValiditySetFilter vsf(validitySet);
    set<const MonitorPointFilter * > mpfs;
    for(; iter!=t2n.end() && count<100; iter++) {
        tagID = iter->first;
        MonitorPointSelector *s = new MonitorPointSelector(tagID,dbc_);
        //s->selectOnlyTagID();
        mps.insert(s);
        tagID2mps[tagID] = s;
        MonitorPointFilter *mpf = new MonitorPointFilter(tagID,FRAME_AVG,&vsf,
                                                         dbc_);
        mpfs.insert(mpf);
        count++;
    }
    carma::util::StopWatch sw;
#ifdef CARMA_JUNK
    MonitorDataQueryManager mdqm(mps,&trFilter3,dbc_);
    CPTRACE( util::Trace::TRACEALL, "agg types " 
             << setToString(mdqm.getAggregateDataTypes()));
    vector<string> setUpQueries = mdqm.getSetUpQueries();
    for(int i=0; i<setUpQueries.size(); i++) {
        CPTRACE( util::Trace::TRACEALL, setUpQueries[i]); 
        //cout << StringUtils::replace(setUpQueries[i],"--",";--") << ";" 
        //<< endl;
    }
    map<int,string> nrsQueries = mdqm.getNarrowRSQueries();
    map<int,string>::const_iterator miter = nrsQueries.begin();
    /*
    for( ; miter!=nrsQueries.end(); miter++) {
        cout << miter->second << ";" << endl;
    }
    */
    sw.start();
    map<int,Table> rs = mdqm.execNarrowQueries();
    sw.stop();
    cout << "Time to execute narrow queries " << sw.getElapsedTime() << endl;
    CPTRACE( util::Trace::TRACEALL, "Number of result sets " << rs.size()); 
    CPTRACE( util::Trace::TRACEALL, "Time to run queries " 
             << sw.getElapsedTime() << " seconds"); 
    map<int,Table>::const_iterator riter = rs.begin();
    /*
    for( ; riter != rs.end(); riter++) {
        const MonitorPointSelector *ss = tagID2mps[riter->first];
        if(riter == rs.begin() || ss->getMonitorConfiguration().getDataType() 
           == DATATYPE_ENUMERATION) {
            cout << riter->second << endl;
        }
    }
    */
#endif
    //MonitorDataQueryManager mdqm2(mps,&trFilter3,mpfs,dbc_);
    MonitorDataQueryManager mdqm2(mps,&trFilter3,dbc_);
    CPTRACE( util::Trace::TRACEALL, "agg types " 
             << setToString(mdqm2.getAggregateDataTypes()));
    vector<string> setUpQueries2 = mdqm2.getSetUpQueries();
    for(unsigned int i=0; i<setUpQueries2.size(); i++) {
        CPTRACE( util::Trace::TRACEALL, setUpQueries2[i]); 
        //cout << StringUtils::replace(setUpQueries2[i],"--",";--") << ";" 
        //<< endl;
    }
    map<int,string> nrsQueries2 = mdqm2.getNarrowRSQueries();
    map<int,string>::const_iterator miter2 = nrsQueries2.begin();
    /*
    for( ; miter2!=nrsQueries2.end(); miter2++) {
        cout << miter2->second << ";" << endl;
    }
    */
    sw.start();
    map<int,Table> rs2 = mdqm2.execNarrowQueries();
    sw.stop();
    //cout << "Time to execute narrow queries " << sw.getElapsedTime() << endl;
    CPTRACE( util::Trace::TRACEALL, "Number of result sets " << rs2.size()); 
    CPTRACE( util::Trace::TRACEALL, "Time to run queries " 
             << sw.getElapsedTime() << " seconds"); 
    map<int,Table>::const_iterator riter2 = rs2.begin();
    /*
    for( ; riter2 != rs2.end(); riter2++) {
        cout << riter2->second << endl;
    }
    */
    sw.start();
    Table wideRS = mdqm2.execWideQuery();
    sw.stop();
    cout << "rowCount " << wideRS.rowCount() << endl;
    cout << "columnCount " << wideRS.columnCount() << endl;
    //cout << carma::util::vectorToString(wideRS.getColumnNames()) << endl;
    cout << "Time to run queries " << sw.getElapsedTime() << " seconds" 
         << endl;
    uint ii = wideRS.columnCount()/2;
    cout << "Column " << ii << "'s name " << wideRS.getColumnNames()[ii] 
         << endl;
    /*
    uint jj = wideRS.rowCount()/2;
    cout << "Value in this column at row " << jj << " is " 
         << wideRS.getStringColumn(ii)[jj] << endl;
    */
    MPSelectorSet::const_iterator mpsiter = mps.begin();
    for (; mpsiter != mps.end(); mpsiter++) {
        delete *mpsiter;
    }
    set<const MonitorPointFilter * >::const_iterator mpfiter = mpfs.begin();
    for (; mpfiter != mpfs.end(); mpfiter++) {
        delete *mpfiter;
    }
}

void MonitorDataQueryManagerTest::setConffile (const std::string& conffile) {
    conffile_ = conffile;
}

void MonitorDataQueryManagerTest::tearDown() {   
    CPTRACE( util::Trace::TRACEALL, "in tearDown() complete");
    ResultsCache::closeCache();
    delete dbc_;
}


