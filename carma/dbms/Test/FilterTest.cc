/** 
 * @file
 * CppUnit test fixture for carma::dbms::DBConnection
 * @author Dave Mehringer
 * @version $Id: FilterTest.cc,v 1.6 2004/12/17 07:21:46 dmehring Exp $
 *
 * $CarmaCopyright$
 */
#include <iostream>
#include <vector>
#include "FilterTest.h"
#include "carma/dbms/filter/BlankingFlagSetFilter.h"
#include "carma/dbms/filter/MonitorPointFilter.h"
#include "carma/dbms/filter/TimeRangeFilter.h"
#include "carma/dbms/filter/TagIDFilter.h"
#include "carma/dbms/filter/ValiditySetFilter.h"

using namespace carma::dbms;
using namespace carma::monitor;
using namespace CppUnit;
using namespace std;

void FilterTest::setUp() {
}

void FilterTest::tearDown() {   
}

void FilterTest::twoComponentFilterTests() {
    TimeRangeFilter trf1(0,1);
    TimeRangeFilter trf2(500,1000);
    MultiComponentFilter tcf(&trf1,&trf2,MultiComponentFilter::AND);
    vector<const TimeFilter*> c1 = trf1.getChildren();
    vector<const TimeFilter*> c2 = trf2.getChildren();
    cout << endl;
    cout << &trf1 << " ";
    cout << c1[0] << " ";
    cout << c1[1] << " ";
    cout << &trf2 << " ";
    cout << c2[0] << " ";
    cout << c2[1] << " ";
    cout << endl;
    vector<const Filter*> descendants;
    tcf.getDescendants(&descendants);
    CPPUNIT_ASSERT( descendants.size() == 6 );
    vector<const Filter*>::const_iterator iter = descendants.begin();
    for( ; iter != descendants.end(); iter++ ) {
        cout << *iter << " ";
    }
    cout << endl;
    vector<const OneComponentFilter * > oneComponentDescendants;
    tcf.getOneComponentFilterDescendants(&oneComponentDescendants);
    cout << "number of one component descendants is "
         << oneComponentDescendants.size() << endl;
    CPPUNIT_ASSERT( oneComponentDescendants.size() == 4 );
}

void FilterTest::monitorPointFilterTests() {
    set<MonitorPoint::BLANKING_FLAGGING> bfs;
    bfs.insert(MonitorPoint::BLANKED);
    BlankingFlagSetFilter bff(bfs);
    MonitorPointFilter mpf(1,FRAME_AVG,&bff,DATATYPE_STRING);
    cout << mpf.toString() << endl;
    try {
        MultiComponentFilter mcf(&bff,&bff,MultiComponentFilter::OR);
        MonitorPointFilter mpf2(1,FRAME_AVG,&mcf,DATATYPE_STRING);
        CPPUNIT_ASSERT(false);
    } catch (const carma::util::IllegalArgumentException& exc) {}
    set<MonitorPoint::VALIDITY> vs;
    vs.insert(MonitorPoint::VALID_ERROR);
    vs.insert(MonitorPoint::VALID);
    ValiditySetFilter vsf(vs);
    MultiComponentFilter mcf2(&bff,&vsf,MultiComponentFilter::OR);
    MonitorPointFilter mpf2(1,FRAME_AVG,&mcf2,DATATYPE_FLOAT);
    cout << mpf2.toString("","mycolum.") << endl;
}
