/**
 * Implementation for the TimeRangeFilter class
 *
 * @author Original: Dave Mehringer
 * @version $Id: TimeRangeFilter.cc,v 1.10 2008/04/23 21:38:34 abeard Exp $
 *
 * $CarmaCopyright$
 *
 */
#include "carma/dbms/filter/TimeRangeFilter.h"
#include "carma/dbms/ColumnNames.h"
#include "carma/util/Trace.h"

#include <assert.h>

using namespace ::std;
using namespace carma::dbms;

TimeRangeFilter::TimeRangeFilter(
    const int &                      time1,
    const int &                      time2,
    const MonitorAverageType &       averageType,
    const bool &                     includeTime1,
    const bool &                     includeTime2, 
    const TimeFilter::TimeColumnType colType ) :
MultiComponentFilter(AND),
averageType_(averageType),
colType_(colType) {
    if(colType == TimeFilter::INTEGRATIONID 
       && (averageType == FRAME_AVG || averageType == MINUTE_AVG)) {
        string emsg = "Inconsistent colType (INTEGRATIONID) and ";
        emsg += "averageType (" + carma::dbms::toString(averageType) + ")";
        throw CARMA_EXCEPTION(TimeRangeFilterException, emsg);
    }
    int minTime,maxTime;
    bool includeMinTime, includeMaxTime;
    if(time1 < time2) {
        minTime = time1;
        includeMinTime = includeTime1;
        maxTime = time2;
        includeMaxTime = includeTime2;
    } else {
        minTime = time2;
        includeMinTime = includeTime2;
        maxTime = time1;
        includeMaxTime = includeTime1;
    }
    assert(minTime <= maxTime);
    NumericFilter<int>::SearchType searchType = (includeMinTime) 
        ? NumericFilter<int>::GREATER_THAN_OR_EQUAL_TO 
        : NumericFilter<int>::GREATER_THAN;
    child1_ = new TimeFilter(colType_,minTime,searchType);
    searchType = (includeMaxTime) ? NumericFilter<int>::LESS_THAN_OR_EQUAL_TO 
        : NumericFilter<int>::LESS_THAN;
    child2_ = new TimeFilter(colType_,maxTime,searchType);
}


TimeRangeFilter::~TimeRangeFilter() {
    delete child1_;
    child1_ = 0;
    delete child2_;
    child2_ = 0;
}

MonitorAverageType TimeRangeFilter::getAverageType() const {
    return averageType_;
}

vector<const TimeFilter *> TimeRangeFilter::getChildren() const {
    vector<const TimeFilter *> v;
    const TimeFilter *c1 = dynamic_cast<const TimeFilter * >(child1_);
    assert(c1 != NULL);
    const TimeFilter *c2 = dynamic_cast<const TimeFilter * >(child2_);
    assert(c2 != NULL);
    v.push_back(c1);
    v.push_back(c2);
    return v;
}

string TimeRangeFilter::name() const {
    return "TimeRangeFilter";
}

string TimeRangeFilter::getColumnName() const {
    return (averageType_ == FRAME_AVG || averageType_ == MINUTE_AVG) 
        ? carma::dbms::getColumnName(COLUMN_FRAMECOUNT) 
        : carma::dbms::getColumnName(COLUMN_INTEGRATIONID);
}
