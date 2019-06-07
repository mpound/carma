/**
 * Implementation for the MonitorDataIndex class
 *
 * @author: Dave Mehringer
 *
 * $CarmaCopyright$
 *
 */
#include "carma/dbms/MonitorDataIndex.h"

using namespace std;
using namespace carma::dbms;

MonitorDataIndex::MonitorDataIndex(const MonitorAggregateType& aggType, 
                                   const MonitorAverageType& avgType, 
                                   const MonitorDataAreaType& areaType) {
    aggType_ = aggType;
    avgType_ = avgType;
    areaType_ = areaType;
    numericIndex_ = 100*aggType + 10*avgType + areaType;
}

MonitorDataIndex::~MonitorDataIndex() {}

