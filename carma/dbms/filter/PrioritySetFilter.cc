/**
 * Implementation for the PrioritySetFilter class
 *
 * @author Original: Dave Mehringer
 * @version $Id: PrioritySetFilter.cc,v 1.1 2005/01/25 20:24:13 dmehring Exp $
 *
 * $CarmaCopyright$
 *
 */
#
#include "carma/dbms/filter/PrioritySetFilter.h"

using namespace std;
using namespace carma::dbms;

PrioritySetFilter::PrioritySetFilter(const set<dbPriorityType>& priorities) 
        : SetFilter<dbPriorityType> (priorities, "priority") {
}

string PrioritySetFilter::toString(const std::string& tableName,
    const std::string& columnName) const {
    ostringstream ss;
    ss << fullyQualifiedColumnName_(tableName,columnName);
    if(values_.size() == 1) {
        ss << "=" << *values_.begin();
        return ss.str();
    }
    ss << " IN (";
    set<carma::dbms::dbPriorityType>::iterator iter 
        = values_.begin();
    int count = 0;
    int size = values_.size();
    for( ; iter != values_.end(); iter++) {
        ss << *iter;
        if(count < size-1) {
            ss << ", ";
        }
        count++;
    }
    ss << ")";
    return ss.str();
}

string PrioritySetFilter::name() const {
    return "PrioritySetFilter";
}
