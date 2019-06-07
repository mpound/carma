/**
 * Implementation for the ValiditySetFilter class
 *
 * @author Original: Dave Mehringer
 * @version $Id: ValiditySetFilter.cc,v 1.4 2004/12/17 07:07:14 dmehring Exp $
 *
 * $CarmaCopyright$
 *
 */
#
#include "carma/dbms/filter/ValiditySetFilter.h"
#include "carma/dbms/MonitorData2DBMSConversions.h"

using namespace std;
using namespace carma::dbms;

ValiditySetFilter::ValiditySetFilter
    (const set<carma::monitor::MonitorPoint::VALIDITY>& validities) 
        : SetFilter<carma::monitor::MonitorPoint::VALIDITY>
    (validities, "validityID") {
}


string ValiditySetFilter::toString(const std::string& tableName,
                                   const std::string& columnName) const {
    ostringstream ss;
    ss << fullyQualifiedColumnName_(tableName,columnName);
    int size = values_.size();
    if(size == 1) {
        ss << " = " << validity2DB(*values_.begin());
        return ss.str();
    }
    ss << " IN (";
    set<carma::monitor::MonitorPoint::VALIDITY>::iterator iter 
        = values_.begin();
    int count = 0;
    for( ; iter != values_.end(); iter++) {
        ss << validity2DB(*iter);
        if(count < size-1) {
            ss << ", ";
        }
        count++;
    }
    ss << ")";
    return ss.str();
}

string ValiditySetFilter::name() const {
    return "ValidititySetFilter";
}
