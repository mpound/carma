/**
 * Implementation for the BlankingFlagSetFilter class
 *
 * @author Original: Dave Mehringer
 * @version $Id: BlankingFlagSetFilter.cc,v 1.3 2004/12/11 08:53:41 dmehring Exp $
 *
 * $CarmaCopyright$
 *
 */
#
#include "carma/dbms/filter/BlankingFlagSetFilter.h"
#include "carma/dbms/MonitorData2DBMSConversions.h"

using namespace std;
using namespace carma::dbms;

BlankingFlagSetFilter::BlankingFlagSetFilter
    (const set<carma::monitor::MonitorPoint::BLANKING_FLAGGING>& 
     blankingFlags) 
        : SetFilter<carma::monitor::MonitorPoint::BLANKING_FLAGGING>
    (blankingFlags, "blankingFlag") {
}

string BlankingFlagSetFilter::toString(const std::string& tableName,
                                       const std::string& columnName) const {
    ostringstream ss;
    ss << fullyQualifiedColumnName_(tableName,columnName);
    if(values_.size() == 1) {
        ss << "=" << blankingFlagging2DB(*values_.begin());
        return ss.str();
    }
    ss << " IN (";
    set<carma::monitor::MonitorPoint::BLANKING_FLAGGING>::iterator iter 
        = values_.begin();
    int count = 0;
    int size = values_.size();
    for( ; iter != values_.end(); iter++) {
        ss << blankingFlagging2DB(*iter);
        if(count < size-1) {
            ss << ", ";
        }
        count++;
    }
    ss << ")";
    return ss.str();
}

string BlankingFlagSetFilter::name() const {
    return "BlankingFlagSetFilter";
}
