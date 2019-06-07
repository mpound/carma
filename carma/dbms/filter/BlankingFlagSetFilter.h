#ifndef CARMA_DBMS_BLANKINGFLAGSETFILTER_H
#define CARMA_DBMS_BLANKINGFLAGSETFILTER_H

/**
 * @file
 * class to represent an SQL query filter for a set of monitor point blanking
 * flags
 *
 * @author Original: Dave Mehringer
 * @version $Id: BlankingFlagSetFilter.h,v 1.3 2004/12/11 08:53:41 dmehring Exp $
 *
 * $CarmaCopyright$
 *
 */

#include <string>
#include "carma/dbms/filter/SetFilter.h"
#include "carma/monitor/MonitorPoint.h"

namespace carma {
namespace dbms {

    /**
     * class to represent an SQL query filter for a set of blanking flags
     * viz <code>blankingFlag IN (UNDETERMINED, OK)</code>
     */
class BlankingFlagSetFilter 
    : public SetFilter<carma::monitor::MonitorPoint::BLANKING_FLAGGING> {

public:
    /**
     * create a BlankingFlagSetFilter by specifying a set of blanking flags
     * to be filtered
     * @param blankingFlags set of blanking flags to be filtered in the
     *        result set
     */
    BlankingFlagSetFilter
        (const std::set<carma::monitor::MonitorPoint::BLANKING_FLAGGING>& 
         blankingFlags);

    /**
     * string representation of the filter.  This is not necessarily how
     * it will appear in the where clause
     * @param tableName the table name to use
     * @param columnName the column name to use
     * @return string representation of the filter
     */
    virtual std::string toString(const std::string& tableName = "",
                                 const std::string& columnName = "") const;
    /**
     * get the class name for log messages etc.
     * @return the class name
     */
    virtual std::string name() const;
};



}}

#endif // CARMA_DBMS_BLANKINGFLAGSETFILTER_H

