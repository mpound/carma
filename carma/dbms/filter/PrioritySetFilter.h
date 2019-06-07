#ifndef CARMA_DBMS_PRIORITYSETFILTER_H
#define CARMA_DBMS_PRIORITYSETFILTER_H

/**
 * @file
 * class to represent an SQL query filter for a set of monitor point blanking
 * flags
 *
 * @author Original: Dave Mehringer
 * @version $Id: PrioritySetFilter.h,v 1.1 2005/01/25 20:24:13 dmehring Exp $
 *
 * $CarmaCopyright$
 *
 */

#include <string>
#include "carma/dbms/filter/SetFilter.h"
#include "carma/monitor/MonitorPoint.h"
#include "carma/dbms/Syslog2DBMSConversions.h"

namespace carma {
namespace dbms {

    /**
     * class to represent an SQL query filter for a set of log priorities
     * viz <code>priority IN (INFO, WARN)</code>
     */
class PrioritySetFilter : public SetFilter<carma::dbms::dbPriorityType> {

public:
    /**
     * create a BlankingFlagSetFilter by specifying a set of blanking flags
     * to be filtered
     * @param blankingFlags set of blanking flags to be filtered in the
     *        result set
     */
    PrioritySetFilter
        (const std::set<carma::dbms::dbPriorityType>& priorities);

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

#endif // CARMA_DBMS_PRIORITYSETFILTER_H

