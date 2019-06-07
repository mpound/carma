#ifndef CARMA_DBMS_VALIDITYSETFILTER_H
#define CARMA_DBMS_VALIDITYSETFILTER_H

/**
 * @file
 * class to represent an SQL query filter for a set of monitor point validities
 *
 * @author Original: Dave Mehringer
 * @version $Id: ValiditySetFilter.h,v 1.4 2004/12/17 07:07:14 dmehring Exp $
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
     * class to represent an SQL query filter for a set of validities
     * viz <code>validity IN (VALID,VALID_GOOD)</code>
     */
class ValiditySetFilter 
    : public SetFilter<carma::monitor::MonitorPoint::VALIDITY> {

public:
    ValiditySetFilter
        (const std::set<carma::monitor::MonitorPoint::VALIDITY>& validities);


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
     * get the class name for use in log messages etc
     * @return the class name
     */
    virtual std::string name() const;
    
};



}}

#endif // CARMA_DBMS_VALIDITYSETFILTER_H
