#ifndef CARMA_DBMS_PRIORITYFILTER_H
#define CARMA_DBMS_PRIORITYFILTER_H

/**
 * @file
 * class to represent a simple log priority filter
 *
 * @author Original: Dave Mehringer
 * @version $Id: PriorityFilter.h,v 1.1 2005/01/25 20:33:56 dmehring Exp $
 *
 * $CarmaCopyright$
 *
 */

#include <string>
#include "carma/dbms/filter/NumericFilter.h"
#include "carma/dbms/Syslog2DBMSConversions.h"

namespace carma {
namespace dbms {

    /**
     * class to represent an SQL query log priority 
     * filter (part of a WHERE clause) .
     */
class PriorityFilter : public NumericFilter<dbPriorityType> {

public:

    PriorityFilter(const dbPriorityType& value, 
               const carma::dbms::NumericFilter<dbPriorityType>::SearchType& searchType);

    /**
     * get the class name for log messages etc.
     * @return the class name
     */
    virtual std::string name() const;

};
}}

#endif // CARMA_DBMS_PRIORITYFILTER_H

