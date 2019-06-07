#ifndef CARMA_DBMS_TIMEFILTER_H
#define CARMA_DBMS_TIMEFILTER_H

/**
 * @file
 * class to represent a simple time filter
 *
 * @author Original: Dave Mehringer
 * @version $Id: TimeFilter.h,v 1.4 2004/12/10 01:25:23 dmehring Exp $
 *
 * $CarmaCopyright$
 *
 */

#include <string>
#include "carma/dbms/filter/NumericFilter.h"

namespace carma {
namespace dbms {

    /**
     * class to represent an SQL query one component
     * filter (part of a WHERE clause) .
     */
class TimeFilter : public NumericFilter<int> {

public:
    typedef enum {
        FRAMECOUNT,
        INTEGRATIONID
    } TimeColumnType;

    TimeFilter(TimeColumnType colType, const int& value, 
               const carma::dbms::NumericFilter<int>::SearchType& searchType);

    /**
     * get the class name for log messages etc.
     * @return the class name
     */
    virtual std::string name() const;

};
}}

#endif // CARMA_DBMS_TIMEFILTER_H

