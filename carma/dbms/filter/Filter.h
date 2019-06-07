#ifndef CARMA_DBMS_FILTER_H
#define CARMA_DBMS_FILTER_H

/**
 * @file
 * abstract base class to represent an SQL query filter (part of a WHERE 
 * clause) .
 *
 * @author Original: Dave Mehringer
 * @version $Id: Filter.h,v 1.5 2005/08/29 20:25:42 tcosta Exp $
 *
 * $CarmaCopyright$
 *
 */

#include <string>

namespace carma {
namespace dbms {

    /**
     * abstract base class to represent an SQL query filter (part of a WHERE 
     * clause) .
     */
class Filter {

public:

    virtual ~Filter( ) {  }

    /**
     * string representation of the filter.  This is not necessarily how
     * it will appear in the where clause
     * @return string representation of the filter
     */
    //virtual std::string toString() const = 0;

    /**
     * the name of the filter
     */
    virtual std::string name() const = 0;

    /**
     * string representation of the filter.  This is not necessarily how
     * it will appear in the where clause
     * @param tableName the table name to use
     * @param columnName the column name to use
     * @return string representation of the filter
     */
    virtual std::string toString(const std::string& tableName = "",
                                 const std::string& columnName = "") const = 0;
};
}}

#endif // CARMA_DBMS_FILTER_H

