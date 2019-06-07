#ifndef CARMA_DBMS_ONECOMPONENTFILTER_H
#define CARMA_DBMS_ONECOMPONENTFILTER_H

/**
 * @file
 * abstract base class to represent a simple one component filter
 *
 * @author Original: Dave Mehringer
 * @version $Id: OneComponentFilter.h,v 1.6 2004/12/12 00:02:03 dmehring Exp $
 *
 * $CarmaCopyright$
 *
 */

#include <string>
#include "carma/dbms/filter/Filter.h"

namespace carma {
namespace dbms {

    /**
     * abstract base class to represent an SQL query one component
     * filter (part of a WHERE clause) .
     */
class OneComponentFilter : public Filter {

public:

    /**
     * return the column name associated with the filter
     * @return the column name associated with the filter
     */
    std::string getColumnName() const;

    /**
     * set the column name associated with the filter
     * @param columnName the column name associated with the filter
     */
    void setColumnName(const std::string& columnName);

    /**
     * get the class name for log messages etc.
     * @return the class name
     */
    virtual std::string name() const = 0;


protected:
    std::string columnName_;
    std::string fullyQualifiedColumnName_(const std::string& tableName,
                                          const std::string& columName) const;

};




}}

#endif // CARMA_DBMS_FILTERONECOMPONENT_H

