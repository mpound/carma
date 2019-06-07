#ifndef CARMA_DBMS_SETFILTER_H
#define CARMA_DBMS_SETFILTER_H

/**
 * @file
 * set filter template class.  
 *
 * @author Original: Dave Mehringer
 * @version $Id: SetFilter.h,v 1.8 2004/12/14 05:51:13 dmehring Exp $
 *
 * $CarmaCopyright$
 *
 */

#include <string>
#include <sstream>
#include <set>
#include "carma/dbms/filter/OneComponentFilter.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/StringUtils.h"

namespace carma {
namespace dbms {

/**
 * template to represent a one component set search filter (viz. 
 * tagID IN (4,20,9876,235)
 */
template <class T> class SetFilter : public OneComponentFilter {

public:

    /**
     * constuctor
     * @param values set of values to test for in the SQL "IN" clause
     */
    SetFilter(const std::set<T>& values, const std::string& columnName) 
        : values_(values) {
        if(values.size() == 0) {
            std::string emsg = "Number of elements in values set must be ";
            emsg += "greater than 0";
            throw CARMA_EXCEPTION(carma::util::IllegalArgumentException, emsg);
        }
        columnName_ = columnName;
    }

    /**
     * destructor, derived classes may want to override
     */
    virtual ~SetFilter() {}

    std::set<T> getValues() const { return values_; }

    /**
     * string representation of the filter.  This is not necessarily how
     * it will appear in the where clause
     * @param tableName the table name to use
     * @param columnName the column name to use
     * @return string representation of the filter
     */
    virtual std::string toString(const std::string& tableName, 
                                 const std::string& columnName) const {
        std::ostringstream ss;
        ss << fullyQualifiedColumnName_(tableName,columnName);
        ss << " IN (" << carma::util::setToString(values_) << ")";
        return ss.str();
    }

    /**
     * get the class name for use in log messages etc
     * @return the class name
     */
    virtual std::string name() const {
        return "SetFilter<>";
    }

protected:
    std::set<T> values_;
};

}}

#endif // CARMA_DBMS_SETFILTER_H

