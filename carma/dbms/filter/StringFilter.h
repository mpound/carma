#ifndef CARMA_DBMS_STRINGFILTER_H
#define CARMA_DBMS_STRINGFILTER_H

/**
 * @file
 * String filter class.
 *
 * @author Original: Dave Mehringer
 * @version $Id: StringFilter.h,v 1.3 2004/12/11 08:53:41 dmehring Exp $
 *
 * $CarmaCopyright$
 *
 */

#include <string>
#include <sstream>
#include "carma/dbms/filter/OneComponentFilter.h"
#include "carma/util/CommonExceptions.h"

namespace carma {
namespace dbms {

/**
 * template to represent a one component string filter. Internally,
 * it consists of a value and a flag  where the value is the string test
 * value (can include standard SQL wildcards such as "*") and the flag 
 * indicates if the type of search to be performed relative to the value 
 * (LIKE, ==);
 */
class StringFilter : public OneComponentFilter {

public:

    /**
     * type of search to perform
     * <ul>
     * <li> EQUAL_TO equal to (=)
     * <li> LIKE like (LIKE)
     * </ul>
     */
    typedef enum {
        EQUAL_TO,
        LIKE
    }
    SearchType;

    /**
     * constuctor
     * @param value test value for the filter
     * @param searchType the search type to perform relative to the test value
     */
    StringFilter(const std::string& value, const SearchType& searchType);

    /**
     * destructor, derived classes may want to override
     */
    virtual ~StringFilter();

    /**
     * get the value of the search
     * @return the value of the search
     */
    std::string getValue() const;

    /**
     * get the search type
     * @return the search type
     */
    SearchType getSearchType() const;

    //std::string toString() const;

    /**
     * string representation of the filter.  This is not necessarily how
     * it will appear in the where clause
     * @param tableName the table name to use
     * @param columnName the column name to use
     * @return string representation of the filter
     */
    std::string toString(const std::string& tableName, 
                         const std::string& columnName) const;

    /**
     * get the class name for log messages etc.
     * @return the class name
     */
    virtual std::string name() const;

protected:
    std::string value_;
    SearchType searchType_;
};


}}

#endif // CARMA_DBMS_STRINGFILTER_H

