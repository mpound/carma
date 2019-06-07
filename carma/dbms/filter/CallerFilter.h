#ifndef CARMA_DBMS_CALLERFILTER_H
#define CARMA_DBMS_CALLERFILTER_H

/**
 * @file
 * caller filter class.
 *
 * @author Original: Dave Mehringer
 * @version $Id: CallerFilter.h,v 1.1 2005/01/25 21:47:35 dmehring Exp $
 *
 * $CarmaCopyright$
 *
 */

#include <string>
#include <sstream>
#include "carma/dbms/filter/StringFilter.h"

namespace carma {
namespace dbms {

/**
 * template to represent a one component caller filter. Internally,
 * it consists of a value and a flag  where the value is the string test
 * value (can include standard SQL wildcards such as "*") and the flag 
 * indicates if the type of search to be performed relative to the value 
 * (LIKE, =);
 */
class CallerFilter : public StringFilter {

public:

    /**
     * constuctor
     * @param value test value for the filter
     * @param searchType the search type to perform relative to the test value
     */
    CallerFilter(const std::string& value, const SearchType& searchType);

    /**
     * destructor, derived classes may want to override
     */
    virtual ~CallerFilter();

    /**
     * get the class name for log messages etc.
     * @return the class name
     */
    virtual std::string name() const;
};


}}

#endif // CARMA_DBMS_STRINGFILTER_H

