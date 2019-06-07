/**
 * Implementation for the PriorityFilter class
 *
 * @author Original: Dave Mehringer
 * @version $Id: PriorityFilter.cc,v 1.1 2005/01/25 20:33:56 dmehring Exp $
 * $CarmaCopyright$
 *
 */
#include "carma/util/CommonExceptions.h"
#include "carma/dbms/filter/PriorityFilter.h"

using namespace std;
using namespace carma::dbms;

PriorityFilter::PriorityFilter
    (const dbPriorityType& value, const SearchType& searchType) 
        : NumericFilter<dbPriorityType>(value, searchType) {
}

string PriorityFilter::name() const {
    return "PriorityFilter";
}
