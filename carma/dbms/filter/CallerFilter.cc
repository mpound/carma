/**
 * @file
 * implementation of caller filter class.
 *
 * @author Original: Dave Mehringer
 * @version $Id: CallerFilter.cc,v 1.1 2005/01/25 21:47:35 dmehring Exp $
 *
 * $CarmaCopyright$
 *
 */

#include <sstream>
#include "carma/dbms/filter/CallerFilter.h"

using namespace carma::dbms;
using namespace std;

CallerFilter::CallerFilter(const string& value, const SearchType& searchType) 
    : StringFilter(value,searchType) {}

CallerFilter::~CallerFilter() {}

string CallerFilter::name() const {
    return "CallerFilter";
}

