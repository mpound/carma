/**
 * @file
 * implementation of log message filter class.
 *
 * @author Original: Dave Mehringer
 * @version $Id: MessageFilter.cc,v 1.1 2005/01/25 21:46:55 dmehring Exp $
 *
 * $CarmaCopyright$
 *
 */

#include <sstream>
#include "carma/dbms/filter/MessageFilter.h"

using namespace carma::dbms;
using namespace std;

MessageFilter::MessageFilter
    (const string& value, const SearchType& searchType) 
    : StringFilter(value,searchType) {}

MessageFilter::~MessageFilter() {}

string MessageFilter::name() const {
    return "MessageFilter";
}

