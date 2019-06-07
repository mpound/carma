/**
 * @file
 * implementation of string filter class.
 *
 * @author Original: Dave Mehringer
 * @version $Id: StringFilter.cc,v 1.3 2004/12/11 08:53:41 dmehring Exp $
 *
 * $CarmaCopyright$
 *
 */

#include <sstream>
#include "carma/dbms/filter/StringFilter.h"
#include "carma/util/CommonExceptions.h"

using namespace carma::dbms;
using namespace std;

StringFilter::StringFilter(const string& value, const SearchType& searchType) 
    : value_(value), searchType_(searchType) {}

StringFilter::~StringFilter() {}

string StringFilter::getValue() const { return value_; }

StringFilter::SearchType StringFilter::getSearchType() const { 
    return searchType_; 
}

string StringFilter::toString(const string& tableName, 
                              const string& columnName) const {
    std::stringstream ss;
    ss << fullyQualifiedColumnName_(tableName,columnName);
    switch(searchType_) {
    case EQUAL_TO:
        ss << " = ";
        break;
    case LIKE:
        ss << " LIKE ";
    default:
        std::stringstream emsg;
        emsg << "Unhandled searchType value " << searchType_;
        throw CARMA_ERROR(emsg.str());
    }
    ss << "'" << value_ << "'";
    return ss.str();
}

string StringFilter::name() const {
    return "StringFilter";
}

