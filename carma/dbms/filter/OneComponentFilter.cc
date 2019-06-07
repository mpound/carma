/**
 * @file
 * implementation of OneComponentFilter
 *
 * @author Original: Dave Mehringer
 * @version $Id: OneComponentFilter.cc,v 1.3 2004/12/12 00:02:03 dmehring Exp $
 *
 * $CarmaCopyright$
 *
 */

#include "carma/dbms/filter/OneComponentFilter.h"

using namespace std;
using namespace carma::dbms;

string OneComponentFilter::getColumnName() const { return columnName_; }

void OneComponentFilter::setColumnName(const string& columnName) {
    columnName_ = columnName; 
}

/*
string OneComponentFilter::toString() const {
    return toString(columnName_);
}
*/

string OneComponentFilter::fullyQualifiedColumnName_
    (const string& tableName, const string& columnName) const {
    string tname = (tableName == "") ? "" : tableName + ".";
    string cname = (columnName == "") ? columnName_ : columnName;
    return tname + cname;
}


