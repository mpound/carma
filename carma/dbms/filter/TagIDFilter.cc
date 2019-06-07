/**
 * Implementation for the TagIDFilter class
 *
 * @author Original: Dave Mehringer
 *
 * $CarmaCopyright$
 *
 */
#include "carma/dbms/filter/TagIDFilter.h"

using namespace std;
using namespace carma::dbms;

TagIDFilter::TagIDFilter(const int& tagID) 
    : NumericFilter<int>(tagID, NumericFilter<int>::EQUAL_TO) {
    columnName_ = "tagID";
}

string TagIDFilter::name() const {
    return "TagIDFilter";
}
