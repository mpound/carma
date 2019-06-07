/**
 * Implementation for the TimeFilter class
 *
 * @author Original: Dave Mehringer
 * @version $Id: TimeFilter.cc,v 1.2 2004/12/10 01:25:23 dmehring Exp $
 * $CarmaCopyright$
 *
 */
#include "carma/util/CommonExceptions.h"
#include "carma/dbms/filter/TimeFilter.h"

using namespace std;
using namespace carma::dbms;

TimeFilter::TimeFilter
    (TimeColumnType colType, const int& value, const SearchType& searchType) 
        : NumericFilter<int>(value, searchType) {
    switch(colType) {
    case FRAMECOUNT:
        columnName_ = "frameCount";
        break;
    case INTEGRATIONID:
        columnName_ = "integrationID";
        break;
    default:
        stringstream emsg;
        emsg << "Unhandled colType value " << colType;
        throw CARMA_ERROR(emsg.str());
    }
}

string TimeFilter::name() const {
    return "TimeFilter";
}
