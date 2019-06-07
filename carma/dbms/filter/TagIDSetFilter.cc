/**
 * Implementation for the TagIDFilter class
 *
 * @author Original: Dave Mehringer
 * @version $Id: TagIDSetFilter.cc,v 1.5 2004/12/14 05:52:53 dmehring Exp $
 *
 * $CarmaCopyright$
 *
 */
#include "carma/dbms/filter/TagIDSetFilter.h"

using namespace std;
using namespace carma::dbms;

TagIDSetFilter::TagIDSetFilter(const set<uint>& tagIDs) 
    : SetFilter<uint>(tagIDs, "tagID") {
}

string TagIDSetFilter::name() const {
    return "TagIDSetFilter";
}

