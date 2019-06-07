#ifndef CARMA_DBMS_TAGIDSETFILTER_H
#define CARMA_DBMS_TAGIDSETFILTER_H

/**
 * @file
 * class to represent an SQL query filter for a set of tagIDs
 *
 * @author Original: Dave Mehringer
 * @version $Id: TagIDSetFilter.h,v 1.5 2004/12/14 05:52:53 dmehring Exp $
 *
 * $CarmaCopyright$
 *
 */

#include <string>
#include "carma/dbms/filter/SetFilter.h"

namespace carma {
namespace dbms {

    /**
     * class to represent an SQL query filter for a set of tagIDs
     * viz "tagID IN (400,600,900,1245)"
     */
class TagIDSetFilter : public SetFilter<uint> {

public:
    TagIDSetFilter(const std::set<uint>& tagIDs);

    /**
     * get the class name for log messages etc.
     * @return the class name
     */
    virtual std::string name() const;
};



}}

#endif // CARMA_DBMS_FILTERTAGID_H

