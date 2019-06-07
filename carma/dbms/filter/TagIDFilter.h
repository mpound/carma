#ifndef CARMA_DBMS_TAGIDFILTER_H
#define CARMA_DBMS_TAGIDFILTER_H

/**
 * @file
 * class to represent a simple tagID
 *
 * @author Original: Dave Mehringer
 * @version $Id: TagIDFilter.h,v 1.3 2004/12/10 01:25:23 dmehring Exp $
 * $CarmaCopyright$
 *
 */

#include <string>
#include "carma/dbms/filter/NumericFilter.h"

namespace carma {
namespace dbms {

    /**
     * class to represent an SQL query filter for a tagID (viz tagID==value)
     */
class TagIDFilter : public NumericFilter<int> {

public:
    TagIDFilter(const int& tagID);

    /**
     * get the class name for log messages etc.
     * @return the class name
     */
    virtual std::string name() const;
};

}}

#endif // CARMA_DBMS_FILTERTAGID_H

