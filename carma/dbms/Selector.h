#ifndef CARMA_DBMS_SELECTOR_H
#define CARMA_DBMS_SELECTOR_H

/**
 * @file
 * Selector class.
 *
 * @author: Dave Mehringer
 * @version $Id: Selector.h,v 1.4 2008/04/23 21:41:00 abeard Exp $ *
 *
 * $CarmaCopyright$
 *
 */

#include <string>

namespace carma {
namespace dbms {


/**
 * Abstract base class to represent an SQL selection (portion of SELECT clause)
 */
class Selector {

public:

    /**
     * destructor
     */
    virtual ~Selector();

    /**
     * string representation of this selector
     */
    virtual std::string toString() const = 0;

    /**
     * set the option table name or aliased table name for this selection
     */
    void setTable(const std::string& tableName);

    /**
     * get the pointer to the table name. if no table name has been set,
     * a NULL pointer is returned.
     */
     std::string * getTable() const; 


protected:
     Selector();

     std::string *tableName_;


};
}}

#endif // CARMA_DBMS_SELECTOR_H

