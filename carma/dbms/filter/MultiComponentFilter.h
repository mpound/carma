#ifndef CARMA_DBMS_MULTICOMPONENTFILTER_H
#define CARMA_DBMS_MULTICOMPONENTFILTER_H

/**
 * @file
 * class to represent an SQL query multi-component filter (part of a WHERE 
 * clause) .
 *
 * @author Original: Dave Mehringer
 * @version $Id: MultiComponentFilter.h,v 1.10 2004/12/17 07:09:15 dmehring Exp $
 *
 * $CarmaCopyright$
 *
 */

#include <string>
#include <vector>
#include "carma/dbms/filter/OneComponentFilter.h"

namespace carma {
namespace dbms {

    /**
     * represents an SQL query multi-component filter (part of a WHERE clause) 
     */
class MultiComponentFilter : public Filter {

public:

    /**
     * describes how to combine two filters
     * <ul>
     * <li> AND combine using AND
     * <li> OR combine using OR
     * </ul>
     */
    typedef enum {
        AND,
        OR
    } Conjunction;

    /**
     * create a multi-component filter by combining two other filters
     * @param child1 the first filter in the union
     * @param child2 the second filter in the union
     * @param conj how the filters are to be united
     * @throws carma::util::IllegalArgumentException if at least one of the
     *         input filters is NULL
     */
    MultiComponentFilter(const Filter * const child1, 
                         const Filter * const child2, const Conjunction &conj);
    
    /**
     * destructor
     */
    ~MultiComponentFilter();
    
    /**
     * string representation of the filter.  This is not necessarily how
     * it will appear in the where clause
     * @return string representation of the filter
     */
    //virtual std::string toString() const;

    /**
     * string representation of the filter.  This is not necessarily how
     * it will appear in the where clause
     * @param tableName the table name to use
     * @param columnNamePrepender the string to prepend to the one component
     *        descendents column names
     * @return string representation of the filter
     */
    virtual std::string toString(const std::string& tableName = "",
                                 const std::string& columnNamePrepender = "") 
        const;

    /**
     * get the children of this filter. The first element of the returned 
     * vector will be <code>child1</code> and the second will be 
     * <code>child2</code> as defined in the constructor
     * @return the children of this filter
     */
    std::vector<const carma::dbms::Filter *> getChildren() const;

    /**
     * Return all the descendents of this filter. The order in which the 
     * descendents occur is not gauranteed
     * @return all the descendents of this filter
     */
    std::vector<const carma::dbms::Filter* > getDescendants() const;

    /**
     * get all the descendents of this filter and put them in the supplied 
     * vector, the order in which the descendents occur is not gauranteed
     * @param descendants [out] the vector into which to place the descendants
     * @return all the descendents of this filter
     */
    void getDescendants(std::vector<const carma::dbms::Filter* > *descendants) 
        const;

    /**
     * get the OneComponentFilter descendants (leaf nodes) of this filter and 
     * put them in the supplied vector. There is no gaurantee made on their
     * ordering
     * @param descendants [out] the vector into which to place the 
     *        OneComponentFilter descendants
     */
    void getOneComponentFilterDescendants
        (std::vector<const carma::dbms::OneComponentFilter* > *descendants) 
        const;

    /**
     * get the class name for log messages etc.
     * @return the class name
     */
    virtual std::string name() const;

protected:
           
    // disallow default constructor
    MultiComponentFilter() {}
    // for use only by derived classes
    MultiComponentFilter(const Conjunction &conj);

    const Filter *child1_;
    const Filter *child2_;
    Conjunction conj_;

private:
    /**
     * prohibit copying to avoid pointer madness
     */
    MultiComponentFilter(MultiComponentFilter& rhs);

};


}}

#endif // CARMA_DBMS_MULTICOMPONENTFILTER_H

