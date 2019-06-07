#ifndef CARMA_DBMS_MONITORPOINTNUMERICVALUEFILTER_H
#define CARMA_DBMS_MONITORPOINTNUMERICVALUEFILTER_H

/**
 * @file
 * Monitor point umeric value filter template class.
 *
 * @author Original: Dave Mehringer
 * @version $Id: MonitorPointNumericValueFilter.h,v 1.1 2004/12/10 01:20:14 dmehring Exp $
 *
 * $CarmaCopyright$
 *
 */

#include "carma/dbms/filter/NumericFilter.h"

namespace carma {
namespace dbms {

/**
 * template to represent a one component filter for numeric searches on
 * value columns in monitor point data tables. 
 */
template <class T> class MonitorPointNumericValueFilter 
    : public carma::dbms::NumericFilter<T> {
public:

    /**
     * constuctor
     * @param value test value for the search
     * @param searchType the search type to perform relative to the test value
     */
    MonitorPointNumericValueFilter
        (const T& value, 
         const typename carma::dbms::NumericFilter<T>::SearchType& searchType) 
        : NumericFilter<T>(value, searchType) {}

    /**
     * destructor, derived classes may want to override
     */
    virtual ~MonitorPointNumericValueFilter() {}
};


}}

#endif // CARMA_DBMS_MONITORPOINTNUMERICVALUEFILTER_H

