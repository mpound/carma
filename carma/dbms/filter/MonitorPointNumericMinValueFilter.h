#ifndef CARMA_DBMS_MONITORPOINTNUMERICMINVALUEFILTER_H
#define CARMA_DBMS_MONITORPOINTNUMERICMINVALUEFILTER_H

/**
 * @file
 * Monitor point umeric min value filter template class.
 *
 * @author Original: Dave Mehringer
 * @version $Id: MonitorPointNumericMinValueFilter.h,v 1.1 2004/12/10 01:19:42 dmehring Exp $
 *
 * $CarmaCopyright$
 *
 */

#include "carma/dbms/filter/NumericFilter.h"

namespace carma {
namespace dbms {

/**
 * template to represent a one component filter for numeric searches on
 * min value columns in monitor point data tables. 
 */
template <class T> class MonitorPointNumericMinValueFilter 
    : public carma::dbms::NumericFilter<T> {
public:

    /**
     * constuctor
     * @param value test value for the search
     * @param searchType the search type to perform relative to the test value
     */
    MonitorPointNumericMinValueFilter
        (const T& value, 
         const typename carma::dbms::NumericFilter<T>::SearchType& searchType) 
        : carma::dbms::NumericFilter<T>(value, searchType) {}

    /**
     * destructor, derived classes may want to override
     */
    virtual ~MonitorPointNumericMinValueFilter() {}

    /**
     * get the class name for log messages etc.
     * @return the class name
     */
    virtual std::string name() const {
        return "MonitorPointNumericMinValueFilter";
    }
};


}}

const std::string filterName_ = "MonitorPointNumericMinValueFilter";

#endif // CARMA_DBMS_MONITORPOINTNUMERICMINVALUEFILTER_H

