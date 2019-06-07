#ifndef CARMA_DBMS_LOGMESSAGEFILTER_H
#define CARMA_DBMS_LOGMESSAGEFILTER_H

/**
 * @file
 * class to represent an SQL query log message filter (part of a WHERE 
 * clause) .
 *
 * @author Original: Dave Mehringer
 * @version $Id: LogMessageFilter.h,v 1.1 2005/02/10 01:25:05 dmehring Exp $
 *
 * $CarmaCopyright$
 *
 */

#include <string>
#include "carma/dbms/filter/MultiComponentFilter.h"
#include "carma/dbms/MonitorSystemAndDBMSRelationships.h"
#include "carma/dbms/Table.h"

namespace carma {
namespace dbms {

    /**
     * Represents an SQL query log message filter (part of a WHERE clause).
     * This class represents a filter on a set of log message attributes
     * @see carma::dbms::MessageFilter
     * @see carma::dbms::CallerFilter
     * @see carma::dbms::PriorityFilter
     * @see carma::dbms::PrioritySetFilter
     */
class LogMessagePointFilter : public MultiComponentFilter {

public:

    /**
     * Create a LogMessageFilter. The @p filter object which is passed to 
     * the constructor must have the following attributes:
     * <ul>
     * <li> It may or may not be NULL.
     * <li> It may contain 0 or 1 PrioritySetFilters or 0 or 1 PriorityFilters.
     * <li> It may contain 0 or 1 MessageFilters.
     * <li> It may contain 0 or 1 CallerFilters.
     * </ul>
     * If the @p filter parameter violates any of these rules, the constructor
     * will throw a <code>carma::util::IllegalArgumentException</code>
     * @param trFilter the time range filter
     * @param filter the filter with constraints described above to be ANDed
     *        with @p trFilter
     * @throws IllegalArgumentException if @p filter is NULL or 
     *         does not make sense given the monitor point data type and/or
     *         average type
     */
    LogMessageFilter(const carma::dbms::TimeRangeFilter& trFilter,
                     const carma::dbms::Filter * const filter);

    ~LogMessageFilter();

    /**
     * get the class name of this filter for log messages etc.
     * @return the class name of this filter
     */
    virtual std::string name() const;

protected:
    // disallow default constructor
    LogMessageFilter();

    void validateFilter_();

private:
    /**
     * prohibit copying to avoid pointer madness
     */
    LogMessageFilter(LogMessageFilter& rhs);
};
}}
#endif // CARMA_DBMS_LOGMESSAGEFILTER_H

