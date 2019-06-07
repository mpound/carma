#ifndef CARMA_DBMS_MONITORPOINTFILTER_H
#define CARMA_DBMS_MONITORPOINTFILTER_H

/**
 * @file
 * class to represent an SQL query monitor point filter (part of a WHERE 
 * clause) .
 *
 * @author Original: Dave Mehringer
 * @version $Id: MonitorPointFilter.h,v 1.9 2004/12/14 06:01:52 dmehring Exp $
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

    class DBConnection;

    /**
     * Represents an SQL query monitor point filter (part of a WHERE clause).
     * This class represents a filter on a set of monitor point attributes
     * for a specified tagID. In addition to the tagID, the other portions of
     * the filter which may be specified are a set of validity values, blanking
     * flag values, and monitor point values.  The constructor takes a tagID
     * from which it determines the data type of the monitor point.  It also
     * takes a MonitorAverageType value.  With these two pieces of information
     * it checks to make sure that the values specified (if any) in the value
     * filter, the maxValue filter, and the minValue filter make sense.  For
     * example, it doesn't make sense to specify a maxValue or minValue filter
     * for a FRAME_AVG search, since these columns are not included in frame
     * monitor data tables. Another example is that it makes no since to 
     * specify a NumericFilter for the value filter if the monitor point
     * in question has string values.
     * @see carma::dbms::BlankingFlagSetFilter
     * @see carma::dbms::ValiditySetFilter
     * @see carma::dbms::MonitorPointNumericValueFilter
     * @see carma::dbms::MonitorPointNumericMaxValueFilter
     * @see carma::dbms::MonitorPointNumericMinValueFilter
     * @see carma::dbms::StringFilter
     */
class MonitorPointFilter : public MultiComponentFilter {

public:

    /**
     * Create a MonitorPointFilter. The @p filter object which is passed to 
     * the constructor must have the following attributes:
     * <ul>
     * <li> It may not be NULL.
     * <li> It may contain 0 or 1 BlankingFlagSetFilters.
     * <li> It may contain 0 or 1 ValiditySetFilters.
     * <li> It may contain any number of MonitorPointNumericValueFilters, 
     * assuming the monitor point in question (as determined by @p tagID) is a
     * numeric type.
     * <li> It may contain any number of MonitorPointNumericMaxValueFilters, 
     * assuming the monitor point in question (as determined by @p tagID) is a 
     * numeric type and the average type is not FRAME_AVG.
     * <li> It may contain any number of MonitorPointNumericMinValueFilters, 
     * assuming the monitor point in question (as determined by @p tagID) is a 
     * numeric type and the average type is not FRAME_AVG.
     * <li> It may contain any number of StringFilters, if
     * the monitor point in question (as determined by @p tagID) has string
     * values.
     * </ul>
     * If the @p filter parameter violates any of these rules, the constructor
     * will throw a <code>carma::util::IllegalArgumentException</code>
     * @param tagID the tagID of the monitor point to filter
     * @param averageType the average type which will be used in this query
     * @param filter the filter with which to AND the tagID filter
     * @param dbc the DBConnection object to use to get the monitor point's
     *        data type from the database.  If the data type does not need
     *        to be determined (ie because the supplied filter does not 
     *        necessitate it, or if this result has already been cahced by
     *        another object), this object is not used and hence can be NULL.
     *        Otherwise, it must be set.
     * @throws IllegalArgumentException if @p filter is NULL or 
     *         does not make sense given the monitor point data type and/or
     *         average type
     */
    MonitorPointFilter
        (const uint& tagID, const carma::dbms::MonitorAverageType& averageType,
         const carma::dbms::Filter * const filter, 
         const carma::dbms::DBConnection * const dbc = NULL);

    /**
     * Create a MonitorPointFilter. The @p filter object which is passed to
     * the constructor must have the following attributes:
     * <ul>
     * <li> It may not be NULL.
     * <li> It may contain 0 or 1 BlankingFlagSetFilters.
     * <li> It may contain 0 or 1 ValiditySetFilters.
     * <li> It may contain any number of MonitorPointNumericValueFilters, 
     * assuming the monitor point in question (as determined by @p tagID) is a
     * numeric type.
     * <li> It may contain any number of MonitorPointNumericMaxValueFilters, 
     * assuming the monitor point in question (as determined by @p tagID) is a 
     * numeric type and the average type is not FRAME_AVG.
     * <li> It may contain any number of MonitorPointNumericMinValueFilters, 
     * assuming the monitor point in question (as determined by @p tagID) is a 
     * numeric type and the average type is not FRAME_AVG.
     * <li> It may contain any number of StringFilters, if
     * the monitor point in question (as determined by @p tagID) has string
     * values.
     * </ul>
     * If the @p filter parameter violates any of these rules, the constructor
     * will throw a <code>carma::util::IllegalArgumentException</code>
     * @param tagID the tagID of the monitor point to filter
     * @param averageType the average type which will be used in this query
     * @param filter the filter with which to AND the tagID filter
     * @param dataType the data type of the monitor point with the specified
     *        tagID.  It had better be right (or use the alternate constructor
     *        to have it determined).
     * @throws IllegalArgumentException if @p filter is NULL or 
     *         does not make sense given the monitor point data type and/or
     *         average type
     */
    MonitorPointFilter
        (const int& tagID, const carma::dbms::MonitorAverageType& averageType,
         const carma::dbms::Filter * const filter, 
         const carma::dbms::MonitorPointDataType& dataType);

    ~MonitorPointFilter();

    /**
     * get the average type of the filter
     * @return the average type of the filter
     */
    carma::dbms::MonitorAverageType getAverageType() const;

    /**
     * get the class name of this filter for log messages etc.
     * @return the class name of this filter
     */
    virtual std::string name() const;

    /**
     * get the data type
     * @return the data type
     */
    carma::dbms::MonitorPointDataType getDataType() const;

    /**
     * get the tagID of this filter
     * @return the tagID of this filter
     */
    uint getTagID() const;

protected:
    uint tagID_;
    const carma::dbms::DBConnection *dbc_;
    // disallow default constructor
    MonitorPointFilter();
    carma::dbms::MonitorAverageType averageType_;
    static std::map<int,carma::dbms::MonitorPointDataType> tagIDToDataType_;
    carma::dbms::MonitorPointDataType dataType_;
    bool dataTypeHasBeenDetermined_;

    void validateFilter_();

    void initializeTagIDToDataTypeIfNecessary_() const;

    void makeTagID2DataTypeMap_(const carma::dbms::Table& table) const;

    carma::dbms::MonitorPointDataType determineDataType_();

    void init_();

private:
    /**
     * prohibit copying to avoid pointer madness
     */
    MonitorPointFilter(MonitorPointFilter& rhs);
};
}}
#endif // CARMA_DBMS_MONITORPOINTFILTER_H

