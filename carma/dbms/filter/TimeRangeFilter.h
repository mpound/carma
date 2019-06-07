#ifndef CARMA_DBMS_TIMERANGEFILTER_H
#define CARMA_DBMS_TIMERANGEFILTER_H

/**
 * @file
 * class to represent an SQL query time range filter (part of a WHERE 
 * clause) .
 *
 * @author Original: Dave Mehringer
 * @version $Id: TimeRangeFilter.h,v 1.9 2004/12/17 21:45:09 dmehring Exp $
 *
 * $CarmaCopyright$
 *
 */

#include <string>
#include "carma/dbms/filter/MultiComponentFilter.h"
#include "carma/dbms/filter/TimeFilter.h"
#include "carma/dbms/MonitorSystemAndDBMSRelationships.h"
#include "carma/util/ErrorException.h"

namespace carma {
namespace dbms {

    /**
     * represents an SQL query time range filter (part of a WHERE clause
     * This class represents a (closed) time range filter of the form<br>
     * <code>testTime >[=] {minTime} AND testTime <[=] {maxTime}, </code><br>
     * where {minTime} and {maxTime} are fixed values passed as parameters
     * to the constructor.
     */
class TimeRangeFilter : public MultiComponentFilter {

public:

    /**
     * create a time range filter 
     * the portion of the WHERE clause represented by this filter will look
     * like <br>
     * <code> timeType <[=] max(time1,time2) 
     * AND timeType >[=] min(time1,time2)</code><br> where timeType is either
     * frameCount (for all average types) or integrationID (for astronomical
     * average types).
     * @param time1 first time endpoint
     * @param time2 second time endpoint
     * @param averageType the type of average to consider
     * @param includeTime1 include time1 in the range (adds an "=" to the 
     *        test)?
     * @param includeTime1 include time2 in the range (adds an "=" to the 
     *        test)?
     * @param colType the time identifier column type
     * @throws TimeFilterException if 
     *         colType==carma::dbms::TimeFilter::INTEGRATIONID 
     *         and (averageType == FRAME_AVG or MINUTE_AVG)
     */
    TimeRangeFilter(const int& time1, const int& time2, 
                    const carma::dbms::MonitorAverageType& averageType
                    =carma::dbms::FRAME_AVG, 
                    const bool& includeTime1=false,
                    const bool& includeTime2=false, 
                    const carma::dbms::TimeFilter::TimeColumnType colType
                    =carma::dbms::TimeFilter::FRAMECOUNT);

    
    ~TimeRangeFilter();

    /**
     * get the average type of the filter
     * @return the average type of the filter
     */
    carma::dbms::MonitorAverageType getAverageType() const;

    /**
     * get the children of this filter. The first element of the returned 
     * vector will be the TimeFilter representing the lower time
     * @return the children of this filter
     */
    std::vector<const carma::dbms::TimeFilter *> getChildren() const;


    /**
     * get the class name for log messages etc.
     * @return the class name
     */
    virtual std::string name() const;

    /**
     * return the column name associated with the filter (frameCount or 
     * integrationID depeding on the average type)
     * @return the column name associated with the filter
     */
    std::string getColumnName() const;

protected:
    // disallow default constructor
    TimeRangeFilter();
    carma::dbms::MonitorAverageType averageType_;
    carma::dbms::TimeFilter::TimeColumnType colType_;
private:
    /**
     * prohibit copying to avoid pointer madness
     */
    TimeRangeFilter(TimeRangeFilter& rhs);
};


class TimeRangeFilterException : public carma::util::ErrorException {
public:
    TimeRangeFilterException (const char* msg, 
                              const char* fileName = __FILE__, 
                              const int lineNum = __LINE__) 
        : carma::util::ErrorException (msg, fileName, lineNum) {};

    TimeRangeFilterException (const std::string msg, 
                              const char* fileName = __FILE__, 
                              const int lineNum = __LINE__) 
        : carma::util::ErrorException (msg.c_str(), fileName, lineNum) {};
};
}}
#endif // CARMA_DBMS_TIMERANGEFILTER_H

