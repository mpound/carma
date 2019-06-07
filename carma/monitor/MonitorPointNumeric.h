#ifndef CARMA_MONITOR_MONITORPOINTNUMERIC_H
#define CARMA_MONITOR_MONITORPOINTNUMERIC_H


/**
 * @file
 *
 * Abstract base class for all monitor points that can be represented
 * as a number.
 *
 * @author: Steve Scott
 *
 * $CarmaCopyright$
 *
 */

#include "carma/monitor/MonitorPoint.h"
#include "carma/monitor/MonitorPointAverageT.h"

namespace carma {
namespace monitor {


typedef MonitorPointAverageT< double > MonitorPointAverageNumeric;

/**
 *
 * Abstract base class for a numeric monitor point.
 * The number of samples per frame defaults to one.
 *
 */
class MonitorPointNumeric: public MonitorPoint {
public:


    /**
     * Destructor.
     */
    virtual ~MonitorPointNumeric() ;

    /**
     * Determines whether data should be interpreted as time series
     * or as an array (spectrum) when there are multiple samples per frame.
     * Only numeric types can be an array; this overrides the base method.
     * Default is time series.
     * @param timeSeries true if data is time series, false if array
     */
    void setTimeSeries(bool timeSeries);

    // Override base class
    bool operator==(const MonitorComponent& component) const;

    /**
     * Resets accumulators and sample count for computing averages.
     * @param average MonitorPointAverageNumeric holds data for computing
     *        average.
     * @see ::carma::monitor::MonitorPointAverageT
     */
    virtual void  resetAccumulator(MonitorPointAverageNumeric& average) const ;

    /**
     * Accumulates data from one monitor point sample (sample index == index)
     * belonging this monitor point.
     *
     * @param average MonitorPointAverageNumeric holds data for computing
     *        average.
     * @return enum MonitorPoint::VALIDITY validity flag of sample.
     * @see ::carma::monitor::MonitorPointAverageT
     */
    virtual enum MonitorPoint::VALIDITY accumulateSample
                       (MonitorPointAverageNumeric& average, int index) const ;

    /**
     * Accumulates data from monitor point samples belonging this monitor
     * point.
     * @param average MonitorPointAverageNumeric holds data for computing
     *        average.
     * @see ::carma::monitor::MonitorPointAverageT
     */
    virtual void   accumulate (MonitorPointAverageNumeric& average) const ;

    /**
     * Accumulates pre-calculated average value from current monitor point
     * for computing averages.
     *
     * @param accumulator MonitorPointAverageNumeric& will accumulate values over
     *        time for computing averages.
     * @return none
     */
    virtual void accumulateAverage (MonitorPointAverageNumeric& accumulator) ;

    /**
     * Computes average from accumulated data in MonitorPointAverageNumeric
     * object.
     * @param average MonitorPointAverageNumeric holds data for computing
     *        average.
     * @see ::carma::monitor::MonitorPointAverageT
     */
    double getAccumulatedAverageDouble
                             (const MonitorPointAverageNumeric& average) const;

    /**
     * Returns maximum value from accumulated data in
     * MonitorPointAverageNumeric object.
     * @param average MonitorPointAverageNumeric holds data for computing
     *        average.
     * @see ::carma::monitor::MonitorPointAverageT
     */
    double getMaxValueDouble (const MonitorPointAverageNumeric& average) const;

    /**
     * Returns minimum value from accumulated data in
     * MonitorPointAverageNumeric object.
     * @param average MonitorPointAverageNumeric holds data for computing
     *        average.
     * @see ::carma::monitor::MonitorPointAverageT
     */
    double getMinValueDouble (const MonitorPointAverageNumeric& average) const;

    /**
     * Returns sample value as a double
     * @param sampleIndex zero is first sample
     */
    virtual double getValueNumeric( int sampleIndex ) const = 0;

    /**
     * Returns average sample value as a double
     */
    virtual double getAveNumeric( ) const = 0;

protected:

    MonitorPointNumeric( const ::std::string & name,
                         MonitorValueType      valuetype,
                         MONITOR_POINT_TYPE    monitorPointType = MONITOR );

    // Also stores numValidSamples and aveValidity
    double computeFrameAverage( MonitorPointAverageNumeric & scratchAvg );

    VALIDITY getAveValidityNumeric( ) const;

    BLANKING_FLAGGING getBlankingFlaggingNumeric( ) const;

private:
    MonitorPointNumeric( );

    double              sum_;
    int                 nValidSamples_;
    VALIDITY            aveValidity_;
    BLANKING_FLAGGING   aveBlankingFlagging_;
};


}  // namespace carma::monitor
}  // namespace carma


inline void
carma::monitor::MonitorPointNumeric::setTimeSeries( const bool timeSeries )
{
    timeSeries_ = timeSeries;
}


inline double
carma::monitor::MonitorPointNumeric::getMaxValueDouble(
    const MonitorPointAverageNumeric & avg ) const
{
    return ((avg.getNumValidSamples() > 0) ? avg.getMaxValue() : 0.0);
}


inline double
carma::monitor::MonitorPointNumeric::getMinValueDouble(
    const MonitorPointAverageNumeric & avg ) const
{
    return ((avg.getNumValidSamples() > 0) ? avg.getMinValue() : 0.0);
}


inline carma::monitor::MonitorPoint::VALIDITY
carma::monitor::MonitorPointNumeric::getAveValidityNumeric( ) const
{
    return aveValidity_;
}


inline carma::monitor::MonitorPoint::BLANKING_FLAGGING
carma::monitor::MonitorPointNumeric::getBlankingFlaggingNumeric( ) const
{
    return aveBlankingFlagging_;
}


#endif
