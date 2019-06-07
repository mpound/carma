#ifndef CARMA_MONITOR_MONITORPOINTSPECIALIZATIONS_H
#define CARMA_MONITOR_MONITORPOINTSPECIALIZATIONS_H


/**
 * @file
 *
 * Classes that provide the pecializations of monitor points
 * and sense poiints for different datatypes.
 * These classes all extend from MonitorPoint and provide an
 * additional get/setValue() method. Because the getValue returns
 * different datatypes, we need a class for each one. Templates
 * may have been a more elegant way to solve this problem.
 *
 * Grouping all these classes together in one file is done because
 * they tend to repeat the same code patterns over and over, with just
 * slight variations because of the datatype.
 * It is easier to maintain all these classes, say add a method,
 * it they are all in one file. I don't like having multiple classes
 * in one file but this seems to be an exceptional case.
 *
 * @author: Steve Scott
 *
 * $CarmaCopyright$
 *
 */


#include "carma/monitor/MonitorPointAverageT.h"
#include "carma/monitor/MonitorPointNumeric.h"


namespace carma {
namespace monitor {

class MonitorPointThreshold;

typedef MonitorPointAverageT< long > MonitorPointAverageBool;
typedef MonitorPointAverageT< int > MonitorPointAverageEnum;
typedef MonitorPointAverageT< long > MonitorPointAverageSerialNo;
typedef MonitorPointAverageT< ::std::string > MonitorPointAverageString;
typedef MonitorPointAverageT< ::std::complex< float > >
        MonitorPointAverageComplex;


/**
 * Character value monitor point.
 */
class MonitorPointChar: public MonitorPointNumeric {
public:
    typedef MonitorPointAverageNumeric AccumType;
    typedef char AccumReportType;

    /**
     * Constructor.
     * @param name monitor point name
     */
    MonitorPointChar(const std::string& name,
        MONITOR_POINT_TYPE monitorPointType = MONITOR);


    /**
     * Destructor.
     * Nothing fancy here - the dynamic storage is managed elsewhere.
     */
    virtual ~MonitorPointChar() {};

    /**
     * Set the value.
     * @param c data value
     * @param sampleIndex sample index (0 is first sample)
     * @see getValue
     */
    void setValue(const char c, int sampleIndex = 0) const;

    /**
     * Get the value.
     * @param sampleIndex sample index (0 is first sample)
     * @see setValue
     */
    char getValue(int sampleIndex = 0) const;

    /**
     * Get the average value.
     */
    char getAve() const;

    /*
     * Converts value to a string with both character value and hex
     * @param sampleIndex sample index (0 is first sample)
     */
    virtual std::string getValueToString(int sampleIndex = 0) const;

    /*
     * Converts average to a string with both character value and hex
     * @param sampleIndex sample index (0 is first sample)
     */
    virtual std::string getAverageToString() const;

    /**
     * Sets THRESHOLD_LOW_WARN_VALUE for this monitor point.
     *
     * @param threshold char character value to use for setting threshold.
     */
    void setWarnLowDefault (const char threshold);

    /**
     * Sets THRESHOLD_HIGH_WARN_VALUE for this monitor point.
     *
     * @param threshold char character value to use for setting threshold.
     */
    void setWarnHighDefault (const char threshold);

    /**
     * Sets THRESHOLD_LOW_ERROR_VALUE for this monitor point.
     *
     * @param threshold char character value to use for setting threshold.
     */
    void setErrorLowDefault (const char threshold);

    /**
     * Sets THRESHOLD_HIGH_ERROR_VALUE for this monitor point.
     *
     * @param threshold char character value to use for setting threshold.
     */
    void setErrorHighDefault (const char threshold);

    /**
     * Create and return string representation for average accumulated in
     * accumulator object.
     *
     * @param accum will accumulate
     *        values over time for computing averages.
     * @return std::string average value as string.
     */
    virtual std::string getAccumulatedAverageAsString (const AccumType & accum) const;

    /**
     * Compute average from accumulated values in accumulator object.
     *
     * @param accum will accumulate
     *        values over time for computing averages.
     * @return char computed average value.
     */
    virtual AccumReportType getAccumulatedAverage (const AccumType & accum) const;

    /**
     * Returns maximum value from accumulated data in accumulator object.
     * @param accum holds data for computing average.
     * @return char max value accumulated in the accumulator object.
     * @see ::carma::monitor::MonitorPointAverageT
     */
    virtual AccumReportType getMaxValue (const AccumType & accum) const;

    /**
     * Returns minimum value from accumulated data in accumulator object
     * @param accum holds data for computing average.
     * @return char min value accumulated in the accumulator object.
     * @see ::carma::monitor::MonitorPointAverageT
     */
    virtual AccumReportType getMinValue (const AccumType & accum) const;

    // Already documented in base class
    virtual void updateFrameAverage( ScratchAverages & scratchAvgs );

    // Already documented in base class
    virtual double getValueNumeric( int sampleIndex ) const;

    // Already documented in base class
    virtual double getAveNumeric( ) const;

    /**
     * Method to return validity for a sample using the range parameters
     * in the MonitorPointThreshold object, <pre>threshold</pre>.
     *
     * @param threshold MonitorPointThreshold object with the threshold
     *        range settings for this monitor point.
     * @param charVal char value of sample to be compared against range
     *        settings.
     * @return MonitorPoint::VALIDITY appropriate VALIDITY flag for sample.
     */
    virtual enum MonitorPoint::VALIDITY evaluateSampleTolerance
                  (const MonitorPointThreshold& threshold, char charVal) const;

    // documented in base class
    virtual void evaluateTolerance (const MonitorPointThreshold& threshold);

private:
    /*
     * Converts to a string with both character value and hex
     * @param value
     */
    std::string getValueToString(char c) const;
};


/**
 * Byte value (unsigned char) monitor point.
 */
class MonitorPointByte: public MonitorPointNumeric {
public:
    typedef MonitorPointAverageNumeric AccumType;
    typedef unsigned char AccumReportType;

    /**
     * Constructor.
     * @param name monitor point name
     */
    MonitorPointByte(const std::string& name,
        MONITOR_POINT_TYPE monitorPointType = MONITOR);


    /**
     * Destructor.
     * Nothing fancy here - the dynamic storage is managed elsewhere.
     */
    virtual ~MonitorPointByte() {};

    /**
     * Set the value.
     * @param c data value
     * @param sampleIndex sample index (0 is first sample)
     * @see getValue
     */
    void setValue(const unsigned char c, int sampleIndex = 0) const;

    /**
     * Get the value.
     * @param sampleIndex sample index (0 is first sample)
     * @see getValue
     */
    unsigned char getValue(int sampleIndex = 0) const;

    /**
     * Get the average value.
     */
    unsigned char getAve() const;

    // Already documented in base class
    virtual std::string getValueToString(int sampleIndex = 0) const;

    /**
     * Sets THRESHOLD_LOW_WARN_VALUE for this monitor point.
     *
     * @param threshold unsigned char character value to use for setting threshold.
     */
    void setWarnLowDefault (const unsigned char threshold);

    /**
     * Sets THRESHOLD_HIGH_WARN_VALUE for this monitor point.
     *
     * @param threshold unsigned char character value to use for setting threshold.
     */
    void setWarnHighDefault (const unsigned char threshold);

    /**
     * Sets THRESHOLD_LOW_ERROR_VALUE for this monitor point.
     *
     * @param threshold unsigned char character value to use for setting threshold.
     */
    void setErrorLowDefault (const unsigned char threshold);

    /**
     * Sets THRESHOLD_HIGH_ERROR_VALUE for this monitor point.
     *
     * @param threshold unsigned char character value to use for setting threshold.
     */
    void setErrorHighDefault (const unsigned char threshold);

    // Already documented in base class
    virtual std::string getAverageToString() const;

    /**
     * Create and return string representation for average accumulated in
     * accumulator object.
     *
     * @param accum will accumulate
     *        values over time for computing averages.
     * @return std::string average value as string.
     */
    virtual std::string getAccumulatedAverageAsString (const AccumType & accum) const;

    /**
     * Compute average from accumulated values in accumulator object.
     *
     * @param accum will accumulate
     *        values over time for computing averages.
     * @return unsigned char computed average value.
     */
    virtual AccumReportType getAccumulatedAverage (const AccumType & accum) const;

    /**
     * Returns maximum value from accumulated data in accumulator object.
     * @param accum holds data for computing average.
     * @return unsigned char max value accumulated in the accumulator object
     * @see ::carma::monitor::MonitorPointAverageT
     */
    virtual AccumReportType getMaxValue (const AccumType & accum) const;

    /**
     * Returns minimum value from accumulated data in accumulator object.
     * @param accum holds data for computing average.
     * @return unsigned char min value accumulated in the accumulator object.
     * @see ::carma::monitor::MonitorPointAverageT
     */
    virtual AccumReportType getMinValue (const AccumType & accum) const;

    // Already documented in base class
    virtual void updateFrameAverage( ScratchAverages & scratchAvgs );

    // Already documented in base class
    virtual double getValueNumeric( int sampleIndex ) const;

    // Already documented in base class
    virtual double getAveNumeric( ) const;

    /**
     * Method to return validity for a sample using the range parameters
     * in the MonitorPointThreshold object, <pre>threshold</pre>.
     *
     * @param threshold MonitorPointThreshold object with the threshold
     *        range settings for this monitor point.
     * @param byteVal unsigned char value of sample to be compared against
     *        range settings.
     * @return MonitorPoint::VALIDITY appropriate VALIDITY flag for sample.
     */
    virtual enum MonitorPoint::VALIDITY evaluateSampleTolerance
         (const MonitorPointThreshold& threshold, unsigned char byteVal) const;

    // documented in base class
    virtual void evaluateTolerance (const MonitorPointThreshold& threshold);

};

/**
 * Short integer value monitor point (2 bytes).
 */
class MonitorPointShort: public MonitorPointNumeric {
public:
    typedef MonitorPointAverageNumeric AccumType;
    typedef short AccumReportType;

    /**
     * Constructor.
     * @param name monitor point name
     */
    MonitorPointShort(const std::string& name,
        MONITOR_POINT_TYPE monitorPointType = MONITOR);


    /**
     * Destructor.
     * Nothing fancy here - the dynamic storage is managed elsewhere.
     */
    ~MonitorPointShort() {};

    /**
     * Set the value.
     * @param i data value
     * @param sampleIndex sample index (0 is first sample)
     * @see getValue
     */
    void setValue(const short i, int sampleIndex = 0) const;

    /**
     * Get the value.
     * @param sampleIndex sample index (0 is first sample)
     * @see setValue
     */
    short getValue(int sampleIndex = 0) const;

    /**
     * Get the average value.
     */
    short getAve() const;

    // Already documented in base class
    virtual std::string getValueToString(int sampleIndex = 0) const;

    /**
     * Sets THRESHOLD_LOW_WARN_VALUE for this monitor point.
     *
     * @param threshold short short value to use for setting threshold.
     */
    void setWarnLowDefault (const short threshold);

    /**
     * Sets THRESHOLD_HIGH_WARN_VALUE for this monitor point.
     *
     * @param threshold short short value to use for setting threshold.
     */
    void setWarnHighDefault (const short threshold);

    /**
     * Sets THRESHOLD_LOW_ERROR_VALUE for this monitor point.
     *
     * @param threshold short short value to use for setting threshold.
     */
    void setErrorLowDefault (const short threshold);

    /**
     * Sets THRESHOLD_HIGH_ERROR_VALUE for this monitor point.
     *
     * @param threshold short short value to use for setting threshold.
     */
    void setErrorHighDefault (const short threshold);

    // Already documented in base class
    virtual std::string getAverageToString() const;

    /**
     * Create and return string representation for average accumulated in
     * accumulator object.
     *
     * @param accum will accumulate
     *        values over time for computing averages.
     * @return std::string average value as string.
     */
    virtual std::string getAccumulatedAverageAsString (const AccumType & accum) const;

    /**
     * Compute average from accumulated values in accumulator object.
     *
     * @param accum will accumulate
     *        values over time for computing averages.
     * @return short computed average value.
     */
    virtual AccumReportType getAccumulatedAverage (const AccumType & accum) const;

    /**
     * Returns maximum value from accumulated data in accumulator object.
     * @param accum holds data for computing average.
     * @return short max value accumulated in the accumulator object.
     * @see ::carma::monitor::MonitorPointAverageT
     */
    virtual AccumReportType getMaxValue (const AccumType & accum) const;

    /**
     * Returns minimum value from accumulated data in accumulator object.
     * @param accum holds data for computing average.
     * @return short min value accumulated in the accumulator object.
     * @see ::carma::monitor::MonitorPointAverageT
     */
    virtual AccumReportType getMinValue (const AccumType & accum) const;

    // Already documented in base class
    virtual void updateFrameAverage( ScratchAverages & scratchAvgs );

    // Already documented in base class
    virtual double getValueNumeric( int sampleIndex ) const;

    // Already documented in base class
    virtual double getAveNumeric( ) const;

    /**
     * Method to return validity for a sample using the range parameters
     * in the MonitorPointThreshold object, <pre>threshold</pre>.
     *
     * @param threshold MonitorPointThreshold object with the threshold
     *        range settings for this monitor point.
     * @param shortVal short value of sample to be compared against
     *        range settings.
     * @return MonitorPoint::VALIDITY appropriate VALIDITY flag for sample.
     */
    virtual enum MonitorPoint::VALIDITY evaluateSampleTolerance
                (const MonitorPointThreshold& threshold, short shortVal) const;

    // documented in base class
    virtual void evaluateTolerance (const MonitorPointThreshold& threshold);


};

/**
 * Long integer value monitor point (4 bytes).
 */
class MonitorPointInt: public MonitorPointNumeric {
public:
    typedef MonitorPointAverageNumeric AccumType;
    typedef long AccumReportType;

    /**
     * Constructor.
     * @param name monitor point name
     */
    MonitorPointInt(const std::string& name,
        MONITOR_POINT_TYPE monitorPointType = MONITOR);


    /**
     * Destructor.
     * Nothing fancy here - the dynamic storage is managed elsewhere.
     */
    ~MonitorPointInt() {};

    /**
     * Set the value.
     * @param i data value
     * @param sampleIndex sample index (0 is first sample)
     * @see getValue
     */
    void setValue(long i, int sampleIndex = 0) const;

    /**
     * Get the value.
     * @param sampleIndex sample index (0 is first sample)
     * @see setValue
     */
    long getValue(int sampleIndex = 0) const;

    /**
     * Get the average value.
     */
    long getAve() const;

    // Already documented in base class
    virtual std::string getValueToString(int sampleIndex = 0) const;

    /**
     * Sets THRESHOLD_LOW_WARN_VALUE for this monitor point.
     *
     * @param threshold long long value to use for setting threshold.
     */
    void setWarnLowDefault (const long threshold);

    /**
     * Sets THRESHOLD_HIGH_WARN_VALUE for this monitor point.
     *
     * @param threshold long long value to use for setting threshold.
     */
    void setWarnHighDefault (const long threshold);

    /**
     * Sets THRESHOLD_LOW_ERROR_VALUE for this monitor point.
     *
     * @param threshold long long value to use for setting threshold.
     */
    void setErrorLowDefault (const long threshold);

    /**
     * Sets THRESHOLD_HIGH_ERROR_VALUE for this monitor point.
     *
     * @param threshold long long value to use for setting threshold.
     */
    void setErrorHighDefault (const long threshold);

    // Already documented in base class
    virtual std::string getAverageToString() const;

    /**
     * Create and return string representation for average accumulated in
     * accumulator object.
     *
     * @param accum will accumulate
     *        values over time for computing averages.
     * @return std::string average value as string.
     */
    virtual std::string getAccumulatedAverageAsString (const AccumType & accum) const;

    /**
     * Compute average from accumulated values in accumulator
     * object.
     *
     * @param accum will accumulate
     *        values over time for computing averages.
     * @return long computed average value.
     */
    virtual AccumReportType getAccumulatedAverage (const AccumType & accum) const;

    /**
     * Returns maximum value from accumulated data in accumulator object.
     * @param accum holds data for computing average.
     * @return long max value accumulated in the accumulator object.
     * @see ::carma::monitor::MonitorPointAverageT
     */
    virtual AccumReportType getMaxValue (const AccumType & accum) const;

    /**
     * Returns minimum value from accumulated data in accumulator object.
     * @param accum holds data for computing average.
     * @return long min value accumulated in the accumulator object.
     * @see ::carma::monitor::MonitorPointAverageT
     */
    virtual AccumReportType getMinValue (const AccumType & accum) const;

    // Already documented in base class
    virtual void updateFrameAverage( ScratchAverages & scratchAvgs );

    // Already documented in base class
    virtual double getValueNumeric( int sampleIndex ) const;

    // Already documented in base class
    virtual double getAveNumeric( ) const;

    /**
     * Method to return validity for a sample using the range parameters
     * in the MonitorPointThreshold object, <pre>threshold</pre>.
     *
     * @param threshold MonitorPointThreshold object with the threshold
     *        range settings for this monitor point.
     * @param longVal long value of sample to be compared against
     *        range settings.
     * @return MonitorPoint::VALIDITY appropriate VALIDITY flag for sample.
     */
    virtual enum MonitorPoint::VALIDITY evaluateSampleTolerance
                  (const MonitorPointThreshold& threshold, long longVal) const;

    // documented in base class
    virtual void evaluateTolerance (const MonitorPointThreshold& threshold);


};

/**
 * Boolean value monitor point.
 */

//! @brief Monitor point holding a boolean value
class MonitorPointBool: public MonitorPoint {
public:
    typedef MonitorPointAverageBool AccumType;
    typedef bool AccumReportType;

    /**
     * Constructor.
     * @param name monitor point name
     */
    MonitorPointBool(const std::string& name,
        MONITOR_POINT_TYPE monitorPointType = MONITOR);

    /**
     * Destructor.
     * Nothing fancy here - the dynamic storage is managed elsewhere.
     */
    virtual ~MonitorPointBool() {};

    /**
     * Set the value.
     * @param b data value
     * @param sampleIndex sample index (0 is first sample)
     * @see getValue
     */
    void setValue(const bool b, int sampleIndex = 0) const;

    /**
     * Get the value.
     * @param sampleIndex sample index (0 is first sample)
     * @see getValue
     */
    bool getValue(int sampleIndex = 0) const;

    /**
     * Get the average value.
     */
    virtual bool getAve() const;

    /**
     * Sets THRESHOLD_LOW_WARN_VALUE for this monitor point. Some
     * transformations required to store boolean value as a double,
     * which is the type for default threshold values. Only one
     * of error or warn thresholds may be set. Since there are only two
     * possible threshold values, it is assumed that one of them must be
     * legal, leaving the other to be either in a warning, or an error range.
     *
     * @param boolVal bool boolean value to use for setting threshold.
     */
    void setWarnLowDefault (bool boolVal);

    /**
     * Sets THRESHOLD_HIGH_WARN_VALUE for this monitor point. Some
     * transformations required to store boolean value as a double,
     * which is the type for default threshold values. Only one
     * of error or warn thresholds may be set. Since there are only two
     * possible threshold values, it is assumed that one of them must be
     * legal, leaving the other to be either in a warning, or an error range.
     *
     * @param boolVal bool boolean value to use for setting threshold.
     */
    void setWarnHighDefault (bool boolVal);

    /**
     * Sets THRESHOLD_LOW_ERROR_VALUE for this monitor point. Some
     * transformations required to store boolean value as a double,
     * which is the type for default threshold values. Only one
     * of error or warn thresholds may be set. Since there are only two
     * possible threshold values, it is assumed that one of them must be
     * legal, leaving the other to be either in a warning, or an error range.
     *
     * @param boolVal bool boolean value to use for setting threshold.
     */
    void setErrorLowDefault (bool boolVal);

    /**
     * Sets THRESHOLD_HIGH_ERROR_VALUE for this monitor point. Some
     * transformations required to store boolean value as a double,
     * which is the type for default threshold values. Only one
     * of error or warn thresholds may be set. Since there are only two
     * possible threshold values, it is assumed that one of them must be
     * legal, leaving the other to be either in a warning, or an error range.
     *
     * @param boolVal bool boolean value to use for setting threshold.
     */
    void setErrorHighDefault (bool boolVal);

    /**
     * Returns current THRESHOLD_LOW_WARN_VALUE for this monitor point.
     * If the threshold object is not set, then it returns the default value
     * if its set. Throws an ErrorException if threshold object and default
     * values are unset, or if the threshold object doesnt correspond to
     * this monitor point.
     *
     * @param threshold MonitorPointThreshold& threshold object corresponding
     *        to this monitor point, holding the values set for various
     *        threshold limits.
     * @return bool value of threshold if set.
     * @exception ErrorException if no threshold values have been set in the
     *            threshold object, and for defaults.
     */
    bool getWarnLow (const MonitorPointThreshold& threshold) const;

    /**
     * Returns current THRESHOLD_HIGH_WARN_VALUE for this monitor point.
     * If the threshold object is not set, then it returns the default value
     * if its set. Throws an ErrorException if threshold object and default
     * values are unset, or if the threshold object doesnt correspond to
     * this monitor point.
     *
     * @param threshold MonitorPointThreshold& threshold object corresponding
     *        to this monitor point, holding the values set for various
     *        threshold limits.
     * @return bool value of threshold if set.
     * @exception ErrorException if no threshold values have been set in the
     *            threshold object, and for defaults.
     */
    bool getWarnHigh (const MonitorPointThreshold& threshold) const;

    /**
     * Returns current THRESHOLD_LOW_ERROR_VALUE for this monitor point.
     * If the threshold object is not set, then it returns the default value
     * if its set. Throws an ErrorException if threshold object and default
     * values are unset, or if the threshold object doesnt correspond to
     * this monitor point.
     *
     * @param threshold MonitorPointThreshold& threshold object corresponding
     *        to this monitor point, holding the values set for various
     *        threshold limits.
     * @return bool value of threshold if set.
     * @exception ErrorException if no threshold values have been set in the
     *            threshold object, and for defaults.
     */
    bool getErrorLow (const MonitorPointThreshold& threshold) const;

    /**
     * Returns current THRESHOLD_HIGH_ERROR_VALUE for this monitor point.
     * If the threshold object is not set, then it returns the default value
     * if its set. Throws an ErrorException if threshold object and default
     * values are unset, or if the threshold object doesnt correspond to
     * this monitor point.
     *
     * @param threshold MonitorPointThreshold& threshold object corresponding
     *        to this monitor point, holding the values set for various
     *        threshold limits.
     * @return bool value of threshold if set.
     * @exception ErrorException if no threshold values have been set in the
     *            threshold object, and for defaults.
     */
    bool getErrorHigh (const MonitorPointThreshold& threshold) const;

    /**
     * Reset accumulator values from for computing averages.
     *
     * @param accum will accumulate values over
     *        time for computing averages.
     * @return none
     */
    virtual void resetAccumulator (AccumType & accum) const;

    /**
     * Accumulates data from one monitor point sample (sample index == index)
     * belonging this monitor point.
     *
     * @param accum holds data for computing average.
     * @return enum MonitorPoint::VALIDITY validity flag of sample.
     * @see ::carma::monitor::MonitorPointAverageT
     */
    virtual enum MonitorPoint::VALIDITY accumulateSample
                       (AccumType & accum, int index) const;

    /**
     * Accumulate values from current monitor point for computing averages.
     *
     * @param accum will accumulate values over
     *        time for computing averages.
     * @return none
     */
    virtual void accumulate (AccumType & accum) const;

    /**
     * Accumulates pre-calculated average value from current monitor point
     * for computing averages.
     *
     * @param accum will accumulate values over
     *        time for computing averages.
     * @return none
     */
    virtual void accumulateAverage (AccumType & accum);

    /**
     * Create and return string representation for average accumulated in
     * accumulator object.
     *
     * @param accum will accumulate
     *        values over time for computing averages.
     * @return std::string average value as string.
     */
    virtual std::string getAccumulatedAverageAsString (const AccumType & accum) const;

    /**
     * Compute average from accumulated values in accumulator object.
     *
     * @param accum will accumulate values over
     *        time for computing averages.
     * @return bool computed average value.
     */
    virtual AccumReportType getAccumulatedAverage (const AccumType & accum) const;

    /**
     * Returns maximum value from accumulated data in accumulator object.
     * @param accum holds data for computing average.
     * @return bool max value accumulated in the accumulator object.
     * @see ::carma::monitor::MonitorPointAverageT
     */
    virtual AccumReportType getMaxValue (const AccumType & accum) const;

    /**
     * Returns minimum value from accumulated data in accumulator object.
     * @param accum holds data for computing average.
     * @return bool min value accumulated in the accumulator object.
     * @see ::carma::monitor::MonitorPointAverageT
     */
    virtual AccumReportType getMinValue (const AccumType & accum) const;

    // Already documented in base class
    virtual std::string getValueToString(int sampleIndex = 0) const;

    // Already documented in base class
    virtual std::string getAverageToString() const;

    // Already documented in base class
    virtual void updateFrameAverage( ScratchAverages & scratchAvgs );

    /**
     * Method to return validity for a sample using the range parameters
     * in the MonitorPointThreshold object, <pre>threshold</pre>.
     *
     * @param threshold MonitorPointThreshold object with the threshold
     *        range settings for this monitor point.
     * @param boolVal bool value of sample to be compared against
     *        range settings.
     * @return MonitorPoint::VALIDITY appropriate VALIDITY flag for sample.
     */
    virtual enum MonitorPoint::VALIDITY evaluateSampleTolerance
                  (const MonitorPointThreshold& threshold, bool boolVal) const;

    // documented in base class
    virtual void evaluateTolerance (const MonitorPointThreshold& threshold);

  private:

    int  nValidSamples_;
};


/**
 * Abstract base class for any enumerated constants, like state machines.
 * Enumerated monitor point types should not have more than 32 constants.
 * Enum monitor types that are thresholded can be associated with at most
 * 32 consecutive threshold values. The getValueToString() should return
 * the enumeration as a string. The default width will be the length of
 * the longest string.
 *
 * Extend from this class and provide:
 * <ol>
 *  <li> a constructor that correctly sets the number of enumeration
 *       states for your class
 *  <li> the enumerations (MyEnums)
 *  <li> get/set methods that cast to your enum:
 *   <ol>
 *    <li><pre> void setValue(MyEnum, int sampleNo=0){
 *             MonitorPointEnum::setValue(static_cast<int>(myenum)); }
 *         </pre>
 *    <li> <pre> MyEnum getValue(sampleNo=0) {
 *             return static_cast<MyEnum>(MonitorPointEnum::getValue()); }
 *         </pre>
 *   </ol>
 *  <li> getValueToString(sampleNo=1)
 * </ol>
 */
class MonitorPointEnum: public MonitorPoint {
public:
    typedef MonitorPointAverageEnum AccumType;
    typedef int AccumReportType;

    /**
     * Constructor.
     * The derived implementation **must** set the number of enumerations.
     * @param name monitor point name
     */
    MonitorPointEnum(const std::string& name,
        const bool bitmask = false,
        MONITOR_POINT_TYPE monitorPointType = MONITOR);

    /**
     * Destructor.
     * Nothing fancy here - the dynamic storage is managed elsewhere.
     */
    ~MonitorPointEnum() {};

    /**
     * Is this monitor point enumeration a bitmask
     */
    bool isBitmask() const { return this->bitmask_; }

    /**
     * Get the string length to use when
     * interpreting the value to a string.
     * @return width of the string representing the monitor point value
     */
    virtual short getWidth() const;

    /**
     * Set the number of enumerations
     * @param num of enums
     */
    void setNumEnumerations(int num);

    /**
     * Get the number of enumerations
     */
    int getNumEnumerations() const;

    /**
     * Add default warning low threshold.
     *
     * @param enumValue long enum value to be added to set of warn low values.
     */
    void addEnumWarnLowDefault (const long enumValue);

    /**
     * Remove enum value from default warning low threshold.
     *
     * @param enumValue long enum value to be removed from set of warn
     *        low values.
     */
    void removeEnumWarnLowDefault (const long enumValue);

    /**
     * Add default warning high threshold.
     *
     * @param enumValue long enum value to be added to set of warn high values.
     */
    void addEnumWarnHighDefault (const long enumValue);

    /**
     * Remove enum value from default warning high threshold.
     *
     * @param enumValue long enum value to be removed from set of warn
     *        high values.
     */
    void removeEnumWarnHighDefault (const long enumValue);

    /**
     * Add default error low threshold.
     *
     * @param enumValue long enum value to be added to set of error low values.
     */
    void addEnumErrorLowDefault (const long enumValue);

    /**
     * Remove enum value from default error high threshold.
     *
     * @param enumValue long enum value to be removed from set of error
     *        high values.
     */
    void removeEnumErrorLowDefault (const long enumValue);

    /**
     * Add default error high threshold.
     *
     * @param enumValue long enum value to be added to set of error high values.
     */
    void addEnumErrorHighDefault (const long enumValue);

    /**
     * Remove enum value from default error low threshold.
     *
     * @param enumValue long enum value to be removed from set of error
     *        low values.
     */
    void removeEnumErrorHighDefault (const long enumValue);

    /**
     * Reset accumulator values from for computing averages.
     *
     * @param accum will accumulate values over
     *        time for computing averages.
     * @return none
     */
    virtual void resetAccumulator (AccumType & accum) const;

    /**
     * Accumulates data from one monitor point sample (sample index == index)
     * belonging this monitor point.
     *
     * @param accum holds data for computing
     *        average.
     * @return enum MonitorPoint::VALIDITY validity flag of sample.
     * @see ::carma::monitor::MonitorPointAverageT
     */
    virtual enum MonitorPoint::VALIDITY accumulateSample
                       (AccumType & accum, int index) const;

    /**
     * Accumulate values from current monitor point for computing averages.
     *
     * @param accum will accumulate values over
     *        time for computing averages.
     * @return none
     */
    virtual void accumulate (AccumType & accum) const;

    /**
     * Accumulates pre-calculated average value from current monitor point
     * for computing averages.
     *
     * @param accum will accumulate values over
     *        time for computing averages.
     * @return none
     */
    virtual void accumulateAverage (AccumType & accum);

    /**
     * Create and return string representation for average accumulated in
     * accumulator object.
     *
     * @param accum will accumulate
     *        values over time for computing averages.
     * @return std::string average value as string.
     */
    virtual std::string getAccumulatedAverageAsString (const AccumType & accum) const;

    /**
     * Compute average from accumulated values in accumulator object.
     *
     * @param accum will accumulate values over
     *        time for computing averages.
     * @return bool computed average value.
     */
    virtual AccumReportType getAccumulatedAverage (const AccumType & accum) const;

    /**
     * Returns maximum value from accumulated data in accumulator object.
     * @param accum holds data for computing average.
     * @return int max value accumulated in the accumulator object.
     * @see ::carma::monitor::MonitorPointAverageT
     */
    virtual AccumReportType getMaxValue (const AccumType & accum) const;

    /**
     * Returns minimum value from accumulated data in accumulator object.
     * @param accum holds data for computing average.
     * @return short min value accumulated in the accumulator object.
     * @see ::carma::monitor::MonitorPointAverageT
     */
    virtual AccumReportType getMinValue (const AccumType & accum) const;

    // Default implementation that inserts the maximum value
    virtual void updateFrameAverage( ScratchAverages & scratchAvgs );

    /**
     * Method to return validity for a sample using the range parameters
     * in the MonitorPointThreshold object, <pre>threshold</pre>.
     *
     * @param threshold MonitorPointThreshold object with the threshold
     *        range settings for this monitor point.
     * @param enumVal int value of sample to be compared against
     *        range settings.
     * @return MonitorPoint::VALIDITY appropriate VALIDITY flag for sample.
     */
    virtual enum MonitorPoint::VALIDITY evaluateSampleTolerance
                  (const MonitorPointThreshold& threshold, int enumVal) const;

    // documented in base class
    virtual void evaluateTolerance (const MonitorPointThreshold& threshold);

    ::std::string getRawStringForEnumValue( int enumValue ) const;

    void setValue(const long i, int sampleNo) const;
    int  getValue(int sampleNo) const;
    int  getAve() const;

protected:

    /**
     * Convert an integer value, interpreted as an enum, into a string.
     * @return string representing the enumeration value
     */
    virtual std::string convertToString(int enumValue) const = 0;

private:
    int  nEnumerations_;
    int  nValidSamples_;
    const bool bitmask_;
};

/**
 * Base class for all real value monitor points (float, double, etc).
 * Used as a marker for these sub-classes; provides no unique functionality
 */
class MonitorPointReal: public MonitorPointNumeric {
public:

    /**
     * Constructor.
     * @param name monitor point name
     * @param monitorValueType the type of real
     */
    MonitorPointReal(const std::string& name, MonitorValueType monitorValueType,
        MONITOR_POINT_TYPE monitorPointType = MONITOR);


    /**
     * Destructor.
     * Nothing fancy here - the dynamic storage is managed elsewhere.
     */
    virtual ~MonitorPointReal() {};

};

/**
 * Float value monitor point.
 */
class MonitorPointFloat: public MonitorPointReal {
public:
    typedef MonitorPointAverageNumeric AccumType;
    typedef float AccumReportType;

    /**
     * Constructor.
     * @param name monitor point name
     */
    MonitorPointFloat(const std::string& name,
        MONITOR_POINT_TYPE monitorPointType = MONITOR);


    /**
     * Destructor.
     * Nothing fancy here - the dynamic storage is managed elsewhere.
     */
    virtual ~MonitorPointFloat() {};

    /**
     * Set the value.
     * @param f data value
     * @param sampleIndex sample index (0 is first sample)
     * @see getValue
     */
    void setValue(const float f, int sampleIndex = 0) const;

    /**
     * Get the value.
     * @param sampleIndex sample index (0 is first sample)
     * @see setValue
     */
    float getValue(int sampleIndex = 0) const;

    /**
     * Get the average value.
     */
    float getAve() const;

    // Already documented in base class
    virtual std::string getValueToString(int sampleIndex = 0) const;

    /**
     * Sets THRESHOLD_LOW_WARN_VALUE for this monitor point.
     *
     * @param threshold float float value to use for setting threshold.
     */
    void setWarnLowDefault (const float threshold);

    /**
     * Sets THRESHOLD_HIGH_WARN_VALUE for this monitor point.
     *
     * @param threshold float float value to use for setting threshold.
     */
    void setWarnHighDefault (const float threshold);

    /**
     * Sets THRESHOLD_LOW_ERROR_VALUE for this monitor point.
     *
     * @param threshold float float value to use for setting threshold.
     */
    void setErrorLowDefault (const float threshold);

    /**
     * Sets THRESHOLD_HIGH_ERROR_VALUE for this monitor point.
     *
     * @param threshold float float value to use for setting threshold.
     */
    void setErrorHighDefault (const float threshold);

    // Already documented in base class
    virtual std::string getAverageToString() const;

    // Already documented in base class
    virtual double getValueNumeric( int sampleIndex ) const;

    // Already documented in base class
    virtual double getAveNumeric( ) const;

    /**
     * Create and return string representation for average accumulated in
     * accumulator object.
     *
     * @param accum will accumulate
     *        values over time for computing averages.
     * @return std::string average value as string.
     */
    virtual std::string getAccumulatedAverageAsString (const AccumType & accum) const;

    /**
     * Compute average from accumulated values in accumulator
     * object.
     *
     * @param accum will accumulate
     *        values over time for computing averages.
     * @return float computed average value.
     */
    virtual AccumReportType getAccumulatedAverage (const AccumType & accum) const;

    /**
     * Returns maximum value from accumulated data in
     * accumulator object.
     * @param accum holds data for computing average.
     * @return float max value accumulated in the
     *         accumulator object.
     * @see ::carma::monitor::MonitorPointAverageT
     */
    virtual AccumReportType getMaxValue (const AccumType & accum) const;

    /**
     * Returns minimum value from accumulated data in accumulator object.
     * @param accum holds data for computing average.
     * @return float min value accumulated in the
     *         accumulator object.
     * @see ::carma::monitor::MonitorPointAverageT
     */
    virtual AccumReportType getMinValue (const AccumType & accum) const;

    // Already documented in base class
    virtual void updateFrameAverage( ScratchAverages & scratchAvgs );

    /**
     * Method to return validity for a sample using the range parameters
     * in the MonitorPointThreshold object, <pre>threshold</pre>.
     *
     * @param threshold MonitorPointThreshold object with the threshold
     *        range settings for this monitor point.
     * @param floatVal float value of sample to be compared against
     *        range settings.
     * @return MonitorPoint::VALIDITY appropriate VALIDITY flag for sample.
     */
    virtual enum MonitorPoint::VALIDITY evaluateSampleTolerance
                (const MonitorPointThreshold& threshold, float floatVal) const;

    // documented in base class
    virtual void evaluateTolerance (const MonitorPointThreshold& threshold);


};

/**
 * Double value monitor point.
 */
class MonitorPointDouble: public MonitorPointReal {
public:
    typedef MonitorPointAverageNumeric AccumType;
    typedef double AccumReportType;

    /**
     * Constructor.
     * @param name monitor point name
     */
    MonitorPointDouble(const std::string& name,
        MONITOR_POINT_TYPE monitorPointType = MONITOR);


    /**
     * Destructor.
     * Nothing fancy here - the dynamic storage is managed elsewhere.
     */
    virtual ~MonitorPointDouble() {};

    /**
     * Set the value.
     * @param f data value
     * @param sampleIndex sample index (0 is first sample)
     * @see getValue
     */
    void setValue(const double f, int sampleIndex = 0) const;

    /**
     * Get the value.
     * @param sampleIndex sample index (0 is first sample)
     * @see setValue
     */
    double getValue(int sampleIndex = 0) const;

    /**
     * Get the average value.
     */
    double getAve() const;

    // Already documented in base class
    virtual std::string getValueToString(int sampleIndex = 0) const;

    /**
     * Sets THRESHOLD_LOW_WARN_VALUE for this monitor point.
     *
     * @param threshold double double value to use for setting threshold.
     */
    void setWarnLowDefault (const double threshold);

    /**
     * Sets THRESHOLD_HIGH_WARN_VALUE for this monitor point.
     *
     * @param threshold double double value to use for setting threshold.
     */
    void setWarnHighDefault (const double threshold);

    /**
     * Sets THRESHOLD_LOW_ERROR_VALUE for this monitor point.
     *
     * @param threshold double double value to use for setting threshold.
     */
    void setErrorLowDefault (const double threshold);

    /**
     * Sets THRESHOLD_HIGH_ERROR_VALUE for this monitor point.
     *
     * @param threshold double double value to use for setting threshold.
     */
    void setErrorHighDefault (const double threshold);

    // Already documented in base class
    virtual std::string getAverageToString() const;

    // Already documented in base class
    virtual double getValueNumeric( int sampleIndex ) const;

    // Already documented in base class
    virtual double getAveNumeric( ) const;

    /**
     * Create and return string representation for average accumulated in
     * accumulator object.
     *
     * @param accum will accumulate
     *        values over time for computing averages.
     * @return std::string average value as string.
     */
    virtual std::string getAccumulatedAverageAsString (const AccumType & accum) const;

    /**
     * Compute average from accumulated values in accumulator object.
     *
     * @param accum will accumulate
     *        values over time for computing averages.
     * @return bool computed average value.
     */
    virtual AccumReportType getAccumulatedAverage (const AccumType & accum) const;

    /**
     * Returns maximum value from accumulated data in accumulator object.
     * @param accum holds data for computing average.
     * @return double max value accumulated in the
     *         accumulator object.
     * @see ::carma::monitor::MonitorPointAverageT
     */
    virtual AccumReportType getMaxValue (const AccumType & accum) const;

    /**
     * Returns minimum value from accumulated data in accumulator object.
     * @param accum holds data for computing
     *        average.
     * @return double min value accumulated in the accumulator object.
     * @see ::carma::monitor::MonitorPointAverageT
     */
    virtual AccumReportType getMinValue (const AccumType & accum) const;

    // Already documented in base class
    virtual void updateFrameAverage( ScratchAverages & scratchAvgs );

    /**
     * Method to return validity for a sample using the range parameters
     * in the MonitorPointThreshold object, <pre>threshold</pre>.
     *
     * @param threshold MonitorPointThreshold object with the threshold
     *        range settings for this monitor point.
     * @param doubleVal double value of sample to be compared against
     *        range settings.
     * @return MonitorPoint::VALIDITY appropriate VALIDITY flag for sample.
     */
    virtual enum MonitorPoint::VALIDITY evaluateSampleTolerance
             (const MonitorPointThreshold& threshold, double doubleVal) const;

    // documented in base class
    virtual void evaluateTolerance (const MonitorPointThreshold& threshold);


};

/**
 * Complex value monitor point.
 */

//! @brief Monitor point holding a complex (in the mathematical sense) value
class MonitorPointComplex: public MonitorPoint {
public:
    typedef MonitorPointAverageComplex AccumType;
    typedef ::std::complex< float > AccumReportType;

    typedef enum stringValueEnum {

	/** getValueString() will return the amplitude */
	AMP,
	/** getValueString() will return the phase */
	PHASE,
	/** getValueString() will return the real part */
	REAL,
	/** getValueString() will return the imaginary part */
	IMAG,
	/** getValueString() will return the full complex value */
	COMPLEX

    } stringValueReturnType;

    /**
     * Constructor.
     * @param name monitor point name
     */
    MonitorPointComplex(const std::string& name,
        MONITOR_POINT_TYPE monitorPointType = MONITOR);


    /**
     * Destructor.
     * Nothing fancy here - the dynamic storage is managed elsewhere.
     */
    virtual ~MonitorPointComplex() {};

    /**
     * Set the value.
     * @param f data value
     * @param sampleIndex sample index (0 is first sample)
     * @see getValue
     */
    void setValue(const std::complex<float>& f, int sampleIndex = 0) const;

    /**
     * Get the value.
     * @param sampleIndex sample index (0 is first sample)
     * @see setValue
     */
    std::complex<float> getValue(int sampleIndex = 0) const;

    /**
     * Get the average value.
     */
    std::complex<float> getAve() const;

    // Already documented in base class
    virtual std::string getValueToString(int sampleIndex = 0) const;

    /**
     * Sets THRESHOLD_LOW_WARN_VALUE for this monitor point.
     *
     * @param threshold float float value to use for setting norm-based
     *        threshold. This float value will be compared to the amplitude
     *        of the complex value to set the THRESHOLD_WARN validity bit.
     */
    void setWarnLowDefault (const float threshold);

    /**
     * Sets THRESHOLD_HIGH_WARN_VALUE for this monitor point.
     *
     * @param threshold float float value to use for setting norm-based
     *        threshold. This float value will be compared to the amplitude
     *        of the complex value to set the THRESHOLD_WARN validity bit.
     */
    void setWarnHighDefault (const float threshold);

    /**
     * Sets THRESHOLD_LOW_ERROR_VALUE for this monitor point.
     *
     * @param threshold float float value to use for setting norm-based
     *        threshold. This float value will be compared to the amplitude
     *        of the complex value to set the THRESHOLD_ERROR validity bit.
     */
    void setErrorLowDefault (const float threshold);

    /**
     * Sets THRESHOLD_HIGH_ERROR_VALUE for this monitor point.
     *
     * @param threshold float float value to use for setting norm-based
     *        threshold. This float value will be compared to the amplitude
     *        of the complex value to set the THRESHOLD_ERROR validity bit.
     */
    void setErrorHighDefault (const float threshold);


    // Already documented in base class
    virtual std::string getAverageToString() const;

    /**
     * Reset accumulator values from for computing averages.
     *
     * @param accum will accumulate
     *        values over time for computing averages.
     * @return none
     */
    virtual void resetAccumulator (AccumType & accum) const;

    /**
     * Accumulates data from one monitor point sample (sample index == index)
     * belonging this monitor point.
     *
     * @param accum holds data for computing average.
     * @return enum MonitorPoint::VALIDITY validity flag of sample.
     * @see ::carma::monitor::MonitorPointAverageT
     */
    virtual enum MonitorPoint::VALIDITY accumulateSample
                       (AccumType & accum, int index) const;

    /**
     * Accumulate values from current monitor point for computing averages.
     *
     * @param accum will accumulate
     *        values over time for computing averages.
     * @return none
     */
    virtual void accumulate (AccumType & accum) const;

    /**
     * Accumulates pre-calculated average value from current monitor point
     * for computing averages.
     *
     * @param accum will accumulate values over
     *        time for computing averages.
     * @return none
     */
    virtual void accumulateAverage (AccumType & accum);

    /**
     * Create and return string representation for average accumulated in
     * accumulator object.
     *
     * @param accum will accumulate
     *        values over time for computing averages.
     * @return std::string average value as string.
     */
    virtual std::string getAccumulatedAverageAsString (const AccumType & accum) const;

    /**
     * Compute average from accumulated values in accumulator object.
     *
     * @param accum will accumulate
     *        values over time for computing averages.
     * @return bool computed average value.
     */
    virtual AccumReportType getAccumulatedAverage (const AccumType & accum) const;

    /**
     * Returns maximum value from accumulated data in accumulator object.
     * @param accum holds data for computing average.
     * @return std::complex<float> max value accumulated in the accumulator object
     * @see ::carma::monitor::MonitorPointAverageT
     */
    virtual AccumReportType getMaxValue (const AccumType & accum) const;

    /**
     * Returns minimum value from accumulated data in accumulator object.
     * @param accum holds data for computing average.
     * @return std::complex<float> min value accumulated in the accumulator object.
     * @see ::carma::monitor::MonitorPointAverageT
     */
    virtual AccumReportType getMinValue (const AccumType & accum) const;

    // Already documented in base class
    virtual void updateFrameAverage( ScratchAverages & scratchAvgs );

    /**
     * Method to return validity for a sample using the range parameters
     * in the MonitorPointThreshold object, <pre>threshold</pre>.
     *
     * @param threshold MonitorPointThreshold object with the threshold
     *        range settings for this monitor point.
     * @param complexVal std::complex<float> value of sample to be compared
     *        against range settings.
     * @return MonitorPoint::VALIDITY appropriate VALIDITY flag for sample.
     */
    virtual enum MonitorPoint::VALIDITY evaluateSampleTolerance
                  (const MonitorPointThreshold& threshold,
                            std::complex<float> complexVal) const;

    // documented in base class
    virtual void evaluateTolerance (const MonitorPointThreshold& threshold);


     /**
     * Set the number of digits to the right of the decimal point for
     * interpreting the value to a string.
     * @param precision number of digits to the right of the decimal
     * @see getPrecision
     */
    void setPrecision(short precision);

    /**
     * Get the number of digits to the right of the decimal point for
     * interpreting the value to a string.
     * @return number of digits to the right of the decimal
     * @see setPrecision
     */
    short getPrecision() const;

    /**
     * Compares this monitor point for equality to the one passed.
     * The vbase types must match; performs a complex value check
     * @param component to compare to this component
     */
    bool operator==(const MonitorComponent& component) const;

    /**
     * Decide how to treat complex number returned as string
     * @param complexDisposition
     */
    void setStringRepresentation(const stringValueReturnType& type);

    stringValueReturnType getStringRepresentation(void) const;

protected:

    /**
     * Gets the maximum sample value (measured as max norm).
     *
     * @return std::complex<float> sample value with the max norm.
     */
    std::complex<float> getMaxSampleValue () const;

    /**
     * Gets the minimum sample value (measured as min norm).
     *
     * @return std::complex<float> sample value with the min norm.
     */
    std::complex<float> getMinSampleValue () const;

private:
    int  nValidSamples_;
    short precision_;
    stringValueReturnType stringReturnType_;
    std::string getValueToString(std::complex<float> f) const;
};

/**
 * Absolute time value monitor point.
 * Absolute time is always kept in MJD.
 * The toString method for this class produces formatted UT time.
 * The precision defaults to zero and the width to 16 chars.
 * You can specify date, time, or date/time (default) as output format.
 */
class MonitorPointAbstime: public MonitorPointReal {
public:
    typedef MonitorPointAverageNumeric AccumType;
    typedef double AccumReportType;

    /**
     * Constructor.
     * @param name monitor point name
     */
    MonitorPointAbstime(const std::string& name,
        MONITOR_POINT_TYPE monitorPointType = MONITOR);

    enum ABSTIME_FORMAT {
        DATE,
        TIME,
        DATE_AND_TIME
    };

    /**
     * Destructor.
     * Nothing fancy here - the dynamic storage is managed elsewhere.
     */
    virtual ~MonitorPointAbstime() {};

    /**
     * Select output format type
     * @param format type
     */
    void setFormat(ABSTIME_FORMAT format);

    /**
     * Get output format type
     */
    ABSTIME_FORMAT getFormat() const;

    /**
     * Format an mjd according to the selected format
     * @param mjd the absolute time to format
     */
    std::string getFormattedAbstime(double mjd) const;

    /**
     * Set the value.
     * @param f data value
     * @param sampleIndex sample index (0 is first sample)
     * @see getValue
     */
    void setValue(const double f, int sampleIndex = 0) const;

    /**
     * Get the value.
     * @param sampleIndex sample index (0 is first sample)
     * @see setValue
     */
    double getValue(int sampleIndex = 0) const;

    /**
     * Get the average value.
     */
    double getAve() const;

    // Already documented in base class
    virtual std::string getValueToString(int sampleIndex = 0) const;

    /**
     * Sets THRESHOLD_LOW_WARN_VALUE for this monitor point.
     *
     * @param threshold double double value to use for setting threshold.
     */
    void setWarnLowDefault (const double threshold);

    /**
     * Sets THRESHOLD_HIGH_WARN_VALUE for this monitor point.
     *
     * @param threshold double double value to use for setting threshold.
     */
    void setWarnHighDefault (const double threshold);

    /**
     * Sets THRESHOLD_LOW_ERROR_VALUE for this monitor point.
     *
     * @param threshold double double value to use for setting threshold.
     */
    void setErrorLowDefault (const double threshold);

    /**
     * Sets THRESHOLD_HIGH_ERROR_VALUE for this monitor point.
     *
     * @param threshold double double value to use for setting threshold.
     */
    void setErrorHighDefault (const double threshold);

    // Already documented in base class
    virtual std::string getAverageToString() const;

    // Already documented in base class
    virtual double getValueNumeric( int sampleIndex ) const;

    // Already documented in base class
    virtual double getAveNumeric( ) const;

    /**
     * Create and return string representation for average accumulated in
     * accumulator object.
     *
     * @param accum will accumulate
     *        values over time for computing averages.
     * @return std::string average value as string.
     */
    virtual std::string getAccumulatedAverageAsString (const AccumType & accum) const;

    // Already documented in base class
    virtual void updateFrameAverage( ScratchAverages & scratchAvgs );

    /**
     * Compute average from accumulated values in accumulator object.
     *
     * @param accum will accumulate
     *        values over time for computing averages.
     * @return bool computed average value.
     */
    virtual AccumReportType getAccumulatedAverage (const AccumType & accum) const;

    /**
     * Returns maximum value from accumulated data in accumulator object.
     * @param accum holds data for computing average.
     * @return double max value accumulated in the accumulator object.
     * @see ::carma::monitor::MonitorPointAverageT
     */
    virtual AccumReportType getMaxValue (const AccumType & accum) const;

    /**
     * Returns minimum value from accumulated data in accumulator object.
     * @param accum holds data for computing average.
     * @return double min value accumulated in the accumulator object.
     * @see ::carma::monitor::MonitorPointAverageT
     */
    virtual AccumReportType getMinValue (const AccumType & accum) const;

    /**
     * Method to return validity for a sample using the range parameters
     * in the MonitorPointThreshold object, <pre>threshold</pre>.
     *
     * @param threshold MonitorPointThreshold object with the threshold
     *        range settings for this monitor point.
     * @param abstimeVal double value of sample to be compared against
     *        range settings.
     * @return MonitorPoint::VALIDITY appropriate VALIDITY flag for sample.
     */
    virtual enum MonitorPoint::VALIDITY evaluateSampleTolerance
              (const MonitorPointThreshold& threshold, double abstimeVal) const;

    // documented in base class
    virtual void evaluateTolerance (const MonitorPointThreshold& threshold);

private:
    ABSTIME_FORMAT format_;

};

class MonitorPointRA: public MonitorPointDouble {
public:
    /**
     * Constructor.
     * @param name monitor point name
     */
    MonitorPointRA(const std::string& name,
        MONITOR_POINT_TYPE monitorPointType = MONITOR);

    /**
     * Destructor.
     * Nothing fancy here - the dynamic storage is managed elsewhere.
     */
    virtual ~MonitorPointRA() {};
    // Already documented in base class
    virtual std::string getAverageToString() const;
};
class MonitorPointDec: public MonitorPointDouble {
public:
    /**
     * Constructor.
     * @param name monitor point name
     */
    MonitorPointDec(const std::string& name,
        MONITOR_POINT_TYPE monitorPointType = MONITOR);

    /**
     * Destructor.
     * Nothing fancy here - the dynamic storage is managed elsewhere.
     */
    virtual ~MonitorPointDec() {};
    // Already documented in base class
    virtual std::string getAverageToString() const;
};

/**
 * String value monitor point (up to 80 characters in length).
 * Multiple samples are not allowed (they are used internally to
 * store strings longer than 8 characters).
 * Thresholds are checked by checking for equality - that is, a
 * MonitorPointString is set to validity VALID_WARN_LOW if its value
 * is equal to the threshold value set for THRESHOLD_WARN_LOW.
 * An artifact of the use of multiple samples in the implementation
 * is that this datatype will show as a spectrum rather than a time series.
 *
 */

class MonitorPointString: public MonitorPoint {
public:
    typedef MonitorPointAverageString AccumType;
    typedef ::std::string AccumReportType;

    /**
     * Constructor.
     * Default width for toString() is 20 characters
     * (including enclosing quotes).
     * @see MonitorPoint::setWidth()
     * @param name monitor point name
     */
    MonitorPointString(const std::string& name,
        MONITOR_POINT_TYPE monitorPointType = MONITOR);


    /**
     * Destructor.
     * Nothing fancy here - the dynamic storage is managed elsewhere.
     */
    ~MonitorPointString() {};

    /**
     * Set the value.
     * @param str input string
     * @see getValue
     */
    void setValue(const std::string& str) const;

    /**
     * Get the value.
     * @see setValue
     */
    std::string getValue() const;

    /**
     * Get the average value.
     */
    std::string getAve() const;

    // Already documented in base class
    virtual std::string getValueToString(int sampleIndex) const;

    /**
     * Gets the full string stored for this mp
     */
    std::string getValueToString() const;

    // Already documented in base class
    virtual std::string getAverageToString() const;

    /**
     * Sets lower warning threshold to be strValue.
     *
     * @param strValue const std::string not reference because then we can
     *        pass in char* strings. If string value is equal to strValue,
     *        then validity is set to VALID_WARNING_LOW.
     */
    void setWarnLowDefault (const std::string strValue);

    /**
     * Sets upper warning threshold to be strValue.
     *
     * @param strValue const std::string not reference because then we can
     *        pass in char* strings. If string value is equal to strValue,
     *        then validity is set to VALID_WARNING_HIGH.
     */
    void setWarnHighDefault (const std::string strValue);


    /**
     * Sets lower error threshold to be strValue.
     *
     * @param strValue const std::string not reference because then we can
     *        pass in char* strings. If string value is equal to strValue,
     *        then validity is set to VALID_ERROR_LOW.
     */
    void setErrorLowDefault (const std::string strValue);

    /**
     * Sets upper error threshold to be strValue.
     *
     * @param strValue const std::string not reference because then we can
     *        pass in char* strings. If string value is equal to strValue,
     *        then validity is set to VALID_ERROR_HIGH.
     */
    void setErrorHighDefault (const std::string strValue);

    /**
     * Returns current THRESHOLD_LOW_WARN_VALUE for this monitor point.
     * If the threshold object is not set, then it returns the default value
     * if its set. Throws an ErrorException if threshold object and default
     * values are unset, or if the threshold object doesnt correspond to
     * this monitor point.
     *
     * @param threshold MonitorPointThreshold& threshold object corresponding
     *        to this monitor point, holding the values set for various
     *        threshold limits.
     * @return std::string value of threshold if set.
     * @exception ErrorException if no threshold values have been set in the
     *            threshold object, and for defaults.
     */
    std::string getWarnLow (const MonitorPointThreshold& threshold) const;

    /**
     * Returns current THRESHOLD_HIGH_WARN_VALUE for this monitor point.
     * If the threshold object is not set, then it returns the default value
     * if its set. Throws an ErrorException if threshold object and default
     * values are unset, or if the threshold object doesnt correspond to
     * this monitor point.
     *
     * @param threshold MonitorPointThreshold& threshold object corresponding
     *        to this monitor point, holding the values set for various
     *        threshold limits.
     * @return std::string value of threshold if set.
     * @exception ErrorException if no threshold values have been set in the
     *            threshold object, and for defaults.
     */
    std::string getWarnHigh (const MonitorPointThreshold& threshold) const;

    /**
     * Returns current THRESHOLD_LOW_ERROR_VALUE for this monitor point.
     * If the threshold object is not set, then it returns the default value
     * if its set. Throws an ErrorException if threshold object and default
     * values are unset, or if the threshold object doesnt correspond to
     * this monitor point.
     *
     * @param threshold MonitorPointThreshold& threshold object corresponding
     *        to this monitor point, holding the values set for various
     *        threshold limits.
     * @return std::string value of threshold if set.
     * @exception ErrorException if no threshold values have been set in the
     *            threshold object, and for defaults.
     */
    std::string getErrorLow (const MonitorPointThreshold& threshold) const;

    /**
     * Returns current THRESHOLD_HIGH_ERROR_VALUE for this monitor point.
     * If the threshold object is not set, then it returns the default value
     * if its set. Throws an ErrorException if threshold object and default
     * values are unset, or if the threshold object doesnt correspond to
     * this monitor point.
     *
     * @param threshold MonitorPointThreshold& threshold object corresponding
     *        to this monitor point, holding the values set for various
     *        threshold limits.
     * @return std::string value of threshold if set.
     * @exception ErrorException if no threshold values have been set in the
     *            threshold object, and for defaults.
     */
    std::string getErrorHigh (const MonitorPointThreshold& threshold) const;

    /**
     * Reset accumulator values from for computing averages.
     *
     * @param accum will accumulate
     *        values over time for computing averages.
     * @return none
     */
    virtual void resetAccumulator (AccumType & accum) const;

    /**
     * Accumulates data from one monitor point sample (sample index == index)
     * belonging this monitor point.
     *
     * @param accum holds data for computing average.
     * @return enum MonitorPoint::VALIDITY validity flag of sample.
     * @see ::carma::monitor::MonitorPointAverageT
     */
    virtual enum MonitorPoint::VALIDITY accumulateSample
                       (AccumType & accum, int index) const;

    /**
     * Accumulate values from current monitor point for computing averages.
     *
     * @param accum will accumulate
     *        values over time for computing averages.
     * @return none
     */
    virtual void accumulate (AccumType & accum) const;

    /**
     * Accumulates pre-calculated average value from current monitor point
     * for computing averages.
     *
     * @param accum will accumulate values over
     *        time for computing averages.
     * @return none
     */
    virtual void accumulateAverage (AccumType & accum);

    /**
     * Create and return string representation for average accumulated in
     * accumulator object.
     *
     * @param accum will accumulate
     *        values over time for computing averages.
     * @return std::string average value as string.
     */
    virtual std::string getAccumulatedAverageAsString (const AccumType & accum) const;

    /**
     * Compute average from accumulated values in accumulator object.
     *
     * @param accum will accumulate
     *        values over time for computing averages.
     * @return bool computed average value.
     */
    virtual AccumReportType getAccumulatedAverage (const AccumType & accum) const;

    /**
     * Returns maximum value from accumulated data in accumulator object.
     * @param accum holds data for computing average.
     * @return std::string max value accumulated in the accumulator object.
     * @see ::carma::monitor::MonitorPointAverageT
     */
    virtual AccumReportType getMaxValue (const AccumType & accum) const;

    /**
     * Returns minimum value from accumulated data in accumulator object.
     * @param accum holds data for computing average.
     * @return std::string min value accumulated in the accumulator object
     * @see ::carma::monitor::MonitorPointAverageT
     */
    virtual AccumReportType getMinValue (const AccumType & accum) const;

    // Already documented in base class
    virtual void updateFrameAverage( ScratchAverages & scratchAvgs );

    /**
     * Method to return validity for a sample using the range parameters
     * in the MonitorPointThreshold object, <pre>threshold</pre>.
     *
     * @param threshold MonitorPointThreshold object with the threshold
     *        range settings for this monitor point.
     * @param stringVal std::string value of sample to be compared against
     *        range settings.
     * @return MonitorPoint::VALIDITY appropriate VALIDITY flag for sample.
     */
    virtual enum MonitorPoint::VALIDITY evaluateSampleTolerance
                  (const MonitorPointThreshold& threshold,
                                     std::string stringVal) const;

    // documented in base class
    virtual void evaluateTolerance (const MonitorPointThreshold& threshold);


    /**
     * Compares this monitor point for equality to the one passed.
     * The vbase types must match; performs a string value check
     * @param component to compare to this component
     */
    bool operator==(const MonitorComponent& component) const;

};

/**
 * Serial number monitor point, stored as integer value(4 bytes).
 * Only one sample per cycle is allowed
 */

class MonitorPointSerialNo: public MonitorPoint {
public:
    typedef MonitorPointAverageSerialNo AccumType;
    typedef long AccumReportType;

    /**
     * Constructor.
     * @param name monitor point name
     */
    MonitorPointSerialNo(const std::string& name,
        MONITOR_POINT_TYPE monitorPointType = MONITOR);


    /**
     * Destructor.
     * Nothing fancy here - the dynamic storage is managed elsewhere.
     */
    ~MonitorPointSerialNo() {};

    /**
     * Set the value.
     * @param i data value
     * @param sampleIndex sample number, ignored as only 0 is allowed
     * @see getValue
     */
    void setValue(long i, int sampleIndex = 0) const;

    /**
     * Get the value.
     * @param sampleIndex sample number; ignored as only a single sample is allowed
     * @see setValue
     */
    int getValue(int sampleIndex = 0) const;

    /**
     * Get the average value.
     */
    int getAve() const;

    /**
     * Set the number of samples to one...
     * @param nSamples ignored
     */
    void setNumSamples(int nSamples) const;

    // Already documented in base class
    virtual std::string getValueToString(int sampleIndex = 0) const;

    // Already documented in base class
    virtual std::string getAverageToString() const;

    /**
     * Reset accumulator values from for computing averages.
     *
     * @param accum will accumulate
     *        values over time for computing averages.
     * @return none
     */
    virtual void resetAccumulator (AccumType & accum) const;

    /**
     * Accumulates data from one monitor point sample (sample index == index)
     * belonging this monitor point.
     *
     * @param accum holds data for computing average.
     * @return enum MonitorPoint::VALIDITY validity flag of sample.
     * @see ::carma::monitor::MonitorPointAverageT
     */
    virtual enum MonitorPoint::VALIDITY accumulateSample
                       (AccumType & accum, int index) const;

    /**
     * Accumulate values from current monitor point for computing averages.
     *
     * @param accum will accumulate
     *        values over time for computing averages.
     * @return none
     */
    virtual void accumulate (AccumType & accum) const;

    /**
     * Accumulates pre-calculated average value from current monitor point
     * for computing averages.
     *
     * @param accum will accumulate values over
     *        time for computing averages.
     * @return none
     */
    virtual void accumulateAverage (AccumType & accum);

    /**
     * Create and return string representation for average accumulated in
     * accumulator object.
     *
     * @param accum will accumulate
     *        values over time for computing averages.
     * @return std::string average value as string.
     */
    virtual std::string getAccumulatedAverageAsString (const AccumType & accum) const;

    /**
     * Compute average from accumulated values in accumulator object.
     *
     * @param accum will accumulate
     *        values over time for computing averages.
     * @return bool computed average value.
     */
    virtual AccumReportType getAccumulatedAverage (const AccumType & accum) const;

    /**
     * Returns maximum value from accumulated data in accumulator object.
     * @param accum holds data for computing average.
     * @return long max value accumulated in the accumulator object.
     * @see ::carma::monitor::MonitorPointAverageT
     */
    virtual AccumReportType getMaxValue (const AccumType & accum) const;

    /**
     * Returns minimum value from accumulated data in accumulator object.
     * @param accum holds data for computing average.
     * @return short min value accumulated in the accumulator object.
     * @see ::carma::monitor::MonitorPointAverageT
     */
    virtual AccumReportType getMinValue (const AccumType & accum) const;

    // Already documented in base class
    virtual void updateFrameAverage( ScratchAverages & scratchAvgs );

    /**
     * Method to return validity for a sample using the range parameters
     * in the MonitorPointThreshold object, <pre>threshold</pre>.
     *
     * @param threshold MonitorPointThreshold object with the threshold
     *        range settings for this monitor point.
     * @param serialNoVal long value of sample to be compared against
     *        range settings.
     * @return MonitorPoint::VALIDITY appropriate VALIDITY flag for sample.
     */
    virtual enum MonitorPoint::VALIDITY evaluateSampleTolerance
                  (const MonitorPointThreshold& threshold, long serialNoVal) const;

    // documented in base class
    virtual void evaluateTolerance (const MonitorPointThreshold& threshold);


};


}  // End namespace carma::monitor
}  // End namespace carma


inline char
carma::monitor::MonitorPointChar::getAve( ) const
{
    return getAveChar();
}


inline unsigned char
carma::monitor::MonitorPointByte::getAve( ) const
{
    return static_cast< unsigned char >( getAveChar() );
}


inline short
carma::monitor::MonitorPointShort::getAve( ) const
{
    return getAveShort();
}


inline long
carma::monitor::MonitorPointInt::getAve( ) const
{
    return getAveLong();
}


inline int
carma::monitor::MonitorPointEnum::getAve( ) const
{
    return getAveLong();
}


inline float
carma::monitor::MonitorPointFloat::getAve( ) const
{
    return getAveFloat();
}


inline double
carma::monitor::MonitorPointDouble::getAve( ) const
{
    return getAveDouble();
}


inline ::std::complex< float >
carma::monitor::MonitorPointComplex::getAve( ) const
{
    return getAveComplex();
}


inline double
carma::monitor::MonitorPointAbstime::getAve( ) const
{
    return getAveDouble();
}


inline ::std::string
carma::monitor::MonitorPointString::getAve( ) const
{
    return getValue();
}


inline int
carma::monitor::MonitorPointSerialNo::getAve( ) const
{
    return getAveSerialNo();
}


#endif
