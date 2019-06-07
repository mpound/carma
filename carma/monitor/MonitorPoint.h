#ifndef CARMA_MONITOR_MONITORPOINT_H
#define CARMA_MONITOR_MONITORPOINT_H


/**
 * @file
 *
 * Abstract base class for all monitor points.
 *
 * @author: Steve Scott
 *
 * $CarmaCopyright$
 *
 */


#include <complex>
#include <string>

#include "carma/monitor/types.h"
#include "carma/monitor/MonitorComponent.h"
#include "carma/monitor/MonitorPointSample.h"
#include "carma/monitor/MonitorPointHeader.h"

namespace carma {
namespace monitor {

struct ScratchAverages;
class MonitorContainer;
class MonitorPointThreshold;

/**
 *
 * Abstract base class for a monitor point.
 * Concrete class will implement (by convention) a typed
 * getValue()/setValue pair that distinguishes the different
 * specialized types of monitor points.
 *
 * The number of samples per frame defaults to one.
 *
 * There are several aspects regarding the name of a monitor point:
 * <UL>
 * <LI>
 *  name - the monitor point name, no hierarchy (leaf node name).
 *  This name is not necessarily unique, e.g. temperature2
 * <LI>
 *  subsystemName - subsystem that a monitor point belongs to, e.g. ovro3
 * <LI>
 *  canonicalName - full physical name starting at the subsystem.
 *  This is the unique name of a monitor point,
 *  and can be used to get monitorPointID
 *    e.g. ovro3.cryo.temperature2
 * <LI>
 *  shortName - used for a compact label, e.g. temp2
 * <LI>
 *  hierarchicalName - hierarchical name in a given situation
 * <BR>
 *    in a subsystem, e.g. ovro3.cryo.temperature2
 * <BR>
 *    in the ACC (physical), e.g. carma.ovro3.cryo.temperature2
 * <BR>
 *    in the ACC (logical),  e.g. science1.ant2.cryo.temperature2
 * </UL>
 *
 * @see http://www.mmarray.org/project/WP/Monitoring/monitoringDesign.pdf
 *
 */
class MonitorPoint: public MonitorComponent {
public:

    /**
     * Validity states of the data value.
     * Many (but not all) of the monitor points have values
     * that can be checked against contiguous ranges of values
     * for being good, warning or error.
     * Other monitor points may have custom code to check for
     * good, warning or errors, without the range concept.
     * For numeric data using the range construct,
     * there is a good range that is bracketed
     * by a warning range that is bracketed by an
     * an error range.
     * <pre>
     *  ||
     *  ||            ERROR_HIGH
     *  ||
     *  --------- High Error Threshold ----------
     *  ||
     *  ||            WARNING_HIGH
     *  ||
     *  --------- High Warning Threshold ----------
     *  ||  ////////////////////////////////////
     *  ||  ////          GOOD              ////
     *  ||  ////////////////////////////////////
     *  --------- Low Warning Threshold ----------
     *  ||
     *  ||            WARNING_LOW
     *  ||
     *  --------- Low Error Threshold ----------
     *  ||
     *  ||            ERROR_LOW
     *  ||
     * </pre>
     * <ul>
     *   <li>INVALID_NO_DATA - Data is invalid because the data was
     *        was not sent on time (the data transmission can be
     *        broken anywhere along the chain)
     *   <li>INVALID_NO_HW - Data is invalid because the hardware
     *        does not exist (or can't be detected)
     *   <li>INVALID_HW_BAD - Data is invalid because the hardware
     *         data collection got a bad sample
     *         (e.g. A/D conversion failure)
     *   <li>VALID - the data is valid and threshold do not
     *        apply to this type of data.
     *   <li>VALID_NOT_CHECKED - the data is valid but has
     *        not been checked against the thresholds
     *   <li>VALID_GOOD - the data is valid and its value is
     *        inside the thresholds
     *   <li>VALID_WARNING- the data is valid but the value warrants
     *        a warning (not using range thresholds)
     *   <li>VALID_ERROR - the data is valid but bad
     *         (not using range thresholds)
     *   <li>VALID_WARNING_LOW - the data is valid and below
     *        the lower warning range threshold
     *   <li>VALID_ERROR_LOW - the data is valid and below
     *        the lower error range threshold
     *   <li>VALID_WARNING_HIGH - the data is valid and above
     *        the upper warning range threshold
     *   <li>VALID_ERROR_HIGH - the data is valid and above
     *        the upper error range threshold
     * </ul>
     */
    enum VALIDITY {
        INVALID_NO_DATA,
        INVALID_NO_HW,
        INVALID_HW_BAD,
        VALID,
        VALID_NOT_CHECKED,
        VALID_GOOD,
        VALID_WARNING,
        VALID_ERROR,
        VALID_WARNING_LOW,
        VALID_WARNING_HIGH,
        VALID_ERROR_LOW,
        VALID_ERROR_HIGH,
        MAX_VALIDITY
    };

    /**
     * Blanking/flagging status of the data.
     * <ul>
     *   <li>OK - The data is not blanked or flagged
     *        inside the thresholds.
     *   <li>UNDETERMINED - The blanking/flagging status not yet determined.
     *   <li>BLANKED - The data is blanked.
     *   <li>FLAGGED - The data is flagged.
     *   <li>BLANKED_FLAGGED - The data is both blanked and flagged.
     * </ul>
     */
    enum BLANKING_FLAGGING {
        UNDETERMINED,
        OK,
        BLANKED,
        FLAGGED,
        BLANKED_FLAGGED,
        MAX_BLANKING_FLAGGING
    };

    /**
     * Monitor point type
     */
    enum MONITOR_POINT_TYPE {
        /**
         * Simple monitor point.
        **/
        MONITOR,

        /**
         * A control point.
        **/
        CONTROL
    };


    /**
     * Constructor, name is later used to lookup the tagID.
     * Number of samples per frame is set to one.
     * @param name of the monitor point - no hierarchy (e.g. powerSupply5volt)
     * @param valuetype (datatype of the value) of the monitor point
     */
    MonitorPoint( const std::string & name,
                  MonitorValueType    valuetype,
                  MONITOR_POINT_TYPE  monitorPointType = MONITOR );

    /**
     * Destructor.
     * Nothing fancy here - the dynamic storage is managed elsewhere.
     */
    virtual ~MonitorPoint( );

    /**
     * Get the units string for the monitor point.
     * White space is not allowed.
     * An example is "milliamps".
     * @return the units for the monitor point.
     * @see setUnits
     */
    std::string getUnits() const;

    /**
     * Get the units string for the monitor point.
     * @param units for the monitor point.
     *   White space allowed.
     * @see getUnits
     */
    void setUnits(const std::string& units);

    /**
     * Set the maximum width(length) of the string for
     * interpreting the value to a string.
     * @param width of the returned string
     * @see getWidth, getValueToString
     */
    void setWidth(short width) const;

    /**
     * Get the string length to use when
     * interpreting the value to a string.
     * @return width of the string representing the monitor point value
     * @see setWidth
     */
    virtual short getWidth() const;

    /**
     * Set the number of digits to the right of the decimal point when
     * translating the value to a string.
     * This is only used for monitor points that have values that are
     * real numbers, but has been included in the base class so that
     * the precision may be manipulated on the base class rather than the
     * derived class.
     * @param precision number of digits to the right of the decimal
     * @see getPrecision
     */
    void setPrecision(short precision);

    /**
     * Get the number of digits to the right of the decimal point when
     * translating the value to a string.
     * @return number of digits to the right of the decimal
     * @see setPrecision
     */
    short getPrecision() const;

    /**
     * Set the persistent state of the data.
     * If the data is not persistent then immediately after
     * the write() method is called the data are set to
     * INVALID_NO_DATA. If it is persistent then the validity
     * is not changed after the write.
     * @param persistent
     * @see isPersistent
     */
    void setPersistent(bool persistent) ;

    /**
     * Returns the persistent state of the data.
     * @see setPersistent
     */
    bool isPersistent( ) const;

    /**
     * Set the upper error default threshold.
     * @param threshold
     * @see setErrorHighDefault, setWarnHighDefault, setWarnLowDefault, setErrorLowDefault
     * @see getErrorHighDefault, getWarnHighDefault, getWarnLowDefault, getErrorLowDefault
     */
    void setErrorHighDefault( const MonitorValue threshold );

    /**
     * Set the upper warning default threshold.
     * @param threshold
     * @see setErrorHighDefault, setWarnHighDefault, setWarnLowDefault, setErrorLowDefault
     * @see getErrorHighDefault, getWarnHighDefault, getWarnLowDefault, getErrorLowDefault
     */
    void setWarnHighDefault( const MonitorValue threshold );

    /**
     * Set the lower warning default threshold.
     * @param threshold
     * @see setErrorHighDefault, setWarnHighDefault, setWarnLowDefault, setErrorLowDefault
     * @see getErrorHighDefault, getWarnHighDefault, getWarnLowDefault, getErrorLowDefault
     */
    void setWarnLowDefault( const MonitorValue threshold );

    /**
     * Set the upper error default threshold.
     * @param threshold
     * @see setErrorHighDefault, setWarnHighDefault, setWarnLowDefault, setErrorLowDefault
     * @see getErrorHighDefault, getWarnHighDefault, getWarnLowDefault, getErrorLowDefault
     */
    void setErrorLowDefault( const MonitorValue threshold );

    /**
     * Get the upper error default threshold. Test with errorHighDefaultIsSet
     * before getting the value. The default value is sometimes in
     * range of valid values, so it is best to test whether the value was set
     * before using the value.
     *
     * @return threshold
     * @see setErrorHighDefault, setWarnHighDefault, setWarnLowDefault, setErrorLowDefault
     * @see getErrorHighDefault, getWarnHighDefault, getWarnLowDefault, getErrorLowDefault
     * @see errorHighDefaultIsSet
     */
    MonitorValue getErrorHighDefault () const;

    /**
     * Get the upper warning default threshold. Test with warnHighDefaultIsSet
     * before getting the value. The default value is sometimes in
     * range of valid values, so it is best to test whether the value was set
     * before using the value.
     *
     * @return threshold
     * @see setErrorHighDefault, setWarnHighDefault, setWarnLowDefault, setErrorLowDefault
     * @see getErrorHighDefault, getWarnHighDefault, getWarnLowDefault, getErrorLowDefault
     * @see warnHighDefaultIsSet
     */
    MonitorValue getWarnHighDefault () const;

    /**
     * Get the lower warning default threshold. Test with warnLowDefaultIsSet
     * before getting the value. The default value is sometimes in
     * range of valid values, so it is best to test whether the value was set
     * before using the value.
     *
     * @return threshold
     * @see setErrorHighDefault, setWarnHighDefault, setWarnLowDefault, setErrorLowDefault
     * @see getErrorHighDefault, getWarnHighDefault, getWarnLowDefault, getErrorLowDefault
     * @see warnLowDefaultIsSet
     */
    MonitorValue getWarnLowDefault () const;

    /**
     * Get the upper error default threshold. Test with errorLowDefaultIsSet
     * before getting the value. The default value is sometimes in
     * range of valid values, so it is best to test whether the value was set
     * before using the value.
     *
     * @return threshold
     * @see setErrorHighDefault, setWarnHighDefault, setWarnLowDefault, setErrorLowDefault
     * @see getErrorHighDefault, getWarnHighDefault, getWarnLowDefault, getErrorLowDefault
     * @see errorLowDefaultIsSet
     */
    MonitorValue getErrorLowDefault () const;

    /**
     * Returns true if a default value was set for warnLow for this
     * monitor point. Should be tested before using the value returned from
     * getWarnLowDefault.
     *
     * @param none
     * @return bool true if default was set for warnLow threshold.
     */
    bool warnLowDefaultIsSet() const;

    /**
     * Returns true if a default value was set for errorLow for this
     * monitor point. Should be tested before using the value returned from
     * getErrorLowDefault.
     *
     * @param none
     * @return bool true if default was set for errorLow threshold.
     */
    bool errorLowDefaultIsSet() const;

    /**
     * Returns true if a default value was set for warnHigh for this
     * monitor point. Should be tested before using the value returned from
     * getWarnHigh.
     *
     * @param none
     * @return bool true if default was set for warnHigh threshold.
     */
    bool warnHighDefaultIsSet() const;

    /**
     * Returns true if a default value was set for errorHigh for this
     * monitor point. Should be tested before using the value returned from
     * getErrorHigh.
     *
     * @param none
     * @return bool true if default was set for errorHigh threshold.
     */
    bool errorHighDefaultIsSet() const;

    /**
     * Set the validity flags for the MonitorPoint using the threshold
     * limits stored in the input MonitorPointThreshold object.
     *
     * @param threshold MonitorPointThreshold object holding user set
     *        threshold limits for this monitor point
     * @see setErrorHigh, setWarnHigh, setWarnLow, setErrorLow
     * @see getErrorHigh, getWarnHigh, getWarnLow, getErrorLow
     */
    virtual void evaluateTolerance( const MonitorPointThreshold & threshold );

    /**
     * Set a comment for the monitor point.
     * For example, "Thresholds changed because of A/D offset",
     * or "Intermittent encoder readout error around 25 degrees".
     * The comment time field is automatically updated.
     * Because the comments are usually retrieved from a configuration
     * along with the time of the comment and username,
     * this interface is usually just used for debugging.
     * The username "test" is automatically inserted.
     * @param text string
     * @see getComment, getCommentTime, getCommentUser
     */
    void setComment( const std::string & text );

    /**
     * Set all comment attributes for the monitor point.
     * Used when setting from a configuration file or database.
     * @param text string
     * @param user - userid for author of the comment
     * @param mjd - date of origin of comment
     * @see getComment, getCommentTime, getCommentUser
     */
    void setComment( const std::string & text,
                     const std::string & user,
                     double              mjd );

    /**
     * Clear all comment attributes for the monitor point.
     */
    void clearComment( );

    /**
     * Get a comment for the monitor point.
     * For example, "Thresholds changed because of A/D offset"
     * The comment time field is automatically updated.
     * @return comment string
     * @see setComment, getCommentTime, getCommentUser
     */
    std::string getComment() const;

    /**
     * Get username that generated comment for the monitor point.
     * @return username (userID) of originator of comment
     * @see setComment, getComment, getComment
     */
    std::string getCommentUser() const;

    /**
     * Get time last comment was inserted.
     * @return time of last comment in MJD
     * @see getComment, setComment
     */
    double getCommentTime() const;

    /**
     * Abstract method that updates the average value/validity for the frame.
     * Both computes and stores, based on the sample values/validities.
     * Must be implemented by concrete class.
     */
    virtual void updateFrameAverage( ScratchAverages & scratchAvgs ) = 0;

    /**
     * Abstract method that returns the data value as a string.
     * Must be implemented by concrete class.
     * @param sampleIndex sample index (0 is first sample), negative gets ave
     * @return data value as a string
     */
    virtual std::string getValueToString(int sampleIndex) const = 0;

    /**
     * Abstract method that returns the average value as a string.
     * Must be implemented by concrete class.
     * @return average data value as a string
     */
    virtual std::string getAverageToString() const = 0;

    /**
     * Gets the data value as a string, padded to requested width.
     * If the interpretation results in a string longer than width,
     * the string will be truncated to width and represented by
     * asterisks "*"'s.
     * @param width the string width to return
     * @param sampleIndex sample index (0 is first sample), negative gets ave
     * @return data value as a string
     */
    std::string getPaddedValueString(int width, int sampleIndex) const;

    /**
     * Gets the data value as a string, padded to internal width.
     * If the interpretation results in a string longer than width,
     * the string will be truncated to width and represented by
     * asterisks "*"'s.
     * @param sampleIndex sample index (0 is first sample), negative gets ave
     * @return data value as a string
     */
    std::string getPaddedValueString(int sampleIndex) const;

    /**
     * Gets the average value as a string, padded to requested width.
     * If the interpretation results in a string longer than width,
     * the string will be truncated to width and represented by
     * asterisks "*"'s.
     * @param width the string width to return
     * @return average value as a string
     */
    std::string getPaddedAverageString(int width) const;

    /**
     * Gets the average value as a string, padded to internal width.
     * If the interpretation results in a string longer than width,
     * the string will be truncated to width and represented by
     * asterisks "*"'s.
     * @return average value as a string
     */
    std::string getPaddedAverageString() const;

    /**
     * Get value as a string using the underlying basic type representation.
     * Returns underlying core value as a string with the full precision and 
     * range of it's basic type.
     */
    std::string getCoreValueAsString( int sampleIndex = 0 ) const;

    /**
     * Dumps name and value of monitor point as a short string (name=value).
     * No carriage returns or line feeds.
     * @param sampleIndex sample index (0 is first sample), negative gets ave
     * @return just the name=value.
     */
    virtual std::string toStringShort(int sampleIndex = 0) const;

    // Virtual, so doc is inherited
    std::string toString(bool canonicalName = false, bool verbose = false,
        bool value = true, int sampleIndex = 0, int indent = 0) const;

    // Virtual, so doc is inherited
    std::string toStringAverage(bool canonicalName = false,
        bool verbose = false, bool value = true, int indent = 0) const;

    // Virtual, so doc is inherited
    virtual std::string monitorPointTags(bool untagged = false) const;

    /**
     * Get the datatype of the value.
     * @return datatype of the monitor point value.
     * @see MonitorPoint
     */
    MonitorValueType getValuetype( ) const;

    /**
     * Get the type of the monitor point.
     * @return type of the monitor point.
     * @see MonitorPoint
     */
    MONITOR_POINT_TYPE getMonitorPointType( ) const;

    /**
     * Get the tagID (unique identifier of monitor point).
     * @return tagID.
     */
    tagIDType getTagID( ) const;

    /**
     * Get the number of samples per frame.
     * @return number of samples per frame.
     * @see setNumSamples
     */
    int getNumSamples( ) const;

    /**
     * Set the number of samples per frame.
     * This is virtual because some monitor points may want to
     * restrict the number of samples per frame.
     * @param nSamples number of samples per frame.
     * @see getNumSamples
     */
    virtual void setNumSamples(int nSamples) const;

    /**
     * Get the validity flag for a sample.
     * @return validity of the monitor point.
     * @param sampleIndex sample number (0 is first sample), defaults to 0
     * @see setValidity
     */
    VALIDITY getValidity( int sampleIndex ) const;
    VALIDITY getValidity( ) const;

    /**
     * Set the validity for a single sample.
     * @param validity of the monitor point.
     * @param sampleIndex sample index (0 is first sample), defaults to 0
     * @see getValidity
     */
    void setValidity(VALIDITY validity, int sampleIndex = 0) const;

    /**
     * Set the validity for all samples and the average.
     * @param validity of the monitor point.
     * @see getValidity setValidity
     */
    void setAllValidity(VALIDITY validity, bool markMpAsModified = true ) const;

    /**
     * Get the validity for the average.
     * This is virtual in case a specific type of monitor point needs
     * to do something different from the default implementation,
     * which returns the validity of MonitorSample(0), which is normally
     * reserved for the averagge.
     * @see getValidity setValidity
     */
    virtual VALIDITY getAveValidity() const;

    /**
     * Check for any of the valid states.
     * @param validity to check
     */
    static bool isValid(VALIDITY validity) ;

    /**
     * Check to see if the average is valid.
     * @return true if the average is valid
     */
    bool isAveValid() const;

    /**
     * Check if this monitor point validity is any of the valid states.
     * @param sampleIndex sample index (0 is first sample), defaults to 0
     */
    bool isValid( int sampleIndex ) const;
    bool isValid( ) const;

    /**
     * Set the validity for the average.
     * @see getValidity setValidity
     */
    void setAveValidity(VALIDITY validity) const;

    /**
     * Get the number of valid samples per frame.
     * @return number of valid samples per frame.
     * @see isValid
     */
    int getNumValidSamples() const;

    /**
     * Get the blanking/flagging flag for the average.
     * Only applies for the average.
     * @return blanking/flagging status of the average of the monitor point.
     * @see setBlankingFlagging
     */
    BLANKING_FLAGGING getBlankingFlagging() const;

    /**
     * Set the blanking/flagging flag for the average.
     * Blanking/flagging only apply to the average.
     * @param flag blanking/flagging status of the monitor point.
     * @see getBlankingFlagging
     */
    void setBlankingFlagging(BLANKING_FLAGGING flag) const;

    /**
     * Set the archive priority
     * Priority may be used by archiver to control data volume.,
     * @param priority the priority for archiving
     */
    void setArchivePriority(ARCHIVE_PRIORITY priority);

    /**
     * Set the default archive priority
     * If the archive priority is set to DEFAULT, it is replaced with the
     * new priority.
     * @param priority the priority for archiving
     */
    void setDefaultArchivePriority(ARCHIVE_PRIORITY priority);

    /**
     * Get the archive priority
     */
    ARCHIVE_PRIORITY getArchivePriority( ) const;

    /**
     * Set the basic sampling update interval.
     * This can be many frames in length.
     * Set to 1 if there are multiple samples in a frame.
     * May be used by archiver to control data volume.
     * @param interval sampling interval in half second frames
     */
    void setUpdateInterval(int interval);

    /**
     * Get the basic sampling update interval in frames.
     */
    int getUpdateInterval( ) const;

    /**
     * Determines whether data should be interpreted as time series
     * or as an array (spectrum) when there are multiple samples per frame.
     * Only numeric types can be an array, so the default implementation
     * of this method throws an exception.
     * Default is time series.
     * @param timeSeries true if data is time series, false if array
     * @exception ErrorException .
     */
    virtual void setTimeSeries(bool timeSeries);

    /**
     * Are multiple samples in a frame a time series or an array.
     */
    bool isTimeSeries( ) const;

    /**
     * Get a string representation for a valuetype.
     * @param valuetype
     */
    static std::string valuetypeToString(MonitorValueType valuetype);

    /**
     * Get a string representation for the datatype of this monitor point.
     */
    std::string valuetypeToString() const;

   /**
     * Get a string representation for a monitor point type.
     * @param monitorPointType
     */
    static std::string monitorPointTypeToString(MONITOR_POINT_TYPE monitorPointType);

    /**
     * Get a string representation for the monitor point type of this monitor
     * point.
     */
    std::string monitorPointTypeToString() const;

    /**
     * Get a string representation for a validity.
     * @param validity
     */
    static std::string validityToString(VALIDITY validity);

    /**
     * Get a string representation for a blanking/flagging flag.
     * @param flag
     */
    static std::string blankingFlaggingToString(BLANKING_FLAGGING flag);

    /**
     * Get a string for the archive priority for this monitor point.
     */
    std::string archivePriorityToString() const;

    /**
     * Create a string with sample and average values and validities.
     * Debugging quality. The validity is represented by a 'G' or 'B'
     * for good or bad.
     * @param includeAverage defaults to true
     * @param includeValidity defaults to true
     */
    virtual std::string dumpSamples(bool includeAverage  = true,
                                    bool includeValidity = true) const;

    /**
     * Set the monitor point header.
     * The monitor point is not usable until the header has been set
     * and storage set.
     */
    void setMonitorPointHeader(MonitorPointHeader header);

    /**
     * Set tagID
     * @param tagID tag id to assign to MP
     * @param assignedOTF if true the MP tagID is assigned on the fly
     */
    void setTagID(tagIDType tagID, bool      assignedOTF);

    /**
     * Controls whether averaging is done as a normal true average
     * or if it is just a snapshot of the last value.
     * Snapshot mode is useful for things that are averaged in other parts of the code
     * and then passed to an averager.
     * @param state if true, snapshot averaging is done; if false, normal averaging
     */
     void setSnapshotAverage(bool state);

    /**
     * Returns state of snapshot averaging for this MP
     * @see setSnapshotAverage
     */
     bool isSnapshotAverage() const;


//-----------------------------------------------------------------
// The methods that make this a monitor component

    // Specialize the docs..
    /**
     * Compares monitor point for equality to the monitor component passed.
     * The names must match; value checks will have to be
     * implemented by the derived types.
     * @param rhs monitor component to compare to
     */
    virtual bool operator==( const MonitorComponent & rhs ) const;

    /**
     * Compares monitor point for equality to the one passed.
     * The name and tagID's must match; value checks will have to be
     * implemented by the derived types.
     * @param rhs monitor point to compare to
     */
    virtual bool operator==( const MonitorPoint & rhs ) const;

    /**
     * Compares monitor point for precedence to the one passed.
     * This monitor point precedes the one passed if this monitor point's
     * tagID is less than that of one passed.
     * (*this) < (mpoint) == (this->getTagID() < mpoint.getTagID())
     * @param rhs monitor point to compare to
     */
    virtual bool operator<( const MonitorPoint & rhs ) const;

    // Virtual so docs are inherited
    void setNoData() const;

    // Virtual, so docs are inherited
    bool hasAllData() const;

    // Virtual, so docs are inherited
    virtual bool isMonitorPoint() const;


    /**
     * Method checks that threshold object corresponds to this monitor point -
     * checks to see that threshold's tagID == this monitor point's tagID,
     * and that value type of threshold == value type of this monitor point.
     * If the threshold fails the test, then the method throws an
     * ErrorException with a canned error message.
     *
     * @param threshold, const MonitorPointThreshold& reference to a
     *        MonitorPointThreshold object.
     * @exception throws a ::carma::monitor::InvalidThresholdException with
     *            a canned error message.
     */
    void checkThreshold( const MonitorPointThreshold & threshold ) const;

    // Virtual, so doc is inherited
    virtual ::std::string hierarchyToString(
        bool canonical = false,
        bool verbose = false,
        bool value = true,
        int  sampleIndex = 0,
        int  indent = 0,
        int  levels = -1 ) const;

    // Virtual, so doc is inherited
    virtual void hierarchyToVector(
        ::std::vector< ::std::string > & hierarchyList,
        bool                             canonical = false,
        bool                             verbose = false,
        int                              sampleIndex = 0 ) const;

    // Virtual, so doc is inherited
    virtual ::std::string hierarchyToStringAverage(
        bool canonical = false,
        bool verbose = false,
        bool value = true,
        int  indent = 0,
        int  levels = -1 ) const;

    // Virtual, so doc is inherited
    virtual std::string leafToString( bool verbose = false,
                                      bool value = true,
                                      int sampleIndex = 0 ) const;
                                      
    MonitorPointSample getSampleAverage() const;

protected:

    /**
     * Clears all default threshold values (makes them zero) and
     * sets thresholdFlags_ to THRESHOLD_NONE_SET.
     */
    void clearAllDefaults( );

public:

    /**
     * Get the monitor point sample.
     * @param sampleIndex sample index (0 is first sample)
     */
    MonitorPointSample getMonitorPointSample(int sampleIndex) const;
    MonitorPointSample getMonitorPointSample0() const;

protected:

    char                    getValueChar( int sampleIndex ) const;
    short                   getValueShort( int sampleIndex ) const;
    long                    getValueLong( int sampleIndex ) const;
    bool                    getValueBoolean( int sampleIndex ) const;
    float                   getValueFloat( int sampleIndex ) const;
    double                  getValueDouble( int sampleIndex ) const;
    ::std::complex< float > getValueComplex( int sampleIndex ) const;

    MonitorValueStringChunk getValueStringChunk( int sampleIndex ) const;

    long getValueSerialNo() const;

    void setValue( char d,                            int sampleIndex ) const;
    void setValue( short d,                           int sampleIndex ) const;
    void setValue( long d,                            int sampleIndex ) const;
    void setValue( bool d,                            int sampleIndex ) const;
    void setValue( float d,                           int sampleIndex ) const;
    void setValue( double d,                          int sampleIndex ) const;
    void setValue( const ::std::complex< float > & d, int sampleIndex ) const;

    void setValuesStringChunksAndValidities(
        const MonitorValueStringChunk * chunks,
        int                             numChunks,
        VALIDITY                        validity ) const;

    void setValueSerialNo( long d ) const;

    char                    getAveChar() const;
    short                   getAveShort() const;
    long                    getAveLong() const;
    bool                    getAveBoolean() const;
    float                   getAveFloat() const;
    double                  getAveDouble() const;
    ::std::complex< float > getAveComplex() const;
    long                    getAveSerialNo() const;

    void setAve( char d                            ) const;
    void setAve( short d                           ) const;
    void setAve( long d                            ) const;
    void setAve( bool d                            ) const;
    void setAve( float d                           ) const;
    void setAve( double d                          ) const;
    void setAve( const ::std::complex< float > & d ) const;

    void setAveSerialNo( long d ) const;

    bool isEqualTo( const MonitorPoint & mp ) const;
    std::string getSnapshotAverageToString() const;

private:
    explicit MonitorPoint( );

    // No copying
    MonitorPoint( const MonitorPoint & rhs );
    MonitorPoint & operator=( const MonitorPoint & rhs );

    std::string toString( bool canonicalName,
                          bool verbose,
                          bool value,
                          int  sampleIndex,
                          int  indent,
                          bool average ) const;

    struct CommentAttrs;
    struct DefThreshAttrs;

protected:
    bool                     timeSeries_;
    mutable short            width_;

private:
    std::string              units_;
    mutable short            precision_;
    bool                     persistent_;
    tagIDType                tagID_;
    const MonitorValueType   valuetype_;
    const MONITOR_POINT_TYPE monitorPointType_;
    MonitorPointHeader *     monitorPointHeader_;
    ARCHIVE_PRIORITY         archivePriority_;
    int                      updateInterval_;
    CommentAttrs *           commentAttrs_;
    bool                     snapshotAverage_;

    // These live here permanently - default values picked up from
    // mpml files.
    DefThreshAttrs *         defThreshAttrs_;
};


}  // namespace carma::monitor
}  // namespace carma


inline bool
carma::monitor::MonitorPoint::isTimeSeries( ) const
{
    return timeSeries_;
}


inline bool
carma::monitor::MonitorPoint::isPersistent( ) const
{
    return persistent_ ;
}


inline carma::monitor::tagIDType
carma::monitor::MonitorPoint::getTagID( ) const
{
    return tagID_;
}


inline carma::monitor::MonitorValueType
carma::monitor::MonitorPoint::getValuetype( ) const
{
    return valuetype_;
}


inline carma::monitor::MonitorPoint::MONITOR_POINT_TYPE
carma::monitor::MonitorPoint::getMonitorPointType( ) const
{
    return monitorPointType_;
}


inline carma::monitor::MonitorComponent::ARCHIVE_PRIORITY
carma::monitor::MonitorPoint::getArchivePriority( ) const
{
    return archivePriority_;
}


inline int
carma::monitor::MonitorPoint::getUpdateInterval( ) const
{
    return updateInterval_;
}


inline int
carma::monitor::MonitorPoint::getNumSamples( ) const
{
    return monitorPointHeader_->getNumSamplesPerCycle();
}

inline bool
carma::monitor::MonitorPoint::isSnapshotAverage( ) const
{
    return snapshotAverage_;
}


inline void
carma::monitor::MonitorPoint::setTagID(
    const carma::monitor::tagIDType tagId,
    const bool                      assignedOTF )
{
    tagID_ = tagId;

    // Do not archive if the tagID was assigned on-the-fly
    if ( assignedOTF )
        setArchivePriority( MonitorComponent::DONTARCHIVE );
}


inline carma::monitor::MonitorPointSample
carma::monitor::MonitorPoint::getSampleAverage( ) const
{
    return monitorPointHeader_->getSampleAverage();
}


inline bool
carma::monitor::MonitorPoint::isValid( const VALIDITY v )
{
    return ((v >= VALID) && (v < MAX_VALIDITY));
}


inline bool
carma::monitor::MonitorPoint::isAveValid( ) const
{
    return isValid( getAveValidity() );
}


inline carma::monitor::MonitorPoint::BLANKING_FLAGGING
carma::monitor::MonitorPoint::getBlankingFlagging( ) const
{
    return
        static_cast< BLANKING_FLAGGING >(
            getSampleAverage().getBlankingFlags() );
}


inline char
carma::monitor::MonitorPoint::getAveChar( ) const
{
    return getSampleAverage().getMonitorValue().byte;
}


inline short
carma::monitor::MonitorPoint::getAveShort( ) const
{
  return getSampleAverage().getMonitorValue().sh;
}


inline long
carma::monitor::MonitorPoint::getAveLong( ) const
{
    return getSampleAverage().getMonitorValue().lo;
}


inline bool
carma::monitor::MonitorPoint::getAveBoolean( ) const
{
    return getSampleAverage().getMonitorValue().bo;
}


inline float
carma::monitor::MonitorPoint::getAveFloat( ) const
{
    return getSampleAverage().getMonitorValue().fl;
}


inline double
carma::monitor::MonitorPoint::getAveDouble( ) const
{
    return getSampleAverage().getMonitorValue().db;
}


inline ::std::complex< float >
carma::monitor::MonitorPoint::getAveComplex( ) const
{
    const float * const c = getSampleAverage().getMonitorValue().complex;
    return ::std::complex< float >( c[0], c[1] );
}


inline long
carma::monitor::MonitorPoint::getAveSerialNo( ) const
{
    return getSampleAverage().getMonitorValue().sn;
}


#endif  // CARMA_MONITOR_MONITORPOINT_H
