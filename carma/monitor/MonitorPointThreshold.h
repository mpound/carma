/* MonitorPointThreshold.h - Contains class & types useful for
 * monitor point thresholding.
 *
 * @CarmaCopyright@
 */

/*!
 * @file MonitorPointThreshold.h
 * This is the class declaration file for a threshold frame that holds
 * all the threshold values for a monitor system.
 *
 * @author N. S. Amarnath
 *
 * File containing declarations for MonitorPointThreshold class.
 *
 */

#ifndef CARMA_MONITOR_POINT_THRESHOLD_H
#define CARMA_MONITOR_POINT_THRESHOLD_H

#include <complex>
#include <string>

#include "carma/util/ErrorException.h"
#include "carma/monitor/types.h"
#include "carma/monitor/MonitorPointSample.h"


namespace carma {
namespace monitor {


class MonitorPoint;


struct ThresholdStruct {
    long             flags;
    tagIDType        tagID;  // identifies associated monitor point
    MonitorValueType type;   // identifies type of threshold value
    MonitorValue     thresholdRange[THRESHOLD_NUM_VALUES];
    // warning range -
        // low end is warningRange[low]
        // high end is warningRange[high]
    // error range -
        // low end is errorRange[THRESHOLD_LOW_VALUE]
        // high end is errorRange[THRESHOLD_HIGH_VALUE]
};
// Note that warningRange is contained within errorRange, so
// warningRange[THRESHOLD_LOW_VALUE] >= errorRange[THRESHOLD_LOW_VALUE] and
// warningRange[THRESHOLD_HIGH_VALUE] <= errorRange[THRESHOLD_HIGH_VALUE]
// Also, warningRange[THRESHOLD_LOW_VALUE] <= warningRange[THRESHOLD_HIGH_VALUE], and
// errorRange[THRESHOLD_LOW_VALUE] <= errorRange[THRESHOLD_HIGH_VALUE].


class MonitorPointThreshold  {
  public:

    explicit MonitorPointThreshold( ThresholdStruct & thresholdStruct );

    ~MonitorPointThreshold( );

    /**
     * get tagID of associated monitor point
     */
    tagIDType getTagID( ) const;

    /**
     * get value type of associated monitor point
     */
    MonitorValueType getValueType( ) const;

    /**
     * @return * true if threshold has been set at least once.
     */
    bool isSet( ) const;

    /** @return  true if errorLow threshold is set  */
    bool errorLowIsSet( ) const;

    /** @return  true if warnLow is set at least once */
    bool warnLowIsSet( ) const;

    /** @return  true if errorHigh is set at least once */
    bool errorHighIsSet( ) const;

    /** @return true if warnHigh is set at least once */
    bool warnHighIsSet( ) const;

    void setRangeValue( ThresholdValueEnum lowHigh, MonitorValue value );
    void setRangeValue( ThresholdValueEnum lowHigh, char value );
    void setRangeValue( ThresholdValueEnum lowHigh, short value );
    void setRangeValue( ThresholdValueEnum lowHigh, long value );
    void setRangeValue( ThresholdValueEnum lowHigh, bool value );
    void setRangeValue( ThresholdValueEnum lowHigh, float value );
    void setRangeValue( ThresholdValueEnum lowHigh, double value );
    void setRangeValue( ThresholdValueEnum lowHigh, const ::std::string & value );
    void setRangeValue( ThresholdValueEnum lowHigh, ::std::complex< float > value );
    void setRangeValueSerialNo( ThresholdValueEnum lowHigh, long value );

    const MonitorValue& getThresholdValue( ThresholdValueEnum lowHigh ) const;
    char getByteThresholdValue( ThresholdValueEnum lowHigh ) const;
    short getShortThresholdValue( ThresholdValueEnum lowHigh ) const;
    long getLongThresholdValue( ThresholdValueEnum lowHigh ) const;
    bool getBoolThresholdValue( ThresholdValueEnum lowHigh ) const;
    float getFloatThresholdValue( ThresholdValueEnum lowHigh ) const;
    double getDoubleThresholdValue( ThresholdValueEnum lowHigh ) const;
    const ::std::string getStringThresholdValue( ThresholdValueEnum lowHigh ) const;
    const ::std::complex< float > getComplexThresholdValue( ThresholdValueEnum lowHigh ) const;
    long getSerialNoThresholdValue( ThresholdValueEnum lowHigh ) const;

    /**
     * Sets threshold values using default values stored in the
     * monitor point. These defaults values are obtained from
     * the mpml definition of the monitor point if set there, else
     * these values are zero.
     *
     * @param mp MonitorPoint& reference to monitor point corresponding
     *        to this threshold object.
     */
    void setThresholdValuesFromDefaults( const MonitorPoint & mp );

  protected:
    void unset( ThresholdValueEnum lowhigh );

  private:
    ThresholdStruct & thresholds_;
};


/**
 * @class InvalidThresholdexception
 * @brief thrown when wrong threshold object is passed to a monitor point.
 * Mismatch may be because tagID's of monitor point and threshold object
 * are not equal, or because value types are not the same.
 */
class InvalidThresholdException : public ::carma::util::ErrorException {
    public:
        /**
         * Constructor.
         */
        InvalidThresholdException( const ::std::string & message,
                                   const char *          fileName,
                                   int                   fileNo );
        /**
         * Destructor.
         */
        ~InvalidThresholdException( ) throw( );
};


} // namespace carma::monitor
} // namespace carma


inline
carma::monitor::MonitorPointThreshold::MonitorPointThreshold(
    ThresholdStruct & threshold ) :
thresholds_( threshold )
{
}


inline
carma::monitor::MonitorPointThreshold::~MonitorPointThreshold( )
{
}


inline long
carma::monitor::MonitorPointThreshold::getTagID( ) const
{
    return thresholds_.tagID;
}


inline carma::monitor::MonitorValueType
carma::monitor::MonitorPointThreshold::getValueType( ) const
{
    return thresholds_.type;
}


inline bool
carma::monitor::MonitorPointThreshold::isSet( ) const
{
    return (thresholds_.flags != THRESHOLD_NONE_SET);
}


inline bool
carma::monitor::MonitorPointThreshold::errorLowIsSet( ) const
{
    return ((thresholds_.flags & THRESHOLD_ERROR_LOW_SET)
                                           == THRESHOLD_ERROR_LOW_SET);
}


inline bool
carma::monitor::MonitorPointThreshold::warnLowIsSet( ) const
{
    return ((thresholds_.flags & THRESHOLD_WARN_LOW_SET)
                                           == THRESHOLD_WARN_LOW_SET);
}


inline bool
carma::monitor::MonitorPointThreshold::errorHighIsSet( ) const
{
    return ((thresholds_.flags & THRESHOLD_ERROR_HIGH_SET)
                                           == THRESHOLD_ERROR_HIGH_SET);
}


inline bool
carma::monitor::MonitorPointThreshold::warnHighIsSet( ) const
{
    return ((thresholds_.flags & THRESHOLD_WARN_HIGH_SET)
                                           == THRESHOLD_WARN_HIGH_SET);
}


inline const carma::monitor::MonitorValue &
carma::monitor::MonitorPointThreshold::getThresholdValue(
    const ThresholdValueEnum lowHigh ) const
{
    return thresholds_.thresholdRange[ lowHigh ];
}


inline char
carma::monitor::MonitorPointThreshold::getByteThresholdValue(
    const ThresholdValueEnum lowHigh ) const
{
    return getThresholdValue( lowHigh ).byte;
}


inline short
carma::monitor::MonitorPointThreshold::getShortThresholdValue(
    const ThresholdValueEnum lowHigh ) const
{
    return getThresholdValue( lowHigh ).sh;
}


inline long
carma::monitor::MonitorPointThreshold::getLongThresholdValue(
    const ThresholdValueEnum lowHigh ) const
{
    return getThresholdValue( lowHigh ).lo;
}


inline bool
carma::monitor::MonitorPointThreshold::getBoolThresholdValue(
    const ThresholdValueEnum lowHigh ) const
{
    return getThresholdValue( lowHigh ).bo;
}


inline float
carma::monitor::MonitorPointThreshold::getFloatThresholdValue(
    const ThresholdValueEnum lowHigh ) const
{
    return getThresholdValue( lowHigh ).fl;
}


inline double
carma::monitor::MonitorPointThreshold::getDoubleThresholdValue(
    const ThresholdValueEnum lowHigh ) const
{
    return getThresholdValue( lowHigh ).db;
}


inline const ::std::string
carma::monitor::MonitorPointThreshold::getStringThresholdValue(
    const ThresholdValueEnum lowHigh ) const
{
    return ::std::string( getThresholdValue( lowHigh ).str );
}


inline const ::std::complex< float >
carma::monitor::MonitorPointThreshold::getComplexThresholdValue(
    const ThresholdValueEnum lowHigh ) const
{
    return ::std::complex< float >( getThresholdValue( lowHigh ).complex[0],
                                    getThresholdValue( lowHigh ).complex[1] );
}


inline long
carma::monitor::MonitorPointThreshold::getSerialNoThresholdValue(
    const ThresholdValueEnum lowHigh ) const
{
    return getThresholdValue( lowHigh ).sn;
}


inline
carma::monitor::InvalidThresholdException::InvalidThresholdException(
    const ::std::string & message,
    const char * const    fileName,
    const int             fileNo ) :
::carma::util::ErrorException( message, fileName, fileNo )
{
}


inline
carma::monitor::InvalidThresholdException::~InvalidThresholdException( )
throw( )
{
}


#endif
