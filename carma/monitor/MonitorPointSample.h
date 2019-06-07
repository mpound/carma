/**
 * @file
 *
 * Class wrapper for monitor point samples stored in a subsystem frame.
 *
 * @author: N. S. Amarnath
 *
 * $CarmaCopyright$
 *
 */

#ifndef CARMA_MONITOR_MONITOR_POINT_SAMPLE_H
#define CARMA_MONITOR_MONITOR_POINT_SAMPLE_H

#include <string>

#include "carma/corba/corba.h"
#include "carma/util/BaseException.h"
#include "carma/monitor/monitorframe.h"
#include "carma/monitor/types.h"

namespace carma {

namespace monitor {

struct TransportMonitorValue;
struct TransportMonitorSample;

/**
 * Union of all possible types of monitor sample
 * values. Designed for a maximum size of 8 bytes.
 * Note the two floats, first for the real part
 * and the second for the imaginary part of the
 * complex value.
 */
typedef union {
    uchar       byte;
    short       sh;
    long        lo;
    bool        bo;
    float       fl;
    double      db;
    float       complex[2];
    char        str[8];
    long        sn;
} MonitorValue; // 8 bytes

/**
 * Structure for storing monitor point sample, including value.
 * this structure directly stores validity/blanking flags,
 * sample number, and a dummy thats used to point back to
 * the monitor point header, modulo max value (if the sample
 * number is > 0).
 */
struct MonitorSampleValue {
    MonitorValue        value;
    uchar               blankingFlags;
    uchar               validityFlags;
    uchar               iSample;
    uchar               dummy;

    uchar getValidityFlags( ) const;
    uchar getBlankingFlags( ) const;
}; // 12 bytes


struct MonitorValueStringChunk {
    char chunkChars[8];
};


class MonitorPointSet;

/**
 * @class MonitorPointSample
 * @brief Class representing a data sample for a MonitorPoint. Manages
 * access to low level storage structures and handles the bookkeeping.
 */
class MonitorPointSample {
  public:
    /**
     * @constructor
     * Constructor - takes the base level structure, index to the
     * corresponding monitor point's header information and a pointer
     * to the enclosing MonitorPointSet which is non-NULL when in
     * a subsystem.
     *
     * @param sample MonitorSampleValue& reference to MonitorSampleValue
     *        structure
     * @param index int offset of monitor point header that represents the
     *        monitor point to which this sample is assigned.
     * @param set MonitorPointSet* const pointer to MonitorPointSet
     *        that contains and manages storage for monitor point and
     *        this assigned sample.
     */
    explicit MonitorPointSample( MonitorSampleValue & sample,
                                 int                  index = -1,
                                 MonitorPointSet *    set = 0 );

     /**
      * Destructor
      */
    ~MonitorPointSample( );

    /**
     * Method to get monitor sample value as a union.
     *
     * @return MonitorValue& union structure with value expressed as
     *         nine possible types.
     */
    MonitorValue & getMonitorValue () const;

    /**
     * Method to set monitor sample value as a char.
     *
     * @param value value expressed as a char.
     * @param saneFlags validity flags which are already known to be "sane".
     */
    void setMonitorValueAndKnownSaneValidityFlags( char  value,
                                                   uchar saneFlags );
    /**
     * Method to set monitor sample value as a short.
     *
     * @param value value expressed as a short.
     * @param saneFlags validity flags which are already known to be "sane".
     */
    void setMonitorValueAndKnownSaneValidityFlags( short value,
                                                   uchar saneFlags );

    /**
     * Method to set monitor sample value as a long.
     *
     * @param value value expressed as a long.
     * @param saneFlags validity flags which are already known to be "sane".
     */
    void setMonitorValueAndKnownSaneValidityFlags( long  value,
                                                   uchar saneFlags );

    /**
     * Method to set monitor sample value as a bool.
     *
     * @param value value expressed as a bool.
     * @param saneFlags validity flags which are already known to be "sane".
     */
    void setMonitorValueAndKnownSaneValidityFlags( bool  value,
                                                   uchar saneFlags );

    /**
     * Method to set monitor sample value as a float.
     *
     * @param value value expressed as a float.
     * @param saneFlags validity flags which are already known to be "sane".
     */
    void setMonitorValueAndKnownSaneValidityFlags( float value,
                                                   uchar saneFlags );

    /**
     * Method to set monitor sample value as a double.
     *
     * @param value value expressed as a double.
     * @param saneFlags validity flags which are already known to be "sane".
     */
    void setMonitorValueAndKnownSaneValidityFlags( double value,
                                                   uchar  saneFlags );

    /**
     * Method to set monitor sample value as a complex.
     *
     * @param value value expressed as a complex.
     * @param saneFlags validity flags which are already known to be "sane".
     */
    void setMonitorValueAndKnownSaneValidityFlags( const float value[2],
                                                   uchar       saneFlags );

    /**
     * Method to set monitor sample value as a string chunk
     * and already "sane" validity flags at the same.
     *
     * @param value value expressed as a string chunk.
     * @param saneFlags validity flags which are already known to be "sane".
     */
    void setValueStringChunkAndKnownSaneValidityFlags(
        const MonitorValueStringChunk & value,
        uchar                           saneFlags );

    /**
     * Method to set monitor sample value as a serial number.
     *
     * @param value value expressed as a serial number.
     * @param saneFlags validity flags which are already known to be "sane".
     */
    void setValueSerialNoAndKnownSaneValidityFlags( long  value,
                                                    uchar saneFlags );

    /**
     * Method to return blanking flag value of the monitor sample.
     * The value is numerically equal to the value of one of the
     * enum constants of the enum type MonitorPoint::BLANKING_FLAGGING.
     *
     * @return uchar blanking/flagging value assigned to monitor point
     *               sample.
     * @see ::carma::monitor::MonitorPoint::BLANKING_FLAGGING
     */
    uchar getBlankingFlags( ) const;

    /**
     * Method to set blanking flag value of the monitor sample.
     * The value should be numerically equal to the value of one of the
     * enum constants of the enum type MonitorPoint::BLANKING_FLAGGING.
     *
     * @param flags uchar blanking/flagging value assigned to monitor point
     *               sample.
     * @see ::carma::monitor::MonitorPoint::BLANKING_FLAGGING
     */
    void setBlankingFlags( uchar flags );

    /**
     * Method to clear blanking flag value of the monitor sample.
     * The blanking/flagging value is set to MonitorPoint::UNDETERMINED.
     *
     * @return none
     * @see ::carma::monitor::MonitorPoint::BLANKING_FLAGGING
     */
    void clearBlankingFlags( );

    /**
     * Method to return sample number asscoiated with this MonitorPointSample.
     * Value is returned as an unsigned short, which restricts the number
     * of samples to a maximum of 65536 per monitor point
     *
     * @return ushort unsigned short representing the monitor point
     *         sample number.
     */
    ushort getSampleNumber( ) const;

    /**
     * Method to set sample number associated with this MonitorPointSample.
     * Value is set as an unsigned short, which restricts the number
     * of samples to a maximum of 65536 per monitor point
     *
     * @param iSample unsigned short representing the monitor point
     *         sample number.
     */
    void setSampleNumber( ushort iSample );

    /**
     * Method to return validity flag value of the monitor sample.
     * The value is numerically equal to the value of one of the
     * enum constants of the enum type MonitorPoint::VALIDITY.
     *
     * @return uchar validity value assigned to monitor point
     *               sample.
     * @see ::carma::monitor::MonitorPoint::VALIDITY
     */
    uchar getValidityFlags( ) const ;

    /**
     * Method to set validity flag value of the monitor sample.
     * The value should be numerically equal to the value of one of the
     * enum constants of the enum type MonitorPoint::VALIDITY. If within
     * a subsystem, then it marks the corresponding monitor point as
     * modified and ready for transport.
     *
     * @param flags uchar validity value assigned to monitor point
     *               sample.
     * @see ::carma::monitor::MonitorPoint::VALIDITY
     */
    void setValidityFlags( uchar flags );

    void setKnownSaneValidityFlags( uchar saneFlags, 
                                    bool markMpAsModified = true );

    /**
     * Method to clear validity flag value of the monitor sample.
     * The blanking/flagging value is set to MonitorPoint::INVALID_NO_DATA.
     *
     * @return none
     * @see ::carma::monitor::MonitorPoint::INVALID_NO_DATA
     */
    void        clearValidityFlags ();

    /**
     * Method to clear validity and blanking/flagging flag values of the
     * monitor sample. The blanking/flagging value is cleared using
     * MonitorPointSample::clearBlankingFlags and the validity is cleared
     * using MonitorPointSample::clearValidityFlags.
     *
     * @return none
     * @see ::carma::monitor::MonitorPoint::clearBlankingFlags
     * @see ::carma::monitor::MonitorPoint::clearValidityFlags
     */
    void        clearFlags ();

    /**
     * Assignment operator. Returns a reference to this MonitorPointSample,
     * which has values assigned from MonitorPointSample rhs.
     *
     * @param rhs MonitorPointSample& reference to MonitorPointSample
     *        to assign from.
     * @return MonitorPointSample& reference to this MonitorPointSample
     *         with its private variables assigned to the same values
     *         as the corresponding private variables in rhs.
     */
    MonitorPointSample & operator=( const MonitorPointSample & rhs );

    /**
     * Associates this sample with a monitor point represented by
     * a MonitorPointHeader at offset headerOffset within the
     * subsystem frame containing this sample. A MonitorPointSample is
     * allocated if
     * o dummy != 0 &&
     * o MonitorPoint::numSamples() == 1 && iSample != 0
     *
     * @param headerOffset ushort offset of MonitorPointHeader within
     *        the subsystem frame that manages the storage for this sample.
     * @param iSample ushort sample number of the sample asssigned to the
     *        monitor point represented by headerOffset. Sample numbers
     *        start with 1.
     * @return none
     * @see ::carma::monitor::MonitorPointHeader
     * @see ::carma::monitor::SubsystemFrame
     */
    void        allocate (ushort headerOffset, ushort iSample);

    /**
     * Clears all flags and (optionally) deallocates the sample so
     * the sample is no longer associated with a monitor point.
     * which has values assigned from MonitorPointSample rhs.
     *
     * @param deallocate bool true if sample must be de-allocated.
     */
    void                clear (bool deallocate = false);

    /**
     * Returns a reference to the internal MonitorSampleValue structure.
     *
     * @return MonitorSampleValue& reference to internal structure
     * @see ::carma::monitor::MonitorSampleValue
     */
    MonitorSampleValue & getSample( ) const;

    /**
     * This method and MonitorPointSample:;getTransportedSample are
     * transport related methods. These are methods provided to make
     * transport of monitor data convenient. Loads monitor point sample data
     * from this sample into a TransportMonitorSample so its ready to be
     * transported out.
     *
     * @param outSample TransportMonitorSample& sample to be transported out
     * @param mvt MonitorValueType type of the monitor sample
     * @return TransportMonitorSample& reference to TransportMonitorSample
     *         with sample data loaded into it.
     * @see ::carma::monitor::MonitorValueType
     * @see ::carma::monitor::TransportMonitorSample
     */
    void fillInTransportSample( CORBA::ULong                    metaIdx,
                                MonitorSampleValues &    outSamples,
                                MonitorValueType                mvt,
                                tagIDType                       tagId,
                                int                             sampleIndex ) const;
    /**
     * This method and MonitorPointSample::fillInTransportSample are
     * transport related methods. These are methods provided to make
     * transport of monitor data convenient. Retrieves sample data
     * from a TransportMonitorSample structure and loads it into this
     * MonitorPointSample.
     *
     * @param valueType const MonitorValueType type of the monitor sample
     * @param inSample const TransportMonitorSample& transported sample data to
     *        be loaded into this sample.
     * @param headerOffset const int offset of MonitorPointHeader for the
     *        monitor point associated with this sample. Offset is measured
     *        as an index within the SubsystemFrame containing this sample.
     * @param iSample const ushort sample number of transported sample
     *        == monitor point sample number.
     * @return none
     * @see ::carma::monitor::SubsystemFrame
     * @see ::carma::monitor::MonitorPointHeader
     * @see ::carma::monitor::MonitorValueType
     * @see ::carma::monitor::TransportMonitorSample
     */
    void getTransportedSample( CORBA::ULong metaIdx,
                               const MonitorSampleValues & inValues,
                               int                            headerOffset );

    /**
     * Sets monitor point sample value to be transported using
     * value from this sample. Convenience method.
     *
     * @param valueType MonitorValueType type of monitor point sample value
     * @param value TransportMonitorValue& transport structure into which
     *        this sample's value will be written.
     * @return none
     * @see ::carma::monitor::MonitorValueType
     * @see ::carma::monitor::TransportMonitorValue
     */
    void setTransportValue(MonitorValueType       valueType,
                           TransportMonitorValue& value ) const;
                           
  protected:
    /**
     * Returns internal union structure containingv the value of
     * the monitor point sample. Discriminating the union requires
     * the monitor points value type, which has to be obtained from
     * the corresponing monitor point header.
     *
     * @return MonitorValue& reference to internal union structure containing
     *         this sample's value.
     */
    MonitorValue & getValue( ) const;

  private:
    void setMonitorValueAndKnownSaneValidityFlags(
        const MonitorValue & value,
        uchar                saneFlags );

    /**
     * Sets monitor point sample value to be transported using
     * value from this sample. Convenience method.
     *
     * @param valueType MonitorValueType type of monitor point sample value
     * @param value TransportMonitorValue& transport structure into which
     *        this sample's value will be written.
     * @return none
     * @see ::carma::monitor::MonitorValueType
     * @see ::carma::monitor::TransportMonitorValue
     */
    void getTransportedValue( MonitorValueType              valueType,
                              const TransportMonitorValue & value );


    // MonitorSampleValue structure in subsystem frame container.
    MonitorSampleValue & sample_;

    // index to monitor point header in containing subsystem frame
    const int index_;

    // pointer to containing MonitorPointSet
    MonitorPointSet * const set_;
};



/*!
 * @class SampleInvalidExceptionObj MonitorPointSample.h "carma/monitor/MonitorPointSample.h"
 * @brief Exception class thrown when an invalid sample is detected.
 *
 * An exception class to inform users that an invalid sample has been
 * detected. A sample is invalid if the value type is unrecognizable,
 * or if the sample number is greater than the allowed number of samples
 * for that monitor point.
 */
class SampleInvalidExceptionObj : public virtual carma::util::BaseException  {
    public:
        explicit SampleInvalidExceptionObj(
            const char * mesg,
            const char * fileName = __FILE__,
            int          lineNum = __LINE__ );

        explicit SampleInvalidExceptionObj(
            const std::ostringstream & errStream,
            const char *               fileName = __FILE__,
            int                        lineNum = __LINE__ );

        SampleInvalidExceptionObj(
            const MonitorSampleValue & sample,
            MonitorValueType           type,
            const char *               fileName = __FILE__,
            int                        lineNum = __LINE__ );

        SampleInvalidExceptionObj(
            const MonitorPointSample & sample,
            MonitorValueType           type,
            const char *               fileName = __FILE__,
            int                        lineNum = __LINE__ );
};

} } // namespace monitor, carma


inline carma::monitor::uchar
carma::monitor::MonitorSampleValue::getValidityFlags( ) const
{
    return validityFlags;
}


inline carma::monitor::uchar
carma::monitor::MonitorSampleValue::getBlankingFlags( ) const
{
    return blankingFlags;
}


inline
carma::monitor::MonitorPointSample::MonitorPointSample(
    MonitorSampleValue &    sample,
    const int               index,
    MonitorPointSet * const set ) :
sample_( sample ),
index_( index ),
set_( set )
{
}


inline
carma::monitor::MonitorPointSample::~MonitorPointSample( )
{
}


inline carma::monitor::MonitorPointSample &
carma::monitor::MonitorPointSample::operator=( const MonitorPointSample & rhs )
{
    if ( this != &rhs ) {
        sample_.value         = rhs.sample_.value;
        sample_.blankingFlags = rhs.sample_.blankingFlags;
        sample_.validityFlags = rhs.sample_.validityFlags;
    }

    return *this;
}


inline carma::monitor::MonitorValue &
carma::monitor::MonitorPointSample::getMonitorValue( ) const
{
    return sample_.value;
}


inline carma::monitor::uchar
carma::monitor::MonitorPointSample::getValidityFlags( ) const
{
    return sample_.validityFlags;
}


inline carma::monitor::uchar
carma::monitor::MonitorPointSample::getBlankingFlags( ) const
{
    return sample_.blankingFlags;
}


inline unsigned short
carma::monitor::MonitorPointSample::getSampleNumber( ) const
{
    return sample_.iSample;
}


inline void
carma::monitor::MonitorPointSample::setSampleNumber( const ushort iSample )
{
    sample_.iSample = iSample;
}


inline carma::monitor::MonitorSampleValue &
carma::monitor::MonitorPointSample::getSample( ) const
{
    return sample_;
}


inline carma::monitor::MonitorValue &
carma::monitor::MonitorPointSample::getValue( ) const
{
    return sample_.value;
}


inline void
carma::monitor::MonitorPointSample::setBlankingFlags( const uchar flags )
{
    sample_.blankingFlags = flags;
}


inline void
carma::monitor::MonitorPointSample::clearBlankingFlags( )
{
    sample_.blankingFlags = 0;
}


inline void
carma::monitor::MonitorPointSample::clearValidityFlags( )
{
    sample_.validityFlags = 0;
}


inline void
carma::monitor::MonitorPointSample::clearFlags( )
{
    sample_.blankingFlags = 0;
    sample_.validityFlags = 0;
}


inline void
carma::monitor::MonitorPointSample::clear( const bool deallocate )
{
    sample_.blankingFlags = 0;
    sample_.validityFlags = 0;

    if ( deallocate ) {
        sample_.dummy = 0;
        sample_.iSample = 0;
    }
}


inline void
carma::monitor::MonitorPointSample::allocate( const ushort headerOffset,
                                              const ushort iSample )
{
    sample_.blankingFlags = 0;
    sample_.validityFlags = 0;
    sample_.dummy = (headerOffset & 0x00FF);
    sample_.iSample = iSample;
}


#endif // CARMA_MONITOR_MONITOR_POINT_SAMPLE_H
