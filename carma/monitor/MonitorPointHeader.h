/**
 * @file
 *
 * Class wrapper for monitor point headers stored in a subsystem frame.
 *
 * @author: N. S. Amarnath
 *
 * $CarmaCopyright$
 *
 */

#ifndef CARMA_MONITOR_MONITOR_POINT_HEADER_H
#define CARMA_MONITOR_MONITOR_POINT_HEADER_H

#include "carma/monitor/types.h"
#include "carma/monitor/MonitorPointSample.h"


namespace carma  {
  namespace monitor  {


class MonitorPointSet;


struct MonitorHeader {
    tagIDType tagID;

    /**
     * @var unsigned short sampleOffset
     * @brief Offset into SubsystemFrame::monitorValues
     */
    ushort sampleOffset;

    /**
     * @var unsigned char nSamples
     * @brief Number of samples associated with the monitor point.
     *        Represents the sampling rate - # of samples per sampling
     *        cycle, where sampling cycle is half a second.
     */
    unsigned char nSamples;

    /**
     * @var type unsigned char
     * @brief Type of monitor sample - must be one of the possible
     *        types supported by the union structure 
     *        ::carma::monitor::MonitorValue.
     */
    unsigned char type;
    
    static unsigned char valueType2Char( MonitorValueType mvt );

    static MonitorValueType char2ValueType( unsigned char uc );

    tagIDType getTagID( ) const;

    ushort getSampleOffset( ) const;

    ushort getNumSamplesPerCycle( ) const;

    MonitorValueType getValueType( ) const;
    void setValueType( MonitorValueType mvt );
};  // 8 bytes


/*!
 * @class MonitorPointHeader monitorPointHeader.h "carma/monitor/monitorPointHeader.h"
 * @brief Wrapper class for monitor point header information.
 *
 * A wrapper class to manage header information for monitor points - manages
 * tagID, sample rate, a reference to the collection of sampled data, Also
 * provides interfaces for getting sample data and the averaged value of
 * sampled data.
 */
class MonitorPointHeader {
  public:
    /*! 
     * @brief public constructor to build MonitorPointHeader object.
     *               Requires reference to a MonitorHeader structure and a
     *               pointer to the collection of sampled data associated with 
     *               the monitor point.
     *
     * The intent is to put a wrapper around a MonitorHeader structure
     * so that monitor header information can be modified and read
     * using methods, rather than direct structure access. MonitorHeader
     * structures should not exist in isolation - they are part of a 
     * SubsystemHeader of as part of MonitorSystemFrame. As a result the 
     * constructor does not construct an instance of a MonitorHeader structure.
     *
     * @param header MonitorHeader& reference to MonitorHeader 
     *        structure
     * @param samples MonitorSampleValue* const pointer to beginning of
     *        array of monitor point samples in the enclosing
     *        subsystem frame.
     * @param index int offset of this monitor point header that represents the 
     *        monitor point in the containing SubsystemFrame.
     * @param set MonitorPointSet* const pointer to MonitorPointSet
     *        that contains and manages storage for this monitor point.
     * @return none - constructor.
     * @see carma::monitor::MonitorHeader
     * @see carma::monitor::MonitorSampleValue
     * @see carma::monitor::MonitorPointSample
     * @see carma::monitor::SubsystemHeader
     * @see carma::monitor::SubsystemFrame
     */
    MonitorPointHeader( MonitorHeader &      header,
                        MonitorSampleValue * samples,
                        int                  index = -1,
                        MonitorPointSet *    set = 0 );

     /*!
      * @brief Destructor
      */
    ~MonitorPointHeader( );

    /*! 
     * @brief operator for assigning values to a MonitorPointHeader.
     *
     * This method uses standard techniques to allow for a MonitorPointHeader
     * to be part of treated as either an rvalue or as an lvalue.
     *
     * @return reference to the modified MonitorPointHeader.
     */
    MonitorPointHeader & operator=( const MonitorPointHeader & rhs );


    /*! 
     * @brief gets the unique tagID for a monitor point.
     *
     * The tagID is a structured ID which is unique across all of CARMA.
     * It identifies a monitor point, and is used within all Frame classes
     * to address monitor points. 
     *
     * @return tagIDType tagID
     * @see carma::monitor::MonitorHeader::tagID
     */
    tagIDType getTagID( ) const;

    /*! 
     * @brief gets the number of samples per sampling cycle - which is
     *        currently standardized at half-second, and is managed
     *        by carma::util::Time.
     *
     * The number of samples per cycle is determined by the sampling rate
     * for the monitor point, which, for hardware, is determined by the micro
     * controlling that piece of hardware.
     *
     * @return unsigned short # of samples per sampling cycle
     * @see carma::util::Time
     * @see carma::monitor::MonitorHeader::nSamples
     */
    ushort getNumSamplesPerCycle( ) const;

    /*! 
     * @brief sets the number of samples per sampling cycle - which is
     *        currently standardized at half-second. Time to next cycle
     *        is determined by carma::util::Time.
     *
     * The number of samples per cycle is determined by the sampling rate
     * for the monitor point, which, for hardware, is determined by the micro
     * controlling that piece of hardware.
     *
     * @return void
     * @see carma::monitor::MonitorHeader::nSamples
     */
    void setNumSamplesPerCycle( const ushort nSamples );

    /*! 
     * @brief gets a reference to the set of sampled data associated with
     *        the monitor point.
     *
     * The set of sampled data associated with the monitor point contains
     * a number of samples determined by the number of samples per cycle, 
     * i.e. numSamplesPerCycle. This method provides a reference to the
     * set of sampled monitor values within a frame structure.
     *
     * @return unsigned short reference to set of sampled values within 
     *         a frame structure (SubsystemHeader or MonitorSystemFrame).
     * @see carma::monitor::MonitorPointSample
     * @see carma::monitor::SubsystemHeader
     * @see carma::monitor::MonitorSystemFrame
     */
    ushort getSampleOffset( ) const;

    /*! 
     * @brief sets a reference to the set of sampled data associated with
     *        the monitor point. For use by Frame methods only
     *
     * The set of sampled data associated with the monitor point contains
     * a number of samples determined by the number of samples per cycle, 
     * i.e. numSamplesPerCycle. This method provides a way to set this 
     * reference to the set of sampled monitor values within a frame structure.
     *
     * @return void
     * @see carma::monitor::MonitorPointSample
     * @see carma::monitor::SubsystemHeader
     * @see carma::monitor::MonitorSystemFrame
     */
    void                setSampleOffset (const ushort sampleOffset)  {  this->header_.sampleOffset = sampleOffset;  }

    void setValueType( MonitorValueType mvt );

    MonitorValueType getValueType( ) const;

    const MonitorSampleValue & getMonitorSampleValueRef( int iSample ) const;
    MonitorSampleValue & getMonitorSampleValueRef( int iSample );

    const MonitorSampleValue & getAverageMonitorSampleValueRef( ) const;
    MonitorSampleValue & getAverageMonitorSampleValueRef( );

    /*! 
     * @brief gets the ith sample from the set of sampled monitor values 
     *        associated with the monitor point.
     *
     * The number of samples per cycle is determined by the sampling rate
     * for the monitor point, which, for hardware, is detrmined by the micro
     * controlling that piece of hardware. This method gets the ith sampled
     * monitor data value, where i ranges form 1 through nSamplesPerCycle.
     *
     * @return sampled monitor value as a MonitorPointSample object.
     * @see carma::monitor::MonitorPointSample.
     * @see carma::monitor::MonitorHeader::nSamples
     */
    MonitorPointSample getMonitorPointSample( int iSample );


    /*! 
     * @brief gets the value of sampled data averaged over all 
     *        monitor point samples that contain filled up data.
     *
     * Gets the averaged value as a MonitorPointSample object. Average value
     * is meaningful only for monitor points associated with numeric value 
     * types, namely float, double, long, short, byte and complex.
     *
     * @return value averaged over set of sampled data associated with 
     *         the monitor point, as a MonitorPointSample object.
     * @see carma::monitor::MonitorPointSample.
     */
    MonitorPointSample getSampleAverage( );

    /*! 
     * @brief clears all the samples associated with this header.
     *
     * Clears flags and, if sample is to be deallocated, resets all fields
     * of the sample. To deallocate sample, 'deallocate' must be set to 'true'.
     *
     * @param bool deallocate free sample storage if true, default value 
     *             is false.
     * @return none
     */
    void        clearSamples (bool deallocate = false);

    /*! 
     * @brief sets the the filed "nSamples" in the MonitorHeader struct.
     *        Meant for use by other classes in this file. Do not use
     *        this method to change the sample rate - use setNumSamplesPerCycle
     *        instead.
     *
     * The number of samples per cycle is determined by the sampling rate
     * for the monitor point, which, for hardware, is detrmined by the micro
     * controlling that piece of hardware.
     *
     * @return void
     * @see carma::monitor::MonitorHeader::nSamples
     * @see carma::monitor::MonitorPointHeader::setNumSamplesPerCycle
     */
    void                setSamplesPerCycle (const ushort nSamples)
        {  this->header_.nSamples = nSamples;  }
  
  private:

    /*! 
     * @brief reference to header information for the monitor point.
     *
     * It is assumed that the class is a wrapper around an existing 
     * MonitorHeader structure. This variable stores the reference to that
     * existing strcuture.
     *
     * @see carma::monitor::MonitorHeader.
     */
    MonitorHeader & header_;

    /*! 
     * @brief stores a pointer to the first element in the set of sampled 
     *        values associated with the monitor point.
     *
     * This class is a wrapper around existing monitor point information.
     * The class requires a way to get the collection of sampled data to
     * return sampled data and averages. This variable stores the pointer
     * to the collection of sampled data, seen as an array.
     *
     * @see carma::monitor::MonitorSampleValue.
     * @see carma::monitor::MonitorPointHeader::getMonitorPointSample.
     */
    MonitorSampleValue*        const samples_;

    /**
     * @brief index of this monitor point header in ths subsystem 
     * monitor header array.
     *
     * @see ::carma::monitor::SubsystemFrame
     */
    const int        index_;

    /**
     * @brief Points to enclosing MonitorPointSet if one is associated with
     * this MonitorPointHeader.
     *
     * @see ::carma:;monitor::MonitorPointSet
     */
    MonitorPointSet * const        set_;
};


} } // namespace monitor, carma


inline unsigned char
carma::monitor::MonitorHeader::valueType2Char( const MonitorValueType mvt )
{
    return static_cast< unsigned char >( mvt );
}


inline carma::monitor::MonitorValueType
carma::monitor::MonitorHeader::char2ValueType( const unsigned char uc )
{
    return static_cast< MonitorValueType >( uc );
}


inline carma::monitor::tagIDType
carma::monitor::MonitorHeader::getTagID( ) const
{
    return tagID;
}


inline ushort
carma::monitor::MonitorHeader::getSampleOffset( ) const
{
    return sampleOffset;
}


inline ushort
carma::monitor::MonitorHeader::getNumSamplesPerCycle( ) const
{
    return nSamples;
}


inline carma::monitor::MonitorValueType
carma::monitor::MonitorHeader::getValueType( ) const
{
    return char2ValueType( type );
}


inline void
carma::monitor::MonitorHeader::setValueType( const MonitorValueType mvt )
{
    type = valueType2Char( mvt );
}


inline
carma::monitor::MonitorPointHeader::MonitorPointHeader(
    MonitorHeader &            header,
    MonitorSampleValue * const samples,
    const int                  index,
    MonitorPointSet * const    set ) :
header_( header ),
samples_( samples ),
index_( index ),
set_( set )
{
}


inline
carma::monitor::MonitorPointHeader::~MonitorPointHeader( )
{
}


inline carma::monitor::MonitorPointHeader &
carma::monitor::MonitorPointHeader::operator=( const MonitorPointHeader & rhs )
{
    if ( this != &rhs )
        header_ = rhs.header_;

    return *this;
}


inline carma::monitor::tagIDType
carma::monitor::MonitorPointHeader::getTagID( ) const
{
    return header_.tagID;
}


inline ushort
carma::monitor::MonitorPointHeader::getSampleOffset( ) const
{
    return header_.sampleOffset;
}


inline ushort
carma::monitor::MonitorPointHeader::getNumSamplesPerCycle( ) const
{
    return header_.nSamples;
}


inline carma::monitor::MonitorValueType
carma::monitor::MonitorPointHeader::getValueType( ) const
{
    return header_.getValueType();
}


inline void
carma::monitor::MonitorPointHeader::setValueType( const MonitorValueType mvt )
{
    header_.setValueType( mvt );
}


inline const carma::monitor::MonitorSampleValue &
carma::monitor::MonitorPointHeader::getMonitorSampleValueRef(
    const int iSample ) const
{  
    return samples_[ header_.sampleOffset + iSample ];
}


inline carma::monitor::MonitorSampleValue &
carma::monitor::MonitorPointHeader::getMonitorSampleValueRef(
    const int iSample )
{  
    return samples_[ header_.sampleOffset + iSample ];
}


inline const carma::monitor::MonitorSampleValue &
carma::monitor::MonitorPointHeader::getAverageMonitorSampleValueRef( ) const
{  
    return samples_[ header_.sampleOffset ];
}


inline carma::monitor::MonitorSampleValue &
carma::monitor::MonitorPointHeader::getAverageMonitorSampleValueRef( )
{  
    return samples_[ header_.sampleOffset ];
}


inline carma::monitor::MonitorPointSample
carma::monitor::MonitorPointHeader::getMonitorPointSample( const int iSample )
{  
    return MonitorPointSample( samples_[ header_.sampleOffset + iSample ],
                               index_,
                               set_);
}


inline carma::monitor::MonitorPointSample
carma::monitor::MonitorPointHeader::getSampleAverage( )
{  
    return MonitorPointSample( samples_[ header_.sampleOffset ],
                               index_,
                               set_ ); 
}


#endif // CARMA_MONITOR_MONITOR_POINT_HEADER_H
