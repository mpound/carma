#ifndef CARMA_MONITOR_ACCUMULATORT_H
#define CARMA_MONITOR_ACCUMULATORT_H

/*
 * MonitorPointAccumulatorT.h - Template file for accumulating monitor point
 * values of various types, all numeric.
 */

/*!
 * @file MonitorPointAccumulatorT.h
 * This is the template file for accumulators of various types for
 * computing averages.
 *
 * @author N. S. Amarnath
 *
 * File containing declarations for monitor point average accumulators.
 * Accumulator classes associate a monitor point of a specific type (T) with
 * an accumulator for an average of the appropriate type (U).
 */

#include "carma/monitor/types.h"
#include "carma/monitor/MonitorPoint.h"
#include "carma/monitor/MonitorPointAverageT.h"
#include "carma/monitor/monitorPointSpecializations.h"
#include "carma/dbms/dbFFIO.h"
#include "carma/util/programLogging.h"


namespace carma {
namespace monitor {


class MonitorPointAccumulatorBase {
    protected:
        static void valueToFile( const bool & avgAcc,
                                 FILE *       file );

        static void valueToFile( const ::std::complex< float > & avgAcc,
                                 FILE *                          file );

        static void valueToFile( const double & avgAcc,
                                 FILE *         file );

        static void valueToFile( const float & avgAcc,
                                 FILE *        file );

        static void valueToFile( const int &  avgAcc,
                                 FILE *       file );

        static void valueToFile( const long & avgAcc,
                                 FILE *       file );

        static void valueToFile( const short & avgAcc,
                                 FILE *        file );

        static void valueToFile( const ::std::string & avgAcc,
                                 FILE *                file );

        static void valueToFileThrice( const bool & avgAcc,
                                       FILE *       file );

        static void valueToFileThrice( const ::std::complex< float > & avgAcc,
                                       FILE *                          file );

        static void valueToFileThrice( const double & avgAcc,
                                       FILE *         file );

        static void valueToFileThrice( const float & avgAcc,
                                       FILE *        file );

        static void valueToFileThrice( const int &  avgAcc,
                                       FILE *       file );

        static void valueToFileThrice( const long & avgAcc,
                                       FILE *       file );

        static void valueToFileThrice( const short & avgAcc,
                                       FILE *        file );

        static void valueToFileThrice( const ::std::string & avgAcc,
                                       FILE *                file );

        static void cstringToFileThrice( const char * b,
                                         FILE *       file );
};


/**
 * @template MonitorPointAccumulatorT< T, U >
 * @brief Associates an average accumulator obejct with a specific
 *        monitor point (of type T).
 *
 * Associates a monitor point of type T with an average accumulator
 * accumulating sample values as type U.
 * For example, MonitorPointAccumulatorT<MonitorPointInt, double, long>
 * associates a monitor point of type MonitorPointInt with an average
 * object of type MonitorPointAverageT<double>, which accumulates sample
 * values in a double, and calculates averages as long.
 */
template < typename T, typename U >
class MonitorPointAccumulatorT : public MonitorPointAccumulatorBase {
  public:
    /**
     * Constructor
     * Creates an array of average objects for a monitor point with spectral
     * data, or creates one average object for a monitor point with time
     * series data.
     *
     * @param typedPoint type T& monitor point associated with the
     *                   average object(s).
     */
    explicit MonitorPointAccumulatorT( T & typedPoint );


    /**
     * Destructor
     * Destroys the created set of average objects.
     */
    ~MonitorPointAccumulatorT( );

    /**
     * swap two instances
     */
    void swap( MonitorPointAccumulatorT & rhs );
    
    /**
     * Method to reset associated average objects so
     * they're initialized for a fresh average calculation.
     */
    void resetAccumulator( );

    /**
     * Method to accumulate sample values in associated
     * average objects - walks through samples and accumulates
     * sample value.
     */
    void accumulate( );

    /**
     * Method to accumulate sample averages in associated
     * average objects - uses pre-computed average values from monitor
     * points.
     */
    void accumulateAverage( );


    /**
     * Method to compute average value from accumulated sample values
     * in the associated average objects.
     *
     * @param index const int index of sample for spectral data.
     * @return calculated average value.
     */
    const typename T::AccumReportType getAverage( int index = 0 ) const;


    /**
     * Method to test for equality of two MonitorPointAccumulator
     * objects. Required for insertion into a map.
     *
     * @param other const MonitorPointAccumulatorT<T&, U> accumulator to be
     *              compared to this.
     * @return bool true if associated monitor points are the same,
     *              else false.
     */
    bool operator==( const MonitorPointAccumulatorT & rhs ) const;


    /**
     * Method to test ordering of two MonitorPointAccumulator
     * objects. Required for insertion into a map.
     *
     * @param other const MonitorPointAccumulatorT<T&, U> accumulator to be
     *              compared to this.
     * @return bool true if associated monitor point of this precedes
     *              associated monitor point of other, else false.
     */
    bool operator<( const MonitorPointAccumulatorT & rhs ) const;


    /**
     * dump instantaneous (frame) averages to a file for the dbloader to
     * read
     * @param frameCount the frame count associated with the averages
     * @param file the file to write to
     */

    void dumpInstAveragesToFile( long   frameCount,
                                 FILE * file ) const;

    /**
     * dump instantaneous (frame) averages to a file for the dbloader to
     * read. This differs from the old version in that the dbFFIO object
     * handles any formatting and I/O.
     * @param frameCount the frame count associated with the averages
     * @param file the file to write to.
     */
    void dumpInstAveragesToFile( long                  frameCount,
                                 carma::dbms::dbFFIO & file ) const;

    /**
     * dump long (minute,subarray) averages to a file for the dbloader to
     * read
     * @param frameCount the frame count associated with the averages
     * @param file the file to write to
     */
    void dumpLongAveragesToFile( const char * const frameCountText,
                                 FILE * const       file ) const;


  private:
    typedef ::std::vector< MonitorPointAverageT< U > > AverageVec;

    T *        typedPoint_;
    AverageVec averageVec_;
}; // template MonitorPointAccumulatorT


} // end namespace monitor
} // end namespace carma


inline void
carma::monitor::MonitorPointAccumulatorBase::
cstringToFileThrice( const char * const b,
                     FILE * const       file )
{
    fputs( b, file );
    fputc( '\t', file );
    fputs( b, file );
    fputc( '\t', file );
    fputs( b, file );
}


inline void
carma::monitor::MonitorPointAccumulatorBase::
valueToFile( const bool & v,
             FILE * const file )
{
    fprintf( file, "%2d", v );
}


inline void
carma::monitor::MonitorPointAccumulatorBase::
valueToFileThrice( const bool & v,
             FILE * const file )
{
    char b[ 32 ];

    sprintf( b, "%2d", v );

    cstringToFileThrice( b, file );
}


inline void
carma::monitor::MonitorPointAccumulatorBase::
valueToFile( const ::std::complex< float > & v,
             FILE * const                    file )
{
    fprintf( file, "%15.8e\t%15.8e", v.real(), v.imag() );
}


inline void
carma::monitor::MonitorPointAccumulatorBase::
valueToFileThrice( const ::std::complex< float > & v,
                   FILE * const                    file )
{
    char b[ 64 ];

    sprintf( b, "%15.8e\t%15.8e", v.real(), v.imag() );

    cstringToFileThrice( b, file );
}


inline void
carma::monitor::MonitorPointAccumulatorBase::
valueToFile( const double & v,
             FILE * const   file )
{
    fprintf( file, "%23.16e", v );
}


inline void
carma::monitor::MonitorPointAccumulatorBase::
valueToFileThrice( const double & v,
                   FILE * const   file )
{
    char b[ 64 ];

    sprintf( b, "%23.16e", v );

    cstringToFileThrice( b, file );
}


inline void
carma::monitor::MonitorPointAccumulatorBase::
valueToFile( const float & v,
             FILE * const  file )
{
    fprintf( file, "%15.8e", v );
}


inline void
carma::monitor::MonitorPointAccumulatorBase::
valueToFileThrice( const float & v,
                   FILE * const  file )
{
    char b[ 32 ];

    sprintf( b, "%15.8e", v );

    cstringToFileThrice( b, file );
}


inline void
carma::monitor::MonitorPointAccumulatorBase::
valueToFile( const int &  v,
             FILE * const file )
{
    fprintf( file, "%12d", v );
}


inline void
carma::monitor::MonitorPointAccumulatorBase::
valueToFileThrice( const int &  v,
                   FILE * const file )
{
    char b[ 32 ];

    sprintf( b, "%12d", v );

    cstringToFileThrice( b, file );
}


inline void
carma::monitor::MonitorPointAccumulatorBase::
valueToFile( const long & v,
             FILE * const file )
{
    fprintf( file, "%12ld", v );
}


inline void
carma::monitor::MonitorPointAccumulatorBase::
valueToFileThrice( const long & v,
                   FILE * const file )
{
    char b[ 32 ];

    sprintf( b, "%12ld", v );

    cstringToFileThrice( b, file );
}


inline void
carma::monitor::MonitorPointAccumulatorBase::
valueToFile( const short & v,
             FILE * const  file )
{
    fprintf( file, "%7d", v );
}


inline void
carma::monitor::MonitorPointAccumulatorBase::
valueToFileThrice( const short & v,
                   FILE * const  file )
{
    char b[ 32 ];

    sprintf( b, "%7d", v );

    cstringToFileThrice( b, file );
}


inline void
carma::monitor::MonitorPointAccumulatorBase::
valueToFile( const ::std::string & v,
             FILE * const          file )
{
    fputs( v.c_str(), file );
}


inline void
carma::monitor::MonitorPointAccumulatorBase::
valueToFileThrice( const ::std::string & v,
                   FILE * const          file )
{
    cstringToFileThrice( v.c_str(), file );
}


template < typename T, typename U >
inline
carma::monitor::MonitorPointAccumulatorT< T, U >::
    MonitorPointAccumulatorT( T & typedPoint ) :
typedPoint_( &typedPoint ),
averageVec_()
{
    size_t numAccumulators;
    
    if ( typedPoint_->isTimeSeries() )
        numAccumulators = 1;
    else
        numAccumulators = ::std::max( 1, typedPoint_->getNumSamples() );
        
    averageVec_.resize( numAccumulators );
        
    resetAccumulator();
}


template < typename T, typename U >
inline
carma::monitor::MonitorPointAccumulatorT< T, U >::~MonitorPointAccumulatorT( )
{
}


template < typename T, typename U >
inline void
carma::monitor::MonitorPointAccumulatorT< T, U >::swap(
    MonitorPointAccumulatorT & rhs )
{
    ::std::swap( typedPoint_, rhs.typedPoint_ );
    averageVec_.swap( rhs.averageVec_ );
}


template < typename T, typename U >
inline void
carma::monitor::MonitorPointAccumulatorT< T, U >::resetAccumulator( )
{
    typename AverageVec::iterator i = averageVec_.begin();
    const typename AverageVec::iterator iEnd = averageVec_.end();

    for ( ; i != iEnd; ++i )
        typedPoint_->resetAccumulator( *i );
}


template < typename T, typename U >
inline void
carma::monitor::MonitorPointAccumulatorT< T, U >::accumulate( )
{
    typename AverageVec::iterator i = averageVec_.begin();
    const typename AverageVec::iterator iEnd = averageVec_.end();

    if ( typedPoint_->isTimeSeries() ) {
        for ( ; i != iEnd; ++i )
            typedPoint_->accumulate( *i );
    } else {
        for ( int averageIndex = 0; i != iEnd; ++i, ++averageIndex )
            typedPoint_->accumulateSample( *i, averageIndex );
    }
}


template < typename T, typename U >
inline void
carma::monitor::MonitorPointAccumulatorT< T, U >::accumulateAverage( )
{
    typename AverageVec::iterator i = averageVec_.begin();
    const typename AverageVec::iterator iEnd = averageVec_.end();

    if ( typedPoint_->isTimeSeries() ) {
        for ( ; i != iEnd; ++i )
            typedPoint_->accumulateAverage( *i );
    } else {
        for ( int averageIndex = 0; i != iEnd; ++i, ++averageIndex )
            typedPoint_->accumulateSample( *i, averageIndex );
    }
}


template < typename T, typename U >
inline const typename T::AccumReportType
carma::monitor::MonitorPointAccumulatorT< T, U >::getAverage(
    const int index ) const
{
    return typedPoint_->getAccumulatedAverage( averageVec_[ index ] );
}


template < typename T, typename U >
inline bool
carma::monitor::MonitorPointAccumulatorT< T, U >::operator==(
    const MonitorPointAccumulatorT & rhs ) const
{
    return (typedPoint_->getTagID() == rhs.getTagID());
}


template < typename T, typename U >
inline bool
carma::monitor::MonitorPointAccumulatorT< T, U >::operator<(
    const MonitorPointAccumulatorT & rhs ) const
{
    return (typedPoint_->getTagID() < rhs.getTagID());
}


template < typename T, typename U >
inline void
carma::monitor::MonitorPointAccumulatorT< T, U >::dumpInstAveragesToFile(
    const long   frameCount,
    FILE * const file ) const
{
    const tagIDType tagID = typedPoint_->getTagID();
    const bool isNotString =
        (typedPoint_->getValuetype() != MONITOR_VALUE_TYPE_STRING);

    typename AverageVec::const_iterator i = averageVec_.begin();
    const typename AverageVec::const_iterator iEnd = averageVec_.end();

    for ( int averageIndex = 0; i != iEnd; ++i, ++averageIndex ) {
        fprintf( file, "%11ld\t%11ld\t", frameCount, tagID );

        i->writeAvePropsToFile( file );

        valueToFile( typedPoint_->getAccumulatedAverage( *i ), file );

        if ( isNotString ) {
            //  string values have no iSamples
            fprintf( file, "\t%2d", averageIndex );
        }
        
        fputc( '\n', file );
    }
}


template < typename T, typename U >
inline void
carma::monitor::MonitorPointAccumulatorT< T, U >::dumpInstAveragesToFile(
    const long            frameCount,
    carma::dbms::dbFFIO & file ) const
{
    const long fc = frameCount;
    const long tid = typedPoint_->getTagID();

    typename AverageVec::const_iterator i = averageVec_.begin();
    const typename AverageVec::const_iterator iEnd = averageVec_.end();

    for ( int averageIndex = 0; i != iEnd; ++i, ++averageIndex ) {
        const ushort blanking = i->getDbBlanking();
        const ushort validity = i->getDbValidity();
    
        file.dumpInstAverage( fc,
                              tid,
                              blanking,
                              validity,
                              typedPoint_->getAccumulatedAverage( *i ),
                              averageIndex );
    }
}


template < typename T, typename U >
inline void
carma::monitor::MonitorPointAccumulatorT< T, U >::dumpLongAveragesToFile(
    const char * const frameCountText,
    FILE * const       file ) const
{
    char numTotalSampsText[ 16 ];
    char tagIdText[ 16 ];

    const bool isString =
        (typedPoint_->getValuetype() == MONITOR_VALUE_TYPE_STRING);

    sprintf( tagIdText, "\t%11ld\t", typedPoint_->getTagID() );

    typename AverageVec::const_iterator i = averageVec_.begin();
    const typename AverageVec::const_iterator iEnd = averageVec_.end();

    for ( int averageIndex = 0; i != iEnd; ++i, ++averageIndex ) {
        fputs( frameCountText, file );
        fputs( tagIdText, file );

        i->writeAvePropsToFile( file );

        if ( isString )  {
            //  string values have no iSamples
            valueToFile( typedPoint_->getAccumulatedAverage( *i ),
                         file );
        } else {
            const typename T::AccumReportType avgVal =
                typedPoint_->getAccumulatedAverage( *i );

            const typename T::AccumReportType maxVal =
                typedPoint_->getMaxValue( *i );

            const typename T::AccumReportType minVal =
                typedPoint_->getMinValue( *i );

            if ( (avgVal == maxVal) && (avgVal == minVal) )
                valueToFileThrice( avgVal, file );  // Pretty common case
            else {
                valueToFile( avgVal, file );
                fputc( '\t', file );
                valueToFile( maxVal, file );
                fputc( '\t', file );
                valueToFile( minVal, file );
            }

            if ( averageIndex == 0 )
                fputs( "\t 0", file );  // Very common case
            else
                fprintf( file, "\t%2d", averageIndex );
        }

        const int numTotalSamps = i->getNumTotalSamples();

        sprintf( numTotalSampsText, "\t%2d", numTotalSamps );

        const int numValidSamps = i->getNumValidSamples();

        if ( numValidSamps == numTotalSamps )
            fputs( numTotalSampsText, file );  // Very common case
        else if ( numValidSamps == 0 )
            fputs( "\t 0", file );  // Pretty common case
        else
            fprintf( file, "\t%2d", numValidSamps );

        fputs( numTotalSampsText, file );
        fputc( '\n', file );
    }
}


#endif
