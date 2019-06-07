#ifndef CARMA_MONITOR_MONITORPOINTAVERAGET_H
#define CARMA_MONITOR_MONITORPOINTAVERAGET_H

/*
 * MonitorPointAverageT.h - Template file for monitor point average
 * values of various types.
 */

/*!
 * @file MonitorPointAverageT.h
 * This is the template file for accumulators of various types for
 * computing averages.
 *
 * @author N. S. Amarnath
 *
 * File containing declarations for MonitorPointAverageT template.
 *
 * Usage:  There are two usage contexts for the code in this file. The
 * contexts
 * @see http://www.mmarray.org/project/WP/Monitoring/monitoringDesign.pdf
 */

#include "carma/monitor/types.h"
#include "carma/monitor/MonitorPoint.h"


namespace carma {
namespace monitor {


/**
 * Structure keeps track of sample properties of a monitor point
 * for the purposes of calculating various averages.
 * Used by monitorPointSpecializations.h, MonitorPointNumeric.h
 * and MonitorPointAccumulatorT.h
 */
struct  AveProperties {
   /**
    * # of samples not marked invalid in some fashion
    *
    * @see enum VALIDITY
    */
    int  nValidSamples; // # of samples not marked invalid in some fashion

   /**
    * total # of samples associated with a monitor point - half of
    * sampling frequency for that monitor point.
    *
    * @see MonitorPoint::getNumSamples
    */
    int  nTotalSamples; // total # of samples for this monitor point

   /**
    * Cumulative validity for all valid samples.
    *
    * @see MonitorPoint::VALIDITY
    */
    enum MonitorPoint::VALIDITY validity;

   /**
    * Cumulative blanking for all valid samples.
    *
    * @see MonitorPoint::BLANKING_FLAGGING
    */
    // cumulative blanking for all valid samples
    enum MonitorPoint::BLANKING_FLAGGING blanking;
};


class MonitorPointAverageBase {
    public:

        /**
         * Get total number of samples.
         *
         * @return int total # of samples.
         */
        int getNumTotalSamples( ) const;

        /**
         * Set total number of samples.
         *
         * @param nTotalSamples int total # of samples.
         */
        void setNumTotalSamples( int nTotalSamples );

        /**
         * Increments total number of samples.
         *
         * @param nTotalSamples int defaults to 1
         */
        void incrementNumTotalSamples( int nTotalSamples = 1 );

        /**
         * Get number of valid samples.
         *
         * @return int # of valid samples
         */
        int getNumValidSamples( ) const;

        /**
         * Set number of valid samples.
         *
         * @param nValidSamples int # of valid samples
         */
        void setNumValidSamples( int nValidSamples );

        /**
         * Increments number of valid samples.
         *
         * @param nValidSamples int defaults to 1
         */
        void incrementNumValidSamples( int nValidSamples = 1 );

        /**
         * Get cumulative blanking flag.
         *
         * @return enum MonitorPoint::VALIDITY
         */
        enum MonitorPoint::VALIDITY getValidity( ) const;

        /**
         * Set cumulative validity flag.
         *
         * @param validity enum MonitorPoint::VALIDITY flag value
         */
        void setValidity( enum MonitorPoint::VALIDITY validity );


        /**
         * Get cumulative blanking flag.
         *
         * @return enum MonitorPoint::BLANKING_FLAGGING
         */
        enum MonitorPoint::BLANKING_FLAGGING getBlanking( ) const;

        /**
         * Set cumulative blanking flag.
         *
         * @param blanking enum MonitorPoint::BLANKING_FLAGGING
         */
        void setBlanking( enum MonitorPoint::BLANKING_FLAGGING blanking );

        ushort getDbBlanking( ) const;

        ushort getDbValidity( ) const;

        void writeAvePropsToFile( FILE * f ) const;

        void writeAvePropsToBuf( char * c ) const;

        /**
         * Method used to reset # of valid samples, total # of samples,
         * cumulative validity and blanking flags. Used when accumulated
         * values for average have to be reset.
         */
        void resetAveProperties( );

    protected:

        MonitorPointAverageBase( );

        ~MonitorPointAverageBase( );  // no v-table

    private:
        class TableInit;

        static ushort gBfToDbTable_[ MonitorPoint::MAX_BLANKING_FLAGGING ];
        static ushort gValToDbTable_[ MonitorPoint::MAX_VALIDITY ];

        static char gBfToDbFileTable_[ MonitorPoint::MAX_BLANKING_FLAGGING ][4];
        static char gValToDbFileTable_[ MonitorPoint::MAX_VALIDITY ][4];

        AveProperties properties_;
};


/**
 * Template for collecting data for computing average associated with one
 * monitor point.
 *
 * Encapsulates base level methods for accumulating sample values,
 * keeping track of max/min values across all accumulated sample values, and
 * keeping track of number of valid samples, total # of samples and total
 * validity and blanking status.
 *
 * @see MonitorPoint::MonitorPointAccumulatorT
 */
template < typename U >
class MonitorPointAverageT : public MonitorPointAverageBase {
    public:

        /**
         * Constructor - sets specified initial value, which may be different
         * for various types, and for specific monitor points.
         * Initializes max/min values. # of valid ssamples, total # of samples,
         * and cumulative validity and blanking flags.
         */
        MonitorPointAverageT( const U & initialValue,
                              const U & max,
                              const U & min );

        /**
         * Default constructor - sets all values to zero using
         * template specialization to do it appropriately.
         * Used only by MonitorPointAccumulatorT template.
         *
         * @see ::carma::monitor::MonitorPointAccumulatorT
         */
        MonitorPointAverageT( );

        /**
         * Destructor - Doesnt do anything by default as the compiler
         * takes care of deleting component members.
         *
         */
        ~MonitorPointAverageT( );  // no v-table


        /**
         * Get cumulative value of samples.
         *
         * @return U accumulated value
         */
        U getAccumulator( ) const;

        /**
         * Set cumulative value of samples.
         *
         * @param value U accumulated value of samples.
         */
        void setAccumulator( const U & value );

        /**
         * Increment cumulative value of samples.
         *
         * @param value U value of sample to be added.
         */
        void incrementAccumulator(const U& value);


        /**
         * Get maximum of sample values.
         *
         * @return U maximum accumulated sample value.
         */
        U getMaxValue( ) const;

        /**
         * Set maximum sample value.
         *
         * @param value const U maximum value of accumulated samples.
         */
        void setMaxValue( const U & value );


        /**
         * Get minimum value across all accumulated samples.
         *
         * @return U minimum sample value.
         */
        U getMinValue( ) const;

        /**
         * Set minimum across all accumulated samples.
         *
         * @param value const U minimum sample value.
         */
        void setMinValue( const U & value );


    private:
        U accumulator_;
        U maxValue_;
        U minValue_;
}; // template MonitorPointAverageT


} // end namespace monitor
} // end namespace carma


inline
carma::monitor::MonitorPointAverageBase::MonitorPointAverageBase( )
{
    resetAveProperties();
}


inline
carma::monitor::MonitorPointAverageBase::~MonitorPointAverageBase( )
{
}


inline void
carma::monitor::MonitorPointAverageBase::resetAveProperties( )
{
    properties_.nTotalSamples = 0;
    properties_.nValidSamples = 0;
    properties_.validity = MonitorPoint::INVALID_NO_DATA;
    properties_.blanking = MonitorPoint::UNDETERMINED;
}


inline int
carma::monitor::MonitorPointAverageBase::getNumTotalSamples( ) const
{
    return properties_.nTotalSamples;
}


inline void
carma::monitor::MonitorPointAverageBase::setNumTotalSamples(
    const int nTotalSamples )
{
    properties_.nTotalSamples = nTotalSamples;
}


inline void
carma::monitor::MonitorPointAverageBase::incrementNumTotalSamples(
    const int nTotalSamples )
{
    properties_.nTotalSamples += nTotalSamples;
}



inline int
carma::monitor::MonitorPointAverageBase::getNumValidSamples( ) const
{
    return properties_.nValidSamples;
}


inline void
carma::monitor::MonitorPointAverageBase::setNumValidSamples(
    const int nValidSamples )
{
    properties_.nValidSamples = nValidSamples;
}


inline void
carma::monitor::MonitorPointAverageBase::incrementNumValidSamples(
    const int nValidSamples )
{
    properties_.nValidSamples += nValidSamples;
}


inline enum carma::monitor::MonitorPoint::VALIDITY
carma::monitor::MonitorPointAverageBase::getValidity( ) const
{
    return properties_.validity;
}


inline void
carma::monitor::MonitorPointAverageBase::setValidity(
    const enum MonitorPoint::VALIDITY validity )
{
    properties_.validity = validity;
}



inline enum carma::monitor::MonitorPoint::BLANKING_FLAGGING
carma::monitor::MonitorPointAverageBase::getBlanking( ) const
{
    return properties_.blanking;
}


inline void
carma::monitor::MonitorPointAverageBase::setBlanking(
    const enum MonitorPoint::BLANKING_FLAGGING blanking )
{
    properties_.blanking = blanking;
}


inline ushort
carma::monitor::MonitorPointAverageBase::getDbBlanking( ) const
{
    const int bf = properties_.blanking;

    if ( (bf >= 0) && (bf < MonitorPoint::MAX_BLANKING_FLAGGING) )
        return gBfToDbTable_[ bf ];

    return gBfToDbTable_[ MonitorPoint::UNDETERMINED ];
}


inline ushort
carma::monitor::MonitorPointAverageBase::getDbValidity( ) const
{
    const int val = properties_.validity;

    if ( (val >= 0) && (val < MonitorPoint::MAX_VALIDITY) )
        return gValToDbTable_[ val ];

    return gValToDbTable_[ MonitorPoint::INVALID_NO_DATA ];
}


inline void
carma::monitor::MonitorPointAverageBase::writeAvePropsToFile( FILE * f ) const
{
    {
        const int bf = properties_.blanking;

        const char * sBf;
        if ( (bf >= 0) && (bf < MonitorPoint::MAX_BLANKING_FLAGGING) )
            sBf = gBfToDbFileTable_[ bf ];
        else
            sBf = gBfToDbFileTable_[ MonitorPoint::UNDETERMINED ];

        fputs( sBf, f );
    }

    {
        const int val = properties_.validity;

        const char * sVal;
        if ( (val >= 0) && (val < MonitorPoint::MAX_VALIDITY) )
            sVal = gValToDbFileTable_[ val ];
        else
            sVal = gValToDbFileTable_[ MonitorPoint::INVALID_NO_DATA ];

        fputs( sVal, f );
    }
}


inline void
carma::monitor::MonitorPointAverageBase::writeAvePropsToBuf( char * c ) const
{
    {
        const int bf = properties_.blanking;

        const char * sBf;
        if ( (bf >= 0) && (bf < MonitorPoint::MAX_BLANKING_FLAGGING) )
            sBf = gBfToDbFileTable_[ bf ];
        else
            sBf = gBfToDbFileTable_[ MonitorPoint::UNDETERMINED ];

        c[0] = sBf[0];
        c[1] = sBf[1];
        c[2] = sBf[2];
    }

    {
        const int val = properties_.validity;

        const char * sVal;
        if ( (val >= 0) && (val < MonitorPoint::MAX_VALIDITY) )
            sVal = gValToDbFileTable_[ val ];
        else
            sVal = gValToDbFileTable_[ MonitorPoint::INVALID_NO_DATA ];

        c[3] = sVal[0];
        c[4] = sVal[1];
        c[5] = sVal[2];
    }

    c[6] = '\0';
}


template< typename U >
inline
carma::monitor::MonitorPointAverageT< U >::MonitorPointAverageT(
    const U & initialValue,
    const U & max,
    const U & min ) :
accumulator_( initialValue ),
maxValue_( max ),
minValue_( min )
{
}


template< typename U >
inline
carma::monitor::MonitorPointAverageT< U >::~MonitorPointAverageT( )
{
}


template< typename U >
inline U
carma::monitor::MonitorPointAverageT< U >::getAccumulator( ) const
{
    return accumulator_ ;
}


template< typename U >
inline void
carma::monitor::MonitorPointAverageT< U >::setAccumulator( const U & value )
{
    accumulator_ = value;
}

template< typename U >
inline void
carma::monitor::MonitorPointAverageT<U>::incrementAccumulator(const U& value)
{
    accumulator_ += value;
}


template< typename U >
inline U
carma::monitor::MonitorPointAverageT< U >::getMaxValue( ) const
{
    return maxValue_ ;
}


template< typename U >
inline void
carma::monitor::MonitorPointAverageT< U >::setMaxValue( const U & value )
{
    maxValue_ = value;
}


template< typename U >
inline U
carma::monitor::MonitorPointAverageT< U >::getMinValue( ) const
{
    return minValue_ ;
}


template< typename U >
inline void
carma::monitor::MonitorPointAverageT< U >::setMinValue( const U & value )
{
    minValue_ = value;
}


// Specialize the default constructor to do the right thing for each type we
// care about

namespace carma {
namespace monitor {

template< >
inline
carma::monitor::MonitorPointAverageT< ::std::string >::
    MonitorPointAverageT( ) :
accumulator_(),
maxValue_(),
minValue_()
{
}


template< >
inline
carma::monitor::MonitorPointAverageT< bool >::
    MonitorPointAverageT( ) :
accumulator_( false ),
maxValue_( false ),
minValue_( false )
{
}


template< >
inline
carma::monitor::MonitorPointAverageT< char >::
    MonitorPointAverageT( ) :
accumulator_( 0 ),
maxValue_( 0 ),
minValue_( 0 )
{
}


template< >
inline
carma::monitor::MonitorPointAverageT< unsigned char >::
    MonitorPointAverageT( ) :
accumulator_( 0 ),
maxValue_( 0 ),
minValue_( 0 )
{
}


template< >
inline
carma::monitor::MonitorPointAverageT< short >::
    MonitorPointAverageT( ) :
accumulator_( 0 ),
maxValue_( 0 ),
minValue_( 0 )
{
}


template< >
inline
carma::monitor::MonitorPointAverageT< unsigned short >::
    MonitorPointAverageT( ) :
accumulator_( 0 ),
maxValue_( 0 ),
minValue_( 0 )
{
}


template< >
inline
carma::monitor::MonitorPointAverageT< int >::
    MonitorPointAverageT( ) :
accumulator_( 0 ),
maxValue_( 0 ),
minValue_( 0 )
{
}


template< >
inline
carma::monitor::MonitorPointAverageT< unsigned int >::
    MonitorPointAverageT( ) :
accumulator_( 0 ),
maxValue_( 0 ),
minValue_( 0 )
{
}


template< >
inline
carma::monitor::MonitorPointAverageT< long >::
    MonitorPointAverageT( ) :
accumulator_( 0 ),
maxValue_( 0 ),
minValue_( 0 )
{
}


template< >
inline
carma::monitor::MonitorPointAverageT< unsigned long >::
    MonitorPointAverageT( ) :
accumulator_( 0 ),
maxValue_( 0 ),
minValue_( 0 )
{
}


template< >
inline
carma::monitor::MonitorPointAverageT< long long >::
    MonitorPointAverageT( ) :
accumulator_( 0 ),
maxValue_( 0 ),
minValue_( 0 )
{
}


template< >
inline
carma::monitor::MonitorPointAverageT< unsigned long long >::
    MonitorPointAverageT( ) :
accumulator_( 0 ),
maxValue_( 0 ),
minValue_( 0 )
{
}


template< >
inline
carma::monitor::MonitorPointAverageT< float >::
    MonitorPointAverageT( ) :
accumulator_( 0.0 ),
maxValue_( 0.0 ),
minValue_( 0.0 )
{
}


template< >
inline
carma::monitor::MonitorPointAverageT< double >::
    MonitorPointAverageT( ) :
accumulator_( 0.0 ),
maxValue_( 0.0 ),
minValue_( 0.0 )
{
}


template< >
inline
carma::monitor::MonitorPointAverageT< long double >::
    MonitorPointAverageT( ) :
accumulator_( 0.0 ),
maxValue_( 0.0 ),
minValue_( 0.0 )
{
}


template< >
inline
carma::monitor::MonitorPointAverageT< ::std::complex< float > >::
    MonitorPointAverageT( ) :
accumulator_( 0.0 ),
maxValue_( 0.0 ),
minValue_( 0.0 )
{
}


template< >
inline
carma::monitor::MonitorPointAverageT< ::std::complex< double > >::
    MonitorPointAverageT( ) :
accumulator_( 0.0 ),
maxValue_( 0.0 ),
minValue_( 0.0 )
{
}


template< >
inline
carma::monitor::MonitorPointAverageT< ::std::complex< long double > >::
    MonitorPointAverageT( ) :
accumulator_( 0.0 ),
maxValue_( 0.0 ),
minValue_( 0.0 )
{
}


}} // namespace carma::monitor
#endif
