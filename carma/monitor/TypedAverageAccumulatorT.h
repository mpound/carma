/*
 * TypedAverageAccumulatorT.h - Template file for accumulating monitor point
 * values of a spoecific type.
 */

#ifndef CARMA_MONITOR_TYPED_AVERAGE_ACCUMULATORT_H
#define CARMA_MONITOR_TYPED_AVERAGE_ACCUMULATORT_H

/*!
 * @file TypedAverageAccumulatorT.h
 * This is the template file for accumulators of specific types for
 * computing averages.
 *
 * @author N. S. Amarnath
 *
 */

#include <vector>

#include "carma/corba/corba.h"
#include "carma/dbms/dbFFIO.h"
#include "carma/monitor/monitorframe.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/MonitorPointAccumulatorT.h"


namespace carma {
namespace monitor {


//! @brief Common base class for TypedAverageAccumulatorT< T, U > classes.
//!
class AverageAccumulatorBase {
    public:
        virtual ~AverageAccumulatorBase( );

    protected:
        explicit AverageAccumulatorBase( );

        void setMonSys( const MonitorSystem &              monSys,
                        MonitorComponent::ARCHIVE_PRIORITY priority );

    private:
        virtual void
        setSeq( const ::std::vector< MonitorPoint * > & mpVec ) = 0;
};


/**
 * @template TypedAverageAccumulatorT
 * @brief template for storing accumulators (of type U), producing
 *        averages for monitor points of type T.
 *
 */
template < typename T, typename U >
class TypedAverageAccumulatorT : public AverageAccumulatorBase {
    public:
        /**
         * Constructor
         * Binds itself to monitor system monSys. All averages
         * are accumulated form monitor values gathered from
         * monitorSystem.
         */
        TypedAverageAccumulatorT(
            const MonitorSystem &              monSys,
            MonitorComponent::ARCHIVE_PRIORITY priority );


        /**
         * Destructor - destroys all strcutures built up during
         * construction.
         */
        ~TypedAverageAccumulatorT( );


        /**
         * Accumulates values for each monitor point accumulator in this
         * typed average accumulator.
         *
         * @return none
         * @see MonitorPointAccumulatorT< T_ &, U >::accumulate
         */
        void accumulate( );


        /**
         * Resets values in all typed accumulators so that a new average
         * computation can begin.
         *
         * @return none
         * @see MonitorPointAccumulatorT< T &, U >::resetAccumulator
         */
        void resetAccumulator( );


        void dumpInstAveragesToDBFile( long   frameCount,
                                       FILE * file ) const;


        void dumpInstAveragesToDBFile( long     frameCount,
                                       carma::dbms::dbFFIO & file ) const;


        void dumpLongAveragesToDBFile( const char * frameCountText,
                                       FILE *       file ) const;


    private:
        void setSeq( const ::std::vector< MonitorPoint * > & mpVec );

        typedef ::std::vector< MonitorPointAccumulatorT< T, U > > AccumVec;

        typedef typename AccumVec::iterator       IteratorType;
        typedef typename AccumVec::const_iterator ConstIteratorType;

        // vector of MonitorPointAccumulators, ordered by tagID.
        AccumVec seq_;
}; // template TypedAverageAccumulatorT


} // end namespace carma::monitor
} // end namespace carma


inline
carma::monitor::AverageAccumulatorBase::AverageAccumulatorBase( )
{
}


inline
carma::monitor::AverageAccumulatorBase::~AverageAccumulatorBase( )
{
}


template < typename T, typename U >
carma::monitor::TypedAverageAccumulatorT< T, U >::TypedAverageAccumulatorT(
    const MonitorSystem &                    monitorSystem,
    const MonitorComponent::ARCHIVE_PRIORITY priority ) :
AverageAccumulatorBase(),
seq_()
{
    // Notice that I have this call in the derived class c'tor because it
    // uses the virtual setSeq method internally and hence I don't call
    // it until that method and anything it uses is constructed.
    setMonSys( monitorSystem, priority );
}


template < typename T, typename U >
carma::monitor::TypedAverageAccumulatorT< T, U >::~TypedAverageAccumulatorT( )
{
}


template < typename T, typename U >
void
carma::monitor::TypedAverageAccumulatorT< T, U >::setSeq(
    const ::std::vector< MonitorPoint * > & mpVec )
{
    const ::std::vector< MonitorPoint * >::const_iterator iEnd = mpVec.end();

    ::std::vector< MonitorPoint * >::const_iterator i = mpVec.begin();
    
    ::size_t count = 0;
    
    for ( ; i != iEnd; ++i ) {
        if ( dynamic_cast< T * >( *i ) != 0 )
            ++count;
    }

    seq_.reserve( count );

    i = mpVec.begin();
    
    for ( ; i != iEnd; ++i ) {
        T * const typedPoint = dynamic_cast< T * >( *i );

        if ( typedPoint != 0 ) {
            seq_.push_back( MonitorPointAccumulatorT< T, U >( *typedPoint ) );
        }
    }
}


template < typename T, typename U >
void
carma::monitor::TypedAverageAccumulatorT< T, U >::accumulate( )
{
    IteratorType i = seq_.begin();
    const IteratorType iEnd = seq_.end();

    for ( ; i != iEnd; ++i )
        i->accumulateAverage();
}


template < typename T, typename U >
void
carma::monitor::TypedAverageAccumulatorT< T, U >::resetAccumulator( )
{
    IteratorType i = seq_.begin();
    const IteratorType iEnd = seq_.end();

    for ( ; i != iEnd; ++i )
        i->resetAccumulator();
}


template < typename T, typename U >
void
carma::monitor::TypedAverageAccumulatorT< T, U >::dumpInstAveragesToDBFile(
    const long frameCount,
    FILE *     file ) const
{
    ConstIteratorType i = seq_.begin();
    const ConstIteratorType iEnd = seq_.end();

    for ( ; i != iEnd; ++i )
       i->dumpInstAveragesToFile( frameCount, file );
}

template < typename T, typename U >
void
carma::monitor::TypedAverageAccumulatorT< T, U >::dumpInstAveragesToDBFile(
    const long frameCount,
    carma::dbms::dbFFIO &   file ) const
{
    ConstIteratorType i = seq_.begin();
    const ConstIteratorType iEnd = seq_.end();

    for ( ; i != iEnd; ++i )
       i->dumpInstAveragesToFile( frameCount, file );
}


template < typename T, typename U >
void
carma::monitor::TypedAverageAccumulatorT< T, U >::dumpLongAveragesToDBFile(
    const char * const frameCountText,
    FILE *             file ) const
{
    ConstIteratorType i = seq_.begin();
    const ConstIteratorType iEnd = seq_.end();

    for ( ; i != iEnd; ++i )
       i->dumpLongAveragesToFile( frameCountText, file );
}


#endif
