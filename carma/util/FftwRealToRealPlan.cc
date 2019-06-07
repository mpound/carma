/**
 * @file
 * Definition for FftwRealToRealPlan class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.3 $
 * $Date: 2012/02/22 18:44:49 $
 * $Id: FftwRealToRealPlan.cc,v 1.3 2012/02/22 18:44:49 iws Exp $
 */

#include "carma/util/FftwRealToRealPlan.h"

#include "carma/util/ErrorException.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/ScopedLock.h"
#include "carma/util/Trace.h"

#include <boost/assign/list_of.hpp>
#include <cerrno>
#include <cstdio>
#include <iostream>
#include <pthread.h>

using namespace carma::util;
using namespace std;

namespace {

    const Trace::TraceLevel TRACE_CTOR_DTOR = Trace::TRACE3;
    const Trace::TraceLevel TRACE_PLAN = Trace::TRACE3;

    ::pthread_mutex_t gPlanCreationMutex = PTHREAD_MUTEX_INITIALIZER;
    
    FftwRealVector & checkSize( FftwRealVector & vec ) {
        if ( vec.empty() ) 
            throw CARMA_EXCEPTION( ErrorException, "FftwRealToRealPlan "
                "data vector must have a size > 0." );
        return vec;
    }

    ::fftw_plan 
    createPlan( FftwRealVector::size_type size,
                FftwRealVector & data,
                const enum FftwRealToRealPlan::Kind kind, 
                const enum FftwRealToRealPlan::Rigor rigor )
    {
        if ( data.capacity() < size ) {
            ostringstream err;
            err << "FftwRealToRealPlan " << "createPlan( size= " << size 
                << " ) input FftwRealVector capacity is " << data.capacity() 
                << " which would force an underlying vector memory "
                << "reallocation and invalidate any other plans using "
                << "this vector (see class documentation).";
            throw CARMA_EXCEPTION( ErrorException, err.str() );
        }

        fftw_plan plan = 0;

        unsigned fftw_rigor = 0;
        switch ( rigor ) {
            case FftwRealToRealPlan::ESTIMATE: 
                fftw_rigor = FFTW_ESTIMATE;
                break;
            case FftwRealToRealPlan::MEASURE:
                fftw_rigor = FFTW_MEASURE;
                break;
            case FftwRealToRealPlan::PATIENT:
                fftw_rigor = FFTW_PATIENT;
                break;
            case FftwRealToRealPlan::EXHAUSTIVE:
                fftw_rigor = FFTW_EXHAUSTIVE;
                break;
            default:
                fftw_rigor = 0; 
                break;
        }

        const unsigned planner_flags = fftw_rigor;

        // fftw may overwrite input and output data when creating plans
        // so we preserve the data here to be nice.  Remember, what really
        // matters is that the underlying dynamic memory allocation size
        // (reserved memory) is big enough to handle all plans.
        FftwRealVector temp( data ); 

        data.resize( size );

        {

            ScopedLock<pthread_mutex_t> lock( gPlanCreationMutex ); 
            switch ( kind ) {
                case FftwRealToRealPlan::REAL_TO_HALFCOMPLEX:
                    plan = ::fftw_plan_r2r_1d( static_cast<int>( size ),
                            &( data.at( 0 ) ),
                            &( data.at( 0 ) ),
                            FFTW_R2HC,
                            planner_flags ); // No flags
                    break;
                case FftwRealToRealPlan::HALFCOMPLEX_TO_REAL:
                    plan = ::fftw_plan_r2r_1d( static_cast<int>( size ),
                            &( data.at( 0 ) ),
                            &( data.at( 0 ) ),
                            FFTW_HC2R,
                            planner_flags ); // No flags
                    break;
                default:
                    throw CARMA_EXCEPTION( carma::util::ErrorException, 
                            "Invalid Kind." );
            }
        }
    
        if ( plan == 0 ) {
            ostringstream errmsg; 
            errmsg << "Unable to create plan of size " << size << ".";
            throw CARMA_EXCEPTION( carma::util::ErrorException, 
                                   errmsg.str() );
        }

        data.assign( temp.begin( ), temp.end( ) );

        return plan;
    }

} // namespace < unnamed >

FftwRealToRealPlan::FftwRealToRealPlan( 
    FftwRealVector::size_type size, 
    FftwRealVector & data,
    enum Kind transformKind,
    enum Rigor minimumRigor )
    : size_( size ),
      data0_( &( checkSize( data ).at( 0 ) ) ),
      kind_( transformKind ),
      rigor_( minimumRigor ),
      data_( data )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "FftwRealToRealPlan( size=" << size
        << ", kind=" << transformKind << ");" );

    plan_ = createPlan( size_, data_, kind_, rigor_ );
}

FftwRealToRealPlan::FftwRealToRealPlan( const FftwRealToRealPlan & from )
    : size_( from.size_ ),
      data0_( from .data0_ ),
      kind_( from.kind_ ),
      rigor_( from.rigor_ ),
      data_( from.data_ )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "FftwRealToRealPlan( size=" << size_
        << ", kind=" << kind_ << ") - Copy C'tor." );

    plan_ = createPlan( size_, data_, kind_, rigor_ );
}

FftwRealToRealPlan::~FftwRealToRealPlan( ) 
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "~FftwRealToRealPlan( )" );
            
    ScopedLock<pthread_mutex_t> lock( gPlanCreationMutex ); 

    ::fftw_destroy_plan( plan_ );
}
    
FftwRealToRealPlan::RigorEnumMap 
FftwRealToRealPlan::rigors() 
{
    static FftwRealToRealPlan::RigorEnumMap answer = 
        boost::assign::map_list_of( FftwRealToRealPlan::ESTIMATE, "ESTIMATE" )
        ( FftwRealToRealPlan::MEASURE, "MEASURE" )
        ( FftwRealToRealPlan::PATIENT, "PATIENT" )
        ( FftwRealToRealPlan::EXHAUSTIVE, "EXHAUSTIVE" );

    return answer;
}

void
FftwRealToRealPlan::execute( )
{
    if ( &( data_.at( 0 ) ) != data0_ || data_.size( ) < size_ ) {
        ostringstream msg;
        msg << "FftwRealToRealPlan::execute( ) - Data vector has been "
            << "reallocated or trimmed ( data_.size( ) = " 
            << data_.size( ) << ", size_ = " << size_ << " ).";
        throw CARMA_EXCEPTION( carma::util::ErrorException, msg.str( ) );
    }

    ::fftw_execute( plan_ );

    // Normalize
    if ( kind_ == HALFCOMPLEX_TO_REAL ) {
        FftwRealVector::iterator i = data_.begin( );
        for ( ; i < data_.end( ); ++i ) {
            ( *i ) /= size_;
        }
    }
}

FftwRealVector::size_type
FftwRealToRealPlan::getSize( ) const 
{
    return size_;
}

enum FftwRealToRealPlan::Kind
FftwRealToRealPlan::getKind( ) const
{
    return kind_;
}

void
FftwRealToRealPlan::importWisdom( const string & filename ) 
{
    const string errMsg = "FftwRealToRealPlan::importWisdom(" + filename + ")";

    FILE * pFile = ::fopen( filename.c_str(), "r" );
    if ( pFile == 0 ) {
        const int savedErrno = errno;
        throwPosixError( savedErrno, errMsg + " failed on fopen." );
    }

    int result = ::fftw_import_wisdom_from_file( pFile );
    if ( result != 1 ) {
        throw CARMA_EXCEPTION( ErrorException, errMsg + 
                               " fftw_import_wisdom_from_file failed." );
    }

    result = ::fclose( pFile );
    if ( result != 0 ) {
        const int savedErrno = errno;
        throwPosixError( savedErrno, errMsg + " failed on fclose." );
    }
}

void
FftwRealToRealPlan::exportWisdom( const string & filename )
{
    const string errMsg = "FftwRealToRealPlan::exportWisdom(" + filename + ")";

    FILE * pFile = ::fopen( filename.c_str(), "w" );
    if ( pFile == 0 ) {
        const int savedErrno = errno;
        throwPosixError( savedErrno, errMsg + " failed on fopen." );
    }
    
    ::fftw_export_wisdom_to_file( pFile );
    
    const int result = ::fclose( pFile );
    if ( result != 0 ) {
        const int savedErrno = errno;
        throwPosixError( savedErrno, errMsg + " failed on fclose." );
    }
}

void
FftwRealToRealPlan::forgetWisdom( )
{
    ::fftw_forget_wisdom();
}

