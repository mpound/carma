/**
 * @file
 * Implementation for FftwRealToRealPlanManager class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.1 $
 * $Date: 2011/04/11 20:39:43 $
 * $Id: FftwRealToRealPlanManager.cc,v 1.1 2011/04/11 20:39:43 abeard Exp $
 */

#include "carma/util/FftwRealToRealPlanManager.h"

#include "carma/util/ErrorException.h"
#include "carma/util/programLogging.h"
#include "carma/util/Trace.h"

#include <iostream>

using namespace carma::util;
using namespace std;

namespace {

    const Trace::TraceLevel TRACE_CTOR_DTOR = Trace::TRACE5;
    const Trace::TraceLevel TRACE_PLAN_COPY = Trace::TRACE1;
    const Trace::TraceLevel TRACE_PLAN = Trace::TRACE4;

} // namespace < unnamed >

FftwRealToRealPlanManager::FftwRealToRealPlanManager( 
    FftwRealVector::size_type maxSize )
    : maxSize_( maxSize ),
      data_( maxSize )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "FftwRealToRealPlanManager( maxSize="
        << maxSize << " )" );

    if ( data_.capacity( ) < maxSize ) {
        throw CARMA_EXCEPTION( ErrorException, "FftwRealToRealPlanManager( ) - "
            "Input data vector doesn't have enough capicity."  );
    }
}
            
FftwRealToRealPlanManager::FftwRealToRealPlanManager( 
    const FftwRealToRealPlanManager & from ) : 
    maxSize_( from.maxSize_ ),
    data_( from.data_ )
{
    CARMA_CPTRACE( TRACE_PLAN_COPY, 
                   "FftwRealToRealPlanManager - Copy ctor invoked." );
    // Loop through plans and create new ones pointing to its own data_ copy.
    PlanMultimap::const_iterator piBegin = from.plans_.begin();
    PlanMultimap::const_iterator piEnd = from.plans_.end();
    for ( PlanMultimap::const_iterator pi = piBegin; pi != piEnd; ++pi )
    {
        plans_.insert( PlanMultimap::value_type( 
                            pi->first, 
                            FftwRealToRealPlan( pi->second.getSize(),
                                                data_,
                                                pi->second.getKind() ) ) );
    }
}

FftwRealToRealPlanManager & 
FftwRealToRealPlanManager::operator=( const FftwRealToRealPlanManager & from )
{
    CARMA_CPTRACE( TRACE_PLAN_COPY, 
                   "FftwRealToRealPlanManager - operator= invoked." );
    if ( this == &from ) return *this;

    data_ = from.data_;

    // Loop through plans and create new ones pointing to its own data_ copy.
    PlanMultimap::const_iterator piBegin = from.plans_.begin();
    PlanMultimap::const_iterator piEnd = from.plans_.end();
    for ( PlanMultimap::const_iterator pi = piBegin; pi != piEnd; ++pi )
    {
        plans_.insert( PlanMultimap::value_type( 
                            pi->first, 
                            FftwRealToRealPlan( pi->second.getSize(),
                                                data_,
                                                pi->second.getKind() ) ) );
    }

    return *this;
} 

FftwRealToRealPlanManager::~FftwRealToRealPlanManager( )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "~FftwRealToRealPlanManager( )" );
}

FftwRealToRealPlan & 
FftwRealToRealPlanManager::retrievePlan( FftwRealVector::size_type size,
                                         FftwRealToRealPlan::Kind kind )
{
    if ( plans_.find( size ) != plans_.end( ) ) {

        PlanMultimap::iterator pos = plans_.lower_bound( size );
        for ( ; pos != plans_.upper_bound( size ); ++pos ) {
            if ( (*pos).second.getKind( ) == kind ) {
                return ( *pos ).second;
            }
        }
    }
    
    CARMA_CPTRACE( TRACE_PLAN, "Creating plan on the fly." );

    ostringstream log;
    log << "Creating plan on the fly of size " << size 
        << ", data vector is sized to " << data_.size() << ".";

    programLogInfoIfPossible( log.str() );

    return createPlan( size, kind );
}

FftwRealToRealPlan & 
FftwRealToRealPlanManager::createPlan( FftwRealVector::size_type size,
                                       enum FftwRealToRealPlan::Kind kind )
{
    if ( size > maxSize_ ) { 
        throw CARMA_EXCEPTION( ErrorException, "Requested a size > maxSize." );
    }

    CARMA_CPTRACE( TRACE_PLAN, "FftwRealToRealPlanManager::createPlan( size="
        << size << ", kind=" << kind << ");" );


    PlanMultimap::value_type val( size, 
                                  FftwRealToRealPlan( size, data_, kind ) );

    PlanMultimap::iterator returnval = plans_.insert( val );

    return ( *returnval ).second;
}

FftwRealVector &
FftwRealToRealPlanManager::getFftwRealVector( ) 
{
    return data_;
}
