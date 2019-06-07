#include "carma/util/WorkRequest.h"

#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLock.h"
#include "carma/util/Trace.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;


namespace {


const Trace::TraceLevel kProblemTraceLevel = Trace::TRACE1;
const Trace::TraceLevel kLifecycleTraceLevel = Trace::TRACE5;
const Trace::TraceLevel kRefCountTraceLevel = Trace::TRACE6;
const Trace::TraceLevel kEverythingTraceLevel = Trace::TRACE7;

typedef const ScopedLock< PthreadMutex > RefCountGuardLock;


}  // namespace < anonymous >


WorkRequest::WorkRequest( Impl * const rhs ) :
impl_( rhs )
{
    if ( rhs == 0 )
        throw CARMA_ERROR( "null pointer" );
}


WorkRequest::~WorkRequest( )
try {
    Impl::removeRef( impl_ );
} catch ( ... ) {
    try {
        programLogErrorIfPossible(
            "Stifling exception in WorkRequest::~WorkRequest: " +
            getStringForCaught() );
    } catch ( ... ) {
    }

    // just stifle the exception
    
    return;
}


void
WorkRequest::service( )
{
    CARMA_CPTRACE( kEverythingTraceLevel, "WorkRequest::service()" );
    
    impl_->service();
}


WorkRequest::Impl::Impl( const string &     id,
                         const WorkResult & workResult ) :
id_( id ),

refCountGuard_(),
refCount_( 1 ),

workResult_( workResult ),
resultPosted_( false )
{
    // okay to use id_ because we are the only ones with a ref to this object
    // AND because we know the object should still
    // exist and the id_ member is well and truly immutable const
    
    CARMA_CPTRACE( kLifecycleTraceLevel, "constructing " << id_ );
}


WorkRequest::Impl::~Impl( )
try {
    // It's okay to use id_ because we know the object should still
    // exist and the id_ member is well and truly immutable const

    CARMA_CPTRACE( kLifecycleTraceLevel, "destructing " << id_ );

    if ( resultPosted_ == false )
        postAbnormalResult( "Abnormal WorkRequest::Impl::~Impl post" );
} catch ( ... ) {
    try {
        const string msg =
            "Stifling exception in WorkRequest::Impl::~Impl: " +
            getStringForCaught();
                           
        programLogErrorIfPossible( msg );
    } catch ( ... ) {
    }

    // just stifle the exception
    
    return;
}


WorkRequest::Impl *
WorkRequest::Impl::addRef( Impl * const impl )
{
    if ( impl == 0 ) {
        CARMA_CPTRACE( kProblemTraceLevel, "null pointer" );
    } else {
        ::size_t newRefCount;

        {
            const RefCountGuardLock lock( impl->refCountGuard_ );

            newRefCount = impl->refCount_ + 1;

            if ( newRefCount <= impl->refCount_ ) {
                // It's okay to use id_ because we know the object should still
                // exist and the id_ member is well and truly immutable const

                const string msg = impl->id_ + " ref count overflow";

                CARMA_CPTRACE( kProblemTraceLevel, msg );

                throw CARMA_ERROR( msg );
            }

            impl->refCount_ = newRefCount;
        }

        // It's okay to use id_ because we know the object should still
        // exist and the id_ member is well and truly immutable const

        CARMA_CPTRACE( kRefCountTraceLevel,
                       "Incremented set ref count for " << impl->id_ <<
                       " is " << newRefCount );
    }

    return impl;
}


void
WorkRequest::Impl::removeRef( const Impl * const impl )
{
    if ( impl == 0 ) {
        CARMA_CPTRACE( kProblemTraceLevel, "null pointer" );
    } else {
        // It's okay to use id_ because we know the object should still
        // exist and the id_ member is well and truly immutable const
        
        const string id = impl->id_;

        ::size_t newRefCount;

        {
            const RefCountGuardLock lock( impl->refCountGuard_ );

            newRefCount = (impl->refCount_ - 1);

            if ( newRefCount >= impl->refCount_ ) {
                const string msg = id + " set ref count underflow";

                CARMA_CPTRACE( kProblemTraceLevel, msg );

                throw CARMA_ERROR( msg );
            }
                
            impl->refCount_ = newRefCount;
        }

        // By this point id_ may indeed have been deleted by another thread
        // along with the rest of the impl instance

        CARMA_CPTRACE( kRefCountTraceLevel,
                       "Decremented ref count for " << id <<
                       " is " << newRefCount );

        if ( newRefCount < 1 )
            delete impl;
    }
}


void
WorkRequest::Impl::service( )
{
    CARMA_CPTRACE( kEverythingTraceLevel, "WorkRequest::Impl::service()" );
    
    bool normal = false;
    string errorText;
    
    try {
        serviceImpl();
        
        normal = true;
    } catch ( ... ) {
        try {
            errorText = getStringForCaught();
        } catch ( ... ) {
            // just stifle the exception
        }
        
        // just stifle the exception
    }
       
    if ( normal )
        postNormalResult();
    else if ( errorText.empty() )
        postAbnormalResult( "< UNKNOWN ERROR >" );
    else
        postAbnormalResult( errorText );
}


void
WorkRequest::Impl::postNormalResult( )
{
    resultPosted_ = true;
    
    workResult_.postNormal();
}


void
WorkRequest::Impl::postAbnormalResult( const string & errorText )
{
    resultPosted_ = true;

    workResult_.postAbnormal( errorText );
}
