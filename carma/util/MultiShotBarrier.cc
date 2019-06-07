#include "carma/util/MultiShotBarrier.h"

#include <sstream>

#include "carma/util/ErrorException.h"
#include "carma/util/ScopedLock.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;

namespace {

template< typename T >
class ScopedRefCount {
public:

    explicit ScopedRefCount( T & var, PthreadMutex & guard ) :
        var_( var ),
        guard_( guard )
    {
        const ScopedLock< PthreadMutex > scopelock( guard_ );
        ++var_;
    };

    /* virtual */ ~ScopedRefCount( ) 
    {
        const ScopedLock< PthreadMutex > scopelock( guard_ );
        --var_;
    };

private:

    T & var_;
    PthreadMutex & guard_;
};

} // namespace < unnamed >

MultiShotBarrier::MultiShotBarrier( const size_t initialSatisfyCount ) :
satisfyCount_( initialSatisfyCount ),
guard_(),
satisfiedCond_(),
waitCount_( 0 ),
activeWaiters_( 0 ),
activeWaitersGuard_( )
{

}

MultiShotBarrier::MultiShotBarrier( ) :
satisfyCount_( 0 ),
guard_(),
satisfiedCond_(),
waitCount_( 0 ),
activeWaiters_( 0 ),
activeWaitersGuard_( )
{

}

MultiShotBarrier::~MultiShotBarrier( )
try {
} catch ( ... ) {
    // Just stifle the exception
    
    return;
}

void
MultiShotBarrier::reset( const size_t satisfyCount )
{
    {
        const ScopedLock< PthreadMutex > lock( activeWaitersGuard_ );
        if ( activeWaiters_ != 0 ) 
            throw CARMA_EXCEPTION( ErrorException, "Error - active waiters." );

    }

    const ScopedLock< PthreadMutex > lock( guard_ );
    satisfyCount_ = satisfyCount;
    waitCount_ = 0;
}

void
MultiShotBarrier::wait( )
{
    // Note I use separate variables for ref counting so as not to muck with
    // the signal predicate condition.
    ScopedRefCount<size_t> refcount( activeWaiters_, activeWaitersGuard_ );

    bool needToBroadcast = false;
    {
        const ScopedLock< PthreadMutex > lock( guard_ );

        if ( satisfyCount_ < 1 ) {
            ostringstream oss;
            
            oss << "Wait called but satisfyCount is " << satisfyCount_ << ". ";
            oss << "Call reset() with a satisfy count > 0 prior to using.";
            
            throw CARMA_EXCEPTION( ErrorException, oss.str() );
        }
        
        ++waitCount_;

        if ( waitCount_ == satisfyCount_ )
            needToBroadcast = true;
        else {
            while ( waitCount_ < satisfyCount_ )
                satisfiedCond_.Wait( guard_ );
        }
    }
    
    if ( needToBroadcast ) 
        satisfiedCond_.Broadcast();
}
