#include "carma/util/OneShotBarrier.h"

#include <sstream>

#include "carma/util/IllegalArgumentException.h"
#include "carma/util/ScopedLock.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;


OneShotBarrier::OneShotBarrier( const size_t satisfyCount ) :
satisfyCount_( satisfyCount ),
guard_(),
satisfiedCond_(),
waitCount_( 0 )
{
    if ( satisfyCount < 1 ) {
        ostringstream oss;
        
        oss << "Illegal satisfyCount of " << satisfyCount;
        
        throw CARMA_EXCEPTION( IllegalArgumentException, oss.str() );
    }
}


OneShotBarrier::~OneShotBarrier( )
try {
} catch ( ... ) {
    // Just stifle the exception
    
    return;
}


void
OneShotBarrier::wait( )
{
    bool needToBroadcast = false;
    {
        const ScopedLock< PthreadMutex > lock( guard_ );
        
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
