#include "carma/util/ScopedQILockManager.h"

#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/programLogging.h"
#include "carma/util/QuadraticInterpolator.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;


ScopedQILockManager::ScopedQILockManager(
    QuadraticInterpolator & qi,
    const bool              logIfLeftLocked ) :
qi_( qi ),
logIfLeftLocked_( logIfLeftLocked ),
locked_( false )
{
}


ScopedQILockManager::~ScopedQILockManager( )
try {
    if ( locked_ ) {
        locked_ = false;

        if ( logIfLeftLocked_ ) {
            try {
                programLogErrorIfPossible(
                    "Unlocking QuadraticInterpolator that was left locked" );
            } catch ( ... ) {
                // Just stifle the exception
            }
        }
        
        qi_.unlock();
    }
} catch ( ... ) {
    try {
        programLogErrorIfPossible(
            "Stifling exception in ~ScopedQILockManager: " +
            getStringForCaught() );
    } catch ( ... ) {
        // Just stifle any exception
    }
    
    return;
}


void
ScopedQILockManager::lockQI( )
{
    if ( locked_ != false ) {
        const string msg = "QuadraticInterpolator already locked";
        
        programLogErrorIfPossible( msg );
        
        throw CARMA_EXCEPTION( ErrorException, msg );
    }
    
    qi_.lock();

    locked_ = true;
}


void
ScopedQILockManager::unlockQI( )
{
    if ( locked_ != true ) {
        const string msg = "QuadraticInterpolator is not locked";
        
        programLogErrorIfPossible( msg );
        
        throw CARMA_EXCEPTION( ErrorException, msg );
    }
    
    qi_.unlock();
    
    locked_ = false;
}
