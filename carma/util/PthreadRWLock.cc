#include "carma/util/PthreadRWLock.h"

#include "carma/util/posixErrors.h"
#include "carma/util/PthreadRWLockAttr.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;


PthreadRWLock::PthreadRWLock( )
{
    const int posixErr = ::pthread_rwlock_init( &rwlock_, 0 );
    
    failIfPosixError( posixErr, "pthread_rwlock_init failure" );
}


PthreadRWLock::PthreadRWLock( const ::pthread_rwlockattr_t & attr )
{
    const int posixErr = ::pthread_rwlock_init( &rwlock_, &attr );
    
    failIfPosixError( posixErr, "pthread_rwlock_init failure" );
}


PthreadRWLock::PthreadRWLock( const PthreadRWLockAttr & attr )
{
    const int posixErr =
        ::pthread_rwlock_init( &rwlock_, &(attr.InternalPthreadRWLockAttr()) );
    
    failIfPosixError( posixErr, "pthread_rwlock_init failure" );
}


PthreadRWLock::~PthreadRWLock( )
try {
    const int posixErr = ::pthread_rwlock_destroy( &rwlock_ );
    
    logIfPosixError( posixErr, "pthread_rwlock_destroy failure" );
} catch ( ... ) {
    // just stifle any exceptions
    
    return;
}
