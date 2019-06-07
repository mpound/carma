#include "carma/util/PthreadMutexAttr.h"

#include "carma/util/posixErrors.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;


PthreadMutexAttr::PthreadMutexAttr( )
{
    const int posixErr = ::pthread_mutexattr_init( &mutexAttr_ );
    
    failIfPosixError( posixErr, "pthread_mutexattr_init failure" );
}


PthreadMutexAttr::PthreadMutexAttr( const int type )
{
    {
        const int posixErr = ::pthread_mutexattr_init( &mutexAttr_ );
        
        failIfPosixError( posixErr, "pthread_mutexattr_init failure" );
    }
    
    try {
        const int posixErr = ::pthread_mutexattr_settype( &mutexAttr_, type );
        
        failIfPosixError( posixErr, "pthread_mutexattr_settype failure" );
    } catch ( ... ) {
        const int posixErr = ::pthread_mutexattr_destroy( &mutexAttr_ );
        
        logIfPosixError( posixErr, "pthread_mutexattr_destroy failure" );
        
        throw;
    }
}


PthreadMutexAttr::~PthreadMutexAttr( )
try {
    const int posixErr = ::pthread_mutexattr_destroy( &mutexAttr_ );
    
    logIfPosixError( posixErr, "pthread_mutexattr_destroy failure" );
} catch ( ... ) {
    // Just stifle any exception
    
    return;
}
