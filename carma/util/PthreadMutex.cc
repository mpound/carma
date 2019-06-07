#include "carma/util/PthreadMutex.h"

#include "carma/util/posixErrors.h"
#include "carma/util/PthreadMutexAttr.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;


PthreadMutex::PthreadMutex( )
{
    const PthreadMutexAttr attr( PTHREAD_MUTEX_ERRORCHECK );

    const int posixErr =
        ::pthread_mutex_init( &mutex_, &(attr.InternalPthreadMutexAttr()) );

    failIfPosixError( posixErr, "pthread_mutex_init failure" );
}


PthreadMutex::PthreadMutex( const ::pthread_mutexattr_t & attr )
{
    const int posixErr = ::pthread_mutex_init( &mutex_, &attr );

    failIfPosixError( posixErr, "pthread_mutex_init failure" );
}


PthreadMutex::PthreadMutex( const PthreadMutexAttr & attr )
{
    const int posixErr =
        ::pthread_mutex_init( &mutex_, &(attr.InternalPthreadMutexAttr()) );

    failIfPosixError( posixErr, "pthread_mutex_init failure" );
}


PthreadMutex::PthreadMutex( const int type )
{
    if ( type == PTHREAD_MUTEX_DEFAULT ) {
        const int posixErr = ::pthread_mutex_init( &mutex_, 0 );
        
        failIfPosixError( posixErr, "pthread_mutex_init failure" );
    } else {
        const PthreadMutexAttr attr( type );

        const int posixErr =
            ::pthread_mutex_init( &mutex_,
                                  &(attr.InternalPthreadMutexAttr()) );

        failIfPosixError( posixErr, "pthread_mutex_init failure" );
    }
}


PthreadMutex::~PthreadMutex( )
try {
    const int posixErr = ::pthread_mutex_destroy( &mutex_ );
    
    logIfPosixError( posixErr, "pthread_mutex_destroy failure" );
} catch ( ... ) {
    // just stifle any exceptions
    
    return;
}
