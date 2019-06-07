#include "carma/util/PthreadRWLockAttr.h"

#include "carma/util/posixErrors.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;


PthreadRWLockAttr::PthreadRWLockAttr( )
{
    const int posixErr = ::pthread_rwlockattr_init( &rwlockattr_ );
    
    failIfPosixError( posixErr, "pthread_rwlockattr_init failure" );
}


PthreadRWLockAttr::~PthreadRWLockAttr( )
try {
    const int posixErr = ::pthread_rwlockattr_destroy( &rwlockattr_ );
    
    logIfPosixError( posixErr, "pthread_rwlockattr_destroy failure" );
} catch ( ... ) {
    // just stifle any exceptions
    
    return;
}
