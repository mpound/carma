#include "carma/util/PthreadCondAttr.h"

#include "carma/util/posixErrors.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;


PthreadCondAttr::PthreadCondAttr( )
{
    const int posixErr = ::pthread_condattr_init( &condAttr_ );
    
    failIfPosixError( posixErr, "pthread_condattr_init failure" );
}


PthreadCondAttr::~PthreadCondAttr( )
try {
    const int posixErr = ::pthread_condattr_destroy( &condAttr_ );
    
    logIfPosixError( posixErr, "pthread_condattr_destroy failure" );
} catch ( ... ) {
    // Just stifle any exception
    
    return;
}
