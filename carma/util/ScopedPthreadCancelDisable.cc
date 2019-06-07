#include "carma/util/ScopedPthreadCancelDisable.h"

#include <sstream>

#include <pthread.h>

#include "carma/util/posixErrors.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;


ScopedPthreadCancelDisable::ScopedPthreadCancelDisable( )
{
    const int posixErr =
        ::pthread_setcancelstate( PTHREAD_CANCEL_DISABLE, &oldState_ );

    failIfPosixError( posixErr, "pthread_setcancelstate failure" );
}


ScopedPthreadCancelDisable::~ScopedPthreadCancelDisable( )
try {
    const ScopedLogNdc
        ndc( "ScopedPthreadCancelDisable::~ScopedPthreadCancelDisable" );
    
    int prevState = PTHREAD_CANCEL_ENABLE;

    const int posixErr = ::pthread_setcancelstate( oldState_, &prevState );
    
    if ( posixErr != 0 )
        logPosixError( posixErr, "pthread_setcancelstate failure" );
    else if ( prevState != PTHREAD_CANCEL_DISABLE ) {
        ostringstream oss;
        
        oss << "Bad previous cancel state " << prevState << " (";
        
        bool knownState = false;
        
        switch ( prevState ) {
            case PTHREAD_CANCEL_ENABLE:
                knownState = true;
                oss << "PTHREAD_CANCEL_ENABLE";
                break;
                
            case PTHREAD_CANCEL_DISABLE:
                knownState = true;
                oss << "PTHREAD_CANCEL_DISABLE";
                break;
        }
        
        if ( knownState == false )
            oss << "< unknown >";
            
        oss << ")";
        
        programLogErrorIfPossible( oss.str() );
    }
} catch ( ... ) {
    // Just stifle the exception
    
    return;
}
