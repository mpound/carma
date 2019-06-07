#include "carma/util/PthreadAttr.h"

#include "carma/util/posixErrors.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;


PthreadAttr::PthreadAttr( )
{
    const int posixErr = ::pthread_attr_init( &attr_ );
    
    failIfPosixError( posixErr, "pthread_attr_init failure" );
}


PthreadAttr::PthreadAttr( const int detachState )
{
    {
        const int posixErr = ::pthread_attr_init( &attr_ );
        
        failIfPosixError( posixErr, "pthread_attr_init failure" );
    }
    
    try {
        const int posixErr =
            ::pthread_attr_setdetachstate( &attr_, detachState );
        
        failIfPosixError( posixErr, "pthread_attr_setdetachstate failure" );
    } catch ( ... ) {
        const int posixErr = ::pthread_attr_destroy( &attr_ );
        
        logIfPosixError( posixErr, "pthread_attr_destroy failure" );
        
        throw;
    }
}


PthreadAttr::~PthreadAttr( )
try {
    const int posixErr = ::pthread_attr_destroy( &attr_ );
    
    logIfPosixError( posixErr, "pthread_attr_destroy failure" );
} catch ( ... ) {
    // Just stifle any exception
    
    return;
}


int
PthreadAttr::getDetachState( ) const
{
    int detachState = PTHREAD_CREATE_DETACHED;
    
    const int posixErr = ::pthread_attr_getdetachstate( &attr_, &detachState );
    
    failIfPosixError( posixErr, "pthread_attr_getdetachstate failure" );
                      
    return detachState;
}


void
PthreadAttr::setDetachState( const int detachState )
{
    const int posixErr = ::pthread_attr_setdetachstate( &attr_, detachState );
    
    failIfPosixError( posixErr, "pthread_attr_setdetachstate failure" );
}


::size_t
PthreadAttr::getStackSize( ) const
{
    ::size_t stackSize = 0;
    
    const int posixErr = ::pthread_attr_getstacksize( &attr_, &stackSize );
    
    failIfPosixError( posixErr, "pthread_attr_getstacksize failure" );
                      
    return stackSize;
}


void
PthreadAttr::setStackSize( const ::size_t stackSize )
{
    const int posixErr = ::pthread_attr_setstacksize( &attr_, stackSize );

    failIfPosixError( posixErr, "pthread_attr_setstacksize failure" );
}


::size_t
PthreadAttr::getGuardSize( ) const
{
    ::size_t guardSize = 0;
    
    const int posixErr = ::pthread_attr_getguardsize( &attr_, &guardSize );

    failIfPosixError( posixErr, "pthread_attr_getguardsize failure" );
                      
    return guardSize;
}


void
PthreadAttr::setGuardSize( const ::size_t guardSize )
{
    const int posixErr = ::pthread_attr_setguardsize( &attr_, guardSize );

    failIfPosixError( posixErr, "pthread_attr_setguardsize failure" );
}
