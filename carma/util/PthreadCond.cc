#include "carma/util/PthreadCond.h"

#include <sstream>
#include <cerrno>

#include "carma/util/posixErrors.h"
#include "carma/util/PthreadCondAttr.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;


void
carma::util::broadcastCond( ::pthread_cond_t & c )
{
    const int posixErr = ::pthread_cond_broadcast( &c );

    failIfPosixError( posixErr, "pthread_cond_broadcast failure" );
}


void
carma::util::signalCond( ::pthread_cond_t & c )
{
    const int posixErr = ::pthread_cond_signal( &c );

    failIfPosixError( posixErr, "pthread_cond_signal failure" );
}


void
carma::util::waitCond( ::pthread_cond_t &  c,
                       ::pthread_mutex_t & m )
{
    const int posixErr = ::pthread_cond_wait( &c, &m );

    failIfPosixError( posixErr, "pthread_cond_wait failure" );
}


bool
carma::util::timedWaitCond( ::pthread_cond_t &        c,
                            ::pthread_mutex_t &       m,
                            const struct ::timespec & abstime )
{
    bool result = false;

    const int posixErr = ::pthread_cond_timedwait( &c, &m, &abstime );
    
    switch ( posixErr ) {
        case 0:
            result = true;
            break;

        case ETIMEDOUT:
            result = false;
            break;

        default:
            {
                ostringstream oss;
                
                oss << "pthread_cond_timedwait("
                    << static_cast< void * >( &c ) << ", "
                    << static_cast< void * >( &m ) << ", "
                    << "{" << abstime.tv_sec << ", " << abstime.tv_nsec << "}"
                    << ") failure";
                    
                throwPosixError( posixErr, oss.str() );
            }
            break;
    }

    return result;
}


PthreadCond::PthreadCond( )
{
    const int posixErr = ::pthread_cond_init( &cond_, 0 );
    
    failIfPosixError( posixErr, "pthread_cond_init failure" );
}


PthreadCond::PthreadCond( const ::pthread_condattr_t & attr )
{
    const int posixErr = ::pthread_cond_init( &cond_, &attr );
    
    failIfPosixError( posixErr, "pthread_cond_init failure" );
}


PthreadCond::PthreadCond( const PthreadCondAttr & attr )
{
    const int posixErr =
        ::pthread_cond_init( &cond_, &(attr.InternalPthreadCondAttr()) );
    
    failIfPosixError( posixErr, "pthread_cond_init failure" );
}


PthreadCond::~PthreadCond( )
try {
    const int posixErr = ::pthread_cond_destroy( &cond_ );
    
    logIfPosixError( posixErr, "pthread_cond_destroy failure" );
} catch ( ... ) {
    // Just stifle any exception
    
    return;
}


void
PthreadCond::Broadcast( )
{
    broadcastCond( cond_ );
}


void
PthreadCond::Signal( )
{
    signalCond( cond_ );
}


void
PthreadCond::Wait( ::pthread_mutex_t & m )
{
    waitCond( cond_, m );
}


bool
PthreadCond::TimedWait( ::pthread_mutex_t &       m,
                        const struct ::timespec & abstime )
{
    return timedWaitCond( cond_, m, abstime );
}
