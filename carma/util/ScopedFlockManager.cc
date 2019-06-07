#include "carma/util/ScopedFlockManager.h"

#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/programLogging.h"

#include <errno.h>
#include <fcntl.h>
#include <iostream>
#include <time.h>
#include <unistd.h>

using namespace carma::util;
using namespace std;

namespace {

const struct timespec kSleepyTime = { 0, 5000000 }; // 5ms
const unsigned int kBusyCountReport = 100; // Corresponds to frame
const unsigned int kIntrCountReport = 1000;

void
unlockFileDescriptor( const int fd )
try {
    const struct flock lock_command = { F_UNLCK, 0, 0, 0, 0 };

    while ( fcntl( fd, F_SETLK, &lock_command) == -1 ) {
        const int savedErrno = errno;

        if ( savedErrno == EINTR ) {
            // Interrupted by signal, try again.
        } else if ( savedErrno == EAGAIN ) {
            // Resource busy, sleep then try again.

            struct timespec rem;

            nanosleep( &kSleepyTime, &rem );
        } else {
            // Oh oh, a real problem.

            const string msg = "IPQ unlock failed:" +
                               string( strerror( savedErrno ) );

            throw CARMA_ERROR( msg );
        }
    }
} catch ( ... ) {
    try {
        const string msg =
            "Coming out of unlockFileDescriptor on an exception: " +
            getStringForCaught();

        programLogErrorIfPossible( msg );
    } catch ( ... ) {

    }

    throw;
}

} // namespace < unnamed >

ScopedFlockManager::ScopedFlockManager( const bool logIfLeftLocked ) :
logIfLeftLocked_( logIfLeftLocked ),
locked_( false ),
fd_( -1 )
{
}

ScopedFlockManager::~ScopedFlockManager( )
try {
    if ( locked_ ) {
        locked_ = false;

        if ( logIfLeftLocked_ ) {
            try {
                programLogErrorIfPossible( "IPQ: Left it locked" );
            } catch ( ... ) {
                // Just stifle any exception
            }
        }

        unlockFileDescriptor(fd_);
    }
} catch ( ... ) {
    try {
        const string msg =
            "Stifling exception in ~ScopedFlockManager";

        programLogErrorIfPossible( msg );
    } catch ( ... ) {
        // Just stifle any exception
    }

    return;
}

void
ScopedFlockManager::lockRead( int fd )
{
    if ( locked_ )
        throw CARMA_ERROR( "IPQ: Already locked" );

    const struct flock lock_command = { F_RDLCK, 0, 0, 0, 0 };

    unsigned int busyCount = 0;
    unsigned int intrCount = 0;

    while ( fcntl( fd, F_SETLK, &lock_command ) == -1 ) {
        const int savedErrno = errno;

        if ( savedErrno == EINTR ) {
            // Interrupted by signal, try again.
            ++intrCount;
        } else if ( savedErrno == EAGAIN ) {
            // Resource busy, sleep then try again.
            ++busyCount;
            struct timespec rem;
            nanosleep( &kSleepyTime, &rem );
        } else {
            // Oh oh, a real problem.
            const string msg = "IPQ lockRead failed:" +
                               string( strerror( savedErrno ) );
            throw CARMA_ERROR( msg );
        }
    }

    fd_ = fd;
    locked_ = true;
    
    if ( busyCount >= kBusyCountReport ) {
        ostringstream msg; 
        msg << "ScopedFlockManager::lockRead slept " 
            << ( ( kSleepyTime.tv_nsec * busyCount ) / 1000000 ) << "ms " 
            << "( " << busyCount << " busy cycles ) while waiting for "
            << "read flock.";
        programLogErrorIfPossible( msg.str( ) );
    }

    if ( intrCount > kIntrCountReport ) {
        ostringstream msg; 
        msg << "ScopedFlockManager::lockRead signals interrupted fcntl " 
            << intrCount << " times while waiting for read flock."; 
        programLogErrorIfPossible( msg.str( ) );
    }
}

void
ScopedFlockManager::lockWrite( int fd )
{
    if (locked_) throw CARMA_ERROR( "IPQ: Already locked" );

    const struct flock lock_command = { F_WRLCK, 0, 0, 0, 0 };

    while ( fcntl( fd, F_SETLK, &lock_command ) == -1 ) {
        const int savedErrno = errno;

        if ( savedErrno == EINTR ) {
            // Interrupted by signal, try again.
        } else if ( savedErrno == EAGAIN ) {
            // Resource busy, sleep then try again.

            struct timespec rem;

            nanosleep( &kSleepyTime, &rem );
        } else {
            // Oh oh, a real problem.
            const string msg = "IPQ lockWrite failed:" +
                               string( strerror( savedErrno ) );
            throw CARMA_ERROR( msg );
        }
    }

    fd_ = fd;
    locked_ = true;
}

void
ScopedFlockManager::unlock( int fd )
{
    if ( locked_ != true ) throw CARMA_ERROR( "IPQ: Not locked" );
    if (fd_ != fd) throw CARMA_ERROR( "IPQ: file descriptor mismatch" );
    unlockFileDescriptor( fd );

    locked_ = false;
    fd_ = -1;
}
