/**
 * 
 * Trace.cc
 *
 */

#include "carma/util/Trace.h"

#include <iostream>

#include <pthread.h>
#include <sys/utsname.h>

#include <log4cpp/Priority.hh>

#include "carma/util/Logger.h"
#include "carma/util/Program.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/PthreadRWLock.h"
#include "carma/util/ScopedExclusiveLock.h"
#include "carma/util/ScopedLock.h"
#include "carma/util/ScopedSharedLock.h"


using namespace ::std;
using namespace ::log4cpp;


namespace carma {
namespace util {

namespace {


string
getSystemNodeName( )
{
    string result( "localhost.localdomain" );

    struct ::utsname systemNames;
    
    if ( ::uname( &systemNames ) == 0 )
        result = string( systemNames.nodename );

    return result;
}


Trace::TraceDestination
traceDestinationForString( const string & s )
{
    Trace::TraceDestination result = Trace::FILE;
    
    if ( s == "stdout" )
        result = Trace::STDOUT;
    else if ( s == "syslog" )
        result = Trace::SYSLOG;
    else
        result = Trace::FILE;

    return result;
}


// initialize mutexes

::pthread_mutex_t gFileMutex = PTHREAD_MUTEX_INITIALIZER;
::pthread_mutex_t gOstreamMutex = PTHREAD_MUTEX_INITIALIZER;
::pthread_mutex_t gSyslogMutex = PTHREAD_MUTEX_INITIALIZER;

// It's not a real performance hit to use one static r/w lock for all
// instances. Only changing trace level in one instance will ever grab an
// exclusive write lock on it and that should be an exceptionally rare event.
// All other uses will all be fully concurrent as they are all shared read
// locks.
// I know PTHREAD_RWLOCK_INITIALIZER is not fully portable POSIX coding but
// it works for now and I say great. It really should be part of POSIX again.
// If I have to then I can replace it with a pthread_once construct to gate
// runtime initializing gTraceLevelRWLock exactly once.
::pthread_rwlock_t gTraceLevelRWLock = PTHREAD_RWLOCK_INITIALIZER;

}  // anonymous namespace


// - construct all four categories, even if we'll only be using one
//   of them
// - need to have different values for the 3rd argument of the
//   categories to differentiate them
// - always send Trace output as DEBUG
Trace::Trace( const TraceLevel   traceLevel, 
              const string &     traceDestination,
	      const bool         traceVerbose,
              const string &     objectName,
              const facilityType facility ) :
systemNodeName_( getSystemNodeName( ) ),
traceDestination_( traceDestinationForString( traceDestination ) ),
traceLevel_( traceLevel ),
traceVerbose_( traceVerbose ),
sysLogCategory( Logger::getSyslogger(
    objectName,
    systemNodeName_, 
    "carma.util.Trace.syslog",
    Priority::DEBUG,
    facility ) ),
fileCategory( Logger::getFilelogger(
    objectName,
    (((traceDestination == "stdout") || (traceDestination == "syslog")) ?
        "/dev/null" :
        traceDestination),
    "carma.util.Trace.file",
    true, 
    00644, 
    Priority::DEBUG,
    traceVerbose_) ),
ostreamCategory( Logger::getOstreamlogger(
    objectName,
    &std::cout,
    "carma.util.Trace.ostream",
    Priority::DEBUG,
    traceVerbose_) )
{
}


Trace::~Trace( )
try {
} catch ( ... ) {
    // Just stifle any exception

    return;
}


Trace *
Trace::getProgramTrace( )
{
  return Program::getTraceObject();
}


Trace *
Trace::getProgramTraceIfAvailable( )
try {
    return Program::getTraceObjectIfAvailable();
} catch ( ... ) {
    // Just stifle any exception and return NULL
    
    return 0;
}


Trace *
Trace::getProgramTraceIfWillWrite( const TraceLevel traceLevel )
try {
    Trace * const programTO = Program::getTraceObjectIfAvailable();
    
    if ( (programTO != 0) && programTO->willWrite( traceLevel ) )
        return programTO;
    else
        return 0;
} catch ( ... ) {
    // Just stifle any exception and return NULL
    
    return 0;
}


void
Trace::stdoutMethod( const string & debugMessage )
{
    const ScopedLock< ::pthread_mutex_t > lock( gOstreamMutex );
    
    ostreamCategory << Priority::DEBUG << debugMessage;
}


void
Trace::fileMethod( const string & debugMessage )
{
    const ScopedLock< ::pthread_mutex_t > lock( gFileMutex );
    
    fileCategory << Priority::DEBUG << debugMessage;
}


// note: if using TRACEALL, this message may not end up in /var/log/messages
void
Trace::sysLogMethod( const string & debugMessage )
{
    const ScopedLock< ::pthread_mutex_t > lock( gSyslogMutex );
  
    sysLogCategory << Priority::DEBUG << debugMessage;
}


void
Trace::setObjectTraceLevel( const TraceLevel traceLevel )
{
    const ScopedExclusiveLock< ::pthread_rwlock_t >
        writeLock( gTraceLevelRWLock );
    
    traceLevel_ = traceLevel;
}


bool
Trace::willWrite( const TraceLevel traceLevel ) const
{
    bool writeIt = false;
    
    {
        const ScopedSharedLock< ::pthread_rwlock_t >
            readLock( gTraceLevelRWLock );

        writeIt = (traceLevel <= traceLevel_);
    }
    
    return writeIt;
}


void
Trace::writeMethod( const TraceLevel traceLevel,
                    const string &   debugMessage )
{
    bool writeIt = false;
    
    {
        const ScopedSharedLock< ::pthread_rwlock_t >
            readLock( gTraceLevelRWLock );

        writeIt = (traceLevel <= traceLevel_);
    }

    if ( writeIt != true )
        return;

    switch ( traceDestination_ ) {
        default:
        case STDOUT:
            stdoutMethod(debugMessage);
            break;
            
        case FILE:
            fileMethod(debugMessage);
            break;

        case SYSLOG:
            sysLogMethod(debugMessage);
            break;
    }
}


}  // namespace carma::util
}  // namespace carma
