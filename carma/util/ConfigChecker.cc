// $Id: ConfigChecker.cc,v 1.18 2007/10/11 02:04:41 tcosta Exp $

#include <iostream>
#include "carma/util/ConfigChecker.h"

#include <cstdlib>  // for atoi
#include <sys/time.h>  // for gettimeofday

#include "carma/util/AutoPthreadQuitAndJoinGroup.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/KeyValueConfigFile.h"
#include "carma/util/NotFoundException.h"
#include "carma/util/programLogging.h"
#include "carma/util/PthreadCond.h"
#include "carma/util/ScopedExclusiveLock.h"
#include "carma/util/ScopedLock.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/ScopedSharedLock.h"
#include "carma/util/StartPthread.h"
#include "carma/util/ThreadQuit.h"

using namespace ::std;
using namespace ::log4cpp;
using namespace carma;
using namespace carma::util;


namespace {


class Sleeper {
    public:
        class QuitHandler;
        
        explicit Sleeper( );
        
        void sleepForSecs( int secs );
        
    private:
        PthreadCond  cond_;
        PthreadMutex mutex_;
        bool         quitRequested_;
};


class Sleeper::QuitHandler : public ThreadQuitRequestHandler {
    public:
        explicit QuitHandler( Sleeper & sleeper );
        
        void HandleQuitRequest( ::pthread_t thread );
        
    private:
        Sleeper & sleeper_;
};


Sleeper::Sleeper( ) :
cond_(),
mutex_(),
quitRequested_( false )
{
}


void
Sleeper::sleepForSecs( const int secs )
{
    struct ::timeval now;
    gettimeofday( &now, 0 );

    ThreadQuitTestSelf();

    struct ::timespec endTime;
    endTime.tv_sec = now.tv_sec + secs;
    endTime.tv_nsec = 1000 * now.tv_usec;
    
    bool timedOut = false;

    {
        const ScopedLock< PthreadMutex > lock( mutex_ );
    
        while ( (quitRequested_ == false) && (timedOut == false) )
            timedOut = (cond_.TimedWait( mutex_, endTime ) == false);
    }
    
    ThreadQuitTestSelf();
    
    if ( timedOut == false )
        programLogErrorIfPossible( "Sleeper timedOut was false" );
}


Sleeper::QuitHandler::QuitHandler( Sleeper & sleeper ) :
sleeper_( sleeper )
{
}


void
Sleeper::QuitHandler::HandleQuitRequest( const ::pthread_t )
{
    {
        const ScopedLock< PthreadMutex > lock( sleeper_.mutex_ );
        
        sleeper_.quitRequested_ = true;
    }
    
    // At most one thread should be waiting so
    // Signal will work instead of Broadcast.
    sleeper_.cond_.Signal();
}


typedef ScopedSharedLock< PthreadRWLock > ScopedGuardReadLock;
typedef ScopedExclusiveLock< PthreadRWLock > ScopedGuardWriteLock;


}  // namespace < anonymous >


ConfigChecker::ConfigChecker( const string & fileName ) :
// default value if file can't be found or parameter is missing
// units are in seconds.
kOneString_( "1" ),
kZeroString_( "0" ),
delayBetweenFileReads_( 5 ),
fileName_( fileName ),
guard_(),
configFileNotFound_( true ),
pairs_(),
haveFirstReadCtime_( false ),
firstReadCtime_(),
log_( Program::getProgram().getLogger() ),
updateThreadGuard_(),
updateThreadStarted_( false ),
updateThread_()
{
    // try reading the file now, before thread is started
    try {
        {
            const ScopedGuardWriteLock writeLock( guard_ );
            
            pairs_ = KeyValueConfigFile::load( fileName_ );
            configFileNotFound_ = false;
        }

        struct stat filestats;
        if ( stat(fileName.c_str(), &filestats ) == 0 ) {
            firstReadCtime_ = filestats.st_ctime;
            haveFirstReadCtime_ = true;
        }
    } catch ( const NotFoundException & err ) {
        configFileNotFound_ = true;
        
        if ( fileName_.empty() ) {
            programLogWarnIfPossible(
                "ConfigChecker: configuration filename is empty" );
        }
        
        programLogWarnIfPossible(
            "ConfigChecker: configuration file \"" + fileName_ +
            "\" not found. This should not affect execution. - " +
            getStringForCaught() );
    }
}


ConfigChecker::~ConfigChecker( )
try {
    const ScopedLogNdc
        ndc( "ConfigChecker(" + fileName_ + ")::~ConfigChecker" );
    
    const ScopedLock< PthreadMutex > lock( updateThreadGuard_ );

    if ( updateThreadStarted_ ) {
        // programLogInfoIfPossible( "Stopping update thread..." );

        {
            const AutoPthreadQuitAndJoinGroup quitAndJoin( updateThread_ );
        }
        
        // programLogInfoIfPossible( "Update thread stopped" );
    }
} catch ( ... ) {
    try {
        programLogErrorIfPossible(
            "Stifling exception in ConfigChecker::~ConfigChecker() - " +
            getStringForCaught() );
    } catch ( ... ) {
        // Just stifle any exceptions
    }

    // Just stifle any exceptions
    return;
}


bool
ConfigChecker::getConfigFileNotFound( ) const
{
    return configFileNotFound_;
}


string
ConfigChecker::getValue( const string & key ) const
{
    string val;

    // return empty string if key does not exists
    {
        const ScopedGuardReadLock readLock( guard_ );
        
        const map< string, string >::const_iterator i = pairs_.find( key );
        
        if ( i != pairs_.end() )
            val = i->second;
    }

    return val;
}


bool
ConfigChecker::valueIsEmpty( const string & key ) const
{
    bool result = true;

    // return empty string if key does not exists
    {
        const ScopedGuardReadLock readLock( guard_ );
        
        const map< string, string >::const_iterator i = pairs_.find( key );
        
        if ( i != pairs_.end() )
            result = i->second.empty();
    }

    return result;
}


bool
ConfigChecker::valueIsOneString( const string & key ) const
{
    bool result = false;

    // return empty string if key does not exists
    {
        const ScopedGuardReadLock readLock( guard_ );
        
        const map< string, string >::const_iterator i = pairs_.find( key );
        
        if ( i != pairs_.end() )
            result = (i->second == kOneString_);
    }

    return result;
}


bool
ConfigChecker::valueIsZeroString( const string & key ) const
{
    bool result = false;

    // return empty string if key does not exists
    {
        const ScopedGuardReadLock readLock( guard_ );
        
        const map< string, string >::const_iterator i = pairs_.find( key );
        
        if ( i != pairs_.end() )
            result = (i->second == kZeroString_);
    }

    return result;
}


int
ConfigChecker::getDelay( ) const
{
    const string val = getValue( "delayBetweenFileReads" );
    
    if ( val.empty() )
        return delayBetweenFileReads_;

    const int delay = atoi( val.c_str() );

    if (delay <= 0)
        return delayBetweenFileReads_;
        
    return delay;
}


Category &
ConfigChecker::getLogger( ) const
{
  return log_;
}


string
ConfigChecker::getFilename( ) const
{
    const string val = getValue( "configFilename" );

    if ( val.empty() || (val == "default") )
        return fileName_;
    else
        return val;
}


void
ConfigChecker::start( )
try {
    const ScopedLogNdc ndc( "ConfigChecker(" + fileName_ + ")::start" );
    
    {
        const ScopedLock< PthreadMutex > lock( updateThreadGuard_ );
    
        if ( updateThreadStarted_ )
            programLogWarnIfPossible( "Update thread already was started" );
        else {
            const string initialNdc =
                "ConfigChecker(" + fileName_ + ") Update Thread";
                
            // programLogInfoIfPossible( "Starting update thread..." );

            updateThread_ = 
                StartPthreadWithRef( updateThreadEntrypoint,
                                     *this,
                                     initialNdc );
        
            updateThreadStarted_ = true;

            // programLogInfoIfPossible( "Update thread started" );
        }
    }
} catch ( ... ) {
    programLogErrorIfPossible(
        "Coming out of ConfigChecker::start() on an exception - " +
        getStringForCaught() );
        
    throw;
}


void
ConfigChecker::updateThreadEntrypoint( ConfigChecker & cc )
try {
    programLogInfoIfPossible( "Thread running" );
    
    const string kDebugAllKey = "debugAll";
    const string kDebugConfigCheckerKey = "debugConfigChecker";

    string filename = cc.getFilename();
    int delaySecs = cc.getDelay();
    bool emitDebugMessages = 
        (cc.valueIsOneString( kDebugAllKey ) || 
         cc.valueIsOneString( kDebugConfigCheckerKey ));

    bool haveLastReadCtime = cc.haveFirstReadCtime_;
    time_t lastReadCtime = cc.firstReadCtime_;

    Sleeper sleeper;
    Sleeper::QuitHandler quitHandler( sleeper );
    const ScopedThreadQuitRequestHandlerSelf quitHandlerInstall( quitHandler );
    
    while ( true ) {
        // load config file
        try {
            // check to see if file has been changed. If so, the re-read it.
            struct stat filestats;
            const int statErr = stat( filename.c_str(), &filestats );
            
            if ( statErr != 0 ) {
                if ( haveLastReadCtime ) {
                    haveLastReadCtime = false;

                    const string msg = "file disappeared";
                    
                    programLogErrorIfPossible( msg );
                    if ( emitDebugMessages )
                        cerr << "ConfigChecker::update - " << msg << endl;
                }
            } else if ( (haveLastReadCtime == false) ||
                        (filestats.st_ctime != lastReadCtime) ) {
                {
                    string msg;
                    
                    msg += "file ";
                    msg += (haveLastReadCtime ? "changed" : "appeared");
                    msg += ", re-reading...";
                    
                    programLogInfoIfPossible( msg );
                    if ( emitDebugMessages )
                        cerr << "ConfigChecker::update - " << msg << endl;
                }

                lastReadCtime = filestats.st_ctime;
                haveLastReadCtime = true;

                {
                    const ScopedGuardWriteLock writeLock( cc.guard_ );
                    
                    cc.pairs_ = KeyValueConfigFile::load( filename );
                }
            
                // Update our values to reflect any changes we just read
                delaySecs = cc.getDelay();
                filename = cc.getFilename();
                emitDebugMessages =
                    (cc.valueIsOneString( kDebugAllKey ) || 
                     cc.valueIsOneString( kDebugConfigCheckerKey ));
            }
        } catch ( const ErrorException & err ) {
            programLogErrorIfPossible( getStringForCaught() );
            
            if ( emitDebugMessages )
                cerr << err << endl;
        }
        
        sleeper.sleepForSecs( delaySecs );
    }

    programLogErrorIfPossible( "Thread completing" );
} catch ( ... ) {
    if ( CaughtExceptionIsThreadQuitRequestedError() )
        programLogInfoIfPossible( "Quitting as requested" );
    else {
        programLogErrorIfPossible(
            "Coming out of ConfigChecker::updateThreadEntrypoint"
            " on an exception - " +
            getStringForCaught() );
    }
    
    throw;
}
