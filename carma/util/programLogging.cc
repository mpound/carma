#include "carma/util/programLogging.h"

#include "carma/util/programExtras.h"
#include "carma/util/Logger.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/Time.h"

using namespace ::std;
using namespace log4cpp;
using namespace carma;
using namespace carma::util;

namespace {

    string 
    getNdcTimestampMessage( )
    {
        const int kSecondsPrecision = 3;

        string msg;
        msg += " Logged locally at ";
        msg += Time::getTimeString( kSecondsPrecision );
        msg += " ";

        return msg;
    }
    
    class ScopedLogNdcTimestamp {
    private:
        
 //        ScopedLogNdc ndc_;

    public:

        // explicit ScopedLogNdcTimestamp( ) : ndc_( getNdcTimestampMessage( ) ) { };

        explicit ScopedLogNdcTimestamp( ) { };

        /* virtual */ ~ScopedLogNdcTimestamp( ) { };

    }; // class ScopedLogNdcTimestamp 

} // namespace < unnamed >

void
carma::util::programLogCritical( const string & msg )
{
    ScopedLogNdcTimestamp ndc;

    getProgramLogger().crit( msg );
}


void
carma::util::programLogCritical( const char * const msg )
{
    ScopedLogNdcTimestamp ndc;

    getProgramLogger().crit( msg );
}


void
carma::util::programLogError( const string & msg )
{
    ScopedLogNdcTimestamp ndc;

    getProgramLogger().error( msg );
}


void
carma::util::programLogError( const char * const msg )
{
    ScopedLogNdcTimestamp ndc;

    getProgramLogger().error( msg );
}


void
carma::util::programLogWarn( const string & msg )
{
    ScopedLogNdcTimestamp ndc;

    getProgramLogger().warn( msg );
}


void
carma::util::programLogWarn( const char * const msg )
{
    ScopedLogNdcTimestamp ndc;

    getProgramLogger().warn( msg );
}


void
carma::util::programLogNotice( const string & msg )
{
    ScopedLogNdcTimestamp ndc;

    getProgramLogger().notice( msg );
}


void
carma::util::programLogNotice( const char * const msg )
{
    ScopedLogNdcTimestamp ndc;

    getProgramLogger().notice( msg );
}


void
carma::util::programLogInfo( const string & msg )
{
    ScopedLogNdcTimestamp ndc;

    getProgramLogger().info( msg );
}


void
carma::util::programLogInfo( const char * const msg )
{
    ScopedLogNdcTimestamp ndc;

    getProgramLogger().info( msg );
}


void
carma::util::programLogDebug( const string & msg )
{
    ScopedLogNdcTimestamp ndc;

    getProgramLogger().debug( msg );
}


void
carma::util::programLogDebug( const char * const msg )
{
    ScopedLogNdcTimestamp ndc;

    getProgramLogger().debug( msg );
}


bool
programLoggingIsPossible( )
{
    return (getProgramLoggerIfAvailable() != 0);
}


void
carma::util::programLogCriticalIfPossible( const string & msg )
{
    ScopedLogNdcTimestamp ndc;

    Category * const logger = getProgramLoggerIfAvailable();
    
    if ( logger != 0 )
        logger->crit( msg );
}


void
carma::util::programLogCriticalIfPossible( const char * const msg )
{
    ScopedLogNdcTimestamp ndc;

    Category * const logger = getProgramLoggerIfAvailable();
    
    if ( logger != 0 )
        logger->crit( msg );
}


void
carma::util::programLogErrorIfPossible( const string & msg )
{
    ScopedLogNdcTimestamp ndc;

    Category * const logger = getProgramLoggerIfAvailable();
    
    if ( logger != 0 )
        logger->error( msg );
}


void
carma::util::programLogErrorIfPossible( const char * const msg )
{
    ScopedLogNdcTimestamp ndc;

    Category * const logger = getProgramLoggerIfAvailable();
    
    if ( logger != 0 )
        logger->error( msg );
}


void
carma::util::programLogWarnIfPossible( const string & msg )
{
    ScopedLogNdcTimestamp ndc;

    Category * const logger = getProgramLoggerIfAvailable();
    
    if ( logger != 0 )
        logger->warn( msg );
}


void
carma::util::programLogWarnIfPossible( const char * const msg )
{
    ScopedLogNdcTimestamp ndc;

    Category * const logger = getProgramLoggerIfAvailable();
    
    if ( logger != 0 )
        logger->warn( msg );
}


void
carma::util::programLogNoticeIfPossible( const string & msg )
{
    ScopedLogNdcTimestamp ndc;

    Category * const logger = getProgramLoggerIfAvailable();
    
    if ( logger != 0 )
        logger->notice( msg );
}


void
carma::util::programLogNoticeIfPossible( const char * const msg )
{
    ScopedLogNdcTimestamp ndc;

    Category * const logger = getProgramLoggerIfAvailable();
    
    if ( logger != 0 )
        logger->notice( msg );
}


void
carma::util::programLogInfoIfPossible( const string & msg )
{
    ScopedLogNdcTimestamp ndc;

    Category * const logger = getProgramLoggerIfAvailable();
    
    if ( logger != 0 )
        logger->info( msg );
}


void
carma::util::programLogInfoIfPossible( const char * const msg )
{
    ScopedLogNdcTimestamp ndc;

    Category * const logger = getProgramLoggerIfAvailable();
    
    if ( logger != 0 )
        logger->info( msg );
}


void
carma::util::programLogDebugIfPossible( const string & msg )
{
    ScopedLogNdcTimestamp ndc;

    Category * const logger = getProgramLoggerIfAvailable();
    
    if ( logger != 0 )
        logger->debug( msg );
}


void
carma::util::programLogDebugIfPossible( const char * const msg )
{
    ScopedLogNdcTimestamp ndc;

    Category * const logger = getProgramLoggerIfAvailable();
    
    if ( logger != 0 )
        logger->debug( msg );
}
