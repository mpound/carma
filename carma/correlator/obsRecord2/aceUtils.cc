#include "carma/correlator/obsRecord2/aceUtils.h"

#include <ace/Log_Msg.h>
#include <ace/Log_Msg_Backend.h>
#include <ace/Log_Priority.h>
#include <ace/Log_Record.h>

#include "carma/util/PthreadMutex.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLock.h"
#include "carma/util/ScopedLogNdc.h"

using namespace ::std;
using namespace carma;
using namespace carma::correlator;
using namespace carma::correlator::obsRecord2;
using namespace carma::util;


namespace {


class Backend : public ACE_Log_Msg_Backend {
    public:
        int open( const ACE_TCHAR * logger_key );
        int reset( );
        int close( );
        ssize_t log( ACE_Log_Record & log_record );
};


int
Backend::open( const ACE_TCHAR * logger_key )
{
    // programLogInfoIfPossible( "Backend::open" );
    
    return 0;
}


int
Backend::reset( )
{
    // programLogInfoIfPossible( "Backend::reset" );

    return 0;
}


int
Backend::close( )
{
    // programLogInfoIfPossible( "Backend::close" );

    return 0;
}


ssize_t
Backend::log( ACE_Log_Record & logRecord )
{
    // programLogInfoIfPossible( "Backend::log" );

    const unsigned long msgType = logRecord.type();

    const ACE_Log_Priority msgPrior =
        static_cast< ACE_Log_Priority >( msgType );
        
    if ( msgPrior < LM_WARNING )
        return 0;
        
    const string msgPriorName( ACE_Log_Record::priority_name( msgPrior ) );

    const string msgText( logRecord.msg_data() );

    const string msg = "ACE " + msgPriorName + ": " + msgText;

    if ( msgPrior < LM_WARNING )
        programLogInfoIfPossible( msg );
    else if ( msgPrior < LM_ERROR )
        programLogWarnIfPossible( msg );
    else
        programLogWarnIfPossible( msg );

    return 0;
}


::pthread_mutex_t gAllocGuard = PTHREAD_MUTEX_INITIALIZER;

Backend * gBackend = 0;


}  // namespace < anonymous >


void
carma::correlator::obsRecord2::installAceLoggingBackend( )
{
    const ScopedLogNdc ndc( "installAceLoggingBackend" );
    
    const string programName = Program::getProgram().getArg0();
    
    const ScopedLock< pthread_mutex_t > lock( gAllocGuard );
    
    if ( gBackend == 0 ) {
        gBackend = new Backend;
        
        programLogInfoIfPossible( "Allocated Backend instance" );
    }
    
    ACE_LOG_MSG->msg_backend( gBackend );

    // Now we need to open the singleton in order to set the
    // "use my custom backend" flag
    
    ACE_LOG_MSG->open( programName.c_str(), ACE_Log_Msg::CUSTOM );

    programLogInfoIfPossible( "Backend installed" );
}
