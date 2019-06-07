#include "carma/util/ExceptionUtils.h"

#include "carma/corba/corba.h"
#include <orbsvcs/CosNamingC.h>

#include <stdexcept>

#include "carma/util/BaseException.h"
#include "carma/util/UserException.h"
#include "carma/util/programExtras.h"
#include "carma/util/Trace.h"
#include "carma/util/Logger.h"
#include "carma/util/Backtrace.h"
#include "carma/util/loggingUtils.h"


using namespace ::std;
using namespace ::log4cpp;
using namespace ::CORBA;
using namespace ::CosNaming;
using namespace carma;
using namespace carma::util;


namespace {


Trace::TraceLevel
convertToTraceLevelEnum( const int x )
{
    if ( (x >= Trace::TRACE0) && (x <= Trace::TRACE7) )
        return static_cast< Trace::TraceLevel >( x );
        
    if ( x > Trace::TRACE7 )
        return Trace::TRACE7;

    return Trace::TRACE0;
}


string
getStringForNotFoundReason( const NamingContext::NotFoundReason nfr )
{
    switch ( nfr ) {
        case NamingContext::missing_node:  return "missing_node";
        case NamingContext::not_context:   return "not_context";
        case NamingContext::not_object:    return "not_object";
    }
    
    return "< unknown >";
}


string
getStringForNameComponent( const NameComponent & nameComponent )
{
    const string idString =
        static_cast< const char * >( nameComponent.id );
        
    const string kindString =
        static_cast< const char * >( nameComponent.kind );

    if ( kindString.empty() )
        return idString;
    else
        return idString + "(" + kindString + ")";
}


string
getStringForName( const Name & name )
{
    string result;
    
    for ( ::size_t i = 0; i < name.length(); ++i ) {
        if ( i != 0 )
            result += ".";
            
        result += getStringForNameComponent( name[ i ] );
    }
        
    return result;
}


void
internalLogCaught( Category * const        inLogger,
                   Priority::PriorityLevel priority,
                   const string &          preface )
{
    Category & logger = ((inLogger != 0) ? (*inLogger) : getProgramLogger());
    
    logger << priority << preface << getStringForCaught();
}


void
internalLogCaught( Category * const        inLogger,
                   Priority::PriorityLevel priority,
                   const char * const      preface )
{
    Category & logger = ((inLogger != 0) ? (*inLogger) : getProgramLogger());
    
    if ( preface == 0 )
        logger << priority << getStringForCaught();
    else
        logger << priority << preface << getStringForCaught();
}


}  // namespace < anonymous >


string
carma::util::getStringForCaught( )
try {
    throw;
} catch ( const CosNaming::NamingContext::NotFound & nfe ) {
    string msg;

    try {
        ostringstream oss;
        
        oss << "CosNaming::NamingContext::NotFound ("
            << "why=" << getStringForNotFoundReason( nfe.why )
            << ", rest_of_name=" << getStringForName( nfe.rest_of_name )
            << ", " << nfe << ")";
        
        msg = oss.str();
    } catch ( ... ) {
        // Stifle
    }

    return msg;
} catch ( const util::UserException & ue ) {
    string s;

    try {
        ostringstream oss;

        oss << "util::UserException ("
            << "File: " << ue.fileName << ", "
            << "Line: " << ue.lineNo << ", "
            << "Message: " << ue.errorMsg
            << ")";

        s = oss.str();
    } catch ( ... ) {
        // Just stifle any exception
    }

    return s;
} catch ( const CORBA::Exception & e ) {
    string s;

    try {
        ostringstream oss;

        oss << "CORBA::Exception (" << e << ")";

        s = oss.str();
    } catch ( ... ) {
        // Just stifle any exception
    }

    return s;
} catch ( const BaseException & e ) {
    return e.getLogString();
} catch ( const ::std::exception & e ) {
    return e.what();
} catch ( const ::std::string & e ) {
    return e;
} catch ( const char * const e ) {
    string s;
    
    try {
        ostringstream oss;
        
        oss << "const char * (" << static_cast< const void * >( e ) << ")";
        
        s = oss.str();
    } catch ( ... ) {
        // Just stifle any exception
    }
    
    return s;
} catch ( char * const e ) {
    string s;
    
    try {
        ostringstream oss;
        
        oss << "char * (" << static_cast< void * >( e ) << ")";
        
        s = oss.str();
    } catch ( ... ) {
        // Just stifle any exception
    }
    
    return s;
} catch ( const char & e ) {
    string s;
    
    try {
        ostringstream oss;
        
        oss << "char (" << e << ")";
        
        s = oss.str();
    } catch ( ... ) {
        // Just stifle any exception
    }
    
    return s;
} catch ( const unsigned char & e ) {
    string s;
    
    try {
        ostringstream oss;
        
        oss << "unsigned char (" << e << ")";
        
        s = oss.str();
    } catch ( ... ) {
        // Just stifle any exception
    }
    
    return s;
} catch ( const short & e ) {
    string s;
    
    try {
        ostringstream oss;
        
        oss << "short (" << e << ")";
        
        s = oss.str();
    } catch ( ... ) {
        // Just stifle any exception
    }
    
    return s;
} catch ( const unsigned short & e ) {
    string s;
    
    try {
        ostringstream oss;
        
        oss << "unsigned short (" << e << ")";
        
        s = oss.str();
    } catch ( ... ) {
        // Just stifle any exception
    }
    
    return s;
} catch ( const int & e ) {
    string s;
    
    try {
        ostringstream oss;
        
        oss << "int (" << e << ")";
        
        s = oss.str();
    } catch ( ... ) {
        // Just stifle any exception
    }
    
    return s;
} catch ( const unsigned int & e ) {
    string s;
    
    try {
        ostringstream oss;
        
        oss << "unsigned int (" << e << ")";
        
        s = oss.str();
    } catch ( ... ) {
        // Just stifle any exception
    }
    
    return s;
} catch ( const long & e ) {
    string s;
    
    try {
        ostringstream oss;
        
        oss << "long (" << e << ")";
        
        s = oss.str();
    } catch ( ... ) {
        // Just stifle any exception
    }
    
    return s;
} catch ( const unsigned long & e ) {
    string s;
    
    try {
        ostringstream oss;
        
        oss << "unsigned long (" << e << ")";
        
        s = oss.str();
    } catch ( ... ) {
        // Just stifle any exception
    }
    
    return s;
} catch ( const long long & e ) {
    string s;
    
    try {
        ostringstream oss;
        
        oss << "long long (" << e << ")";
        
        s = oss.str();
    } catch ( ... ) {
        // Just stifle any exception
    }
    
    return s;
} catch ( const unsigned long long & e ) {
    string s;
    
    try {
        ostringstream oss;
        
        oss << "unsigned long long (" << e << ")";
        
        s = oss.str();
    } catch ( ... ) {
        // Just stifle any exception
    }
    
    return s;
} catch ( const float & e ) {
    string s;
    
    try {
        ostringstream oss;
        
        oss << "float (" << e << ")";
        
        s = oss.str();
    } catch ( ... ) {
        // Just stifle any exception
    }
    
    return s;
} catch ( const double & e ) {
    string s;
    
    try {
        ostringstream oss;
        
        oss << "double (" << e << ")";
        
        s = oss.str();
    } catch ( ... ) {
        // Just stifle any exception
    }
    
    return s;
} catch ( const long double & e ) {
    string s;
    
    try {
        ostringstream oss;
        
        oss << "long double (" << e << ")";
        
        s = oss.str();
    } catch ( ... ) {
        // Just stifle any exception
    }
    
    return s;
} catch ( ... ) {
    return "< Unknown exception >";
}


void
carma::util::logCaught( Category &                    logger,
                        const Priority::PriorityLevel priority )
{
    internalLogCaught( &logger, priority, 0 );
}


void
carma::util::logCaught( const Priority::PriorityLevel priority )
{
    internalLogCaught( 0, priority, 0 );
}


void
carma::util::logCaughtAsError( Category & logger )
{
    internalLogCaught( &logger, Priority::ERROR, 0 );
}

void
carma::util::logCaughtAsError( Category & logger, 
                               const ::std::string & preface )
{
    internalLogCaught( &logger, Priority::ERROR, preface );
}

void
carma::util::logCaughtAsError( )
{
    internalLogCaught( 0, Priority::ERROR, 0 );
}


void
carma::util::logCaughtAndRethrowAsUser(
    Category &                    logger,
    const Priority::PriorityLevel priority)
{
    try {
        internalLogCaught( &logger, priority, 0 );
    } catch ( ... ) {
        // Just stifle any exception
    }
    
    rethrowCaughtAsUser();
}


void
carma::util::logCaughtAndRethrowAsUser(
    const Priority::PriorityLevel priority )
{
    try {
        internalLogCaught( 0, priority, 0 );
    } catch ( ... ) {
        // Just stifle any exception
    }

    rethrowCaughtAsUser();
}


void
carma::util::logCaughtAsErrorAndRethrowAsUser( Category & logger )
{
    try {
        internalLogCaught( &logger, Priority::ERROR, 0 );
    } catch ( ... ) {
        // Just stifle any exception
    }

    rethrowCaughtAsUser();
}


void
carma::util::logCaughtAsErrorAndRethrowAsUser( )
{
    try {
        internalLogCaught( 0, Priority::ERROR, 0 );
    } catch ( ... ) {
        // Just stifle any exception
    }

    rethrowCaughtAsUser();
}


void
carma::util::logCaughtAsErrorAndRethrowAsUser( const char * const preface )
{
    try {
        internalLogCaught( 0, Priority::ERROR, preface );
    } catch ( ... ) {
        // Just stifle any exception
    }

    rethrowCaughtAsUser();
}


void
carma::util::logCaughtAsErrorAndRethrowAsUser( const string & preface )
{
    try {
        internalLogCaught( 0, Priority::ERROR, preface );
    } catch ( ... ) {
        // Just stifle any exception
    }

    rethrowCaughtAsUser();
}


void
carma::util::traceCaught( const int traceLevel )
{
}


void
carma::util::traceCaughtAndRethrowAsUser( const int traceLevel )
{
    try {
        traceCaught( traceLevel );
    } catch ( ... ) {
        // Just stifle any exception
    }
    
    rethrowCaughtAsUser();
}


void
carma::util::rethrowCaughtAsUser( )
try {
    throw;
} catch ( const util::CancelException & ) {
    throw;
} catch ( const util::UserException & ) {
    throw;
} catch ( const CosNaming::NamingContext::NotFound & nfe ) {
    string msg;

    try {
        ostringstream oss;
        
        oss << "CosNaming::NamingContext::NotFound ("
            << "why=" << getStringForNotFoundReason( nfe.why )
            << ", rest_of_name=" << getStringForName( nfe.rest_of_name )
            << ")";
        
        msg = oss.str();
    } catch ( ... ) {
        // Stifle
    }

    throw util::UserException( msg.c_str(),
                               __FILE__,
                               __LINE__ );
} catch ( const CORBA::Exception & e ) {
    string msg;

    try {
        ostringstream oss;
        
        oss << "CORBA::Exception (" << e << ")";
        
        msg = oss.str();
    } catch ( ... ) {
        // Stifle
    }

    throw util::UserException( msg.c_str(),
                               __FILE__,
                               __LINE__ );
} catch ( const BaseException & e ) {
    throw util::UserException( e.getMessage(),
                               e.getSourceFile(),
                               e.getLineNumber() );
} catch ( const ::std::exception & e ) {
    throw util::UserException( e.what(),
                               __FILE__,
                               __LINE__ );
} catch ( ... ) {
    throw util::UserException( "< Unknown >",
                               __FILE__,
                               __LINE__ );
}


void
carma::util::terminateHandler( )
try {
    Category * const logger = getProgramLoggerIfAvailable();
    Trace * const trace = getProgramTraceObjectIfAvailable();
    
    if ( (logger != 0) || (trace != 0) ) {
        if ( logger != 0 )
            *logger << Priority::ERROR << "terminate() called.";
            
        if ( trace != 0 )
            trace->write( Trace::TRACE1, "terminate() called." );
        
        string btText;
        
        {
            Backtrace bt;
            
            bt.captureNoThrow();
            
            btText = bt.formattedAsString( "  terminate() bt ",
                                           "\n" );
        }
        
        if ( logger != 0 ) {
            *logger << Priority::ERROR << "Backtrace is:";
            logMultipleLines( *logger, Priority::ERROR, btText );
        }

        if ( trace != 0 ) {
            trace->write( Trace::TRACE1,
                          string( "Backtrace is:\n" ) + btText );
        }
    }
    
    abort();
} catch ( ... ) {
    abort();
}


void
carma::util::unexpectedHandler()
try {
    Category * const logger = getProgramLoggerIfAvailable();
    Trace * const trace = getProgramTraceObjectIfAvailable();
    
    if ( (logger != 0) || (trace != 0) ) {
        if ( logger != 0 )
            *logger << Priority::ERROR << "unexpected() called.";
            
        if ( trace != 0 )
            trace->write( Trace::TRACE1, "unexpected() called." );
        
        string btText;
        
        {
            Backtrace bt;
            
            bt.captureNoThrow();
            
            btText = bt.formattedAsString( "  unexpected() bt ",
                                           "\n" );
        }
        
        if ( logger != 0 ) {
            *logger << Priority::ERROR << "Backtrace is:";
            logMultipleLines( *logger, Priority::ERROR, btText );
        }
        
        if ( trace != 0 ) {
            trace->write( Trace::TRACE1,
                          string( "Backtrace is:\n" ) + btText );
        }
    }
    
    throw ::std::bad_exception();
} catch ( const ::std::bad_exception & ) {
    throw;
} catch ( ... ) {
    throw ::std::bad_exception();
}


namespace {


const char * const kDefBtLinePrefix = "  bt ";
const char * const kDefBtLineSuffix = "\n";


Backtrace
getCaughtBacktrace( )
try {
    throw;
} catch ( const BaseException & be ) {
    return be.getBacktrace();
} catch ( ... ) {
    return Backtrace();
}


}  // namespace < anonymous >


bool
carma::util::caughtBacktraceCaptured( )
{
    const Backtrace bt = getCaughtBacktrace();

    return bt.captured();
}


string
carma::util::getCaughtBacktraceAsString( )
{
    const Backtrace bt = getCaughtBacktrace();

    return bt.formattedAsString( string( kDefBtLinePrefix ),
                                 string( kDefBtLineSuffix ) );
}


string
carma::util::getCaughtBacktraceAsString( const string & linePrefix,
                                         const string & lineSuffix )
{
    const Backtrace bt = getCaughtBacktrace();

    return bt.formattedAsString( linePrefix, lineSuffix );
}


string
carma::util::getCaughtBacktraceAsString( const char * const linePrefix,
                                         const char * const lineSuffix )
{
    const Backtrace bt = getCaughtBacktrace();

    return bt.formattedAsString( linePrefix, lineSuffix );
}


void
carma::util::logCaughtBacktraceAsError( Category &     logger,
                                        const string & linePrefix )
{
    const Backtrace bt = getCaughtBacktrace();
    
    if ( bt.captured() ) {
        const string btText = bt.formattedAsString( linePrefix,
                                                    kDefBtLineSuffix );
        
        logMultipleLines( logger, Priority::ERROR, btText );
    }
}


void
carma::util::logCaughtBacktraceAsErrorIfPossible( const string & linePrefix )
{
    Category * const logger = getProgramLoggerIfAvailable();

    if ( logger != 0 )
        logCaughtBacktraceAsError( *logger, linePrefix );
}


void
carma::util::logCaughtBacktraceAsError( Category &         logger,
                                        const char * const linePrefix )
{
    string lp;
    
    if ( linePrefix != 0 )
        lp = linePrefix;
        
    return logCaughtBacktraceAsError( logger, lp );
}


void
carma::util::logCaughtBacktraceAsErrorIfPossible(
    const char * const linePrefix )
{
    string lp;
    
    if ( linePrefix != 0 )
        lp = linePrefix;
        
    return logCaughtBacktraceAsErrorIfPossible( lp );
}


void
carma::util::logCaughtBacktraceAsError( Category & logger )
{
    return logCaughtBacktraceAsError( logger, string( kDefBtLinePrefix ) );
}


void
carma::util::logCaughtBacktraceAsErrorIfPossible( )
{
    return logCaughtBacktraceAsErrorIfPossible( string( kDefBtLinePrefix ) );
}
