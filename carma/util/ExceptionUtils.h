#ifndef CARMA_UTIL_EXCEPTION_UTILS_H
#define CARMA_UTIL_EXCEPTION_UTILS_H


#include <string>

#include <log4cpp/Priority.hh>
#include <log4cpp/Category.hh>

namespace carma {
namespace util {


class Logger;


::std::string getStringForCaught( );


void logCaught( log4cpp::Category &              logger,
                log4cpp::Priority::PriorityLevel priority );
void logCaught( log4cpp::Category &              logger,
                log4cpp::Priority::PriorityLevel priority,
                const ::std::string &            preface );

void logCaught( log4cpp::Priority::PriorityLevel priority );
void logCaught( log4cpp::Priority::PriorityLevel priority,
                const ::std::string &            preface );

void logCaughtAsError( log4cpp::Category & logger );
void logCaughtAsError( log4cpp::Category &   logger,
                       const ::std::string & preface );

void logCaughtAsError( );
void logCaughtAsError( const ::std::string & preface );

void traceCaught( int traceLevel );


void rethrowCaughtAsUser( );

void logCaughtAndRethrowAsUser( log4cpp::Category &              logger,
                                log4cpp::Priority::PriorityLevel priority );
void logCaughtAndRethrowAsUser( log4cpp::Category &              logger,
                                log4cpp::Priority::PriorityLevel priority,
                                const ::std::string &            preface );
void logCaughtAndRethrowAsUser( log4cpp::Category &              logger,
                                log4cpp::Priority::PriorityLevel priority,
                                const char *                     preface );

void logCaughtAndRethrowAsUser( log4cpp::Priority::PriorityLevel priority );
void logCaughtAndRethrowAsUser( log4cpp::Priority::PriorityLevel priority,
                                const ::std::string &            preface );
void logCaughtAndRethrowAsUser( log4cpp::Priority::PriorityLevel priority,
                                const char *                     preface );

void logCaughtAsErrorAndRethrowAsUser( log4cpp::Category & logger );
void logCaughtAsErrorAndRethrowAsUser( log4cpp::Category &   logger,
                                       const ::std::string & preface );
void logCaughtAsErrorAndRethrowAsUser( log4cpp::Category &   logger,
                                       const char *          preface );

void logCaughtAsErrorAndRethrowAsUser( );
void logCaughtAsErrorAndRethrowAsUser( const ::std::string & preface );
void logCaughtAsErrorAndRethrowAsUser( const char *          preface );
    
void traceCaughtAndRethrowAsUser( int traceLevel );


void terminateHandler( );

void unexpectedHandler( );


bool caughtBacktraceCaptured( );


::std::string getCaughtBacktraceAsString( );

::std::string getCaughtBacktraceAsString( const ::std::string & linePrefix,
                                          const ::std::string & lineSuffix );

::std::string getCaughtBacktraceAsString( const char * linePrefix,
                                          const char * lineSuffix );


void logCaughtBacktraceAsError( log4cpp::Category & logger );

void logCaughtBacktraceAsError( log4cpp::Category &   logger,
                                const ::std::string & linePrefix );

void logCaughtBacktraceAsError( log4cpp::Category & logger,
                                const char *        linePrefix );
                                          
void logCaughtBacktraceAsErrorIfPossible( );

void logCaughtBacktraceAsErrorIfPossible( const ::std::string & linePrefix );
                                          
void logCaughtBacktraceAsErrorIfPossible( const char * linePrefix );


// old (deprecated) names for some of these


inline void
logAndRethrowCaughtExceptionAsUserException(
    log4cpp::Category &                    logger,
    const log4cpp::Priority::PriorityLevel priority )
{
    logCaughtAndRethrowAsUser( logger, priority );
}


inline void
logAndRethrowCaughtExceptionAsUserException(
    const log4cpp::Priority::PriorityLevel priority )
{
    logCaughtAndRethrowAsUser( priority );
}


}  // namespace carma::util
}  // namespace carma


#endif
