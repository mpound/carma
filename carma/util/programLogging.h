#ifndef CARMA_UTIL_PROGRAM_LOGGING_H
#define CARMA_UTIL_PROGRAM_LOGGING_H

//! @file
//! @brief This is the interface file for extra APIs for program logging
//!


#include <string>

#define CARMALOGINFO(statement) \
{\
    std::ostringstream _macroOs; \
    _macroOs << statement; \
    carma::util::programLogInfoIfPossible(_macroOs.str());	\
}


namespace carma{
namespace util {


void programLogCritical( const ::std::string & msg );
void programLogCritical( const char * msg );

void programLogError( const ::std::string & msg );
void programLogError( const char * msg );

void programLogWarn( const ::std::string & msg );
void programLogWarn( const char * msg );

void programLogNotice( const ::std::string & msg );
void programLogNotice( const char * msg );

void programLogInfo( const ::std::string & msg );
void programLogInfo( const char * msg );

void programLogDebug( const ::std::string & msg );
void programLogDebug( const char * msg );


bool programLoggingIsPossible( );

void programLogCriticalIfPossible( const ::std::string & msg );
void programLogCriticalIfPossible( const char * msg );

void programLogErrorIfPossible( const ::std::string & msg );
void programLogErrorIfPossible( const char * msg );

void programLogWarnIfPossible( const ::std::string & msg );
void programLogWarnIfPossible( const char * msg );

void programLogNoticeIfPossible( const ::std::string & msg );
void programLogNoticeIfPossible( const char * msg );

void programLogInfoIfPossible( const ::std::string & msg );
void programLogInfoIfPossible( const char * msg );

void programLogDebugIfPossible( const ::std::string & msg );
void programLogDebugIfPossible( const char * msg );


}  // namespace carma::util
}  // namespace carma


#endif
