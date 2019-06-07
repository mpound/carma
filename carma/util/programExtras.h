#ifndef CARMA_UTIL_PROGRAM_EXTRAS_H
#define CARMA_UTIL_PROGRAM_EXTRAS_H

//! @file
//! @brief This is the temporary interface file for extra globals APIs for the
//!        ProgramBase and Program classes.
//!


namespace log4cpp {

class Category;

}  // namespace log4cpp


namespace carma {
namespace util {

class Trace;


log4cpp::Category & getProgramLogger( );

log4cpp::Category * getProgramLoggerIfAvailable( );

Trace & getProgramTraceObject( );

Trace * getProgramTraceObjectIfAvailable( );


}  // namespace carma::util
}  // namespace carma

#endif
