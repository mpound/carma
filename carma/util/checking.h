#ifndef CARMA_UTIL_CHECKING_H
#define CARMA_UTIL_CHECKING_H


//! @file
//! Header file for the CARMA checked build diagnostic macros.


//! @def CARMA_CHECK(assertion)
//! @brief Diagnostic macro for checking an assertion in checked builds.
//!
//! Checks that @a assertion evaluates to true in checked builds and which
//! does not evaluate anything (i.e. it is a NOOP) in non-checked builds.

#ifndef CARMA_CHECKED_BUILD
    #define CARMA_CHECKED_BUILD 0
#endif


#if CARMA_CHECKED_BUILD


namespace carma {
namespace util {

void CheckingMessage( const char * messagePrefix,
                      const char * message,
                      const char * messageSuffix,
                      const char * fileName,
                      const long   lineNo );
    
}  // namespace util
}  // namespace carma


#define CARMA_CHECK(assertion)                                              \
    do {                                                                    \
        if ( !(assertion) ) {                                               \
            carma::util::CheckingMessage(                                   \
                 "Assertion failed CARMA_CHECK( ",                          \
                 #assertion,                                                \
                 " )",                                                      \
                __FILE__,                                                   \
                __LINE__);                                                  \
        }                                                                   \
    } while ( false )


#else


#define CARMA_CHECK(assertion)


#endif


#endif
