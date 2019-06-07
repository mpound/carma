#ifndef CARMA_UTIL_POSIX_ERRORS_H
#define CARMA_UTIL_POSIX_ERRORS_H

#include <string>


namespace carma {
namespace util {


::std::string getPosixFailureMessage( const char * prefix,
                                      int          errorCode );

::std::string getPosixFailureMessage( const ::std::string & prefix,
                                      int                   errorCode );


void throwPosixError( int errorCode );
void throwPosixError( int errorCode, const char * message );
void throwPosixError( int errorCode, const ::std::string & message );


void logPosixError( int errorCode );
void logPosixError( int errorCode, const char * message );
void logPosixError( int errorCode, const ::std::string & message );


void failIfPosixError( int errorCode );
void failIfPosixError( int errorCode, const char * message );
void failIfPosixError( int errorCode, const ::std::string & message );


void logIfPosixError( int errorCode );
void logIfPosixError( int errorCode, const char * message );
void logIfPosixError( int errorCode, const ::std::string & message );


}  // namespace carma::util
}  // namespace carma


inline void
carma::util::failIfPosixError( const int errorCode )
{
    if ( errorCode != 0 )
        throwPosixError( errorCode );
}


inline void
carma::util::failIfPosixError( const int          errorCode,
                               const char * const message )
{
    if ( errorCode != 0 )
        throwPosixError( errorCode, message );
}


inline void
carma::util::failIfPosixError( const int             errorCode,
                               const ::std::string & message )
{
    if ( errorCode != 0 )
        throwPosixError( errorCode, message );
}


inline void
carma::util::logIfPosixError( const int errorCode )
{
    if ( errorCode != 0 )
        logPosixError( errorCode );
}


inline void
carma::util::logIfPosixError( const int          errorCode,
                              const char * const message )
{
    if ( errorCode != 0 )
        logPosixError( errorCode, message );
}


inline void
carma::util::logIfPosixError( const int             errorCode,
                              const ::std::string & message )
{
    if ( errorCode != 0 )
        logPosixError( errorCode, message );
}


#endif
