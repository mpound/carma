#include "carma/util/posixErrors.h"

#include <sstream>
#include <cerrno>

#include "carma/util/Backtrace.h"
#include "carma/util/ErrorException.h"
#include "carma/util/programLogging.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;


string
carma::util::getPosixFailureMessage( const char * const prefix,
                                     const int          errorCode )
{
    ostringstream oss;
    
    if ( prefix == 0 )
        oss << "POSIX error";
    else
        oss << prefix;
        
    oss << " (err=" << errorCode << ", ";
        
    errno = 0;
    
    const char * const sysErrText = ::strerror( errorCode );
    
    const int savedErrNo = errno;
    
    if ( (savedErrNo != 0) || (sysErrText == 0) )
        oss << "< INVALID >";
    else
        oss << sysErrText;
        
    oss << ")";
        
    return oss.str();
}


void
carma::util::throwPosixError( const int          errorCode,
                              const char * const prefix )
{
    throw CARMA_ERROR( getPosixFailureMessage( prefix, errorCode ) );
}


namespace {

__thread bool gLogReentrancyGuard = false;

}  // namespace < anonymous >


void
carma::util::logPosixError( const int          errorCode,
                            const char * const prefix )
{
    if ( gLogReentrancyGuard == false ) {
        gLogReentrancyGuard = true;
        
        try {
            const string message = getPosixFailureMessage( prefix, errorCode );
            
            if ( true )
                programLogErrorIfPossible( message );
            else {
                const string text = message + ". Backtrace is:\n" +
                                    Backtrace::captureAsString( "  ", "\n" );
            }
        } catch ( ... ) {
            gLogReentrancyGuard = false;
            
            throw;
        }
        
        gLogReentrancyGuard = false;
    }
}


void
carma::util::throwPosixError( const int errorCode )
{
    throwPosixError( errorCode, 0 );
}


void
carma::util::throwPosixError( const int      errorCode,
                              const string & prefix )
{
    throwPosixError( errorCode, prefix.c_str() );
}


void
carma::util::logPosixError( const int errorCode )
{
    logPosixError( errorCode, 0 );
}


void
carma::util::logPosixError( const int      errorCode,
                            const string & prefix )
{
    logPosixError( errorCode, prefix.c_str() );
}
