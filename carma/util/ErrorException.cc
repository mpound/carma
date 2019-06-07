/**
 * 
 * Implementation of an exception class for errors
 *
 * @author: Steve Scott
 *
 * $Id: ErrorException.cc,v 1.15 2006/06/20 23:51:16 colby Exp $ 
 *                  
 * $CarmaCopyright$
 *
 */

#include "carma/util/ErrorException.h"

#include <iomanip>
#include <cstring>

#include "carma/util/Logger.h"

using namespace ::std;
using namespace ::log4cpp;
using namespace carma;
using namespace carma::util;


namespace {


const char *
allocateCopy( const char * const s )
{
    if ( (s == 0) || (s[ 0 ] == '\0') )
        return 0;
    
    const char * const sCopy = ::strdup( s );
    
    if ( sCopy == 0 ) {
        throw runtime_error( "::strdup failed" );
        // throw BaseException( "::strdup failed", __FILE__, __LINE__ );
    }
    
    return sCopy;
}


void
deallocateCopy( const char * const sCopy )
{
    if ( sCopy != 0 )
        ::free( const_cast< char * >( sCopy ) );
}


}  // namespace < anonymous >


ErrorException::ErrorException( const string &     msg,
                                const char * const filename,
                                const int          lineNo ) :
BaseException( msg, string( filename ), lineNo ),
errorMsg_( 0 )
{
    buildErrorMsg( filename );
}


ErrorException::ErrorException( const ostringstream & oss,
                                const char * const    filename,
                                const int             lineNo ) :
BaseException( oss.str(), string( filename ), lineNo ),
errorMsg_( 0 )
{   
    buildErrorMsg( filename );
}


ErrorException::ErrorException( ) :
BaseException(),
errorMsg_( 0 )
{
}


ErrorException::ErrorException( const ErrorException & rhs ) :
BaseException( rhs ),
errorMsg_( allocateCopy( rhs.errorMsg_ ) )  // full err messsage
{
}


ErrorException::~ErrorException() throw()
try {
    deallocateCopy( errorMsg_ );
} catch ( ... ) {
    // Just stifle the exception
    
    return;
}


void
ErrorException::buildErrorMsg( const char * const filename )
{
    ostringstream oss;

    oss << "File: "  << filename << ", "
        << "Line: " << getLineNumber() << ", "
        << "Message: " << getMessage();

    if ( errorMsg_ != 0 ) {
        deallocateCopy( errorMsg_ );
        
        errorMsg_ = 0;
    }
    
    errorMsg_ = allocateCopy( oss.str().c_str() );
}


string
ErrorException::getErrorMessage() const
{
    if ( errorMsg_ == 0 )
        return string();
    else
        return errorMsg_;
}


const char *
ErrorException::what( ) const throw()
{
    if ( errorMsg_ == 0 )
        return "";
    else
        return errorMsg_;
}


string
ErrorException::getLogString( ) const
{
    if ( errorMsg_ == 0 )
        return string();
    else
        return errorMsg_;
}


void
ErrorException::report() const
{
    cerr << what() << endl;
}


void
ErrorException::log( const Priority::PriorityLevel priority ) const
{
    logException( priority );
}


ostream &
operator<<( ostream & os, const exception & error )
{
    os << error.what();
    
    return os;
}
