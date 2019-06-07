#include "carma/util/SystemException.h"

#include <sstream>


using namespace ::std;


namespace carma {
namespace util {


SystemException::SystemException( ) :
BaseException(),
sysMesg_( 0 ),
weAllocatedSysMesg_( false )
{
    // default protected c'tor
}


SystemException::SystemException( const char * mesg,
                                  const char * sysMesg, 
                                  const char * fileName, 
                                  const int    lineNum ) : 
BaseException( mesg, fileName, lineNum ),
sysMesg_( sysMesg ),
weAllocatedSysMesg_( false )
{
    // constructor that allows for tracing exception and putting in
    // a customized message
}


SystemException::SystemException( ostringstream & message, 
                                  const char *    sysMessage,
                                  const char *    fileName,
                                  const int       lineNum ) :
BaseException( message.str(), fileName, lineNum ),
sysMesg_( sysMessage ),
weAllocatedSysMesg_( false )
{
    // constructor that allows for tracing exception and putting in
    // a customized message
}


SystemException::SystemException( ostringstream &     message,
                                  const ostringstream sysMessage,
                                  const char *        fileName,
                                  const int           lineNum ) :
BaseException( message.str( ), fileName, lineNum ),
sysMesg_( 0 ),
weAllocatedSysMesg_( false )
{
    // constructor that allows for tracing exception and putting in
    // a customized message

    setSysMesg( sysMessage.str() );
}


SystemException::SystemException( string &     istring,
                                  const string sysMessage,
                                  const char * fileName,
                                  const int    lineNum ) :
BaseException( istring, fileName, lineNum ),
sysMesg_( 0 ),
weAllocatedSysMesg_( false )
{
    // constructor that allows for tracing exception and putting in
    // a customized message

    setSysMesg( sysMessage );
}


SystemException::~SystemException( ) throw( )
try {
    if ( weAllocatedSysMesg_ ) {
        delete [] sysMesg_;
        sysMesg_ = 0;
    }
} catch ( ... ) {
    // Just stifle any exceptions
}


void
SystemException::clearSysMesg( )
{
    const char * oldSysMesg = 0;
    bool         weAllocatedOldSysMesg = false;
    
    ::std::swap( sysMesg_, oldSysMesg );
    ::std::swap( weAllocatedSysMesg_, weAllocatedOldSysMesg );
    
    if ( weAllocatedOldSysMesg )
        delete [] oldSysMesg;
}


void
SystemException::setSysMesg( const string & sysMesg )
{
    char * const newSysMesg = new char[ sysMesg.size() + 1 ];

    strcpy( newSysMesg, sysMesg.c_str() );
    
    clearSysMesg();
    
    sysMesg_ = newSysMesg;
    weAllocatedSysMesg_ = true;
}


const char *
SystemException::getSysMessage( ) const {
    if ( sysMesg_ == 0 )
        return "";
    else
        return sysMesg_;
}


string
SystemException::getLogString( ) const {
    ostringstream oss;
    
    oss << "File: " << getSourceFile( ) 
        << " Line: " << getLineNumber( ) 
        << "\nMessage: " 
        << getMessage( ) 
        << " System error : "
        << getSysMessage( );
        
    return oss.str( );
}


}  // namespace carma::util
}  // namespace carma
