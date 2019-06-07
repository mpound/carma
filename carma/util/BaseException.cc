#include "carma/util/BaseException.h"

#include <sstream>

#include "carma/util/Program.h"
#include "carma/util/Backtrace.h"
#include "carma/util/Logger.h"


using namespace ::std;
using namespace ::log4cpp;
using namespace carma;
using namespace carma::util;


BaseException::BaseException( ) :
mesg_( 0 ),
weAllocatedMesg_( false ),
sourceFile_( 0 ),
weAllocatedSourceFile_( false ),
lineNo_( 0 ),
backtrace_()
{
    // default protected c'tor

    backtrace_.captureNoThrow();
}           


BaseException::BaseException( const BaseException & rhs ) :
mesg_( 0 ),
weAllocatedMesg_( false ),
sourceFile_( 0 ),
weAllocatedSourceFile_( false ),
lineNo_( rhs.lineNo_ ),
backtrace_( rhs.backtrace_ )
{
    if ( rhs.weAllocatedMesg_ )
        setMessageToCopy( rhs.mesg_ );
    else
        mesg_ = rhs.mesg_;

    if ( rhs.weAllocatedSourceFile_ )
        setSourceFileToCopy( rhs.sourceFile_ );
    else
        sourceFile_ = rhs.sourceFile_;
}


BaseException::BaseException( const char * const mesg, 
                              const char * const sourceFile,
                              const int          lineNo ) :
mesg_( mesg ),
weAllocatedMesg_( false ),
sourceFile_( sourceFile ),
weAllocatedSourceFile_( false ),
lineNo_( lineNo ),
backtrace_()
{
    // constructor that allows for tracing exception and putting in
    // a customized message

    backtrace_.captureNoThrow();
}


BaseException::BaseException( const string &     mesg,
                              const char * const sourceFile,
                              const int          lineNo ) :
mesg_( 0 ),
weAllocatedMesg_( false ),
sourceFile_( sourceFile ),
weAllocatedSourceFile_( false ),
lineNo_( lineNo ),
backtrace_()
{
    // constructor that allows for tracing exception and putting in
    // a customized message
    
    backtrace_.captureNoThrow();

    if ( mesg.empty() == false )
        setMessageToCopy( mesg );
}


BaseException::BaseException( const char * const mesg,
                              const string &     sourceFile,
                              const int          lineNo ) :
mesg_( mesg ),
weAllocatedMesg_( false ),
sourceFile_( 0 ),
weAllocatedSourceFile_( false ),
lineNo_( lineNo ),
backtrace_()
{
    // constructor that allows for tracing exception and putting in
    // a customized message
    
    backtrace_.captureNoThrow();

    if ( sourceFile.empty() == false )
        setSourceFileToCopy( sourceFile );
}


BaseException::BaseException( const string & mesg,
                              const string & sourceFile,
                              const int      lineNo ) :
mesg_( 0 ),
weAllocatedMesg_( false ),
sourceFile_( 0 ),
weAllocatedSourceFile_( false ),
lineNo_( lineNo ),
backtrace_()
{
    // constructor that allows for tracing exception and putting in
    // a customized message
    
    backtrace_.captureNoThrow();

    if ( mesg.empty() == false )
        setMessageToCopy( mesg );

    if ( sourceFile.empty() == false )
        setSourceFileToCopy( sourceFile );
}


BaseException::BaseException( const ostringstream & oss,
                              const char * const    sourceFile,
                              const int             lineNo ) :
mesg_( 0 ),
weAllocatedMesg_( false ),
sourceFile_( sourceFile ),
weAllocatedSourceFile_( false ),
lineNo_( lineNo ),
backtrace_()
{
    // constructor that allows for tracing exception and putting in
    // a customized message

    backtrace_.captureNoThrow();

    const string mesg = oss.str();

    if ( mesg.empty() == false )
        setMessageToCopy( mesg );
}


BaseException::BaseException( const ostringstream & oss,
                              const string &        sourceFile,
                              const int             lineNo ) :
mesg_( 0 ),
weAllocatedMesg_( false ),
sourceFile_( 0 ),
weAllocatedSourceFile_( false ),
lineNo_( lineNo ),
backtrace_()
{
    // constructor that allows for tracing exception and putting in
    // a customized message

    backtrace_.captureNoThrow();

    const string mesg = oss.str();

    if ( mesg.empty() == false )
        setMessageToCopy( mesg );

    if ( sourceFile.empty() == false )
        setSourceFileToCopy( sourceFile );
}


BaseException::~BaseException( ) throw()
try {
    if ( weAllocatedMesg_ ) {
        if ( mesg_ != 0 ) {
            ::free( const_cast< char * >( mesg_ ) );

            mesg_ = 0;
        }
        
        weAllocatedMesg_ = false;
    }

    if ( weAllocatedSourceFile_ ) {
        if ( sourceFile_ != 0 ) {
            ::free( const_cast< char * >( sourceFile_ ) );

            sourceFile_ = 0;
        }
        
        weAllocatedSourceFile_ = false;
    }
} catch ( ... ) {
    // Just stifle any exception
    
    return;
}


const char *
BaseException::what( ) const throw()
{
    return getMessage();
}


const char *
BaseException::getMessage( ) const {
    if ( mesg_ == 0 )
        return "";
    else
        return mesg_;
}


void
BaseException::clearMesg( )
{
    const char * oldMesg = 0;
    bool weAllocatedOldMesg = false;
    
    ::std::swap( mesg_, oldMesg );
    ::std::swap( weAllocatedMesg_, weAllocatedOldMesg );
    
    if ( weAllocatedOldMesg && (oldMesg != 0) )
        ::free( const_cast< char * >( oldMesg ) );
}


void
BaseException::setMessageDirectly( const char * const mesg )
{
    clearMesg();
    
    mesg_ = mesg;
    weAllocatedMesg_ = false;
}


void
BaseException::setMessageToCopy( const char * const mesg )
{
    if ( (mesg == 0) || (mesg[ 0 ] == '\0') )
        clearMesg();
    else {
        char * const mesgCopy =
            static_cast< char * >( ::strdup( mesg ) );
    
        if ( mesgCopy == 0 )
            throw BaseException( "strdup failed", __FILE__, __LINE__ );
    
        clearMesg();

        mesg_ = mesgCopy;
        weAllocatedMesg_ = true;
    }
}


void
BaseException::setMessageToCopy( const string & mesg )
{
    if ( mesg.empty() )
        clearMesg();
    else
        setMessageToCopy( mesg.c_str() );
}


void
BaseException::setMessageToCopy( const ostringstream & oss )
{
    setMessageToCopy( oss.str() );
}


const char *
BaseException::getSourceFile( ) const {
    if ( sourceFile_ == 0 )
        return "";
    else
        return sourceFile_;
}


void
BaseException::clearSourceFile( )
{
    const char * oldSourceFile = 0;
    bool weAllocatedOldSourceFile = false;
    
    ::std::swap( sourceFile_, oldSourceFile );
    ::std::swap( weAllocatedSourceFile_, weAllocatedOldSourceFile );
    
    if ( weAllocatedOldSourceFile && (oldSourceFile != 0) )
        ::free( const_cast< char * >( oldSourceFile ) );
}


void
BaseException::setSourceFileDirectly( const char * const sourceFile )
{
    clearSourceFile();
    
    sourceFile_ = sourceFile;
    weAllocatedSourceFile_ = false;
}


void
BaseException::setSourceFileToCopy( const char * const sourceFile )
{
    if ( (sourceFile == 0) || (sourceFile[ 0 ] == '\0') )
        clearSourceFile();
    else {
        char * const sourceFileCopy =
            static_cast< char * >( ::strdup( sourceFile ) );
    
        if ( sourceFileCopy == 0 )
            throw BaseException( "strdup failed", __FILE__, __LINE__ );
    
        clearSourceFile();

        sourceFile_ = sourceFileCopy;
        weAllocatedSourceFile_ = true;
    }
}


void
BaseException::setSourceFileToCopy( const string & sourceFile )
{
    if ( sourceFile.empty() )
        clearSourceFile();
    else
        setSourceFileToCopy( sourceFile.c_str() );
}


int
BaseException::getLineNumber( ) const {
    return lineNo_;
}


void
BaseException::setLineNumber( const int lineNo )
{
    lineNo_ = lineNo; 
}


string
BaseException::getLogString( ) const
{
    ostringstream oss;
    
    oss << "File: " << getSourceFile() << " "
        << "Line: " << getLineNumber() << " "
        << "Message: " << getMessage();

    return oss.str();
}


void
BaseException::logException( const Priority::PriorityLevel priority ) const
{
    Category & logger = Program::getLogger();
    
    logger << priority << getLogString( );
}


Backtrace
BaseException::getBacktrace( ) const
{
    return backtrace_;
}
