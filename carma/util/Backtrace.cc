#include "carma/util/Backtrace.h"

#include <sstream>

#include <cxxabi.h>

#include <execinfo.h>

#include "carma/util/AutoFreedMallocPtr.h"
#include "carma/util/demangle.h"


using namespace ::std;
using namespace carma;
using namespace carma::util;


namespace {


struct StringRange {
    StringRange( string::size_type inPos,
                 string::size_type inN );

    const string::size_type pos;
    const string::size_type n;
};


StringRange::StringRange( const string::size_type inPos,
                          const string::size_type inN ) :
pos( inPos ),
n( inN )
{
}


StringRange
findEntryPointNameRange( const string & line )
{
    do {
        string::size_type endPos = line.rfind( ") [" );
        
        if ( endPos == string::npos )
            break;
        
        endPos = line.rfind( "+0x", endPos );

        if ( endPos == string::npos )
            break;
            
        string::size_type beginPos = line.rfind( '(', endPos );
        
        if ( beginPos == string::npos )
            break;
            
        beginPos += 1;
        
        return StringRange( beginPos, (endPos - beginPos) );
    } while ( false );
        
    return StringRange( 0, 0 );
}


}  // anonymous namespace


Backtrace::Backtrace( ) :
state_( NOT_CAPTURED_STATE ),
numeric_(),
symbolic_()
{
}


Backtrace::~Backtrace( )
try {
} catch ( ... ) {
    // Just stifle any exception
    
    return;
}


void
Backtrace::demangleSymbolLineInPlace( string & line )
{
    const StringRange entryPointNameRange = findEntryPointNameRange( line );
        
    if ( entryPointNameRange.n < 1 )
        return;
        
    const string entryPointName( line,
                                 entryPointNameRange.pos,
                                 entryPointNameRange.n );
    
    if ( entryPointName.empty() )
        return;
        
    line.replace( entryPointNameRange.pos,
                  entryPointNameRange.n,
                  demangleEntryPointName( entryPointName ) );
}


string
Backtrace::demangleSymbolLine( const string & line )
{
    string result = line;
    
    demangleSymbolLineInPlace( result );
    
    return result;
}


string
Backtrace::demangleSymbolLine( const char * const line )
{
    string result = line;
    
    demangleSymbolLineInPlace( result );
    
    return result;
}


bool
Backtrace::captureNoThrow( const size_t maxDepth,
                           const bool   convertToSymbols )
try {
    state_ = INVALID_CAPTURE_STATE;
    
    {
        vector< string > temp;
        
        symbolic_.swap( temp );
    }
    
    vector< void * > btAddresses;
    
    btAddresses.resize( 200, 0 );
    
    const int btCount =
        ::backtrace( &(btAddresses[ 0 ]), btAddresses.size() );
        
    btAddresses.resize( btCount, 0 );
        
    AutoFreedMallocPtr< char * >
        mangledLinesBlock( ::backtrace_symbols( &(btAddresses[ 0 ]),
                                                btCount ) );

    char * const * const mangledLines =
        mangledLinesBlock.get();

    if ( mangledLines == 0 ) {
        // There was insufficient memory for backtrace_symbols to do its thing
        state_ = INVALID_CAPTURE_STATE;
        
        return false;
    }

    for ( int i = 0; i < btCount; ++i ) {
        const char * const mangledLine = mangledLines[ i ];
        
        if ( mangledLine == 0 )
            symbolic_.push_back( "< unknown >" );
        else {
            try {
                symbolic_.push_back( demangleSymbolLine( mangledLine ) );
            } catch ( ... ) {
                symbolic_.push_back( mangledLine );
            }
        }
    }

    state_ = DEMANGLED_STATE;
    complete_ = true;

    return true;
} catch ( ... ) {
    state_ = INVALID_CAPTURE_STATE;
    
    return false;
}


bool
Backtrace::captureNoThrow( const size_t maxDepth )
{
    return captureNoThrow( maxDepth, false );
}


bool
Backtrace::captureNoThrow( const bool convertToSymbols )
{
    return captureNoThrow( 128, convertToSymbols );
}


bool
Backtrace::captureNoThrow( )
{
    return captureNoThrow( false );
}


string
Backtrace::formattedAsString( const char * const linePrefix,
                              const char * const lineSuffix ) const
{
    string result;
    
    if ( state_ == NOT_CAPTURED_STATE ) {
        if ( linePrefix != 0 )
            result += linePrefix;

        result += "<<<NOT_CAPTURED>>>";

        if ( lineSuffix != 0 )
            result += lineSuffix;
    } else if ( state_ == INVALID_CAPTURE_STATE ) {
        if ( linePrefix != 0 )
            result += linePrefix;

        result += "<<<INVALID_CAPTURE>>>";

        if ( lineSuffix != 0 )
            result += lineSuffix;
    } else {
        ostringstream oss;
        
        {
            vector< string >::const_iterator i = symbolic_.begin();
            const vector< string >::const_iterator iEnd = symbolic_.end();
            
            for ( ::size_t j = 0; i != iEnd; ++i, ++j ) {
                if ( linePrefix != 0 )
                    oss << linePrefix;
                    
                oss << "#" << j << ": " << *i;

                if ( lineSuffix != 0 )
                    oss << lineSuffix;
            }
        }
        
        result = oss.str();
    }
    
    return result;
}


string
Backtrace::formattedAsString( ) const
{
    return formattedAsString( "    ", "\n" );
}


string
Backtrace::formattedAsString( const string & linePrefix,
                              const string & lineSuffix ) const
{
    return formattedAsString( linePrefix.c_str(), lineSuffix.c_str() );
}


string
Backtrace::captureAsString( )
{
    Backtrace bt;
    
    bt.captureNoThrow();
    
    return bt.formattedAsString();
}


string
Backtrace::captureAsString( const char * const linePrefix,
                            const char * const lineSuffix )
{
    Backtrace bt;
    
    bt.captureNoThrow();
    
    return bt.formattedAsString( linePrefix, lineSuffix );
}


string
Backtrace::captureAsString( const string & linePrefix,
                            const string & lineSuffix )
{
    Backtrace bt;
    
    bt.captureNoThrow();
    
    return bt.formattedAsString( linePrefix, lineSuffix );

}


bool
Backtrace::captured() const
{
    return (state_ != NOT_CAPTURED_STATE);
}


bool
Backtrace::capturedAndValid() const
{
    return ((state_ != NOT_CAPTURED_STATE) &&
            (state_ != INVALID_CAPTURE_STATE));
}
