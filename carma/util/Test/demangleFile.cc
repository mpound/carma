//
// @version $Revision: 1.1 $
//
// @usage use it
//
// @description
//  Will demangle all C++ mangled words in a text file.
//
// @key files @mandatory string
//            List of input files to process.
//
// @logger TEST_FACILITY carma.test.util.demangleFile
//

#include <istream>
#include <ostream>
#include <fstream>
#include <iostream>

#include "carma/util/demangle.h"
#include "carma/util/ErrorException.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/StringUtils.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;


namespace {


string
cleanUpStdNamespace( const string & s )
{
    string result = s;
    
    result = StringUtils::replace( result, "::std::allocator<", "allocator<" );
    result = StringUtils::replace( result,   "std::allocator<", "allocator<" );

    result = StringUtils::replace( result, "::std::auto_ptr<", "auto_ptr<" );
    result = StringUtils::replace( result,   "std::auto_ptr<", "auto_ptr<" );

    result = StringUtils::replace( result, "::std::pair<", "pair<" );
    result = StringUtils::replace( result,   "std::pair<", "pair<" );

    result = StringUtils::replace( result, "::std::string", "string" );
    result = StringUtils::replace( result,   "std::string", "string" );

    result = StringUtils::replace( result, "::std::vector<", "vector<" );
    result = StringUtils::replace( result,   "std::vector<", "vector<" );

    return result;
}


string
processToken( const string & t,
              const bool     cleanUp )
{
    if ( (t.size() > 2) && (t.at( 0 ) == '_') && (t.at( 2 ) == 'Z') ) {
        const string demangled = demangleEntryPointName( t );
        
        if ( demangled != t ) {
            if ( cleanUp )
                return cleanUpStdNamespace( demangled );
            else
                return demangled;
        }
    }
    
    if ( (t.size() > 1) && (t.at( 0 ) == 'Z')) {
        const string prefixed = "_" + t;
        const string demangledPrefixed = demangleEntryPointName( prefixed );
        
        if ( demangledPrefixed != prefixed ) {
            if ( cleanUp )
                return cleanUpStdNamespace( demangledPrefixed );
            else
                return demangledPrefixed;
        }
    }

    return t;
}


string
processLine( const string & line,
             const bool     cleanUp )
{
    // cout << "-- \"" << line << "\"\n";
    
    string result;
    
    const string delimiterSet = " \t";
    
    // Skip delimiters at beginning.
    string::size_type lastPos = line.find_first_not_of( delimiterSet, 0 );
    
    if ( lastPos != string::npos ) {
        const string beginDelimsBlock = line.substr( 0, lastPos );
        
        result += beginDelimsBlock;
        // cout << "  --> beginDelimsBlock \"" << beginDelimsBlock << "\"\n";
    }
    
    // Find real first delimiter.
    string::size_type pos = line.find_first_of( delimiterSet, lastPos );
    
    while ( (pos != string::npos) || (lastPos != string::npos) ) {
        // Found a token
        {
            string token;
            
            if ( pos == string::npos )
                token = line.substr( lastPos );
            else
                token = line.substr( lastPos, pos - lastPos );

            const string processedToken =
                processToken( token, cleanUp );
            
            result += processedToken;
            // cout << "  --> processedToken \"" << processedToken << "\"\n";
        }
        
        // Find next token start position
        // Skip null tokens
        {
            lastPos = line.find_first_not_of( delimiterSet, pos );

            if ( lastPos != string::npos ) {
                const string delimsBlock = line.substr( pos, (lastPos - pos) );
                
                result += delimsBlock;
                // cout << "  --> delimsBlock \"" << delimsBlock << "\"\n";
            } else if ( pos != string::npos ) {
                const string endDelimsBlock = line.substr( pos );

                result += endDelimsBlock;
                // cout << "  --> endDelimsBlock \"" << endDelimsBlock << "\"\n";
            }
        }
        
        // Find next token end delimiter
        pos = line.find_first_of( delimiterSet, lastPos );
    }
    
    return result;
}


void
processStream( istream &  is,
               ostream &  os,
               const bool cleanUp )
{
    programLogInfoIfPossible( "Processing" );

    vector< char > lineBuf;

    lineBuf.resize( 1024 );

    while ( is.good() ) {
        is.getline( &(lineBuf.at( 0 )), lineBuf.size() );
    
        os << processLine( string( &(lineBuf.at( 0 )) ), cleanUp ) << "\n";
    }
}


}  // namespace < anonymous >


int
Program::main( )
{
    const string fileList = getStringParameter( "files" );

    {
        const vector< string > files = StringUtils::tokenize( fileList );
        
        vector< string >::const_iterator i = files.begin();
        const vector< string >::const_iterator iEnd = files.end();
        
        for ( ; i != iEnd; ++i ) {
            const ScopedLogNdc ndc( "file " + *i );
            
            ifstream ifs( i->c_str() );
            
            if ( ifs.good() == false )
                throw CARMA_ERROR( "Problem constructing ifstream" );
                
            processStream( ifs, cout, true );
        }
    }
    
    return 0;
}
