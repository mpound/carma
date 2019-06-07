//
// @version $Revision: 1.3 $
//
// @usage use it
//
// @description
//  Test program for testing the STL container utilities.
//
// @noKeys
//
// @logger TEST_FACILITY carma.test.util.tStlContainerUtils
//

#include <list>
#include <map>
#include <set>
#include <stdexcept>
#include <string>
#include <typeinfo>
#include <vector>
#include <sstream>

#include "carma/util/demangle.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/stlContainerUtils.h"
#include "carma/util/ScopedLogNdc.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;


namespace {


void
handleFailure( const string & msg )
{
    programLogErrorIfPossible( msg );

    throw runtime_error( msg );
}


template < typename V >
void
addValueToContainer( multiset< V > & c,
                     const V &       v )
{
    c.insert( v );
}


template < typename V >
void
addValueToContainer( set< V > & c,
                     const V &  v )
{
    c.insert( v );
}


template < typename V >
void
addValueToContainer( multimap< V, V > &                            c,
                     const typename multimap< V, V >::value_type & v )
{
    c.insert( v );
}


template < typename V >
void
addValueToContainer( map< V, V > &                            c,
                     const typename map< V, V >::value_type & v )
{
    c.insert( v );
}


template < typename V >
void
addValueToContainer( vector< V > & c,
                     const V &     v )
{
    c.push_back( v );
}


template < typename V >
void
addValueToContainer( list< V > & c,
                     const V &   v )
{
    c.push_back( v );
}


template < typename C >
void
testContainerType( const typename C::value_type & v0,
                   const typename C::value_type & v1 )
{
    // const ScopedLogNdc ndc( "testContainerType< " +
    //                         demangleTypeName( typeid( C ) ) + " >" );
    //
    // programLogInfoIfPossible( "Testing" );

    C c;
    
    if ( c.empty() != true )
        handleFailure( "bad empty() result for an empty container" );

    if ( c.size() != 0 )
        handleFailure( "bad size() result for an empty container" );

    if ( sizeIsExactlyOne( c ) )
        handleFailure( "sizeIsExactlyOne returned true for an empty container" );
        
    if ( sizeIsOneOrLess( c ) != true )
        handleFailure( "sizeIsOneOrLess returned false for an empty container" );
        
    addValueToContainer( c, v0 );

    if ( c.empty() )
        handleFailure( "bad empty() result for a size 1 container" );

    if ( c.size() != 1 )
        handleFailure( "bad size() result for a size 1 container" );

    if ( sizeIsExactlyOne( c ) != true )
        handleFailure( "sizeIsExactlyOne returned false for a size 1 container" );
        
    if ( sizeIsOneOrLess( c ) != true )
        handleFailure( "sizeIsOneOrLess returned false for a size 1 container" );

    addValueToContainer( c, v1 );

    if ( c.empty() )
        handleFailure( "bad empty() result for a size 2 container" );

    if ( c.size() != 2 )
        handleFailure( "bad size() result for a size 2 container" );

    if ( sizeIsExactlyOne( c ) )
        handleFailure( "sizeIsExactlyOne returned true for a size 2 container" );
        
    if ( sizeIsOneOrLess( c ) )
        handleFailure( "sizeIsOneOrLess returned true for a size 2 container" );

}


template < typename V >
void
testValueType( )
{
    const ScopedLogNdc ndc( "testValueType< " +
                            demangleTypeName( typeid( V ) ) + " >" );

    programLogInfoIfPossible( "Testing" );

    const V v0 = V( 0 );
    const V v1 = V( 128 );

    testContainerType< vector< V > >( v0, v1 );
    testContainerType< list< V > >( v0, v1 );
    testContainerType< set< V > >( v0, v1 );
    testContainerType< multiset< V > >( v0, v1 );

    testContainerType< map< V, V > >( make_pair( v0, v1 ),
                                      make_pair( v1, v0 ) );
                                      
    testContainerType< multimap< V, V > >( make_pair( v0, v1 ),
                                           make_pair( v1, v0 ) );
}


void
testKeyRetrieval() 
{
    programLogInfoIfPossible( "Testing key retrieval" );
    const int keyLen = 10;

    // test retrieval of std::map keys
    map< string, float > mymap;
    set< string > inputKeys;
    for( float i = 0; i < keyLen ; ++i ) {
	ostringstream os;
	os << "This is key #" << i;
	const string foo = os.str();
	inputKeys.insert( foo );
	mymap.insert( make_pair( foo, i ) );
    }

    set< string > outputKeys = keys( mymap );
    set< string >::iterator ip = inputKeys.begin();
    set< string >::iterator op = outputKeys.begin();
    while ( ip != inputKeys.end() ) {
	if ( *ip!= *op) {
	    ostringstream os;
	    os  << "Input and output map keys don't match: "
	        << *ip<< " - " << *op
		;
             handleFailure( os.str() );
	}
       ip++;
       op++;
    }

    // test retrieval of std::multimap keys
    multimap< string, int > mymultimap;
    ip = inputKeys.begin();
    // create multiple (N=keyLen) values for each key
    for( int i = 0; i < keyLen ; ++i ) {
       const string foo = *ip++;
       for( int j = 0; j < keyLen ; ++j ) 
 	  mymultimap.insert( make_pair( foo, i*j ) );
    }

    outputKeys = keys( mymultimap );
    ip = inputKeys.begin();
    op = outputKeys.begin();
    while ( ip != inputKeys.end() ) {
	if ( *ip!= *op) {
	    ostringstream os;
	    os  << "Input and output multimap keys don't match: "
	        << *ip<< " - " << *op
		;
             handleFailure( os.str() );
	}
       ip++;
       op++;
    }

}

class Zeke;


void
test( )
{
    testValueType< char >();
    testValueType< short >();
    testValueType< int >();
    testValueType< long >();
    testValueType< long long >();

    testValueType< unsigned char >();
    testValueType< unsigned short >();
    testValueType< unsigned int >();
    testValueType< unsigned long >();
    testValueType< unsigned long long >();

    testValueType< float >();
    testValueType< double >();
    testValueType< long double >();

    testValueType< void * >();
    testValueType< const void * >();

    testValueType< Zeke * >();
    testValueType< const Zeke * >();

    testKeyRetrieval();
}


}  // namespace < anonymous >


int
Program::main( )
{
    test();

    programLogInfoIfPossible( "All tests done" );

    return EXIT_SUCCESS;
}
