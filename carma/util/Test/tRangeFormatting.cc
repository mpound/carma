//
// @version $Revision: 1.7 $
//
// @usage use it
//
// @description
//  Test program for testing the range formatting functions.
//
// @noKeys
//
// @logger TEST_FACILITY carma.test.util.tRangeFormatting
//

#include <string>
#include <stdexcept>
#include <typeinfo>
#include <limits>

#include "carma/util/demangle.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/rangeFormatting.h"
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
addValueToContainer( vector< V > & c,
                     const V &     v )
{
    c.push_back( v );
}


void
checkResult( const string & actualResult,
             const string & expectedResult )
{
    if ( actualResult != expectedResult ) {
        handleFailure( "Test failed. The expected result was \"" +
                       expectedResult + "\" but the actual result was \"" +
                       actualResult + "\"" );
    }
}


template < typename C >
void
checkContainerResult( const C &      c,
                      const string & expectedResult )
{
    checkResult( formatAsRanges( c ), expectedResult );
}


template < typename C >
void
checkContainerResult( const C &      c,
                      const string & expectedResult,
                      const string & expectedPrefixedResult,
                      const string & expectedSuffixedResult,
                      const string & expectedBrackettedResult )
{
    checkResult( formatAsRanges( c ),           expectedResult );
    checkResult( formatAsRanges( c, "", "" ),   expectedResult );
    checkResult( formatAsRanges( c, "C", "" ),  expectedPrefixedResult );
    checkResult( formatAsRanges( c, "", "mm" ), expectedSuffixedResult );
    checkResult( formatAsRanges( c, "<", ">" ), expectedBrackettedResult );
}


template < typename C >
void
testCharacterContainerType( const bool shouldSeeDuplicates )
{
    const ScopedLogNdc ndc( "testCharacterContainerType< " +
                            demangleTypeName( typeid( C ) ) + " >" );

    {
        C c;

        if ( formatAsRanges( c ).empty() != true )
            handleFailure( "Empty test failed" );

        addValueToContainer( c, typename C::value_type( 'b' ) );

        checkContainerResult( c,
                              "b",
                              "Cb",
                              "bmm",
                              "<b>");

        addValueToContainer( c, typename C::value_type( 'c' ) );

        checkContainerResult( c,
                              "b-c",
                              "Cb-Cc",
                              "bmm-cmm",
                              "<b>-<c>" );

        addValueToContainer( c, typename C::value_type( 'a' ) );

        checkContainerResult( c,
                              "a-c",
                              "Ca-Cc",
                              "amm-cmm",
                              "<a>-<c>" );

        addValueToContainer( c, typename C::value_type( 'm' ) );

        checkContainerResult( c,
                              "a-c,m",
                              "Ca-Cc,Cm",
                              "amm-cmm,mmm",
                              "<a>-<c>,<m>" );

        addValueToContainer( c, typename C::value_type( 'g' ) );

        checkContainerResult( c, "a-c,g,m" );

        addValueToContainer( c, typename C::value_type( 'i' ) );

        checkContainerResult( c, "a-c,g,i,m" );

        addValueToContainer( c, typename C::value_type( 'h' ) );

        checkContainerResult( c, "a-c,g-i,m" );

        addValueToContainer( c, typename C::value_type( 'd' ) );
        addValueToContainer( c, typename C::value_type( 'e' ) );
        addValueToContainer( c, typename C::value_type( 'f' ) );
        addValueToContainer( c, typename C::value_type( 'j' ) );
        addValueToContainer( c, typename C::value_type( 'k' ) );
        addValueToContainer( c, typename C::value_type( 'l' ) );
        addValueToContainer( c, typename C::value_type( 'n' ) );
        addValueToContainer( c, typename C::value_type( 'o' ) );
        addValueToContainer( c, typename C::value_type( 'p' ) );

        checkContainerResult( c, "a-p" );

        addValueToContainer( c, typename C::value_type( 'f' ) );

        if ( shouldSeeDuplicates )
            checkContainerResult( c, "a-f,f-p" );
        else
            checkContainerResult( c, "a-p" );

        addValueToContainer( c, typename C::value_type( 'f' ) );

        if ( shouldSeeDuplicates )
            checkContainerResult( c, "a-f,f,f-p" );
        else
            checkContainerResult( c, "a-p" );
    }
}


template < typename C >
void
testContainerType( const bool shouldSeeDuplicates )
{
    const ScopedLogNdc ndc( "testContainerType< " +
                            demangleTypeName( typeid( C ) ) + " >" );

    {
        C c;

        if ( formatAsRanges( c ).empty() != true )
            handleFailure( "Empty test failed" );

        addValueToContainer( c, typename C::value_type( 2 ) );

        checkContainerResult( c, "2" );

        addValueToContainer( c, typename C::value_type( 3 ) );

        checkContainerResult( c, "2-3" );

        addValueToContainer( c, typename C::value_type( 1 ) );

        checkContainerResult( c, "1-3" );

        addValueToContainer( c, typename C::value_type( 11 ) );

        checkContainerResult( c, "1-3,11" );

        addValueToContainer( c, typename C::value_type( 8 ) );

        checkContainerResult( c, "1-3,8,11" );

        addValueToContainer( c, typename C::value_type( 6 ) );

        checkContainerResult( c, "1-3,6,8,11" );

        addValueToContainer( c, typename C::value_type( 7 ) );

        checkContainerResult( c,
                              "1-3,6-8,11",
                              "C1-C3,C6-C8,C11",
                              "1mm-3mm,6mm-8mm,11mm",
                              "<1>-<3>,<6>-<8>,<11>" );

        addValueToContainer( c, typename C::value_type( 4 ) );
        addValueToContainer( c, typename C::value_type( 5 ) );
        addValueToContainer( c, typename C::value_type( 9 ) );
        addValueToContainer( c, typename C::value_type( 10 ) );
        addValueToContainer( c, typename C::value_type( 12 ) );
        addValueToContainer( c, typename C::value_type( 13 ) );
        addValueToContainer( c, typename C::value_type( 14 ) );
        addValueToContainer( c, typename C::value_type( 15 ) );

        checkContainerResult( c,
                              "1-15",
                              "C1-C15",
                              "1mm-15mm",
                              "<1>-<15>" );

        addValueToContainer( c, typename C::value_type( 10 ) );

        if ( shouldSeeDuplicates )
            checkContainerResult( c, "1-10,10-15" );
        else
            checkContainerResult( c, "1-15" );

        addValueToContainer( c, typename C::value_type( 10 ) );

        if ( shouldSeeDuplicates )
            checkContainerResult( c, "1-10,10,10-15" );
        else
            checkContainerResult( c, "1-15" );

        // checkContainerResult( c, "1-100" );
    }

    {
        C c2;

        addValueToContainer( c2, typename C::value_type( 3 ) );

        checkContainerResult( c2, "3" );

        addValueToContainer( c2, typename C::value_type( 2 ) );

        checkContainerResult( c2, "2-3" );

        addValueToContainer( c2, typename C::value_type( 3 ) );

        if ( shouldSeeDuplicates )
            checkContainerResult( c2, "2-3,3" );
        else
            checkContainerResult( c2, "2-3" );
    }

    if ( numeric_limits< typename C::value_type >::is_signed ) {
        // programLogInfoIfPossible( "Testing negative values" );

        C c2;

        addValueToContainer( c2, typename C::value_type( 3 ) );

        checkContainerResult( c2, "3" );

        addValueToContainer( c2, typename C::value_type( -7 ) );

        checkContainerResult( c2, "(-7),3" );

        addValueToContainer( c2, typename C::value_type( -2 ) );

        checkContainerResult( c2,
                              "(-7),(-2),3",
                              "C-7,C-2,C3",
                              "(-7mm),(-2mm),3mm",
                              "<-7>,<-2>,<3>" );

        addValueToContainer( c2, typename C::value_type( -3 ) );

        checkContainerResult( c2,
                              "(-7),(-3)-(-2),3",
                              "C-7,C-3-C-2,C3",
                              "(-7mm),(-3mm)-(-2mm),3mm",
                              "<-7>,<-3>-<-2>,<3>" );

        addValueToContainer( c2, typename C::value_type( -3 ) );

        if ( shouldSeeDuplicates ) {
            checkContainerResult( c2,
                                  "(-7),(-3),(-3)-(-2),3",
                                  "C-7,C-3,C-3-C-2,C3",
                                  "(-7mm),(-3mm),(-3mm)-(-2mm),3mm",
                                  "<-7>,<-3>,<-3>-<-2>,<3>" );
        } else {
            checkContainerResult( c2,
                                  "(-7),(-3)-(-2),3",
                                  "C-7,C-3-C-2,C3",
                                  "(-7mm),(-3mm)-(-2mm),3mm",
                                  "<-7>,<-3>-<-2>,<3>" );
        }
    }
}


template < >
void
testContainerType< multiset< char > >( const bool shouldSeeDuplicates )
{
    testCharacterContainerType< multiset< char > >( shouldSeeDuplicates );
}


template < >
void
testContainerType< set< char > >( const bool shouldSeeDuplicates )
{
    testCharacterContainerType< set< char > >( shouldSeeDuplicates );
}


template < >
void
testContainerType< vector< char > >( const bool shouldSeeDuplicates )
{
    testCharacterContainerType< vector< char > >( shouldSeeDuplicates );
}


template < >
void
testContainerType< multiset< unsigned char > >( const bool shouldSeeDuplicates )
{
    testCharacterContainerType< multiset< unsigned char > >( shouldSeeDuplicates );
}


template < >
void
testContainerType< set< unsigned char > >( const bool shouldSeeDuplicates )
{
    testCharacterContainerType< set< unsigned char > >( shouldSeeDuplicates );
}


template < >
void
testContainerType< vector< unsigned char > >( const bool shouldSeeDuplicates )
{
    testCharacterContainerType< vector< unsigned char > >( shouldSeeDuplicates );
}


template < typename V >
void
testValueType( )
{
    const ScopedLogNdc ndc( "testValueType< " +
                            demangleTypeName( typeid( V ) ) + " >" );

    programLogInfoIfPossible( "Testing" );

    testContainerType< multiset< V > >( true );
    testContainerType< set< V > >( false );
    testContainerType< vector< V > >( true );
}


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
}


}  // namespace < anonymous >


int
Program::main( )
{
    test();

    programLogInfoIfPossible( "All test done" );

    return 0;
}
