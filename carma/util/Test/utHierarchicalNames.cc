#include "carma/util/Test/utHierarchicalNames.h"

#include <iostream>
#include <stdexcept>
#include <string>
#include <vector>

#include <stdlib.h>

#include "carma/util/HierarchicalNames.h"
#include "carma/util/programLogging.h"

using namespace ::std;


namespace carma {
namespace util {
namespace test {


namespace {


typedef struct {
    const char * composedName;
    int          necessaryOptions;
    const char * decomposedPiece[ 5 ];
} DecompTestCase;


void
bombOut( const string & msg )
{
    cerr << "  ERROR: " << msg << endl;
    programLogErrorIfPossible( msg );

    exit( -1 );
}


void
CheckDecomp( const DecompTestCase & x, const int options )
{
    const vector< string > decomp =
        carma::util::DecomposeHierarchicalName( x.composedName, options );

    const ::size_t iEnd =
        sizeof( x.decomposedPiece ) / sizeof( x.decomposedPiece[ 0 ] );

    for ( ::size_t i = 0; i < iEnd; ++i ) {
        if ( decomp.size() <= i ) {
            if ( x.decomposedPiece[ i ] != 0 )
                bombOut( "x.decomposedPiece[ i ] != 0" );
        } else {
            if ( x.decomposedPiece[ i ] == 0 )
                bombOut( "x.decomposedPiece[ i ] == 0" );

            if ( decomp[ i ] != string( x.decomposedPiece[ i ] ) )
                bombOut( "decomp[ i ] != string( x.decomposedPiece[ i ] )" );
        }
    }
}


void
CheckDecompOptions( const DecompTestCase & x )
{
    for ( int allowWS = 0; allowWS < 2; ++allowWS ) {
        for ( int allowEmpty = 0; allowEmpty < 2; ++allowEmpty ) {
            for ( int allowControls = 0; allowControls < 2; ++allowControls ) {
                int options = 0;

                if ( allowWS == 1 )
                    options |= HIERARCHICAL_NAME_ALLOW_WHITESPACE_IN_COMPONENTS_OPTION;

                if ( allowEmpty == 1 )
                    options |= HIERARCHICAL_NAME_ALLOW_EMPTY_COMPONENTS_OPTION;

                if ( allowControls == 1 )
                    options |= HIERARCHICAL_NAME_ALLOW_CONTROL_CHARACTERS_IN_COMPONENTS_OPTION;

                const bool exceptionExpected =
                    ((options & x.necessaryOptions) != x.necessaryOptions);

                bool exceptionThrown = false;

                try {
                    CheckDecomp( x, options );
                } catch ( ... ) {
                    exceptionThrown = true;
                }

                if ( exceptionThrown != exceptionExpected )
                    throw logic_error( "Exception mismatch" );
            }
        }
    }
}


const DecompTestCase kDecompTestCases[ ] = {
    { "", 0, { 0, 0, 0, 0, 0 } },

    { " ", HIERARCHICAL_NAME_ALLOW_WHITESPACE_IN_COMPONENTS_OPTION, { " ", 0, 0, 0, 0 } },

    { "foo",     0, { "foo",     0, 0, 0, 0 } },
    { "foo3",    0, { "foo3",    0, 0, 0, 0 } },
    { "5fah",    0, { "5fah",    0, 0, 0, 0 } },
    { "147",     0, { "147",     0, 0, 0, 0 } },
    { "foo_bar", 0, { "foo_bar", 0, 0, 0, 0 } },
    { "@",       0, { "@",       0, 0, 0, 0 } },
    { "_",       0, { "_",       0, 0, 0, 0 } },
    { "$",       0, { "$",       0, 0, 0, 0 } },

    { "~!@#$%^&*()_+;:<>/", 0, { "~!@#$%^&*()_+;:<>/", 0, 0, 0, 0 } },

    { "hello there", HIERARCHICAL_NAME_ALLOW_WHITESPACE_IN_COMPONENTS_OPTION, { "hello there", 0, 0, 0, 0 } },

    { " . ", HIERARCHICAL_NAME_ALLOW_WHITESPACE_IN_COMPONENTS_OPTION, { " ", " ", 0, 0, 0 } },

    { "foo.bar.baz",       0, { "foo", "bar", "baz", 0,       0 } },
    { "foo.bar.baz.spook", 0, { "foo", "bar", "baz", "spook", 0 } }
};


void
CheckDecomps( )
{
    const ::size_t iEnd =
        sizeof( kDecompTestCases ) / sizeof( kDecompTestCases[ 0 ] );

    for ( ::size_t i = 0; i < iEnd; ++i )
        CheckDecompOptions( kDecompTestCases[ i ] );
}


}  // namespace carma::util::test::< anonymous >


void
UnitTestHierarchicalNames( )
{
    programLogInfoIfPossible( "Checking decomps..." );
    
    CheckDecomps();

    programLogInfoIfPossible( "All decomp checks done" );
}


}  // namespace carma::util::test
}  // namespace carma::util
}  // namespace carma

