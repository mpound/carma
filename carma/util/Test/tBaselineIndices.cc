//
// @version $Revision: 1.8 $
//
// @usage use it
//
// @description
//  Test program for testing the baseline indices utilities.
//
// @key maxInputNo 40 int Max input number to test
// @key verbose false bool Print all baseline indices.
//
// @logger TEST_FACILITY carma.test.util.tBaselineIndices
//

#include <iostream>
#include <sstream>

#include "carma/services/Global.h"
#include "carma/util/baselineIndices.h"
#include "carma/util/ErrorException.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h"

using namespace ::std;
using namespace carma;
using namespace carma::services;
using namespace carma::util;


namespace {


void
logFailure( const string & msg )
{
    programLogErrorIfPossible( msg );

    cerr << "FAILURE: " << msg << endl;
}


void
logFailure( const ostringstream & oss )
{
    logFailure( oss.str() );
}


void
test( const int  maxInputNo,
      const bool withAutos )
{
    // Test that a max input number of 0 gives a baseline count of 0
    {
        const int computedBaselineCount =
            (withAutos ?
                baselineCountForMaxInputNoWithAutos( 0 ) :
                baselineCountForMaxInputNo( 0 ));

        ostringstream oss;

        oss << "Computed baseline count "
            << (withAutos ? "with" : "without")
            << " autos for max input number of 0 was "
            << computedBaselineCount;

        if ( computedBaselineCount != 0 ) {
            oss << " and not 0 as expected";

            logFailure( oss );

            throw CARMA_ERROR( oss );
        } else {
            oss << " as expected";

            programLogInfoIfPossible( oss.str() );
        }
    }

    int expIndex = 0;

    for ( int big = 1; big <= maxInputNo; ++big ) {
        for ( int little = 1; little <= big; ++little ) {
            if ( (withAutos == false) && (little == big) ) {
                // Test we get an exception as expected

                bool good1 = false;
                bool bad = false;
                bool good2 = false;

                try {
                    good1 = true;

                    baselineIndexForInputNos( little, big );

                    bad = true;
                } catch ( ... ) {
                    good2 = true;

                    // Just stifle the exception
                }

                if ( (good1 != true) || (bad != false) || (good2 != true) ) {
                    ostringstream oss;

                    oss << "Expected input numbers of ("
                        << big << ", " << big
                        << ") to throw an exception but it didn't ("
                        << boolalpha << good1 << ", " << bad << ", " << good2
                        << ")";

                    logFailure( oss );

                    throw CARMA_ERROR( oss );
                }

                // Continue the loop without bumping the expected index
                continue;
            }

            // Test < big, little > gives the correct index
            {
                const int computedIndex =
                    (withAutos ?
                        baselineIndexForInputNosWithAutos( big, little ) :
                        baselineIndexForInputNos( big, little ));

                if ( computedIndex != expIndex ) {
                    ostringstream oss;

                    oss << "Expected input numbers of ("
                        << big << ", " << little
                        << ") to compute a baseline index of " << expIndex
                        << " but instead got " << computedIndex;

                    logFailure( oss );

                    throw CARMA_ERROR( oss );
                }
            }

            // Test < little, big > gives the correct index
            {
                const int computedIndex =
                    (withAutos ?
                        baselineIndexForInputNosWithAutos( little, big ) :
                        baselineIndexForInputNos( little, big ));

                if ( computedIndex != expIndex ) {
                    ostringstream oss;

                    oss << "Expected input numbers of ("
                        << little << ", " << big
                        << ") to compute a baseline index of " << expIndex
                        << " but instead got " << computedIndex;

                    logFailure( oss );

                    throw CARMA_ERROR( oss );
                }
            }

            // Test index gives the correct < little, big >
            {
                const pair< int, int > computedInputNoPair =
                    (withAutos ?
                        inputNoPairForBaselineIndexWithAutos( expIndex ) :
                        inputNoPairForBaselineIndex( expIndex ));

                if ( (computedInputNoPair.first != little) ||
                     (computedInputNoPair.second != big) ) {
                    ostringstream oss;

                    oss << "Expected baseline index " << expIndex
                        << " to compute an input number pair of ("
                        << little << ", " << big << ") but instead got ("
                        << computedInputNoPair.first << ", "
                        << computedInputNoPair.second << ")";

                    logFailure( oss );

                    throw CARMA_ERROR( oss );
                }
            }

            // Bump the expected index
            ++expIndex;
        }

        {
            const int computedBaselineCount =
                (withAutos ?
                    baselineCountForMaxInputNoWithAutos( big ) :
                    baselineCountForMaxInputNo( big ));

            const int lastLittle = (withAutos ? big : (big - 1));

            const int computedLastIndex =
                (withAutos ?
                    baselineIndexForInputNosWithAutos( lastLittle, big ) :
                    baselineIndexForInputNos( lastLittle, big ));

            if ( computedBaselineCount != (computedLastIndex + 1) ) {
                ostringstream oss;

                oss << "Computed baseline count "
                    << (withAutos ? "with" : "without")
                    << " autos for max input number of " << big << " was "
                    << computedBaselineCount
                    << " and does not agree with computed last index of "
                    << computedLastIndex
                    << " for input numbers of ("
                    << lastLittle << ", " << big << ")";

                logFailure( oss );

                throw CARMA_ERROR( oss );
            }
        }

        {
            ostringstream oss;

            oss << "Baselines "
                << (withAutos ? "with" : "without" )
                << " autos now completed for a big input number of " << big
                << ". ";

            if ( expIndex == 0 )
                oss << "No indices";
            else if ( expIndex == 1 )
                oss << "Index 0";
            else
                oss << "Indices 0-" << (expIndex - 1);

            oss << " now completed.";

            programLogInfoIfPossible( oss.str() );
        }
    }
}

void
printAllBaselineIndices( ) 
{
    const unsigned short maxAnts = Global::maxAntennas( );
    for ( unsigned short antNo1 = 1; antNo1 <= maxAnts; ++antNo1 ) { 

        for ( unsigned short antNo2 = antNo1 + 1; antNo2 <= maxAnts; ++antNo2 ){
            
            cout << "Baseline " << antNo1 << "-" << antNo2 << ": index "
                << baselineIndexForInputNos( antNo1, antNo2 ) << endl;
        }
    }
}
        

        

}  // namespace < anonymous >


int
Program::main( )
{
    const int maxInputNo = getIntParameter( "maxInputNo" );
    const bool verbose = getBoolParameter( "verbose" );

    test( maxInputNo, true );
    test( maxInputNo, false );

    if ( false ) {
        ostringstream oss;

        oss << "Testing failure handling code";

        logFailure( oss );

        throw CARMA_ERROR( oss );
    }

    if ( verbose )
        printAllBaselineIndices( );

    programLogInfoIfPossible( "All test done" );

    return 0;
}
