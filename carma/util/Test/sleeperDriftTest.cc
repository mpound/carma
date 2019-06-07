#include <iostream>
#include <vector>

#include <sys/time.h>

#include "carma/util/Program.h"
#include "carma/util/simpleStats.h"
#include "carma/util/Sleeper.h"


using namespace ::std;
using namespace carma;
using namespace carma::util;


namespace {


struct Result {
    int              targetWholeSecs;
    struct ::timeval before;
    struct ::timeval after;
};


}  // namespace < anonymous >


//
// @version $Revision: 1.1 $
//
// @description
// Test to determine accuracy of Sleeper class - not extensive
// as the intent is to gauge any drift.
//
// @usage sleeperDriftTest reps=X
//
// @key reps 15 i Number of iterations to test.
//
// @logger TEST_FACILITY carma.test.util.sleeperDriftTest
//
int
Program::main( )
{
    const int numReps = getIntParameter("reps");

    // Initalize the vector of tests to perform
    vector< Result > results;
    if ( numReps > 0 ) {
        results.reserve( numReps );

        unsigned int targetWholeSecs = 1;

        for ( int i = 0; i < numReps; ++i ) {
            Result result;

            result.targetWholeSecs = targetWholeSecs;
            
            results.push_back( result );
            
            if ( targetWholeSecs >= 3 )
                targetWholeSecs = 1;
            else
                targetWholeSecs += 1;
        }
    }

    // Collect test results
    {
        Sleeper sleeper;

        vector< Result >::iterator i = results.begin();
        const vector< Result >::iterator iEnd = results.end();

        for ( ; i != iEnd; ++i ) {
            ::gettimeofday( &(i->before), 0 );

            sleeper.waitForWholeSecDuration( i->targetWholeSecs );

            ::gettimeofday( &(i->after), 0 );
        }
    }

    // Calculate and output stats
    {
        vector< double > errMillis;
        {
            errMillis.reserve( results.size() );

            vector< Result >::const_iterator i = results.begin();
            const vector< Result >::const_iterator iEnd = results.end();

            for ( ; i != iEnd; ++i ) {
                const long long beforeMicros =
                    static_cast< long long >( i->before.tv_sec ) *
                        1000LL * 1000LL +
                    static_cast< long long >( i->before.tv_usec );

                const long long afterMicros =
                    static_cast< long long >( i->after.tv_sec ) *
                        1000LL * 1000LL +
                    static_cast< long long >( i->after.tv_usec );

                const long long errMicros =
                    (afterMicros - beforeMicros) -
                    (1000LL * 1000LL * i->targetWholeSecs);

                errMillis.push_back(
                    static_cast< long >( errMicros ) / 1000.0 );
            }
        }

        double minErrMillis = -1.0;
        double maxErrMillis = -1.0;
        double medianErrMillis = -1.0;
        double meanErrMillis = -1.0;
        double stdDevErrMillis = -1.0;

        calcSimpleStatsInplace( errMillis,
                                &minErrMillis,
                                &maxErrMillis,
                                &medianErrMillis,
                                &meanErrMillis,
                                &stdDevErrMillis );

        cout << "Stats for " << errMillis.size() << " sleeps: "
             << minErrMillis << " ms min error, "
             << maxErrMillis << " ms max error, "
             << medianErrMillis << " ms median error, "
             << meanErrMillis << " ms mean error, "
             << stdDevErrMillis << " ms error standard deviation.\n";
    }

    return EXIT_SUCCESS;
}
