//
// @version $Revision: 1.17 $
//
// @usage use it
//
// @description
//  Test program for tesing and benchmarking monitor system creation
//  and deletion.
//
// @key threads 1 int
//      Number of threads to use.
//
// @key iterations 20 int
//      Number of iterations to do in each thread.
//
// @key coexistCount 1 int
//      Number of monitor system instances to keep alive at the same time in
//      each thread.
//
// @key preconstructTagIdAuthority false bool
//      Whether or not to construct the TagIdAuthority singleton before the
//      first CarmaMonitorSystem instance.
//
// @key acceptableFailures 0 int
//      Number of allocation failures to tolerate in each thread before
//      just giving up in that thread. A value of -1 mean infinite.
//
// @logger TEST_FACILITY carma.test.monitor.tMonSysCreation
//

#include <algorithm>
#include <deque>
#include <iostream>
#include <vector>

#include "carma/dbms/TagIDAuthority.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/simpleStats.h"
#include "carma/util/StartPthread.h"

using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::util;


namespace {


long long
diffMicros( const struct ::timeval & lhs,
            const struct ::timeval & rhs )
{
    const long long lhsMicros =
        static_cast< long long >( lhs.tv_sec ) * 1000LL * 1000LL +
        static_cast< long long >( lhs.tv_usec );

    const long long rhsMicros =
        static_cast< long long >( rhs.tv_sec ) * 1000LL * 1000LL +
        static_cast< long long >( rhs.tv_usec );

    return (lhsMicros - rhsMicros);
}


struct IterTimingInfo {
    struct ::timeval tvBefore;
    struct ::timeval tvAfter;
};


void
cleanupExistingCms( deque< const CarmaMonitorSystem * > & existingCms )
{
    {
        ostringstream oss;
    
        oss << "Cleaning up " << existingCms.size() << " exisiting cms";
    
        programLogInfoIfPossible( oss.str() );
    }
    
    while ( existingCms.empty() == false ) {
        const CarmaMonitorSystem * const deadCmsWalking = existingCms.front();
        
        existingCms.pop_front();
        
        try {
            delete deadCmsWalking;
        } catch ( ... ) {
            programLogErrorIfPossible(
                "exception cleaning up cms - " + getStringForCaught() );
        }
    }
    
    programLogInfoIfPossible( "Existing cms cleaned up" );
}


void
test( const int                  acceptableFailures,
      const size_t               coexistCount,
      vector< IterTimingInfo > & iterTimingInfos )
{
    int failuresThusFar = 0;
    
    deque< const CarmaMonitorSystem * > existingCms;
    try {
        vector< IterTimingInfo >::iterator i = iterTimingInfos.begin();
        const vector< IterTimingInfo >::iterator iEnd = iterTimingInfos.end();
        
        for ( ; i != iEnd; ++i ) {
            const size_t existingCmsSize = existingCms.size();
            if ( existingCmsSize > coexistCount )
                throw CARMA_ERROR( "Too many exist at once" );
                
            if ( existingCmsSize == coexistCount ) {
                const CarmaMonitorSystem * const deadCmsWalking =
                    existingCms.front();
                
                existingCms.pop_front();
                
                try {
                    delete deadCmsWalking;
                } catch ( ... ) {
                    programLogErrorIfPossible(
                        "exception cleaning up cms - " + getStringForCaught() );
                }
            }

            {
                auto_ptr< const CarmaMonitorSystem > newCms;
        
                try {
                    ::gettimeofday( &(i->tvBefore), 0 );
        
                    newCms =
                        auto_ptr< const CarmaMonitorSystem >(
                            new CarmaMonitorSystem );
        
                    ::gettimeofday( &(i->tvAfter), 0 );
                } catch ( ... ) {
                    ::gettimeofday( &(i->tvAfter), 0 );
                    
                    ++failuresThusFar;
                    
                    if ( (acceptableFailures != -1) &&
                         (failuresThusFar >= acceptableFailures) )
                        throw;
                }
                
                if ( newCms.get() != 0 ) {
                    existingCms.push_back( newCms.get() );
                    newCms.release();
                }
            }
        }

        cleanupExistingCms( existingCms );
    } catch ( ... ) {
        cleanupExistingCms( existingCms );

        throw;
    }
}


struct DoItArgs {
    size_t iterations;
    int    acceptableFailures;
    size_t coexistCount;
};


void
doIt( const DoItArgs & args )
try {
    vector< IterTimingInfo > iterTimingInfos;
    if ( args.iterations > 0 )
        iterTimingInfos.resize( args.iterations );

    test( args.acceptableFailures, args.coexistCount, iterTimingInfos );

    vector< double > otherMillis;
    {
        if ( args.iterations > 1 )
            otherMillis.reserve( args.iterations - 1 );
    
        vector< IterTimingInfo >::iterator i = iterTimingInfos.begin();
        const vector< IterTimingInfo >::iterator iEnd = iterTimingInfos.end();
        bool firstOne = true;
        
        for ( ; i != iEnd; ++i ) {
            const long long micros = diffMicros( i->tvAfter, i->tvBefore );
            const double millis = (static_cast< double >( micros ) / 1000.0);
    
            if ( firstOne ) {
                firstOne = false;
                cout << "  First creation: " << millis << " ms.";
            } else
                otherMillis.push_back( millis );
        }
    }
    
    const size_t otherMillisCount = otherMillis.size();

    if ( otherMillisCount == 1 )
        cout << " Second creation: " << (*(otherMillis.begin())) << " ms.";
    else if ( otherMillisCount >= 2 ) {
        double minValue, maxValue, medianValue, meanValue, stdDev;

        calcSimpleStatsInplace( otherMillis,
                                &minValue,
                                &maxValue,
                                &medianValue,
                                &meanValue,
                                &stdDev );

        cout << " Other " << otherMillisCount << " creations: "
             << minValue << " ms min, "
             << maxValue << " ms max, "
             << medianValue << " ms median, "
             << meanValue << " ms mean, "
             << stdDev << " ms standard deviation.";
    }

    if ( args.iterations > 0 )
        cout << "\n";
} catch ( ... ) {
    programLogErrorIfPossible(
        "Coming out of doIt() on an exception - " + getStringForCaught() );
        
    throw;
}


void
test( const size_t threads,
      const size_t iterations,
      const int    acceptableFailures,
      const size_t coexistCount,
      const bool   preconstructTagIdAuthority )
{
    cout << "  Program::getUseDBMS() returned "
         << (Program::getUseDBMS() ? "true" : "false") << ".\n";

    if ( preconstructTagIdAuthority ) {
        struct ::timeval beforeTagIdAuthority;
        ::gettimeofday( &beforeTagIdAuthority, 0 );

        dbms::TagIDAuthority::getAuthority();

        struct ::timeval afterTagIdAuthority;
        ::gettimeofday( &afterTagIdAuthority, 0 );

        const long long micros =
            diffMicros( afterTagIdAuthority, beforeTagIdAuthority );

        const double millis = (static_cast< double >( micros ) / 1000.0);

        cout << "  TagIDAuthority construction: " << millis << " ms.\n";
    }

    DoItArgs args;
    
    args.iterations = iterations;
    args.acceptableFailures = acceptableFailures;
    args.coexistCount = coexistCount;

    if ( threads == 1 )
        doIt( args );
    else {
        vector< pthread_t > t;
        t.reserve( threads );
        
        for ( size_t i = 0; i < threads; ++i ) {
            ostringstream oss;
            
            oss << "Thread #" << (i + 1);
            
            t.push_back( StartPthreadWithCopy( doIt, args, oss.str() ) );
        }
        
        vector< pthread_t >::const_iterator it = t.begin();
        const vector< pthread_t >::const_iterator itEnd = t.end();
        
        for ( ; it != itEnd; ++it ) {
            void * result = 0;
            
            pthread_join( *it, &result );
        }
        
        cout << "Done\n";
    }
}


}  // namespace < anonymous >


int
Program::main( )
try {
    const size_t threads =
        ::std::max( 1, getIntParameter( "threads" ) );
        
    const size_t iterations =
        ::std::max( 0, getIntParameter( "iterations" ) );
        
    const size_t coexistCount =
        ::std::max( 1, getIntParameter( "coexistCount" ) );

    const int acceptableFailures = getIntParameter( "acceptableFailures" );
        
    const bool preconstructTagIdAuthority =
        getBoolParameter( "preconstructTagIdAuthority" );

    programLogInfoIfPossible( "Starting tests..." );

    test( threads,
          iterations,
          acceptableFailures,
          coexistCount,
          preconstructTagIdAuthority );

    programLogInfoIfPossible( "All tests done" );

    return 0;
} catch ( ... ) {
    cerr << "Exiting on an exception - " + getStringForCaught() << endl;

    throw;
}
