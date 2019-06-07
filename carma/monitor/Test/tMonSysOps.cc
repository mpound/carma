//
// @version $Revision: 1.6 $
//
// @usage use it
//
// @description
//  Test program for tesing and benchmarking monitor system operations
//
// @key iters 32 int
//      Number of iterations per test
//
// @key ops all string
//      Which ops to test/benchmark
//
// @logger TEST_FACILITY carma.test.monitor.tMonSysOps
//

#include <algorithm>
#include <iostream>
#include <vector>

#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/MonitorPointIterator.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/simpleStats.h"
#include "carma/util/StringUtils.h"

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
    struct ::timeval beginTime;
    struct ::timeval endTime;
};


string
getReportString( const vector< IterTimingInfo > & itiVec )
{
    if ( itiVec.empty() )
        return "no iterations";
        
    double firstMillis = -1.0;
    vector< double > otherMillis;
    {
        otherMillis.reserve( itiVec.size() - 1 );
    
        const vector< IterTimingInfo >::const_iterator iBegin =
            itiVec.begin();
            
        const vector< IterTimingInfo >::const_iterator iEnd =
            itiVec.end();
            
        vector< IterTimingInfo >::const_iterator i = iBegin;
        
        for ( ; i != iEnd; ++i ) {
            const long long micros = diffMicros( i->endTime, i->beginTime );
            const double millis = (static_cast< double >( micros ) / 1000.0);

            if ( i == iBegin )
                firstMillis = millis;
            else
                otherMillis.push_back( millis );
        }
    }
    
    ostringstream oss;
    
    if ( otherMillis.empty() ) {
        oss << firstMillis << " ms";
    } else {
        oss << "first iteration " << firstMillis << " ms";
    
        double minValue, maxValue, medianValue, meanValue, stdDev;

        calcSimpleStatsInplace( otherMillis,
                                &minValue,
                                &maxValue,
                                &medianValue,
                                &meanValue,
                                &stdDev );

        oss << " and the other " << otherMillis.size() << " iterations were "
            << minValue << " ms min, "
            << maxValue << " ms max, "
            << medianValue << " ms median, "
            << meanValue << " ms mean, "
            << stdDev << " ms std dev.";
    }

    return oss.str();
}


bool
hasOp( const set< string > & lcOps,
       const string &        op )
{
    return (lcOps.find( StringUtils::lowASCIIAlphaNumericToLower( op ) ) !=
            lcOps.end());
}


void
test( const int             itersPerTest,
      const set< string > & lcOps )
{
    const auto_ptr< CarmaMonitorSystem > cms1( new CarmaMonitorSystem );
    const auto_ptr< CarmaMonitorSystem > cms2( new CarmaMonitorSystem );

    vector< IterTimingInfo > itiVec;

    const bool doAllOps = hasOp( lcOps, "all" );

    if ( doAllOps || hasOp( lcOps, "setNoData" ) ) {
        cout << "  Starting CarmaMonitorSystem::setNoData..." << endl;

        itiVec.resize( itersPerTest );
        vector< IterTimingInfo >::iterator i = itiVec.begin();
        const vector< IterTimingInfo >::iterator iEnd = itiVec.end();

        for ( ; i != iEnd; ++i ) {
            ::gettimeofday( &(i->beginTime), 0 );
            
            cms1->setNoData();
            
            ::gettimeofday( &(i->endTime), 0 );
        }
        
        cout << "    Finished CarmaMonitorSystem::setNoData: "
             << getReportString( itiVec ) << endl;
    }
    
    if ( doAllOps || hasOp( lcOps, "synchronize" ) ) {
        cout << "  Starting CarmaMonitorSystem::synchronize..." << endl;

        itiVec.resize( itersPerTest );
        vector< IterTimingInfo >::iterator i = itiVec.begin();
        const vector< IterTimingInfo >::iterator iEnd = itiVec.end();

        for ( ; i != iEnd; ++i ) {
            ::gettimeofday( &(i->beginTime), 0 );
            
            cms2->synchronize( *cms1 );
            
            ::gettimeofday( &(i->endTime), 0 );
        }
        
        cout << "    Finished CarmaMonitorSystem::synchronize: "
             << getReportString( itiVec ) << endl;
    }
    
    if ( hasOp( lcOps, "mpVecSetNoData" ) ) {
        vector< MonitorPoint * > mpVec;
        {
            MonitorPointIterator mpi( *cms1 );
            
            while ( ++mpi )
                mpVec.push_back( &(mpi.getMonitorPoint()) );
        }
        
        {
            cout << "  Starting mpVec setNoData..." << endl;

            itiVec.resize( itersPerTest );
            vector< IterTimingInfo >::iterator i = itiVec.begin();
            const vector< IterTimingInfo >::iterator iEnd = itiVec.end();

            for ( ; i != iEnd; ++i ) {
                ::gettimeofday( &(i->beginTime), 0 );
                
                {
                    vector< MonitorPoint * >::const_iterator i =
                        mpVec.begin();
                        
                    const vector< MonitorPoint * >::const_iterator iEnd =
                        mpVec.end();
                        
                    for ( ; i != iEnd; ++i )
                        (*i)->setNoData();
                }
                
                ::gettimeofday( &(i->endTime), 0 );
            }
            cout << "    Finished mpVec setNoData: "
                 << getReportString( itiVec ) << endl;
        }
    }
}


}  // namespace < anonymous >


int
Program::main( )
try {
    const string opsParam = getStringParameter( "ops" );
    int itersPerTest = getIntParameter( "iters" );
    if ( itersPerTest < 0 )
        itersPerTest = 0;

    set< string > lcOps;
    {
        const vector< string > opsVec =
            StringUtils::tokenize( opsParam, " ,", true );
            
        vector< string >::const_iterator i = opsVec.begin();
        const vector< string >::const_iterator iEnd = opsVec.end();
        
        for ( ; i != iEnd; ++i ) {
            lcOps.insert( StringUtils::lowASCIIAlphaNumericToLower( *i ) );
        }
    }
    
    programLogInfoIfPossible( "Starting tests..." );

    test( itersPerTest, lcOps );

    programLogInfoIfPossible( "Tests done" );

    return 0;
} catch ( ... ) {
    cerr << "Exiting on an exception - " + getStringForCaught() << endl;

    throw;
}
