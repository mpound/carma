//
// @version $Revision: 1.9 $
//
// @usage use it
//
// @description
//  Test program for tesing and benchmarking the MonitorPointIterator class.
//
// @key iterations 50 int
//      Number of iterations to do.
//
// @logger TEST_FACILITY carma.test.monitor.tMonitorPointIterator
//

#include <algorithm>
#include <iostream>
#include <vector>

#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/MonitorPointIterator.h"
#include "carma/monitor/BimaSubsystem.h"
#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/monitor/CarmaSlcBandSubsystem.h"
#include "carma/monitor/SldcSubsystem.h"
#include "carma/monitor/SlPipelineSubsystemExt.h"
#include "carma/monitor/WbcBandSubsystem.h"
#include "carma/monitor/WbdcSubsystem.h"
#include "carma/monitor/WbPipelineSubsystemExt.h"
#include "carma/monitor/OvroSubsystem.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h"

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


void
test( const MonitorContainer & mc,
      const int                iterations )
{
    if ( iterations < 1 )
        return;

    const string mcName = mc.getCanonicalName();
    
    {
        ostringstream oss;
        
        oss << "Iterating " << mcName << " " << iterations << " times...";
    
        programLogInfoIfPossible( oss.str() );
    }
    
    // Get any memory caches hot and record stats
    const int masterNumContainers = 1 + mc.getNumContainerDescendants();
    const int masterNumMps = mc.getNumMonitorPoints( true );
    {
        int numMps = 0;

        MonitorPointIterator mpi( mc );
        
        while ( ++mpi )
            ++numMps;

        if ( numMps != masterNumMps ) {
            ostringstream oss;
            
            oss << "Mismatch in the number of mps returned ("
                << masterNumMps << ", " << numMps << ")";

            throw CARMA_ERROR( oss.str() );
        }
        
        mpi.checkStats( true );
    }

    struct ::timeval before;
    ::gettimeofday( &before, 0 );

    for ( int i = 0; i < iterations; ++i ) {
        int numMps = 0;
        
        MonitorPointIterator mpi( mc );
        
        while ( ++mpi )
            ++numMps;
            
        if ( numMps != masterNumMps ) {
            ostringstream oss;
            
            oss << "Mismatch in the number of mps returned ("
                << masterNumMps << ", " << numMps << ")";

            throw CARMA_ERROR( oss.str() );
        }
    }

    struct ::timeval after;
    ::gettimeofday( &after, 0 );

    {
        const long long micros = diffMicros( after, before );
        const double millis = (static_cast< double >( micros ) / 1000.0);
        const double millisPerIteration = (millis / iterations);

        ostringstream oss;
        
        oss << mcName << ": ellapsed of " << millis << " ms for "
            << iterations << " iterations over "
            << masterNumMps << " mps (across "
            << masterNumContainers << " containers) gives ~"
            << millisPerIteration << " ms/iteration";
            
        programLogInfoIfPossible( oss.str() );
        
        cout << "  " << oss.str() << "\n";
    }
}


void
test( const int iterations )
{
    if ( iterations < 1 )
        return;

    auto_ptr< const CarmaMonitorSystem > ap( new CarmaMonitorSystem );

    test( *ap, iterations );

    test( ap->sldc(), iterations );
    test( ap->slPipeline(), iterations );
    test( ap->carmaSlcBand(1), iterations );

    test( ap->wbdc(), iterations );
    test( ap->wbPipeline(), iterations );
    test( ap->wbcBand(1), iterations );

    test( ap->control(), iterations );
    test( ap->ovro(1), iterations );
    test( ap->bima(1), iterations );
}


}  // namespace < anonymous >


int
Program::main( )
try {
    const int iterations = getIntParameter( "iterations" );
    
    programLogInfoIfPossible( "Starting tests..." );

    test( iterations );

    programLogInfoIfPossible( "All tests done" );

    return 0;
} catch ( ... ) {
    cerr << "Exiting on an exception - " + getStringForCaught() << endl;
    
    throw;
}
