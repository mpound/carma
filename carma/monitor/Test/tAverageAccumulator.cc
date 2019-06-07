//
// @version $Revision: 1.5 $
//
// @usage use it
//
// @description
//  Test program for testing the AverageAccumulator class.
//
// @key intFrames 2 int
//      Frames per integration.
//
// @key numInts 10 int
//      Number of integrations (-1 is infinite).
//
// @key singleFiles true bool
//      Write all data to the same files repeatedly instead of indexing them
//      by frame count.
//
// @logger TEST_FACILITY carma.test.monitor.tAverageAccumulator
//

#include <iostream>
#include <cstdio>

#include "carma/monitor/AverageAccumulator.h"
#include "carma/monitor/MonitorPoint.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/TestSubsystem.h"
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


string
makeFileName( const char * const base,
              const long         frameCount )
{
    ostringstream oss;

    oss << base << "_test_" << frameCount << ".mpdat";

    return oss.str();
}


void
test( const int  intFrames,
      const int  numInts,
      const bool singleFiles )
{
    CarmaMonitorSystem cms;

    const MonitorPoint::ARCHIVE_PRIORITY archivePriority =
        ((intFrames == 1) ? MonitorPoint::USEFUL : MonitorPoint::NORMAL);

    AverageAccumulator avgAccum( cms, archivePriority );

    struct ::timeval beginTime;
    struct ::timeval writeBeginTime;
    struct ::timeval writeEndTime;
    struct ::timeval endTime;

    long long writeMicrosSum = 0;
    long long totalMicrosSum = 0;

    bool havePrevCmsFrame = false;
    int prevCmsFrame = 0;

    for ( long i = 0; ((numInts < 0) || (i < numInts)); ++i ) {
        avgAccum.resetAccumulator();

        cout << "Reading " << intFrames << " monitor system frames..." << endl;

        for ( int j = 0; j < intFrames; ++j ) {
            cms.read();

            {
                const int cmsFrame = cms.getFrameCount();

                if ( havePrevCmsFrame == false ) {
                    prevCmsFrame = cmsFrame;
                    havePrevCmsFrame = true;
                } else {
                    if ( cmsFrame == prevCmsFrame ) {
                        ostringstream oss;

                        oss << "Duplicate frames (" << prevCmsFrame
                            << " -> " << cmsFrame << ")";

                        programLogErrorIfPossible( oss.str() );
                    } else  if ( cmsFrame < prevCmsFrame ) {
                        ostringstream oss;

                        oss << "Misordered frames (" << prevCmsFrame
                            << " -> " << cmsFrame << ")";

                        programLogErrorIfPossible( oss.str() );
                    } else if ( cmsFrame > (prevCmsFrame + 1) ) {
                        const int skipped = (cmsFrame - (prevCmsFrame + 1));

                        ostringstream oss;

                        oss << skipped << " frame skip detected ("
                            << prevCmsFrame << " -> " << cmsFrame
                            << ")";

                        programLogWarnIfPossible( oss.str() );
                    }

                    prevCmsFrame = cmsFrame;
                }
            }

            avgAccum.accumulate();
        }

        const long frameCount = cms.getFrameCount();

        const long fileIndex = (singleFiles ? 0 : frameCount);

        const string shortFileName   = makeFileName( "short",   fileIndex );
        const string numericFileName = makeFileName( "numeric", fileIndex );
        const string stringFileName  = makeFileName( "string",  fileIndex );
        const string complexFileName = makeFileName( "complex", fileIndex );

        ::gettimeofday( &beginTime, 0 );

        FILE * const shortFile =   fopen( shortFileName.c_str(),   "w" );
        FILE * const numericFile = fopen( numericFileName.c_str(), "w" );
        FILE * const stringFile =  fopen( stringFileName.c_str(),  "w" );
        FILE * const complexFile = fopen( complexFileName.c_str(), "w" );

        ::gettimeofday( &writeBeginTime, 0 );

        if ( intFrames == 1 ) {
            avgAccum.writeInstAveragesToFile( frameCount,
                                              shortFile,
                                              numericFile,
                                              stringFile,
                                              complexFile );
        } else {
            avgAccum.writeLongAveragesToFile( frameCount,
                                              shortFile,
                                              numericFile,
                                              stringFile,
                                              complexFile );
        }

        ::gettimeofday( &writeEndTime, 0 );

        fclose( shortFile );
        fclose( numericFile );
        fclose( stringFile );
        fclose( complexFile );

        ::gettimeofday( &endTime, 0 );

        const long long microsPerSec = 1000LL * 1000LL;

        const long long writeMicros =
            (static_cast< long long >( writeEndTime.tv_sec ) * microsPerSec +
             static_cast< long long >( writeEndTime.tv_usec )) -
            (static_cast< long long >( writeBeginTime.tv_sec ) * microsPerSec +
             static_cast< long long >( writeBeginTime.tv_usec ));

        const long long totalMicros =
            (static_cast< long long >( endTime.tv_sec ) * microsPerSec +
             static_cast< long long >( endTime.tv_usec )) -
            (static_cast< long long >( beginTime.tv_sec ) * microsPerSec +
             static_cast< long long >( beginTime.tv_usec ));

        cout << "Integration #" << (i + 1)
             << " (" << (writeMicros / 1000) << ", " << (totalMicros / 1000)
             << ")" << endl;

        writeMicrosSum += writeMicros;
        totalMicrosSum += totalMicros;
    }

    cout << "Average write time: "
         << (static_cast< double >( writeMicrosSum ) / (numInts * 1000.0))
         << " millseconds per integration for "
         << numInts << " integrations\n";

    cout << "Average total time: "
         << (static_cast< double >( totalMicrosSum ) / (numInts * 1000.0))
         << " millseconds per integration for "
         << numInts << " integrations\n";
}


}  // namespace < anonymous >


int
Program::main( )
try {
    const int intFrames = getIntParameter( "intFrames" );
    const int numInts = getIntParameter( "numInts" );
    const bool singleFiles = getBoolParameter( "singleFiles" );

    programLogInfoIfPossible( "Starting tests..." );

    test( intFrames, numInts, singleFiles );

    programLogInfoIfPossible( "All tests done" );

    return 0;
} catch ( ... ) {
    cerr << "Exiting on an exception - " + getStringForCaught() << endl;

    throw;
}
