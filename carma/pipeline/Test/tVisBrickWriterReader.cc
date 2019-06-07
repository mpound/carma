// $Id: tVisBrickWriterReader.cc,v 1.1 2011/08/18 23:25:54 abeard Exp $

#include "carma/correlator/lib/CorrelatorDataTestSl.h"
#include "carma/monitor/PipelineSubsystemSL.h"
#include "carma/pipeline/VisBrickReader.h"
#include "carma/pipeline/VisBrickWriterStage.h"
#include "carma/util/EOFException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"
#include "carma/util/Time.h"

#include <cassert>
#include <cstdio>
#include <iostream>

using namespace carma::util;
using namespace carma::monitor;
using namespace carma::correlator::lib;
using namespace carma::pipeline;
using namespace std;

namespace {
        
    const string filebase = "testvisbrick";

    string
    getTestVisbrickFilename( const frameType frame, 
                             const bool write = false ) 
    {
        const string suffix = ( write ? ".write" : "" );
        const string dir = Program::getProgram().getStringParameter( "dir" );
       
        ostringstream visbrickFilename;

        visbrickFilename << dir << "/" << filebase << "_" << frame << suffix;

        return visbrickFilename.str( );
    }

} // namespace <unnamed>

/**
 * @version $Revision: 1.1 $
 *
 * @description
 * A somewhat ugly program to test VisBrickWriter/Reader classes.
 *
 * @usage tVisBrickWriterReader
 *
 * @key dir /tmp string Directory to place temporary visbrick in.
 *
 * @logger TEST_FACILITY carma.test.tVisBrickWriterReader
 */
int
Program::main( )
try {
    const string dir = Program::getStringParameter( "dir" );

    PipelineSubsystemSL mon;
    VisBrickWriter writer( dir + '/' + filebase, mon );
        
    unsigned int numFrames = 0;

    CorrelatorDataPtr aCorrData( new CorrelatorDataTestSl( 65 ) );

    // Write some frames to a temp visbrick.

    // Write just enough to fill a single file...
    const double maxFilesize = 64; // MB 
    const double corrDatasize = aCorrData->getTotalSerialBytes( ) * 1.e-6; // MB
    

    const int maxFrames = static_cast< int >( maxFilesize / corrDatasize ); 
    const frameType firstframe = Time::computeClosestFrame( Time::MJD( ) );
    const frameType lastframe = firstframe + (maxFrames - 1);

    const string visbrickFilename = getTestVisbrickFilename( firstframe, true );

    cout << "Writing frames " << firstframe << "-" << lastframe << " to "
        << visbrickFilename << "..." << endl;

    for ( frameType f = firstframe; f <= lastframe; ++f ) {
        CorrelatorDataPtr corrData( new CorrelatorDataTestSl( 65 ) );
        corrData->setHeaderMJD( Time::MJD( f ) );
        writer.preprocessCorrelatorData( corrData );
        writer.postprocessCorrelatorData( corrData );
        ++numFrames;
    }
    
    cout << numFrames << " frames written to " 
         << visbrickFilename << "..." << endl;

    // Now wait for last frame to show up - but don't wait forever.
    cout << "Waiting for them to show up in visbrick..." << endl;
    const int maxRetries = 5;
    for ( int r = 0; r < maxRetries; ++r ) { 
        CorrelatorVisBrickReader reader( visbrickFilename );

        RecordsByFrameMap records = reader.getRecordsKeyedByFrame( );
        if ( records.size() != numFrames ) 
            usleep( 1000000 ); // Sleep a second for file to be written out.
        else
            break;
    }

    CorrelatorVisBrickReader reader( visbrickFilename );
    RecordsByFrameMap records = reader.getRecordsKeyedByFrame( );
    assert( records.size() == numFrames );

    const double mjdErrTolerance =
        (1.0 / (24.0 * 60.0 * 60.0 * 1.0e6));  // 1 microsecond
    

    // Read numFrames from the same visbrick.
    {
        cout << "Reading said visbrick and checking times..." << endl;
        CorrelatorVisBrickReader reader( visbrickFilename );
        CorrelatorData * cd = 0; 

        for ( unsigned i = 0; i < numFrames; ++i ) {
            const int frame = firstframe + i;
            cd = new CorrelatorData( reader.readOne( ) );
            const double mjdFrameHeaderErr =
                fabs( Time::MJD( frame ) - cd->getHeader().getMJD() );
            assert( mjdFrameHeaderErr < mjdErrTolerance );
        }

        // Verify other important values 
        assert( cd->getNumberOfBands( ) == aCorrData->getNumberOfBands( ) );

        // Verify that the next read throws EOF.
        try {
            cout << "Testing throw on EOF..." << endl;
            reader.readOne( );
            assert( false );
        } catch ( carma::util::EOFException & ) {
            assert( true );
        }
    }

    // Read numFrames from the visbrick using the 'new' technique.
    {
        cout << "Reading visbrick with records keyed by frame map..." << endl;
        CorrelatorVisBrickReader reader( visbrickFilename );

        RecordsByFrameMap records = reader.getRecordsKeyedByFrame( );

        // Loop over correlator data map 
        int i = 0;
        RecordsByFrameMap::iterator rec = records.begin( );
        const RecordsByFrameMap::const_iterator recEnd = records.end( );
        for ( ; rec != recEnd; ++rec, ++i ) {
            const int frame = firstframe + i;
            const CorrelatorData * cd = rec->second; 
            const double mjdFrameHeaderErr =
                fabs( Time::MJD( frame ) - cd->getHeader().getMJD() );
            assert( mjdFrameHeaderErr < mjdErrTolerance );
            assert( cd->getNumberOfBands( ) == aCorrData->getNumberOfBands( ) );
        }
    }

    // Cleanup after ourselves.
    cout << "Cleaning up temporary visbricks..." << endl;
    remove( visbrickFilename.c_str( ) );

    cout << "Success!" << endl;
    return 0;
} catch (...) {
    logCaughtAsError( );
    cout << "Failure!" << endl;
    return 1;
}
