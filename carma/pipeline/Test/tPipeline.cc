// $Id: tPipeline.cc,v 1.1 2011/08/18 23:25:54 abeard Exp $

#include "carma/correlator/lib/CorrelatorDataTestSl.h"
#include "carma/pipeline/util/BlankFlag.h"
#include "carma/pipeline/util/CorrelatorIntegrator.h"
#include "carma/pipeline/util/Decimator.h"
#include "carma/pipeline/util/SelfCalStage.h"
#include "carma/pipeline/util/TsysStage.h"
#include "carma/pipeline/util/Pipeline.h"
#include "carma/monitor/AstroSubsystem.h"
#include "carma/monitor/CorrelatorPipelineInfoRetriever.h"
#include "carma/monitor/PipelineSubsystemSL.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"
#include "carma/util/Time.h"

#include <iostream>

using namespace carma::correlator::lib;
using namespace carma::monitor;
using namespace carma::pipeline::util;
using namespace carma::util;
using namespace std;

/**
 * @version $Revision: 1.1 $
 *
 * @description
 * Simple test program to exercise carma::pipeline::util::Pipeline class.
 *
 * @usage tPipeline
 * 
 * @key cycles  10  i The number of pipeline creation/teardown cycles.
 *
 * @logger TEST_FACILITY carma.test.tPipeline
 */
int 
Program::main( )
try { 
    const int nCycles = getIntParameter( "cycles" );
    const int chans = 65;

    PipelineSubsystemSL mon;
    AstroSubsystem astroMon;
    CorrelatorPipelineInfoRetriever corrInfo( CORRELATOR_SPECTRAL_LINE );
    BlankFlag bf( mon, corrInfo );
    CorrelatorIntegrator integrator( mon, corrInfo );
    Decimator decimator( mon, CORRELATOR_SPECTRAL_LINE );
    carma::pipeline::util::TsysStage tsys( 
        mon, corrInfo, carma::monitor::CORRELATOR_SPECTRAL_LINE, astroMon );
    SelfCalStage selfcal( mon, tsys, true, corrInfo, CORRELATOR_SPECTRAL_LINE );
    CorrelatorDataPtr corrData( new CorrelatorDataTestSl( chans ) ); 

    // This is an ugly hack but you can trigger CorrelatorPipelineInfoRetriever 
    // to return simulated data by requesting a frame count < 0.  
    // Time::frameType is an unsigned int type, and 
    // CorrelatorPipelineInfoRetriever just happens to use a signed long and
    // long and int just happen to be the same size on our current systems - 
    // so the below trick casts to a negative number for now - but really
    // the whole simulation mechanism needs fixed.  
    // ( Don't look at me, I didn't write it ;-) ).
    corrData->setHeaderMJD( Time::MJD( 0 ) - Time::MJD( 1 ) );

    // Stack the fifo with correlator data several deep and then destroy.
    // This tests coverage and proper creation/teardown more than anything
    for ( int cycle = 0; cycle < nCycles; ++cycle ) { 
        Pipeline pipeline( mon, astroMon );

        pipeline.pushStageBack( decimator );
        pipeline.pushStageBack( tsys );
        pipeline.pushStageBack( bf );
        pipeline.pushStageBack( selfcal );
        pipeline.pushStageBack( integrator );

        for ( unsigned i = 0; i < 10; ++i ) {
            pipeline.processCorrelatorData( corrData );
        }

        // Alternately test clearing prior to teardown
        if ( cycle % 2 == 0 ) 
            pipeline.clear( );

        cout << "." << flush;
    }

    return 0;
} catch ( ... ) {
    const string errorMsg = getStringForCaught();
    cerr << errorMsg << endl;
    return 1;
}
