
// $Id: tIntegrator.cc,v 1.1 2011/08/18 23:25:54 abeard Exp $

// test Integrator

#include "carma/util/BaseException.h"
#include "carma/util/NotFoundException.h"
#include "carma/correlator/lib/CorrelatorData.h"
#include "carma/correlator/lib/CorrelatorDataTestSZA.h"
#include "carma/correlator/lib/CorrelatorConfigChecker.h"
#include "carma/monitor/PipelineSubsystemSL.h"
#include "carma/monitor/PipelineSubsystemWB.h"
#include "carma/monitor/CorrelatorPipelineInfoRetriever.h"
#include "carma/util/Program.h"
#include "carma/util/Time.h"
#include "carma/pipeline/IntegratorStage.h"

#include <netinet/in.h>
#include <math.h>
#include <unistd.h>           // for usleep
#include <stdlib.h>
#include <time.h>
#include <iomanip>
#include <complex>
#include <vector>
#include <exception>

using namespace std;
using namespace carma::util;
using namespace carma::pipeline::util;
using namespace carma::correlator::lib;

//
// @version $Revision: 1.1 $
//
// @usage Usage: tIntegrator - test Correlator Integrator Stage.
//
// @key n 10 int number of (10.0 second) records to integrate
// @key sl false                        b Run as Spectral line pipeline
//
// @logger TEST_FACILITY carma.test.pipeline.util.tIntegrator
//
// @description Correlator Integrator test
//

void runIt(int num2integrate, bool sl, CorrelatorConfigChecker* ccc);

int carma::util::Program::main() {
  cerr << "Running..." << endl;
  cerr << "code will integrate forever" << endl;
  string filename = "conf/correlator/correlator.conf";
  CorrelatorConfigChecker* ccc = 
    CorrelatorConfigChecker::getInstance(filename);
  ccc->start();
  runIt(getIntParameter("n"), getBoolParameter("sl"), ccc);
  return 0;
}

void runIt(int num2integrate, bool sl, CorrelatorConfigChecker* ccc) {
  carma::monitor::PipelineSubsystem * dummy = 0;
  carma::monitor::CorrelatorType ct;

  if (sl) {
    dummy = new carma::monitor::PipelineSubsystemSL( );
        ct = carma::monitor::CORRELATOR_SPECTRAL_LINE;
  }      
  else {
    dummy = new carma::monitor::PipelineSubsystemWB( );
        ct = carma::monitor::CORRELATOR_WIDEBAND;
  }  
  carma::monitor::CorrelatorPipelineInfoRetriever infoRetriever( ct );
  CorrelatorDataPtr cd2( new CorrelatorDataTestSZA() );
  CorrelatorIntegrator ci( *dummy );
  ci.startIntegration( 10.0, num2integrate, 0 );
  typedef vector<carma::correlator::lib::CorrelatorBand> CorrBands;
  while (1) {
    ci.preprocessCorrelatorData( cd2 );
    CorrBands bands = cd2->getBands(); 
    CorrBands::iterator cbBegin = bands.begin();
    CorrBands::iterator cbEnd = bands.end();
    for ( CorrBands::iterator cb = cbBegin; cb != cbEnd; ++cb ) 
        ci.processCorrelatorBand( &( *cb ) );

    ci.postprocessCorrelatorData( cd2 );
  }

}
