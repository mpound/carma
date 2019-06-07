//
// @version $Revision: 1.11 $
//
// @usage use it
//
// @description
//      Allocates a carma monitor system. Plays with it. Deallocates it.
//      Useful for looking for leaks using valgrind or a similar tool.
//      Typical valgrind usage from the top of your build tree would be:
//
//      valgrind --tool=memcheck --leak-check=yes carma/monitor/Test/monitorLeaks
//
//      or
//
//      valgrind --tool=memcheck --leak-check=yes --leak-resolution=high --num-callers=20 carma/monitor/Test/monitorLeaks
//
// @key iters 1 integer
//      number of iterations to perform.
//
// @key level 1 integer
//      level of playing done with the monitor. level <= 0 means do everything.
//
// @key pause 0 integer
//      number of seconds to pause before quitting.
//
// @logger TEST_FACILITY carma.test.monitor.monitorLeaks
//

#include <sstream>

#include "carma/dbms/TagIDAuthority.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/SlPipelineSubsystemExt.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"

using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::util;


int
Program::main( )
{
    const int iters = getIntParameter( "iters" );
    const int level = getIntParameter( "level" );
    const int pause = getIntParameter( "pause" );

    for ( int i = 0; i < iters; ++i ) {
        programLogInfoIfPossible( "making the monitor system" );

        MonitorSystem * cms = new CarmaMonitorSystem;

        if ( (level <= 0) || (level >= 2) ) {
            programLogInfoIfPossible( "playing with the monitor system" );
            
            cms->slPipeline().integratorStageContainer().integratorStage().integrating();
    
            cms->readNewest();
            cms->read();
        }

        programLogInfoIfPossible( "deleting the monitor system" );

        MonitorSystem * deadManWalking = 0;
        
        ::std::swap( deadManWalking, cms );
        
        delete deadManWalking;
    
        deadManWalking = 0;
    }
    
    programLogInfoIfPossible( "shutting down the tag ID authority" );
    
    dbms::TagIDAuthority::closeAuthority();
    
    if ( pause > 0 ) {
        {
            ostringstream oss;
            
            oss << "pausing for " << pause << " seconds";
            
            programLogInfoIfPossible( oss.str() );
        }
        
        sleep( pause );
    }
    
    programLogInfoIfPossible( "done" );

    return 0;
}
