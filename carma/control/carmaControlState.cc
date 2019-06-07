/**
 * Carma control state program.
 * Application to save and restore control subsystem state.  
 *
 * @author: Andy Beard
 *
 * $Id: carmaControlState.cc,v 1.26 2012/01/05 21:18:44 mpound Exp $
 * $CarmaCopyright$
 */

#include "carma/corba/corba.h"
#include "carma/corba/Server.h"

#include "carma/control/AgingPolicy.h"
#include "carma/control/SystemStateImpl.h"
#include "carma/control/SystemState_skel.h"
#include "carma/control/SystemState_skel_tie.h"
#include "carma/control/SystemStateManager.h"
#include "carma/monitor/MonitorSystemSelector.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/posixErrors.h"
#include "carma/util/programLogging.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"


using namespace boost::posix_time;
using namespace carma;
using namespace carma::control;
using namespace carma::monitor;
using namespace carma::util;
using namespace std;

namespace {

const Trace::TraceLevel TRACE_AGING_LOGIC = Trace::TRACE3;

void ageOutStateFiles( PolicyChain & policies,
                       StateManager & manager )
{
    CARMA_CPTRACE( TRACE_AGING_LOGIC, "Aging out state files." );

    ptime startDate = microsec_clock::local_time();

    // Loop over policy chain first update time information
    ptime policyStartDate = startDate;
    typedef PolicyChain::iterator PCIterator;
    PCIterator beginPolicy = policies.begin( );
    PCIterator endPolicy = policies.end( );
    for ( PCIterator policy = beginPolicy; policy != endPolicy; ++policy ) {
        policy->syncToTime( policyStartDate );
        policyStartDate = policy->subtractTotalDuration( policyStartDate );
    }
        
    // Loop over policies and apply to age candidates from prior policy.
    // Nothing ages into the first policy and nothing ages out of the last.
    PCIterator nextPolicy = policies.begin( );
    for (PCIterator policy = beginPolicy; nextPolicy++ != endPolicy; ++policy) {

        // Retrieve age out candidates from first policy 
        FileTimeMap aged = policy->reapAgedCandidates( );

        for ( FileTimeMap::iterator i = aged.begin(); i != aged.end(); ++i ) {
            nextPolicy->handleCandidate( *i, *policy, manager );
        }
    }
}


vector< string >
getPolicyDirs( const PolicyChain & pchain ) 
{
    vector< string > answer;

    const PolicyChain::const_iterator iBegin = pchain.begin();
    const PolicyChain::const_iterator iEnd = pchain.end();
    for ( PolicyChain::const_iterator i = iBegin; i != iEnd; ++i )
        answer.push_back( i->getDirectory() );
        
    return answer;
}

} // namespace < unnamed >


//
// @description Application to save and restore control subsystem state.  
//  Restoration occurs when this application detects that the subarray 
//  is 'uninitialized'.   Note that that this application only restores 
//  the state of the monitor system control subsystem container, it does 
//  not reissue commands which set these values in their respective 
//  subsystems.  The intent is for python code to read the values
//  from the monitor system and reissue the commands as it best sees fit.
//  /t State files are saved in the specified 'statedir' directory and named
//  \"state-\" + frame number + \".txt\".  
//  A symlink to the most recently saved state is named simply \"state.txt\".  
//  Older files will be deleted based upon their age.  
//
// @version $Id: carmaControlState.cc,v 1.26 2012/01/05 21:18:44 mpound Exp $
// 
// @usage carmaControlState [key=value key=value ...]
//
// @key inputCMS "intermediate" string 
//      Input carma monitor system to use ('raw', 'intermediate' or 'final').
// @key statedir "/tmp" string
//      Location of saved state files.
// @key agecycle 600 int
//      How often (in read cycles) to age out older state files.
// @key production false bool
//      Are we running on the production system (if so we exit on file errors).
// @key rebuildindex false bool
//      Rebuild master difference index prior and exit.
// @key threads 8 int
//      The number of threads to use when rebuilding the index files.
// 
// @logger CONTROL_FACILITY carma.control.carmaControlState
// 
int Program::main( ) 
{
    const string inputCmsName = getStringParameter("inputCMS");
    const string stateDirName = getStringParameter("statedir");
    const CmsSelector cmsType = convertStringToCmsSelector(inputCmsName);
    const frameType agingPeriod = getIntParameter("agecycle"); 
    const bool rebuildIndexAndExit = getBoolParameter("rebuildindex");
    const bool production = getBoolParameter("production");
    const int nThreads = getIntParameter("threads");
    
    PolicyChain agingPolicies = createAgingPolicyChain( stateDirName );
    
    // Specify where state files are born: corresponds to first aging policy dir
    string managerDir; 
    if ( agingPolicies.empty() ) {
        managerDir = stateDirName; // Place directly into state dir.
        // Is this an error condition?
        return 0;
    } else {
        managerDir = agingPolicies.begin()->getDirectory();
    }

    const bool throwOnDirErrors = production; // Reveal meaning of 'production'
    StateManager manager( cmsType, managerDir, throwOnDirErrors );

    SystemStateImpl stateServer( manager, agingPolicies );
    corba::Server & server = Program::getCorbaServer();
        server.addServant<POA_carma::control::SystemState_tie>
                    ( stateServer, control::SYSTEM_STATE_NAME );

    if ( rebuildIndexAndExit ) {
        manager.buildIndexFromScratch( getPolicyDirs( agingPolicies ),
                                       nThreads ); 
        return EXIT_SUCCESS;
    } 

    manager.restoreIndexFromFile();

    if ( manager.verifyIndexIntegrity() ) {
        programLogInfoIfPossible( "Verify index integrity successful." );
    } else {
        programLogErrorIfPossible( "Verify index integrity returned errors." );
    }
            
    manager.update( );
    ageOutStateFiles( agingPolicies, manager );

    {
        const pthread_t myThreadId = pthread_self();
        ostringstream info;
        info << "Program::Main threadId is " << myThreadId << ".";
        programLogInfoIfPossible( info.str() );
    }

    bool stateChanged = false;
    unsigned long updateCycle = 0;
    // Program::imrTerminationRequested calls server.terminated() safely
    while ( !imrTerminationRequested( ) ) {
        ++updateCycle;
        stateChanged = manager.update( );

        if ( updateCycle % agingPeriod == 0 || stateChanged ) {
            ageOutStateFiles( agingPolicies, manager );
        }
        server.work();
    }

    return EXIT_SUCCESS;
}
