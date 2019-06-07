
/**
 *
 * Implementation for WbPipeline monitor subsystem.
 * This is an auto-generated stub to which a developer may have added 
 * additional implementation manually.
 *
 * @author: not specified
 *
 * $CarmaCopyright$
 *
 */
#include "carma/monitor/WbPipelineSubsystemExt.h"

#include <iostream>

using namespace std;
using namespace carma::monitor;
using namespace carma::util;


//------------ WbPipelineSubsystem ------------------------------

WbPipelineSubsystem::WbPipelineSubsystem(SystemFrameBuffer * const buffer) : 
    WbPipelineSubsystemBase(buffer)
{
    //cout << "WbPipelineSubsystem:: Extended Constructor" << endl;
    lastIntegration().integNumber().setSnapshotAverage(true); 
}

WbPipelineSubsystem::~WbPipelineSubsystem()
{
    if (debug_) cout << "WbPipelineSubsystem destructor" << endl;
}

// add additional functionality for WbPipelineSubsystem here





      
