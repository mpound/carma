
/**
 *
 * Implementation for SlPipeline monitor subsystem.
 * This is an auto-generated stub to which a developer may have added 
 * additional implementation manually.
 *
 * @author: not specified
 *
 * $CarmaCopyright$
 *
 */
#include "carma/monitor/SlPipelineSubsystemExt.h"

#include <iostream>

using namespace std;
using namespace carma::monitor;
using namespace carma::util;


//------------ SlPipelineSubsystem ------------------------------

SlPipelineSubsystem::SlPipelineSubsystem(SystemFrameBuffer * const buffer) : 
    SlPipelineSubsystemBase(buffer)
{
    //cout << "SlPipelineSubsystem:: Extended Constructor" << endl;
    lastIntegration().integNumber().setSnapshotAverage(true); 
}

SlPipelineSubsystem::~SlPipelineSubsystem()
{
    if (debug_) cout << "SlPipelineSubsystem destructor" << endl;
}

// add additional functionality for SlPipelineSubsystem here





      
