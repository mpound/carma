

#include "carma/monitor/C3gMax8PipelineSubsystemExt.h"
#include <iostream>

using namespace carma::monitor;
using namespace carma::util;
using namespace std;

C3gMax8PipelineSubsystem::C3gMax8PipelineSubsystem(SystemFrameBuffer * const buffer) : 
    C3gMax8PipelineSubsystemBase(buffer)
{
    //cout << "C3gMax8PipelineSubsystem:: Extended Constructor" << endl;
    lastIntegration().integNumber().setSnapshotAverage(true); 
}

C3gMax8PipelineSubsystem::~C3gMax8PipelineSubsystem()
{
    if (debug_) ::std::cout << "C3gMax8PipelineSubsystem destructor" << ::std::endl;
}

