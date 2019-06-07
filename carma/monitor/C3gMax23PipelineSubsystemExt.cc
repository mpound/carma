#include "carma/monitor/C3gMax23PipelineSubsystemExt.h"
#include <iostream>

using namespace carma::monitor;
using namespace carma::util;

C3gMax23PipelineSubsystem::C3gMax23PipelineSubsystem(SystemFrameBuffer * const buffer) : C3gMax23PipelineSubsystemBase(buffer)
{
    //cout << "C3gMax23PipelineSubsystem:: Extended Constructor" << endl;
    lastIntegration().integNumber().setSnapshotAverage(true); 
}

C3gMax23PipelineSubsystem::~C3gMax23PipelineSubsystem()
{
    if (debug_) ::std::cout << "C3gMax23PipelineSubsystem destructor" << ::std::endl;
}

