#ifndef CARMA_MONITOR_PIPELINESUBSYSTEMWB_H
#define CARMA_MONITOR_PIPELINESUBSYSTEMWB_H

/**
 * @file PipelineSubsystemWB.h
 * 
 * Tagged: Tue Aug 16 16:46:25 PDT 2005
 * 
 * @author Rick Hobbs
 * @author Andrew Beard (Templatized)
 */

#include "carma/monitor/PipelineSubsystemTemplate.h"
#include "carma/monitor/WbPipelineSubsystemExt.h"

namespace carma {
namespace monitor {

    typedef PipelineSubsystemTemplate<WbPipelineSubsystem> PipelineSubsystemWB;

  } // End namespace monitor
} // End namespace carma
#endif // End #ifndef CARMA_MONITOR_PIPELINESUBSYSTEMWB_H
