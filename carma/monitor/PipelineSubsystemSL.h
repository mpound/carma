#ifndef CARMA_MONITOR_PIPELINESUBSYSTEMSL_H
#define CARMA_MONITOR_PIPELINESUBSYSTEMSL_H

/**
 * @file PipelineSubsystemSL.h
 * 
 * Tagged: Tue Aug 16 16:46:25 PDT 2005
 * 
 * @author Rick Hobbs
 * @author Andrew Beard (Templatized)
 */

#include "carma/monitor/PipelineSubsystemTemplate.h"
#include "carma/monitor/SlPipelineSubsystemExt.h"

namespace carma {
namespace monitor {

    typedef PipelineSubsystemTemplate<SlPipelineSubsystem> PipelineSubsystemSL;

} // End namespace monitor
} // End namespace carma
#endif // End #ifndef CARMA_MONITOR_PIPELINESUBSYSTEMSL_H
