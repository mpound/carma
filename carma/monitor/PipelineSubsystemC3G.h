#ifndef CARMA_MONITOR_PIPELINESUBSYSTEMC3G_H
#define CARMA_MONITOR_PIPELINESUBSYSTEMC3G_H

#include "carma/monitor/PipelineSubsystemTemplate.h"
#include "carma/monitor/C3gMax23PipelineSubsystemExt.h"
#include "carma/monitor/C3gMax8PipelineSubsystemExt.h"

namespace carma {
namespace monitor {

    typedef PipelineSubsystemTemplate< C3gMax8PipelineSubsystem > 
        PipelineSubsystemC3gMax8;
    typedef PipelineSubsystemTemplate< C3gMax23PipelineSubsystem > 
        PipelineSubsystemC3gMax23;

}} // namespace carma::monitor
#endif //CARMA_MONITOR_PIPELINESUBSYSTEMC3G_H
