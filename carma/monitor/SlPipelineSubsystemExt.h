
#ifndef CARMA_MONITOR_SLPIPELINESUBSYSTEMEXT_H
#define CARMA_MONITOR_SLPIPELINESUBSYSTEMEXT_H

/**
 * @file
 *
 * Semi-hand-forged extensions to the auto-generated classes for the 
 * SlPipeline subsystem.  This file was originally 
 * created by mpml2cpp but then modified manually by the author.  
 *
 * @author: not specified
 *
 * $CarmaCopyright$
 *
 */

#include "carma/monitor/SlPipelineSubsystem.h"

namespace carma {
namespace monitor {

/**
 * @brief The monitor system for the SlPipeline subsystem
 * 
 *
 * This extends the functionality of the auto-generated class, 
 * SlPipelineSubsystemBase.
 */
class SlPipelineSubsystem : 
    public carma::monitor::SlPipelineSubsystemBase 
{
public:

    /**
     * Constructor
     * @param buffer   pointer to system frame storage; NULL 
     *                         within subsystem
     */
    SlPipelineSubsystem(SystemFrameBuffer * const buffer = NULL);

    /**
     * Destructor  
     */
    virtual ~SlPipelineSubsystem();
    
    // add new or overriding method declarations here

};
}}

#endif  // CARMA_MONITOR_SLPIPELINESUBSYSTEMEXT_H
      
