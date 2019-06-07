
#ifndef CARMA_MONITOR_WBPIPELINESUBSYSTEMEXT_H
#define CARMA_MONITOR_WBPIPELINESUBSYSTEMEXT_H

/**
 * @file
 *
 * Semi-hand-forged extensions to the auto-generated classes for the 
 * WbPipeline subsystem.  This file was originally 
 * created by mpml2cpp but then modified manually by the author.  
 *
 * @author: not specified
 *
 * $CarmaCopyright$
 *
 */

#include "carma/monitor/WbPipelineSubsystem.h"

namespace carma {
namespace monitor {

/**
 * @brief The monitor system for the WbPipeline subsystem
 * 
 *
 * This extends the functionality of the auto-generated class, 
 * WbPipelineSubsystemBase.
 */
class WbPipelineSubsystem : 
    public carma::monitor::WbPipelineSubsystemBase 
{
public:

    /**
     * Constructor
     * @param buffer   pointer to system frame storage; NULL 
     *                         within subsystem
     */
    WbPipelineSubsystem(SystemFrameBuffer * const buffer = NULL);

    /**
     * Destructor  
     */
    virtual ~WbPipelineSubsystem();
    
    // add new or overriding method declarations here

};
}}

#endif  // CARMA_MONITOR_WBPIPELINESUBSYSTEMEXT_H
      
