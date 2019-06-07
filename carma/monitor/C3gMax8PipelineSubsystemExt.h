
#ifndef CARMA_MONITOR_C3GMAX8PIPELINESUBSYSTEMEXT_H
#define CARMA_MONITOR_C3GMAX8PIPELINESUBSYSTEMEXT_H

/**
 * @file
 *
 * Semi-hand-forged extensions to the auto-generated classes for the 
 * C3gPipeline subsystem.  This file was originally 
 * created by mpml2cpp but then modified manually by the author.  
 *
 * @author: Marc pound
 *
 * $CarmaCopyright$
 *
 */

#include "carma/monitor/C3gMax8PipelineSubsystem.h"

namespace carma {
namespace monitor {

/**
 * @brief The monitor system for the C3gMax8Pipeline subsystem
 * 
 *
 * This extends the functionality of the auto-generated class, 
 * C3gMax8PipelineSubsystemBase.
 */
class C3gMax8PipelineSubsystem : public C3gMax8PipelineSubsystemBase 
{
public:

    /**
     * Constructor
     * @param buffer   pointer to system frame storage; NULL 
     *                         within subsystem
     */
    C3gMax8PipelineSubsystem(SystemFrameBuffer * const buffer = NULL);

    /**
     * Destructor  
     */
    virtual ~C3gMax8PipelineSubsystem();
    
    // add new or overriding method declarations here

};
}}

#endif  // CARMA_MONITOR_C3GMAX8PIPELINESUBSYSTEMEXT_H
