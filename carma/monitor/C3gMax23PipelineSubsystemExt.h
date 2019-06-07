
#ifndef CARMA_MONITOR_C3GMAX23PIPELINESUBSYSTEMEXT_H
#define CARMA_MONITOR_C3GMAX23PIPELINESUBSYSTEMEXT_H

#include <iostream>

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

#include "carma/monitor/C3gMax23PipelineSubsystem.h"

namespace carma {
namespace monitor {

/**
 * @brief The monitor system for the C3gMax23Pipeline subsystem
 * 
 *
 * This extends the functionality of the auto-generated class, 
 * C3gMax23PipelineSubsystemBase.
 */
class C3gMax23PipelineSubsystem : 
    public carma::monitor::C3gMax23PipelineSubsystemBase 
{
public:

    /**
     * Constructor
     * @param buffer   pointer to system frame storage; NULL 
     *                         within subsystem
     */
    C3gMax23PipelineSubsystem(SystemFrameBuffer * const buffer = NULL);

    /**
     * Destructor  
     */
    virtual ~C3gMax23PipelineSubsystem();
    
    // add new or overriding method declarations here

};
}}

#endif  // CARMA_MONITOR_C3GMAX23PIPELINESUBSYSTEMEXT_H
