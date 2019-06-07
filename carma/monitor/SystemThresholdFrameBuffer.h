#ifndef CARMA_SYSTEM_THRESHOLD_FRAME_BUFFER_H
#define CARMA_SYSTEM_THRESHOLD_FRAME_BUFFER_H

/*
 * SystemThresholdFrameBuffer.h - Class holding thresholds for carma monitor system.
 */

/*!
 * @file SystemThresholdFrameBuffer.h
 * This is the class declaration file for the IPQ version of the threshold 
 * frame for a monitor system.
 *
 * @author N. S. Amarnath
 *
 * File containing declarations for SystemThresholdFrameBuffer class.
 *
 */

#include <memory>

#include "carma/monitor/SystemThresholdFrame.h"

namespace carma {
namespace monitor {


/**
 * Class that manages the IPQ version of SystemThresholdFrame.
 *
 * Order of construction is very important here. The IPQbuffer
 * stores the information in the SystemThresholdFrame object in the IPQ, so 
 * the SystemThresholdFrame <b>must be constructed before</b> the IPQbuffer,
 * otherwise the IPQ buffer will have nothing to store at the time of 
 * construction.
 *
 * @see SystemThresholdFrame.
 * @see ::carma::util::IPQbuffer.
 */
class SystemThresholdFrameBuffer : public SystemThresholdFrame {
  public:

    /**
     * Constructor - Order of class construction is crucial here.
     * Base classes are constructed bottom up, so SystemThresholdFrame
     * is constructed first, then IPQbuffer and, finally, the 
     * SystemThresholdBuffer class instance.
     *
     * @param monitorSystem SystemThreshold is bound to this 
     *        MonitorSystem object.
     */
    explicit SystemThresholdFrameBuffer( MonitorSystem & monitorSystem );


    /**
     * Destructor - deletes frame structure if deleteStruct flag was true
     * when the object was constructed.
     *
     * @see SystemThresholdFrameBuffer::SystemThresholdFrameBuffer
     */
    virtual ~SystemThresholdFrameBuffer();

    /**
     * Reads latest threshold values from IPQ and sets threshold related 
     * validity flags for all monitor points in the MonitorSystem.
     * @return none
     */
    virtual void calibrateMonitorSystem( );

  private:
    // No copying
    SystemThresholdFrameBuffer( SystemThresholdFrameBuffer & );
    SystemThresholdFrameBuffer & operator=( SystemThresholdFrameBuffer & );

    bool readNewest();

    class InternalIpq;
  
    ::std::auto_ptr< InternalIpq > ipq_;
};


}  // namespace carma::monitor
}  // namespace carma

#endif
