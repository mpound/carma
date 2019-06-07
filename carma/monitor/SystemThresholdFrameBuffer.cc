/*
 * SystemThresholdFrameBuffer.cc - Method definitions for class 
 * holding thresholds for carma monitor system.
 */

/**
 * $Id: SystemThresholdFrameBuffer.cc,v 1.8 2007/08/24 14:18:35 tcosta Exp $
 */

/*!
 * @file SystemThresholdFrameBuffer.cc
 * This is the method definition file for the threshold IPQ class.
 *
 * @author N. S. Amarnath
 *
 * File containing method definitions for SystemThresholdFrameBuffer class.
 *
 */
#include "carma/monitor/SystemThresholdFrameBuffer.h"

#include "carma/util/IPQbuffer.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/SystemFrameBuffer.h"
#include "carma/monitor/SystemThresholdFrame.h"

using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::util;


class SystemThresholdFrameBuffer::InternalIpq : public IPQbuffer {
    public:
        InternalIpq( ThresholdFrameStruct & frame,
                     size_t                 frameSize );
};


SystemThresholdFrameBuffer::InternalIpq::InternalIpq(
    ThresholdFrameStruct & frame,
    const size_t           frameSize ) :
IPQbuffer( &frame,
           frameSize,
           "CarmaSysThr",
           true,
           2 ) {
    init();
}


SystemThresholdFrameBuffer::SystemThresholdFrameBuffer( MonitorSystem & ms ) :
SystemThresholdFrame( ms )
{
    ThresholdFrameStruct & frame = getThresholdFrame();

    const size_t frameSize =
        SystemThresholdFrame::sizeThresholdFrame(
            ms.systemFrameBuffer().getMaxTotalMonitorPoints() );
            
    ipq_ = auto_ptr< InternalIpq >( new InternalIpq( frame, frameSize ) );
}


SystemThresholdFrameBuffer::~SystemThresholdFrameBuffer( )
try {
} catch ( ... ) {
    // Just stifle any exceptions
    
    return;
}


bool
SystemThresholdFrameBuffer::readNewest( )
{
    const bool readSuccessful = ipq_->readNewest();
    
    if ( readSuccessful )
        SystemThresholdFrame::fixupFramePointers( getThresholdFrame() );

    return readSuccessful;
}


void
SystemThresholdFrameBuffer::calibrateMonitorSystem( )
{
    readNewest();

    SystemThresholdFrame::calibrateMonitorSystem();
}
