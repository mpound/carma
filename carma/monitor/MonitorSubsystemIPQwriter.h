/**
 * @file
 * $Id: MonitorSubsystemIPQwriter.h,v 1.9 2011/08/01 20:24:40 abeard Exp $ 
 * Frame writer that incorporates a timer thread, used for writing
 * subsystem and system monitor frames.
 *
 * Author: N. S. Amarnath
 * 
 */

#ifndef CARMA_MONITOR_MONITOR_SUBSYSTEM_IPQ_WRITER_H
#define CARMA_MONITOR_MONITOR_SUBSYSTEM_IPQ_WRITER_H

#include "carma/monitor/FrameIPQwriter.h"
#include "carma/monitor/MonitorSystem.h"


namespace boost {
    class mutex;
} // namespace boost

namespace carma  {
namespace monitor  {

class MonitorSubsystemIPQwriter : public FrameIPQwriter {
    public:
        explicit MonitorSubsystemIPQwriter(
            MonitorSubsystem & system, 
            const double       delayInS,
            boost::mutex & frameBufferMutex );
        
        ~MonitorSubsystemIPQwriter( );
        
        void writeBuffer( );
        
    private:

        /*
        * Write method for system frames & monitor systems
        */
        void writeSubsystem( );

        MonitorSubsystem * const monSubsystem_;
        
        const double delayInS_;
        bool notifiedOfAutowriteScriberDelayError_;
            
        boost::mutex & frameBufferMutex_;
};


}  // End namespace monitor
} // End namespace carma


#endif	// CARMA_MONITOR_MONITOR_SUBSYSTEM_IPQ_WRITER_H
