/**
 * @file
 * $Id: MonitorSystemIPQwriter.h,v 1.9 2014/04/23 23:39:43 scott Exp $ 
 * Frame writer that incorporates a timer thread, used for writing
 * subsystem and system monitor frames.
 *
 * Author: N. S. Amarnath
 * 
 * Version: $Revision: 1.9 $ * $Date: 2014/04/23 23:39:43 $ 
 */

#ifndef CARMA_MONITOR_MONITOR_SYSTEM_IPQ_WRITER_H
#define CARMA_MONITOR_MONITOR_SYSTEM_IPQ_WRITER_H

#include "carma/monitor/FrameIPQwriter.h"

namespace carma {

namespace monitor {


class FrameCollatorThread;


class MonitorSystemIPQwriter : public FrameIPQwriter {
    public:
        MonitorSystemIPQwriter(
            double                 delayInS,
            FrameCollatorThread&   collatorThread,
            int                    clearDelayInFrames);

        ~MonitorSystemIPQwriter( );

    protected:

    private:

        /*
         * Write method for system frames & monitor systems
         */
        virtual void writeBuffer( );
        
        FrameCollatorThread &           collatorThread_;
        const double                    delayInS_;
        const int                       clearDelayInFrames_;
};

}  // End namespace monitor
} // End namespace carma

#endif
