//!
//! @file
//! Publishes subsystem frames to the ACC, reading from an IPQ
//! to get a frame.
//!
//! Author: N. S. Amarnath
//!

#ifndef CARMA_MONITOR_FRAME_COLLATOR_THREAD_H
#define CARMA_MONITOR_FRAME_COLLATOR_THREAD_H

#include "carma/monitor/FrameSubscriber.h"

namespace carma  {

namespace corba {
    class Server;
} 

namespace monitor  {


class MonitorSystem;


class FrameCollatorThread {
    public:

        explicit FrameCollatorThread(const ::std::string& subscriberName,
                                     double                delayInS,
                                     bool                  rawMode,
                                     carma::corba::Server& server);
    
        virtual ~FrameCollatorThread( );
        
        void writeMonitorSystemToIPQ(double nextFireTimeMJD,
                                     double delayInSeconds,
                                     int    clearDelayInFrames);

        void setFirstFireTime( double firstFireTimeMJD );
        
        //! functor definition for CollatorThread to be passed to boost::thread.
        //! Retrieves notification, and writes into carma system buffer
        void operator( ) ( );

    private:
        //! references the FrameSubscriber object used to collect
        //! monitor frames.
        FrameSubscriber * subscriber_;
        carma::corba::Server & server_;
};


} // namespace carma::monitor
} // namespace carma

#endif
