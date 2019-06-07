#ifndef CARMA_MONITOR_FRAME_PUBLISHER_THREAD_H
#define CARMA_MONITOR_FRAME_PUBLISHER_THREAD_H

/**
 * Publishes subsystem frames to the ACC, reading from an IPQ
 * to get a frame.
 *
 * Author: N. S. Amarnath
 * 
 */


#include <string>
#include <memory>

namespace carma {

namespace corba {
    class Client;
}

namespace monitor {


class FramePublisher;
class SubsystemFrameBuffer;
class MonitorSubsystem;


class FramePublisherThread {
    public:

        FramePublisherThread( MonitorSubsystem &  subsystem, 
                              const std::string & senderName,
                              carma::corba::Client & client );
        
        ~FramePublisherThread( );
        
        /**
         * Thread part of FramePublisherThread - passed to boost::thread ctor.
         */
        void operator()();

    private:

        /**
        * Puts subsystem frame data in transport
        * structure and ships it out via Notification Service
        *
        * @param buffer SubsystemFrameBuffer (IPQ) whose data
        *               is to be published.
        */
        void publishFrame( SubsystemFrameBuffer & buffer );
        
        /**
        * Private data member that
        * references the FramePublisher object used to publish
        * monitor frames.
        */
        ::std::auto_ptr< FramePublisher > dispatcher_;
        
        /**
        * Reference to the monitor subsystem whose data is to 
        * be published.
        */
        MonitorSubsystem & subsystem_;
        
        size_t publishCount_;
        size_t nextPublishLogCount_;
};


}  // End namespace monitor
} // End namespace carma


#endif
