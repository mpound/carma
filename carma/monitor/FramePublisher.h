#ifndef CARMA_MONITOR_FRAME_PUBLISHER_H
#define CARMA_MONITOR_FRAME_PUBLISHER_H

#include <string>

namespace carma  {

namespace corba {
    class Client;
}

namespace monitor  {

class TransportSubsystemFrame;

class FramePublisher {
public:

    explicit FramePublisher ( const std::string & channelName, 
                              const std::string & publisherName,
                              carma::corba::Client & client );

    /* virtual */ ~FramePublisher () ;

    void dispatchNotification (const TransportSubsystemFrame & frame);

    static void dumpFrame(const TransportSubsystemFrame & f); 

  private:
    
    // No copy
    FramePublisher( const FramePublisher & );
    FramePublisher & operator=( const FramePublisher & );

    const std::string channelName_;
    const std::string publisherName_;
    carma::corba::Client & client_;
};

} // end namespace monitor
} // end namespace carma
#endif
