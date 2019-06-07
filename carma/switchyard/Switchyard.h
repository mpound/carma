#ifndef CARMA_SWITCHYARD_SWITCHYARD_H
#define CARMA_SWITCHYARD_SWITCHYARD_H

#include "carma/canbus/devices/XacDevice.h"

#include <map>

namespace carma {

namespace monitor {
    class StateMonitorPointEnum;
    class Switchyard;
    class Xac;
} // namespace monitor

namespace switchyard {

const carma::canbus::nodeType IFSWITCHYARD_NODE = 1; 
const carma::canbus::nodeType LOSWITCHYARD_NODE = 2; 
const carma::canbus::nodeType LLSWITCHYARD_NODE = 3;
const carma::canbus::nodeType DCLOSWITCHYARD_NODE = 4;

typedef std::map< unsigned short, unsigned short > SwitchMap;

class Switchyard : public carma::canbus::devices::XacDevice {
public:


    explicit Switchyard( carma::canbus::nodeType node,
                         carma::canbus::CanOutput & io, 
                         carma::monitor::StateMonitorPointEnum & state,
                         carma::monitor::Switchyard & switchyard, 
                         carma::monitor::Xac & xac );

    virtual ~Switchyard( );

    void setSwitches( const SwitchMap & switchPosMap ); 
    
    carma::canbus::MsgBriefMap getHalfSecMonitors( ) const;
    carma::canbus::MsgBriefMap getSlowMonitors( ) const;

    void processMsg( carma::canbus::msgType mid,
                     carma::canbus::DataVector & data,
                     bool sim );

    carma::canbus::Message simulateMsg( carma::canbus::msgType mid );

    void updateFrameData( );

    static carma::canbus::apiType getApiId( );

private:

    // Prevent copy and assignment
    Switchyard( const Switchyard & );
    Switchyard &operator=( const Switchyard &);
    
    void processSwitchPosPacket( const carma::canbus::msgType mid,  
                                 carma::canbus::DataVector & data );
    void processBlankingFramePacket6( carma::canbus::DataVector & data );
    void processBlankingFramePacket7( carma::canbus::DataVector & data );
    
    carma::canbus::Message 
    simulateSwitchPosPacket( const carma::canbus::msgType mid );
    carma::canbus::Message simulateBlankingFramePacket6( );
    carma::canbus::Message simulateBlankingFramePacket7( );

    carma::monitor::StateMonitorPointEnum & state_;
    carma::monitor::Switchyard & switchyard_;
    carma::monitor::Xac & xac_;

}; // class Switchyard

}} // namespace carma::switchyard 
#endif
