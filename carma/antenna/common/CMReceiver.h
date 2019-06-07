#ifndef CARMA_ANTENNA_COMMON_CMRECEIVER_H
#define CARMA_ANTENNA_COMMON_CMRECEIVER_H

/**
 * @file CMReceiver.h
 * Definition for carma::antenna::common::CMReceiver class. 
 * 
 * @author Doug Friedel - Original Bima implementation.
 * @author Andy Beard - antenna::commonization and full API implementation.
 */

#include "carma/canbus/devices/XacDevice.h"
#include "carma/canbus/Types.h"

#include "carma/monitor/BimaSubsystem.h"
#include "carma/monitor/OvroSubsystem.h"

namespace carma{

namespace monitor {
    class RxBias;
}

namespace antenna {
namespace common {
      
class CMReceiver : public carma::canbus::devices::XacDevice {
public:
	
	/**
	 * Constructor.
	 */
	CMReceiver( carma::canbus::nodeType node,
                carma::canbus::CanOutput &io,
                carma::monitor::RxBias & rxBias, 
                carma::monitor::OvroSubsystem::RxBiasTemps * ovroRxBiasTemps );
	
	CMReceiver( carma::canbus::nodeType node,
                carma::canbus::CanOutput &io,
                carma::monitor::RxBias & rxBias, 
                carma::monitor::BimaSubsystem::RxBiasTemps * bimaRxBiasTemps );
	/**
	 * Destructor.
	 */
	~CMReceiver();

    // Control commands
    void set30GHzDrainVoltage( unsigned short stage, float volts );
    void set30GHzDrainCurrent( unsigned short stage, float milliAmps );
    void set30GHzIFDrainCurrent( float milliAmps );
    void set90GHzDrainVoltage( unsigned short amplifier, float volts );
    void set90GHzGateVoltage( unsigned short amplifier, unsigned short stage,
                              float volts );
    void set90GHzIFDrainVoltage( float volts );
    void set90GHzIFGateVoltage( float volts );
    void sendGuardMessage();

    // Device interface implementation
	void processMsg( carma::canbus::msgType mid,
                     carma::canbus::DataVector &data,
                     bool sim);

    carma::canbus::Message simulateMsg( carma::canbus::msgType mid );

    carma::canbus::MsgBriefMap getHalfSecMonitors( ) const;

	carma::canbus::MsgBriefMap getSlowMonitors() const;

protected:

    // No protected members.

private:

	void updateFrameData();
	carma::canbus::Message createMsgToChan(int chanNo, carma::canbus::msgType msgID);

	void processBlankingFramePacket1( carma::canbus::DataVector & data );
	void processBlankingFramePacket2( carma::canbus::DataVector & data );
	void processBlankingFramePacket3( carma::canbus::DataVector & data );
	void processBlankingFramePacket4( carma::canbus::DataVector & data );
	void processBlankingFramePacket5( carma::canbus::DataVector & data );
	void processBlankingFramePacket6( carma::canbus::DataVector & data );
	void processBlankingFramePacket7( carma::canbus::DataVector & data );
	void processBlankingFramePacket8( carma::canbus::DataVector & data );
	void processBlankingFramePacket9( carma::canbus::DataVector & data );
	void processBlankingFramePacket10( carma::canbus::DataVector & data );
	void processBlankingFramePacket11( carma::canbus::DataVector & data );

	carma::canbus::Message simBlankingFramePacket1( );
	carma::canbus::Message simBlankingFramePacket2( );
	carma::canbus::Message simBlankingFramePacket3( );
	carma::canbus::Message simBlankingFramePacket4( );
	carma::canbus::Message simBlankingFramePacket5( );
	carma::canbus::Message simBlankingFramePacket6( );
	carma::canbus::Message simBlankingFramePacket7( );
	carma::canbus::Message simBlankingFramePacket8( );
	carma::canbus::Message simBlankingFramePacket9( );
	carma::canbus::Message simBlankingFramePacket10( );
	carma::canbus::Message simBlankingFramePacket11( );

    carma::monitor::RxBias & rxMon_;
    carma::monitor::OvroSubsystem::RxBiasTemps * ovroRxBiasTemps_;
    carma::monitor::BimaSubsystem::RxBiasTemps * bimaRxBiasTemps_;

}; // class CMReceiver
      
}}} // namespace carma::antenna::common
#endif // #ifndef CARMA_ANTENNA_COMMON_CMRECEIVER_H
