/** @file
 * CAN Device implementation for 10-m Antenna Encoder Module.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.15 $
 * $Date: 2011/01/03 18:48:06 $
 * $Id: Encoder.h,v 1.15 2011/01/03 18:48:06 iws Exp $
 */
#ifndef CARMA_ANTENNA_OVRO_CANBUS_ENCODER_H
#define CARMA_ANTENNA_OVRO_CANBUS_ENCODER_H

#include "carma/antenna/ovro/canbus/Drive.h"
#include "carma/canbus/devices/XacDevice.h"
#include "carma/services/Angle.h"
#include "carma/util/PthreadMutex.h"

namespace log4cpp {
    class Category;
} // namespace log4cpp

namespace carma {

namespace monitor {
    class Encoder;
    class OvroSubsystem;
    class StateMonitorPointEnum;
} // namespace monitor

namespace antenna {
namespace ovro {


    /**
     * 10-m Antenna Encoder Module CAN Implementation (API No. 232).
     */
    class Encoder : public carma::canbus::devices::XacDevice {
    public:
    
        enum Axis {
            AZIMUTH   = 1,
            ELEVATION = 2 };

        /**
         * Constructor
         * 
         */
        explicit Encoder( Axis axis, 
                          carma::canbus::CanOutput & canOut,
                          carma::monitor::OvroSubsystem & mon,
                          carma::antenna::ovro::Drive & drive );

        /**
         * Destructor 
         */
        virtual ~Encoder( );

        struct TimestampedPosition {
            TimestampedPosition( );
            double mjd;
            carma::services::Angle position;
        };

        /**
         * Retrieve position and MJD of message retrieval. 
         * Position updates <b><i>should</i></b> be sent from the encoder 
         * modules every frame on the frame.  Check the MJD to make sure the
         * reading is current.  The MJD is the time the message was read from
         * the canbus and processed.
         */
        TimestampedPosition getMostRecentPosition( ) const;
         
        /**
         * Get map of half second monitor points processed by this class.
         */
        carma::canbus::MsgIdInfoMap getHalfSecMonitors( ) const;

        /**
         * Get map of slow monitor points processed by this class.
         */
        carma::canbus::MsgIdInfoMap getSlowMonitors( ) const;
        
        /** 
         * Process CAN message.
         * @see carma::canbus::Device::processMsg
         */
        void processMsg( carma::canbus::msgType messageId,
                         ::std::vector< carma::canbus::byteType > & data,
                         bool sim );

        /**
         * Simulate CAN message.
         * @see carma::canbus::Device::simulateMsg
         */
        carma::canbus::Message simulateMsg( carma::canbus::msgType messageId );
        
        static const carma::canbus::msgType FAKE_AZ_ENCODER_PACKET = 0x114; 
        static const carma::canbus::msgType FAKE_EL_ENCODER_PACKET = 0x115; 
        
    protected:

    private:

        struct Shared {
            mutable carma::util::PthreadMutex mutex;
            struct TimestampedPosition position;
            Shared( );
        } shared_;

        void updateFrameData( );

        void processBlankingFramePacket1( carma::canbus::DataVector & data );
        void processBlankingFramePacket2( carma::canbus::DataVector & data );
        void processBlankingFramePacket3( carma::canbus::DataVector & data );

        // Simulation of the encoder module is shared by the ovro::Drive
        // module.  Since rates and voltages are sent to the drive module, 
        // it maintains simulated encoder readings based on these rates.
        // This is all handled by using the FAKE_AZ and EL message id for sims.
        friend carma::canbus::Message 
        Drive::simulateMsg( carma::canbus::msgType messageId );
        static carma::canbus::Message 
        simulateBlankingFramePacket1( Axis axis, double positionInDeg );

        carma::canbus::Message simulateBlankingFramePacket2( );
        carma::canbus::Message simulateBlankingFramePacket3( );

        const Axis axis_;

        const carma::canbus::MsgIdInfoMap halfSecMIDs_;
        
        carma::monitor::StateMonitorPointEnum & state_;
        carma::monitor::Encoder & encoderMon_;
        carma::monitor::Xac & xacMon_;
        carma::antenna::ovro::Drive & drive_;

    }; // class Encoder

}}} // namespace carma::antenna::ovro
#endif
