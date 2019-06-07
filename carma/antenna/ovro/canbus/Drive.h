/** @file
 * CAN Device implementation for 10-m Antenna Drive Module.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.30 $
 * $Date: 2014/09/02 22:21:13 $
 * $Id: Drive.h,v 1.30 2014/09/02 22:21:13 scott Exp $
 */
#ifndef CARMA_ANTENNA_OVRO_CANBUS_DRIVE_H
#define CARMA_ANTENNA_OVRO_CANBUS_DRIVE_H

#include "carma/canbus/devices/XacDevice.h"
#include "carma/monitor/OvroSubsystem.h"

namespace carma {

namespace services {
    class Angle;
    class Table;
} // End namespace services

namespace antenna {
namespace ovro {

    /**
     * 10-m Antenna Drive Module CAN Implementation (API No. 248).
     * All commands which move an antenna must be called every 1/2 second.
     * If this isn't done, the drive system will timeout, stop and apply the
     * breaks.  This class internally reads from a configuration file which
     * contains important parameters such as DC offsets, rate/voltage gains and
     * max slew rates.  The filename is currently conf/antenna/ovro/drive.conf.
     */
    class Drive : public carma::canbus::devices::XacDevice {
    public:

        /** 
         * Constructor
         * @param canOutput Reference to CanIo or CanOutput instance.
         * @param monitorSubsys Reference to Ovro monitor subsystem instance.
         * @param shareWithVax When true, don't write monitor points which 
         *      typically come from the vax.
         * @param confFilename Name of configuration file.
         */
        explicit Drive( carma::canbus::CanOutput & canOutput,
                        carma::monitor::OvroSubsystem & monitorSubsys,
                        unsigned short antNo,
                        const std::string confFilename = 
                            "antenna/ovro/drive.conf" );

        /**
         * Destructor
         */
        virtual ~Drive( );

        /**
         * Set azimuth and elevation drive rates.
         * @param azRateInRadPerS Requested azimuth drive rate in rad/s.
         * @param elRateInRadPerS Requested elevation drive rate rad/s.
         * @param elevation Current elevation in radians.
         */
        void setAzElDriveRates( double azRateInRadPerS, 
                                double elRateInRadPerS, 
                                const carma::services::Angle & elevation );

        /**
         * Stop the drives
         * This is just a shortcut for enabling the drives followed by 
         * seting the drive rates to 0.
         */
        void stop( );

        /**
         * Timestamped az overlap state.
         * The state of the az overlap switch is critical to properly 
         * updating the actual position of the antenna.  The state 
         * must be valid for a given frame in order to proceed with
         * pointing updates.  This structure exposes the information needed
         * by the DriveEngine to correctly determine the validity of this
         * state.
         */
        struct TimestampedAzOverlapState {
            TimestampedAzOverlapState( );
            double mjd;
            bool azOverlapDetected;
        };

        /**
         * Retrieve boolean indicating if azimuth is in the overlap region.
         * Overlap is the angular position from roughly 0 to -80 degrees
         * where the positive-only encoder readings 'overlap'.
         * @return TimestampedAzOverlapState
         */
        TimestampedAzOverlapState getAzOverlapState( ) const;


        /**
         * Retrieve boolean indicating if any of the three drive controllers
         * shows an overtemp fault.
         */
        bool getControllerOvertemp() const;

        /**
         * Retrieve boolean indicating if drive is enabled.
         */
        bool isDriveEnabled( ) const;

        /**
         * Retrieve boolean indicating if drive is at a hw limit.
         * Note this is a 'processed' limit boolean - in certain cases
         * all drive limits may report true, even though no hardware limit
         * exists.  These conditions are when the drive system is disabled
         * or the local/remote/off switch is in the OFF state.  In these
         * instances, this routine returns false.
         */
        bool isAtHwLimit( ) const;

        /**
         * Is module in engineering mode?
         */
        bool isInEngineeringMode( ) const;

        /**
         * Enum describing base switch state.
         */
        enum BaseSwitchState {
            OFF,            // Off (disabled)
            REMOTE_CONTROL, // Computer control
            LOCAL_CONTROL   // Handpaddle control
        };
        
        BaseSwitchState getBaseSwitchState( ) const;

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

        /**
         * Set raw drive voltages for engineering purposes.
         * Use with caution.  DC offsets are not applied in this mode.
         * @param azRawVolts Raw azimuth voltage in volts.
         * @param elRawVolts Raw elevation voltage in volts.
         */
        void setRawDriveVoltages( float azRawVolts, float elRawVolts );

        /** 
         * Set engineering mode.
         * @param enable True to enable engineering mode, false to disable.
         */
        void setEngineeringMode( bool enable );

        /**
         * Update configuration parameters from conf file.
         * The location of the configuration file is specified as a 
         * constructor parameter to this class.
         * @throw carma::util::FileNotFoundException if config file isn't found.
         * @throw carma::util::IllegalStateException if file is corrupt.
         */
        void updateConfDataFromFile( );

        /**
         * Write monitor data.
         * Write CANbus independent monitor data, this is data which is either
         * calculated or updated from control commands rather than events
         * received from the CANBus.
         */
        void writeMonitorData( );

    protected:

    private:
        
        void enableDrives( bool enable );

        void setDriveVoltages( float azVolts, float elVolts );

        void setMaxVoltage( float azMaxVolts, float elMaxVolts );

        void initialize( ); // Issued in response to an initReq

        void updateFrameData( );

        void processBlankingFramePacket1( carma::canbus::DataVector & data ); 
        void processBlankingFramePacket2( carma::canbus::DataVector & data ); 
        void processBlankingFramePacket3( carma::canbus::DataVector & data ); 
        void processBlankingFramePacket4( carma::canbus::DataVector & data ); 
        void processBlankingFramePacket5( carma::canbus::DataVector & data ); 
        void processBlankingFramePacket6( carma::canbus::DataVector & data ); 
        void processBlankingFramePacket7( carma::canbus::DataVector & data ); 

        carma::canbus::Message simulateBlankingFramePacket1( ); 
        carma::canbus::Message simulateBlankingFramePacket2( ); 
        carma::canbus::Message simulateBlankingFramePacket3( ); 
        carma::canbus::Message simulateBlankingFramePacket4( ); 
        carma::canbus::Message simulateBlankingFramePacket5( ); 
        carma::canbus::Message simulateBlankingFramePacket6( ); 
        carma::canbus::Message simulateBlankingFramePacket7( ); 
        
        carma::monitor::OvroSubsystem & mon_;
        carma::monitor::OvroSubsystem::DriveModule & driveMon_;
        carma::monitor::OvroSubsystem::Limit & limitMon_;
        carma::monitor::OvroSubsystem::Track & trackMon_;
        carma::monitor::AntennaCommon & comMon_;

        const unsigned short antNo_;
        const ::std::string configFilename_;

        struct SimInfo;
        struct Shared;
        std::auto_ptr< Shared > shared_;
        std::auto_ptr< carma::services::Table > table_;

    }; // class Drive

}}} // namespace carma::antenna::ovro
#endif
