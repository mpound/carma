/**@file
 * carma::canbus::devices::XacDevice class declaration.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.10 $
 * $Date: 2011/08/25 20:53:19 $
 * $Id: XacDevice.h,v 1.10 2011/08/25 20:53:19 abeard Exp $
 */
#ifndef CARMA_CANBUS_DEVICES_XACDEVICE_H
#define CARMA_CANBUS_DEVICES_XACDEVICE_H

// Carma includes
#include "carma/canbus/Device.h"

#include "carma/util/types.h"
    
namespace carma {

namespace monitor {
    // Forward dec
    class Xac;
} // End namespace monitor

namespace canbus {
namespace devices {

    /**
     * XacDevice canbus::device class implementation.
     * This class implements the carma::canbus::Device base class for 
     * devices with XAC processors which transmit the standard 
     * 'System Monitor Packets' as defined in the Carma CANbus API Template. 
     * If your CAN module transmits these System Monitor Packets, you can
     * inherit from this device and you won't need to define how to unpack
     * those messages. The class implements several base control commands as 
     * well.  
     */
    class XacDevice : public carma::canbus::Device {
    public:

        /**
         * Constructor.
         * Creates an XacDevice
         * @param api unique code identifying a specific set of instructions
         * and CAN message formats for a specific device.
         * @param node Node id of device.
         * @param canOutput Reference to CanOutput class - should be obtained
         * from downcasting the canbus::Master reference in inherited 
         * canbus::Master class.
         */
        XacDevice(carma::canbus::apiType api, carma::canbus::nodeType node,
            carma::canbus::CanOutput &canOutput);

        /**
         * Destructor
         */
        virtual ~XacDevice();
        
        /**
         * Return a map of slow monitor points supported by this class
         * The XacDevice class supports processing of the standard
         * System Monitor Packets defined in CANbus API docs.
         * @return map<msgType, string> map containing a string description
         * of the CAN message keyed by it's message id.
         * @see Device::simulateMsg
         * @see carma::canbus::Master
         * @see carma::canbus::Device::getSlowMonitors
         */
        virtual std::map<msgType, std::string> getSlowMonitors() const;

        /**
         * Set the state of the device.  
         * This function overrides the Device::setState version and 
         * simply calls the base version while adding logging functionality.
         */
        virtual void setState(deviceStateType state); 
        
        // Standard System Monitor Packets supported in this class
        static const carma::canbus::msgType SYSTEM_MONITOR_PACKET_1  = 0x120;
        static const carma::canbus::msgType SYSTEM_MONITOR_PACKET_2  = 0x121;
        static const carma::canbus::msgType SYSTEM_MONITOR_PACKET_3  = 0x122;
        static const carma::canbus::msgType SYSTEM_MONITOR_PACKET_4  = 0x123;
        static const carma::canbus::msgType SYSTEM_MONITOR_PACKET_5  = 0x124;

    protected:
    
        /**
         * Initialization hook.
         * This get's called when the xac slow monitor packet 1 'initialization 
         * request' flag is set.
         */
        virtual void initialize( );

        /**
         * Check that input message type is a system monitor packet.
         */
        static bool isSystemMonitorPacket( carma::canbus::msgType );

        /**
         * Process a system monitor packet.
         */
        void processSystemMonitorPacket( 
            carma::canbus::msgType mid,
            carma::canbus::DataVector & data,
            carma::monitor::Xac & xac );

        /**
         * Process System Monitor Packet 1
         * This routine processes standard system monitor packet 1 and
         * places the data into the input Xac reference.  Note that this 
         * routine will set the device state to carma::canbus::STARTING if
         * the INITREQ flag is set in the input packet.
         * @param data vector containing bytes of raw data.
         * @param xac monitor system reference
         */
        void processSystemMonitorPacket1(
            carma::canbus::DataVector &data, 
            carma::monitor::Xac& xac);

        /**
         * Process System Monitor Packet 2
         * @param data vector containing bytes of raw data.
         * @param xac monitor system reference
         */
        void processSystemMonitorPacket2(
            carma::canbus::DataVector &data,
            carma::monitor::Xac& xac);

        /**
         * Process System Monitor Packet 3 
         * @param data vector containing bytes of raw data.
         * @param xac monitor system reference
         */
        void processSystemMonitorPacket3(
            carma::canbus::DataVector &data,
            carma::monitor::Xac& xac);

        /**
         * Process System Monitor Packet 4
         * @param data vector containing bytes of raw data.
         * @param xac monitor system reference
         */
        void processSystemMonitorPacket4(
            carma::canbus::DataVector &data,
            carma::monitor::Xac& xac);

        /**
         * Process System Monitor Packet 5
         * @param data vector containing bytes of raw data.
         * @param xac monitor system reference
         */
        void processSystemMonitorPacket5(
            carma::canbus::DataVector &data,
            carma::monitor::Xac& xac);
            
        /**
         * Simulate system monitor packet.
         * @throw IllegalArgumentException if mid is not a system monitor msg.
         */
        carma::canbus::Message simSystemMonitorPacket( 
            carma::canbus::msgType mid );

        /**
         * Simulate System Monitor Packet 1
         */
        carma::canbus::Message simSystemMonitorPacket1();
        
        /**
         * Simulate System Monitor Packet 2
         */
        carma::canbus::Message simSystemMonitorPacket2();
        
        /**
         * Simulate System Monitor Packet 3
         */
        carma::canbus::Message simSystemMonitorPacket3();
        
        /**
         * Simulate System Monitor Packet 4
         */
        carma::canbus::Message simSystemMonitorPacket4();

        /**
         * Simulate System Monitor Packet 5
         */
        carma::canbus::Message simSystemMonitorPacket5();

    public:

        /**
         * Reset the can module via CORBA using Device::softReset() method.
         */
        void reset();

        /**
         * Stop fast sampling on channel one
         */
        void stopChannelOneFastSampling();

        /**
         * Stop fast sampling on channel two
         */
        void stopChannelTwoFastSampling();

        /**
         * Start fast sampling on channel one with the requested data item
         */
        void startChannelOneFastSampling(unsigned short fastItem);

        /**
         * Start fast sampling on channel two with the requested data item
         */
        void startChannelTwoFastSampling(unsigned short fastItem);

        /**
         * Is Device ONLINE?
         */
        bool isOnline();

    private:
        
        // Prevent assignment and copy construction
        XacDevice(const XacDevice &);
        XacDevice &operator=(const XacDevice &);

        const carma::util::frameType startFrame_;

    }; // End XacDevice
} // End devices 
} // End canbus
} // End carma
#endif
