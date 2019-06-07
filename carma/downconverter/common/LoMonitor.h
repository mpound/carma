/**@file
 * LoMonitor Device class implementation for Carma CANbus API 192.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.22 $
 * $Date: 2013/04/19 17:47:00 $
 * $Id: LoMonitor.h,v 1.22 2013/04/19 17:47:00 abeard Exp $
 */

#ifndef CARMA_DOWNCONVERTER_LOMONITOR_H
#define CARMA_DOWNCONVERTER_LOMONITOR_H

// C++ Standard library include
#include <map>
#include <string>
#include <vector>

// Carma includes
#include "carma/canbus/devices/XacDevice.h"

namespace log4cpp {
    // Forward declaration
    class Category;
} // End namespace log4cpp

namespace carma {

namespace monitor {
    class LoMonitor;
    class StateMonitorPointEnum;
    class Xac;
} // End namespace monitor

namespace downconverter {

    /**
     * LoMonitor Device class implementation.
     * This class implements the carma::canbus::Device base class for
     * Carma CAN API 192 describing the LoMonitor CAN device.
     */
    class LoMonitor : public carma::canbus::devices::XacDevice {
    public:

        /**
         * Constructor
         * Creates an LoMonitor device with the given node id.
         * @param node Node id of device.
         * @param io Reference to CanOutput class.
         */
        LoMonitor(
            carma::canbus::nodeType node,
            carma::canbus::CanOutput &io,
            carma::monitor::StateMonitorPointEnum & state,
            carma::monitor::LoMonitor & lomon,
            carma::monitor::Xac & xac,
            bool wideband = false );

        /**
         * Destructor
         */
        virtual ~LoMonitor();

        /**
         * Retrieve a map of this devices half second monitor points.
         * The monitor points returned from this routine will be
         * simulated if the device is in the OFFLINE state.
         * @return map of string description of half second monitors
         * keyed by msgType (message id).
         */
        std::map<carma::canbus::msgType, std::string>
            getHalfSecMonitors() const;

        /**
         * Retrieve a map of this devices slow monitor points.
         * These monitor points will be simulated every 5 seconds if
         * the device is in the OFFLINE state.
         * @return map of string description of slow (5 second) monitors
         * keyed by msgType (message id).
         */
        std::map<carma::canbus::msgType, std::string> getSlowMonitors() const;

        /**
         * Process a CAN message.
         * This routine is responsible for processing all CAN messages
         * addressed to this device.
         * @param mid the 10bit message id (carma::canbus::msgType)
         * @param data refererence to the byte vector containing the raw data.
         */
        void processMsg(carma::canbus::msgType mid,
                std::vector<carma::canbus::byteType> &data, bool sim);

        /**
         * Produce a simulated CAN message for a given msgType.
         * This routine creates a Message with simulated data for the
         * input message id.  The returned message is automatically
         * placed in the CAN message queue for retrieval and processing
         * by the Master class.  When this device is OFFLINE, it is called for
         * each message type returned from Downconverter::getSlowMonitors and
         * Downconverter::getHalfSecMonitors and thus tests the processMsg()
         * method and monitor system. Note that this routine is called by
         * the carma::canbus::Master base class automatically.
         * @param mid Message id of CAN message to simulate.
         * @return Simulated CAN message.
         */
        carma::canbus::Message simulateMsg(carma::canbus::msgType mid);

        /**
         * Staticly retrieve the API Id.
         * This is a helper routine and does the same thing as the getApi
         * member function with the exception that it can be called staticly.
         * @return api id of device.
         */
        static carma::canbus::apiType getApiId();

        /**
         * Update data on a frame time scale
         * This routine is called automatically by carma::canbus::Master
         * every half second.  It is used to set the state monitor point.
         */
        void updateFrameData();

        // Corba control commands
        void initializePowerMeter();

    private:

        // Member variables

        carma::monitor::StateMonitorPointEnum * state_;
        carma::monitor::LoMonitor *mon_;
        carma::monitor::Xac *xacMon_;
        log4cpp::Category &log_; // Reference to logger

        // Member functions

        // Copy and assignment not permitted for this class.
        LoMonitor(const LoMonitor &);
        LoMonitor &operator=(const LoMonitor &);

        // Process Message routines for individual CAN messages.
        // Blanking Frames 1 & 2 are unique but for the remaining 16 packets
        // every other one is identical except for the band it refers to.
        void processBlankingFramePacket1(
            std::vector<carma::canbus::byteType> &data);
        void processBlankingFramePacket2(
            std::vector<carma::canbus::byteType> &data);
        void processLoFrequencyPacket(
            std::vector<carma::canbus::byteType> &data, int loId);
        void processLoStatusPacket(
            std::vector<carma::canbus::byteType> &data, int loId);

        // Methods to produce simulated CAN messages.  These routines are
        // called by simulateMsg and then placed in the CAN message queue
        // where they will eventually be processed by processMsg above.
        carma::canbus::Message simBlankingFramePacket1();
        carma::canbus::Message simBlankingFramePacket2();
        carma::canbus::Message simLoFrequencyPacket(int loId);
        carma::canbus::Message simLoStatusPacket(int loId);

        const bool wideband_;

    };
} // End Namespace downconverter
} // End namespace carma
#endif
