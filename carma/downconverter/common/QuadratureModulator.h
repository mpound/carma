/**@file
 * Carma Quadrature Modulator CAN Device class implementation.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.31 $
 * $Date: 2012/01/31 00:22:03 $
 * $Id: QuadratureModulator.h,v 1.31 2012/01/31 00:22:03 abeard Exp $
 */

#ifndef CARMA_DOWNCONVERTER_QUADRATUREMODULATOR_H
#define CARMA_DOWNCONVERTER_QUADRATUREMODULATOR_H

#include "carma/canbus/devices/XacDevice.h"
#include "carma/util/PthreadMutex.h"

#include <map>
#include <string>
#include <vector>

namespace carma {

namespace monitor {
    class QuadMod;
    class QuadModContainer;
    class StateMonitorPointEnum;
    class Xac;
} // End namespace monitor

namespace downconverter {

    class WalshSequence;

    /**
     * Quadrature modulator Device class implementation.
     * This class implements the carma::canbus::Device base class for
     * <A HREF=http://www.mmarray.org/project/system/CanAPI/Docs/API_064_QuadModVerB.pdf> Carma CANbus API 64</A>
     * describing the Quadrature Modulator CAN device.
     */
	class QuadratureModulator : public carma::canbus::devices::XacDevice {
	public:

        /**
         * Constructor
         * Creates a Quadrature Modulator device with the given node id.
         * @param node Node id of device.
         * @param io Reference to CanOutput class.
         * @param quadModMon Reference to common QuadModContainer class.
         */
		QuadratureModulator(
                carma::canbus::nodeType node,
                carma::canbus::CanOutput &io,
                carma::monitor::QuadModContainer & quadModMon );

        /**
         * Destructor
         */
		virtual ~QuadratureModulator();

        /**
         * Retrieve a map of this devices half second monitor points.
         * These monitor points will be simulated every frame if
         * the device is in the OFFLINE state as described in
         * carma::canbus::Master.
         * @return map of string description of half second monitors
         * keyed by msgType (message id).
         */
        std::map<carma::canbus::msgType, std::string>
            getHalfSecMonitors() const;

        /**
         * Retrieve a map of this devices slow monitor points.
         * These monitor points will be simulated every 5 secondsif
         * the device is in the OFFLINE state as described in
         * carma::canbus::Master.
         * @return map of string description of slow (5 second) monitors
         * keyed by msgType (message id).
         */
        std::map<carma::canbus::msgType, std::string> getSlowMonitors() const;

        /**
         * Process a CAN message addressed to this device.
         * This routine is responsible for processing all CAN messages
         * addressed to this device.
         * @param mid the 10bit message id (carma::canbus::msgType)
         * @param data reference to the byte vector containing the data.
         */
        void processMsg(carma::canbus::msgType mid,
                std::vector<carma::canbus::byteType> &data, bool sim);

        /**
         * Produce a simulated CAN message for a given msgType.
         * This routine creates a Message with simulated data for an
         * input message id.  The returned message is automatically
         * placed in the CAN message queue for retrieval and processing
         * by the Master class.  When this device is OFFLINE, it is called for
         * each message type returned from getSlowMonitors and
         * getHalfSecMonitors and thus tests the processMsg()
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

        void walshTableColumnImminent( unsigned short length,
                                       unsigned char crc );

        void loadWalshTableColumn( unsigned char segmentIndex,
                                   std::vector<unsigned char> segment );

        void setPoutPreset();

        void setPout(double pout);

        void setPoutAtten(unsigned short atten);

        // Enable quadrature modulation and noise gain.
        void enableQuadMod(bool enable);

        // Enable quadrature modulation sans noise gain.
        void enableModulationOnly( bool enable );

        void loadWalshSequence( const WalshSequence & walshSequence );

        bool isEnabled();
        bool checkIfOutPower(double power, double delta);
        bool checkIfInPower(double power, double delta);

    private:

        // Prevent assignment and copy construction.
        QuadratureModulator(const QuadratureModulator &);
        QuadratureModulator &operator=(const QuadratureModulator &);

        // Methods to process individual CAN messages.  These routines are
        // called by processMsg upon receipt of a CAN message.
        void processBlankingFramePacket1( carma::canbus::DataVector & data );
        void processBlankingFramePacket2( carma::canbus::DataVector & data );
        void processBlankingFramePacket3( carma::canbus::DataVector & data );

        // Methods to produce simulated CAN messages.  These routines are
        // called by simulateMsg and then placed in the CAN message queue
        // where they will eventually be processed by processMsg.
        carma::canbus::Message simBlankingFramePacket1();
        carma::canbus::Message simBlankingFramePacket2();
        carma::canbus::Message simBlankingFramePacket3();

        const int inputNo_;   // Corresponds to antenna no (1..8), not index!
        carma::monitor::StateMonitorPointEnum * state_;
        carma::monitor::QuadMod *mon_;
        carma::monitor::Xac *xacMon_;

        double ifInPower_;  // Current IF Input power
        double ifOutPower_; // Current IF Output power
        bool enabled_;      // Is quadrature modulation enabled?
        carma::util::PthreadMutex mutex_;

	}; // End class Quadrature Modulator
} // namespace downconverter
} // namespace carma
#endif
